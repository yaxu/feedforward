{- Feedforward (c) Alex McLean 2018
   Text editor for TidalCycles
   https://github.com/yaxu/feedforward
   Distributed under the terms of the GNU Public License 3.0, see LICENSE
-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (foldM)
import Control.Monad.IO.Class
import Data.Char
import Data.List (intercalate, (\\))
import Data.Maybe (fromMaybe, catMaybes, isJust, fromJust)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Format
import Sound.OSC.FD
import Sound.Tidal.Context (superDirtSetters, dirtSetters, ParamPattern, cpsUtils, stack, orbit, (#), cpsUtils')
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Process
import TidalHint
import UI.NCurses
import Text.Printf
import qualified Network.Socket as N

type Tag = Int

data Status = Success | Error | Normal
            deriving (Show, Eq)

data Block = Block {bTag :: Tag,
                    bActive :: Bool,
                    bModified :: Bool,
                    bStatus :: Status
                   }
             deriving Show

data Line = Line {lBlock :: Maybe Block,
                  lText :: String
                 }
             deriving Show

data Dirt = Classic | Super
          deriving Eq

dirt = Super

lTag :: Line -> Maybe Tag
lTag l = do block <- lBlock l
            return $ bTag block

lActive :: Line -> Bool
lActive (Line {lBlock = Just (Block {bActive = a})}) = a
lActive _ = False

lStatus :: Line -> Maybe Status
lStatus l = do block <- lBlock l
               return $ bStatus block

setTag :: Line -> Tag -> Line
setTag l@(Line {lBlock = (Just b)}) tag = l {lBlock = Just (b {bTag = tag})}
setTag l@(Line {lBlock = Nothing}) tag = l {lBlock = Just (Block {bTag = tag, bActive = True, bModified=True, bStatus = Normal})}

type Code = [Line]
type Pos = (Int, Int)
type CpsUtils = ((Double -> IO (), (Double -> IO ()), IO Rational))

data Mode = EditMode | FileMode | PlaybackMode

data State = State {sCode :: Code,
                    sPos :: Pos,
                    sXWarp :: Int,
                    sEditWindow :: Window,
                    sFileWindow :: Window,
                    sColour :: ColorID,
                    sColourHilite :: ColorID,
                    sColourWarn :: ColorID,
                    sHintIn :: MVar String,
                    sHintOut :: MVar Response,
                    sDirt :: ParamPattern -> IO (),
                    sChangeSet :: ChangeSet,
                    sLogFH :: Handle,
                    sRMS :: [Float],
                    sScroll :: (Int,Int),
                    sCpsUtils :: CpsUtils,
                    sMode :: Mode
                    -- sFileSel :: (Maybe Int, Maybe Int, Maybe Int)
                   }

topMargin    = 1 :: Integer
bottomMargin = 2 :: Integer
leftMargin   = 3 :: Integer
rightMargin  = 0 :: Integer

{- Fires every time the content of the editor is changed. The changeObj
is a {from, to, text, removed, origin} object containing information
about the changes that occurred as second argument. from and to are
the positions (in the pre-change coordinate system) where the change
started and ended (for example, it might be {ch:0, line:18} if the
position is at the beginning of line #19). text is an array of strings
representing the text that replaced the changed range (split by
line). removed is the text that used to be between from and to, which
is overwritten by this change. This event is fired before the end of
an operation, before the DOM updates happen.
-}

data Change = Change {cFrom :: Pos,
                      cTo :: Pos,
                      cText :: [String],
                      cRemoved :: [String],
                      cOrigin :: String,
                      cWhen :: Double,
                      cNewPos :: Pos
                     }
            deriving Show

type ChangeSet = [Change]

hasChar :: Line -> Bool
hasChar = or . map (/= ' ') . lText

updateTags :: Code -> Code
updateTags ls = assignTags freeTags ls'
  where assignTags :: [Tag] -> Code -> Code
        assignTags [] (l:ls) = (l:ls)
        assignTags _ [] = []
        assignTags ids (l:ls) | lTag l == Just (-1) = (setTag l (head ids)):(assignTags (tail ids) ls)
                              | otherwise = l:(assignTags ids ls)
        freeTags = [0 .. 9] \\ tagIds
        tagIds = catMaybes $ map lTag ls'
        ls' = map tag toTag
        tag :: (Bool, Line) -> Line
        tag (False, l) = l {lBlock = Nothing}
        tag (True, l) | isJust (lTag l) = l
                      | otherwise = setTag l (-1) -- mark to tag
        toTag :: [(Bool, Line)]
        toTag = taggable True ls
        taggable :: Bool -> Code -> [(Bool, Line)]
        taggable _ [] = []
        taggable prevEmpty (l:ls) = (prevEmpty && (not empty), l):(taggable empty ls)
          where empty = not $ hasChar l

applyChange :: MVar State -> State -> Change -> IO ()
applyChange mvS s change = do putMVar mvS s'
                              writeLog s change
  where ls | (cOrigin change) == "+input" = updateTags $ applyInput s change
           | (cOrigin change) == "+delete" = updateTags $ applyDelete s change
           | otherwise = sCode s
        changes = sChangeSet s
        s' = s {sChangeSet = change:changes,
                sCode = ls,
                sPos = cNewPos change
               }

withLineText :: Line -> (String -> String)  -> Line
withLineText (Line tag text) f = Line tag (f text )

applyInput :: State -> Change -> Code
applyInput s change = preL ++ added ++ postL
  where (ls, (y,x), preL, l, postL, preX, postX) = cursorContext' s (cFrom change)
        added :: Code
        added = addToHead preX $ addToLast postX $ map (Line Nothing) (cText change)
        addToHead :: String -> Code -> Code
        addToHead x xs = (withLineText (head xs) (x ++)) : tail xs
        addToLast :: String -> Code -> Code
        addToLast x xs = init xs ++ [withLineText (last xs) (++ x)]

applyDelete :: State -> Change -> Code
applyDelete s change = preL ++ ((Line Nothing $ preX ++ postX):postL)
  where (_, _, preL, _, _, preX, _) = cursorContext' s (cFrom change)
        (_, _, _, _, postL, _, postX) = cursorContext' s (cTo change)

insertChange :: Pos -> [String] -> Change
insertChange (y,x) str = Change {cFrom = (y,x),
                                 cTo = (y,x),
                                 cText = str,
                                 cRemoved = [""],
                                 cOrigin = "+input",
                                 cWhen = -1,
                                 cNewPos = (y',x')
                                }
  where y' = y + ((length str) - 1)
        x' | length str == 1 = x + (length $ head str)
           | otherwise = length $ last str


deleteChange :: Pos -> Pos -> [String] -> Change
deleteChange from to removed = Change {cFrom = from,
                                       cTo = to,
                                       cText = [""],
                                       cRemoved = removed,
                                       cOrigin = "+delete",
                                       cWhen = -1,
                                       cNewPos = from
                                      }

goCursor state = moveCursor ((topMargin + (fromIntegral $ fst $ sPos state))-sY) ((leftMargin + (fromIntegral $ snd $ sPos state)) - sX)
  where sY = fromIntegral $ fst $ sScroll state
        sX = fromIntegral $ snd $ sScroll state

doScroll s (h,w) = s {sScroll = (sy',sx')}
  where (y,x) = sPos s
        (sy,sx) = sScroll s
        h' = h - (topMargin + bottomMargin)
        w' = w - (leftMargin + rightMargin)
        sy' | y < sy = y
            | y >= sy + (fromIntegral h') = (y - (fromIntegral h')) + 1
            | otherwise = sy
        sx' | x < sx = x
            | x >= sx + (fromIntegral w') = (x - (fromIntegral w')) + 1
            | otherwise = sx

drawFooter :: State -> Curses ()
drawFooter s =
  do mc <- maxColor
     updateWindow (sEditWindow s) $
       do (h,w) <- windowSize
          moveCursor (h-2) 0
          setColor $ sColourHilite s
          let str = " " ++ show (sPos s)
          drawString $ str ++ replicate ((fromIntegral w) - (length str)) ' '

rmsBlocks = " ▁▂▃▄▅▆▇█"

draw :: MVar State -> Curses ()
draw mvS
  = do s <- (liftIO $ takeMVar mvS)
       s'' <- updateWindow (sEditWindow s) $ do
         clear
         (h,w) <- windowSize
         let s' = doScroll s (h,w)
         setColor (sColour s')
         mapM_ (drawLine s w) $ zip [topMargin..] $ take (fromIntegral $ h - (topMargin + bottomMargin)) $ drop (fst $ sScroll s') $ zip (sCode s) [0 ..]
         return s'
       drawFooter s''
       updateWindow (sEditWindow s) $ goCursor s''
       liftIO $ putMVar mvS s''
  where drawLine :: State -> Integer -> (Integer, (Line, Integer)) -> Update ()
        drawLine s w (y, (l, n)) =
          do let scrollX = snd $ sScroll s
                 skipLeft = drop scrollX $ lText l
                 skipBoth = take (fromIntegral $ w - (leftMargin + rightMargin + 1)) $ skipLeft
             moveCursor y leftMargin
             setColor (sColour s)
             drawString skipBoth

             setColor $ sColourHilite s
             if scrollX > 0
               then do moveCursor y leftMargin
                       drawString "<"
               else return ()
             if length skipLeft > length skipBoth
                then do moveCursor y (w-1)
                        drawString ">"
               else return ()
                    
             moveCursor y 0
             setColor $ sColour s
             lineHead
             drawRMS s w (y-1) l
               where lineHead | isJust (lTag l) = do let c | lStatus l == (Just Error) = setColor $ sColourWarn s
                                                           | lStatus l == (Just Success) = setAttribute AttributeBold True
                                                           | otherwise = setColor $ sColour s
                                                     
                                                     moveCursor y 0
                                                     c
                                                     drawString $ (show $ fromJust (lTag l))
                                                     setAttribute AttributeBold False
                                                     setColor $ sColour s
                                                     drawString "│"
                              | hasChar l = do setColor $ sColour s
                                               moveCursor y 0
                                               drawString " │"
                              | otherwise = return ()
        drawRMS s w y l | lActive l = do let rmsMax = (length rmsBlocks) - 1
                                             id = fromJust $ lTag l
                                             rmsL = min rmsMax $ floor $ 500 * ((sRMS s) !! (id*2))
                                             rmsR = min rmsMax $ floor $ 500 * ((sRMS s) !! (id*2+1))
                                             str = (rmsBlocks !! rmsL):(rmsBlocks !! rmsR):[]
                                         setColor (sColour s)
                                         moveCursor (fromIntegral y + topMargin - 1) 0
                                         drawString $ str
                        | otherwise = return ()

initState :: Curses (MVar State)
initState
  = do w <- defaultWindow
       updateWindow w clear
       fg <- newColorID ColorWhite ColorDefault 1
       bg <- newColorID ColorBlack ColorWhite 2
       warn <- newColorID ColorWhite ColorRed 3
       fileWindow <- newWindow 10 20 3 3
       mIn <- liftIO newEmptyMVar
       mOut <- liftIO newEmptyMVar
       liftIO $ forkIO $ hintJob (mIn, mOut)
       (_, getNow) <- liftIO cpsUtils
       cpsUtils <- liftIO cpsUtils'
       let setters = case dirt of
                      Classic -> dirtSetters
                      Super -> superDirtSetters
       (d, _) <- liftIO (setters getNow)
       logFH <- liftIO openLog
       mvS <- liftIO $ newMVar $ State {sCode = [Line (Just $ Block 0 True True Normal) "sound \"bd sn\""],
                                        sPos = (0,0),
                                        sEditWindow = w,
                                        sFileWindow = fileWindow,
                                        sXWarp = 0,
                                        sColour = fg,
                                        sColourHilite = bg,
                                        sColourWarn = warn,
                                        -- sHilite = (False, []),
                                        sHintIn = mIn,
                                        sHintOut = mOut,
                                        sDirt = d,
                                        sChangeSet = [],
                                        sLogFH = logFH,
                                        sRMS = replicate 20 0,
                                        sScroll = (0,0),
                                        sCpsUtils = cpsUtils,
                                        sMode = EditMode
                                       }
       return mvS

moveHome :: MVar State -> Curses ()
moveHome mvS = do s <- liftIO (readMVar mvS)
                  let (_, x) = sPos s
                  move mvS (0, 0-x)

moveEnd :: MVar State -> Curses ()
moveEnd mvS = do s <- liftIO (readMVar mvS)
                 let (y, x) = sPos s
                     xTo = length (lText $ (sCode s) !! y)
                 move mvS (0, xTo-x)

move :: MVar State -> (Int, Int) -> Curses ()
move mvS (yd,xd) = do s <- liftIO (takeMVar mvS)
                      let maxY = (length $ sCode s) - 1
                          (y,x) = sPos s
                          y' = max 0 $ min maxY (y + yd)
                          maxX | (length $ sCode s) == y' = 0
                               | otherwise = length $ lText $ (sCode s) !! y'
                          x' = max 0 $ min maxX (x + xd)
                          xw | xd /= 0 = x'
                             | otherwise = sXWarp s
                          x'' = min xw maxX
                      liftIO $ putMVar mvS $ s {sPos = (y',x''),
                                                sXWarp = xw
                                                -- sHilite = (False, [])
                                               }


moveTo :: MVar State -> (Int, Int) -> Curses ()
moveTo mvS (y,x) = do s <- liftIO (takeMVar mvS)
                      let maxY = (length $ sCode s) - 1
                          y' = min maxY y
                          maxX = length $ lText $ (sCode s) !! y'
                          x' = min maxX x
                      liftIO $ putMVar mvS $ s {sPos = (y',x'),
                                                sXWarp = x'
                                                -- sHilite = (False, [])
                                               }

openLog :: IO Handle
openLog = do t <- getZonedTime
             id <- getProcessID
             let d = formatTime defaultTimeLocale "%Y%m%dT%H%M%S" t
                 filePath = logDirectory </> d ++ "-" ++ (show id) ++ ".txt"
             createDirectoryIfMissing True logDirectory
             openFile filePath WriteMode
  where logDirectory = "logs"

writeLog :: State -> Change -> IO ()
writeLog s c = do hPutStrLn (sLogFH s) (show c)
                  hFlush (sLogFH s)

listenRMS :: MVar State -> IO ()
listenRMS mvS = do udp <- udpServer "127.0.0.1" 6010
                   subscribe udp
                   loop udp
  where
    loop udp = 
      do m <- recvMessage udp
         act m
         loop udp
    act (Just (m@(Message "/rmsall" _))) =
      do let xs = map (fromMaybe 0 . datum_floating) $ messageDatum m
         s <- takeMVar mvS
         putMVar mvS $ s {sRMS = xs}
    act (Just (m@(Message "/rms" _))) =
      do s <- takeMVar mvS
         let orbit = fromMaybe 0 $ datum_integral $ messageDatum m !! 1
             l = fromMaybe 0 $ datum_floating $ messageDatum m !! 3
             r = fromMaybe 0 $ datum_floating $ messageDatum m !! 5
             rms = sRMS s
             rms' = take (orbit*2) rms ++ [l,r] ++ drop ((orbit*2)+2) rms
         putMVar mvS $ s {sRMS = rms'}
    act _ = return ()
    subscribe udp | dirt == Super =
                      do remote_addr <- N.inet_addr "127.0.0.1"
                         let remote_sockaddr = N.SockAddrInet 57110 remote_addr
                         sendTo udp (Message "/notify" [int32 1]) remote_sockaddr
                  | otherwise = return ()

main :: IO ()
main = do runCurses $ do
            setEcho False
            mvS <- initState
            liftIO $ forkIO $ listenRMS mvS
            draw mvS
            render
            mainLoop mvS


handleEv mvS EditMode ev =
  do let quit = return True
         ok = return False
     case ev of
      Nothing -> ok
      Just (EventCharacter x) -> if x == chr(27)
                                 then quit
                                 else keypress mvS x >> ok
      Just (EventSpecialKey KeyUpArrow) -> move mvS (-1,0) >> ok
      Just (EventSpecialKey KeyDownArrow) -> move mvS (1,0) >> ok
      Just (EventSpecialKey KeyLeftArrow) -> move mvS (0,-1) >> ok
      Just (EventSpecialKey KeyRightArrow) -> move mvS (0,1) >> ok
      Just (EventSpecialKey KeyHome) -> moveHome mvS >> ok
      Just (EventSpecialKey KeyEnd) -> moveEnd mvS >> ok
      Just (EventSpecialKey KeyEnter) -> insertBreak mvS >> ok
      Just (EventSpecialKey KeyDeleteCharacter) -> del mvS >> ok
      Just (EventSpecialKey KeyBackspace) -> backspace mvS >> ok
      Just (EventMouse _ ms) -> mouse mvS ms >> ok
      Just e -> do liftIO $ hPutStrLn stderr $ show e
                   ok
handleEv mvS FileMode Nothing = ok
handleEv mvS FileMode (Just (EventCharacter x))
  | x == chr 27 = do s <- liftIO $ takeMVar mvS
                     liftIO $ putMVar mvS $ s {sMode = EditMode}
                     ok
  | otherwise = ok
                
handleEv mvS FileMode (Just e) = do liftIO $ hPutStrLn stderr $ show e
                                    ok


quit :: Curses Bool
quit = return True

ok :: Curses Bool
ok = return False

mainLoop mvS = loop where
  loop = do s <- liftIO (readMVar mvS)

            case sMode s of
             EditMode     -> draw mvS
             FileMode     -> return ()
             PlaybackMode -> return ()
            render
             
            ev <- getEvent (sEditWindow s) (Just 50)
            done <- handleEv mvS (sMode s) ev
            if done
              then return ()
              else loop

-- emacs movement
keyCtrl mvS 'a' = moveHome mvS
keyCtrl mvS 'e' = moveEnd mvS
keyCtrl mvS 'n' = move mvS (1,0)
keyCtrl mvS 'p' = move mvS (-1,0)
keyCtrl mvS 'b' = move mvS (0,-1)
keyCtrl mvS 'f' = move mvS (0,1)

keyCtrl mvS 'd' = del mvS
keyCtrl mvS 'k' = killLine mvS

keyCtrl mvS 'j' = insertBreak mvS

keyCtrl mvS 'x' = eval mvS

keyCtrl mvS 'l' = fileMode mvS

keyCtrl mvS _ = return ()

{-
keyCtrl mvS c = do s <- (liftIO $ readMVar mvS)
                   updateWindow (sEditWindow s) $ do
                     moveCursor 18 10
                     drawString $ show c
-}

mouse mvS (MouseState {mouseCoordinates = (x,y,_), mouseButtons = [(1, ButtonClicked)]}) = moveTo mvS (fromIntegral (max (y-topMargin) 0),fromIntegral (max (x-leftMargin) 0))
mouse _ _ = return ()

keypress mvS c | isCtrl = keyCtrl mvS (chr $ (ord c) + 96)
               | otherwise = insertChar mvS c
  where isCtrl = ord(c) >= 1 && ord(c) <= 26


cursorContext :: State -> (Code, Pos, Code, Line, Code, String, String)
cursorContext s = cursorContext' s (sPos s)

cursorContext' :: State -> Pos -> (Code, Pos, Code, Line, Code, String, String)
cursorContext' s (y,x) =
  (ls, (y,x), preL, l, postL, preX, postX)
  where  ls = sCode s
         preL = take y ls
         l = head $ drop y ls
         postL = drop (y+1) ls
         preX = take x $ lText l
         postX = drop x $ lText l
     

insertBreak :: MVar State -> Curses ()
insertBreak mvS =
  do s <- (liftIO $ takeMVar mvS)
     let (y,x) = sPos s
         (y',x') = (y+1,0)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let change = (insertChange (y,x) ["",""]) {cWhen = now}
     liftIO $ applyChange mvS (s {sXWarp = 0}) change
     updateWindow (sEditWindow s) clear

insertChar :: MVar State -> Char -> Curses ()
insertChar mvS c =
  do s <- (liftIO $ takeMVar mvS)
     let (y,x) = sPos s
         (y',x') = (y,x+1)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let change = (insertChange (y,x) [[c]]) {cWhen = now}
     liftIO $ applyChange mvS (s {sXWarp = x'}) change
     updateWindow (sEditWindow s) clear

backspaceChar :: State -> State
backspaceChar s =
  s {sCode = ls',
     sPos = (y',x'),
     sXWarp = x'
    }
  where (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
        (y',x') = (y,max 0 (x-1))
        l' | x == 0 = Line Nothing postX
           | otherwise = Line Nothing $ (take ((length preX) - 1) preX) ++ postX
        ls' = preL ++ (l':postL)

charAt :: Code -> (Int,Int) -> Char
charAt ls (y,x) = (lText $ ls !! y) !! x

lineLength :: Code -> Int -> Int
lineLength ls y = length $ lText $ ls !! y

backspace :: MVar State -> Curses ()
backspace mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (y,x) = sPos s
         ls = sCode s
         change | x > 0 = (Just $ (deleteChange (y,x-1) (y,x) [[charAt ls (y,x-1)]]) {cWhen = now})
                | y == 0 = Nothing
                | otherwise = Just $ (deleteChange (y-1,
                                                    lineLength ls (y-1)
                                                   ) (y, x) ["", ""]
                                     ) {cWhen = now}
     liftIO $ maybe (putMVar mvS s) (applyChange mvS s) change
     updateWindow (sEditWindow s) clear

del :: MVar State -> Curses ()
del mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), _, l, _, _, _) = cursorContext s
         change | x < (length $ lText l) = Just $ (deleteChange (y,x) (y,x+1) [[charAt ls (y,x)]]) {cWhen = now}
                | y == ((length ls) - 1) = Nothing
                | otherwise = Just $ (deleteChange (y,x) (y+1,0) ["",""]) {cWhen = now}
     liftIO $ maybe (putMVar mvS s) (applyChange mvS s) change
     updateWindow (sEditWindow s) clear

killLine :: MVar State -> Curses ()
killLine mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), _, l, _, _, postX) = cursorContext s
         change | x < (length $ lText l) = Just $ deleteChange (y,x) (y,(length $ lText l)) [postX]
                | y == ((length ls) - 1) = Nothing
                | otherwise = Just $ deleteChange (y,x) (y+1,0) ["",""]
     liftIO $ maybe (putMVar mvS s) (applyChange mvS s) change
     updateWindow (sEditWindow s) clear

fileMode :: MVar State -> Curses ()
fileMode mvS = do s <- liftIO $ takeMVar mvS
                  let s' = s {sMode = FileMode}
                      fileWindow = sFileWindow s'
                  updateWindow fileWindow $
                    do clear
                       (h,w) <- windowSize
                       moveCursor 0 0
                       drawString "File stuff goes here."
                  liftIO $ hPutStrLn stderr $ "bah"
                  liftIO $ putMVar mvS s'

eval :: MVar State -> Curses ()
eval mvS = 
  do s <- (liftIO $ takeMVar mvS)
     let blocks = activeBlocks 0 $ sCode s
     liftIO $ hPutStrLn stderr $ "eval"
     liftIO $ do (s',ps) <- foldM evalBlock (s, []) blocks
                 (sDirt s) (stack ps)
                 putMVar mvS s'
     return ()


evalBlock :: (State, [ParamPattern]) -> (Int, Code) -> IO (State, [ParamPattern])
evalBlock (s,ps) (n, ls) = do let code = intercalate "\n" (map lText ls)
                                  id = fromJust $ lTag $ head ls
                              liftIO $ putMVar (sHintIn s) code
                              response <- liftIO $ takeMVar (sHintOut s)
                              
                              let block = fromJust $ lBlock $ (sCode s) !! n
                                  (block', ps') = act id response block
                                  s' = setBlock n block'
                              hPutStrLn stderr $ show $ block'
                              hPutStrLn stderr $ show $ sCode s'
                              return (s', ps')
  where act id (HintOK p) b = (b {bStatus = Success, bModified = False}, (p # orbit (pure id)):ps)
        act _ (HintError err) b = (b {bStatus = Error}, ps)
        setBlock n block = s {sCode = ls'}
          where ls = sCode s
                l = (ls !! n) {lBlock = Just block}
                ls' = take n ls ++ (l:(drop (n+1) ls))


activeBlocks :: Int -> Code -> [(Int, Code)]
activeBlocks _ [] = []
activeBlocks n (l:ls) | not (hasChar l) = activeBlocks (n+1) ls
                      | lActive l = (n,b):(activeBlocks (n+(length b)+1) ls')
                      | otherwise = activeBlocks (n+1) ls
  where b = takeWhile hasChar (l:ls)
        ls' = drop (length b) ls

scSub = do udp <- udpServer "127.0.0.1" 0
           remote_addr <- N.inet_addr "127.0.0.1"
           let remote_sockaddr = N.SockAddrInet 57110 remote_addr
           sendTo udp (Message "/notify" []) remote_sockaddr
           loop udp
  where loop udp = do m <- recvMessage udp
                      putStrLn $ show m
                      loop udp
