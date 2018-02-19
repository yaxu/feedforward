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
import Sound.Tidal.Context (superDirtSetters, dirtSetters, ParamPattern, cpsUtils, stack, orbit, (#))
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Process
import TidalHint
import UI.NCurses
import Text.Printf

type Tag = Int

data Status = Success | Error | Normal
            deriving Show

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

lTag :: Line -> Maybe Tag
lTag l = do block <- lBlock l
            return $ bTag block

lActive :: Line -> Bool
lActive (Line {lBlock = Just (Block {bActive = a})}) = a
lActive _ = False

setTag :: Line -> Tag -> Line
setTag l@(Line {lBlock = (Just b)}) tag = l {lBlock = Just (b {bTag = tag})}
setTag l@(Line {lBlock = Nothing}) tag = l {lBlock = Just (Block {bTag = tag, bActive = True, bModified=True, bStatus = Normal})}

type Code = [Line]
type Pos = (Int, Int)

data State = State {sCode :: Code,
                    sPos :: Pos,
                    sXWarp :: Int,
                    sWindow :: Window,
                    sColour :: ColorID,
                    sColourHilite :: ColorID,
                    sColourWarn :: ColorID,
                    -- sHilite :: (Bool, [Int]),
                    sHintIn :: MVar String,
                    sHintOut :: MVar Response,
                    sDirt :: ParamPattern -> IO (),
                    sChangeSet :: ChangeSet,
                    sLogFH :: Handle,
                    sRMS :: [[Float]],
                    sScroll :: (Int,Int)
                   }

offsetY = 2
offsetX = 3

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

goCursor state = moveCursor ((offsetY + (fromIntegral $ fst $ sPos state))-sY) (offsetX + (fromIntegral $ snd $ sPos state))
  where sY = fromIntegral $ fst $ sScroll state

doScroll s (h,w) = do moveCursor 1 0
                      drawString $ "sScroll " ++ show sy' ++ "x" ++ show sx'
                      return $ s {sScroll = (sy',sx')}
  where (y,x) = sPos s
        (sy,sx) = sScroll s
        h' = h - offsetY
        sy' | y < sy = y
            | y >= sy + (fromIntegral h') = (y - (fromIntegral h')) + 1
            | otherwise = sy
        sx' = 0

{-
doScroll' (sy,sx) (y,x) (h,w) = (sy',sx')
  where h' = h - offsetY
        sy' | y < sy = y
            | y >= sy + (fromIntegral h') = (y - (fromIntegral h')) + 1
            | otherwise = sy 
       sx' = 0
-}

draw :: MVar State -> Curses ()
draw mvS
  = do s' <- (liftIO $ readMVar mvS)
       updateWindow (sWindow s') $ do
         clear
         (h,w) <- windowSize
         s <- doScroll s' (h,w)
         setColor (sColour s)

         -- header
         let spaces = ((fromIntegral w) - (length "feedforward")) `div` 2
             -- info = printf " %dx%d" w h
         moveCursor 0 0
         setColor (sColourHilite s)
         drawString $ (replicate spaces ' ') ++ "feedforward" ++ (replicate ((fromIntegral w) - spaces - (length "feedforward")) ' ')
         
         -- let blocks = filter ((< (fromIntegral $ h-offsetY)) . fst) $ activeBlocks 0 $ sCode s
         -- mapM_ (drawRMS s w) blocks
         mapM_ (drawLine s) $ zip [offsetY..] $ take (fromIntegral $ (h - offsetY) ) $ drop (fst $ sScroll s) $ zip (sCode s) [0 ..]
         goCursor s
         return ()
  where drawLine :: State -> (Integer, (Line, Integer)) -> Update ()
        drawLine s (y, (l, n)) =
          do moveCursor y offsetX
{-             if elem (fromIntegral n) (snd $ sHilite s)
               then setColor (if (fst $ sHilite s)
                              then sColourHilite s
                              else sColourWarn s
                             )
               else setColor (sColour s)
-}
             setColor (sColour s)
             drawString (lText l)
             moveCursor y 0
             drawString str
               where str | isJust (lTag l) = (show $ fromJust (lTag l)) ++ "|"
                         | hasChar l = " |"
                         | otherwise = ""
        drawRMS s w (y,ls) = do let rmsw = ((fromIntegral w) / 2)
                                    id = fromJust $ lTag $ head ls
                                    rms = map ((min rmsw) . (1000 *)) $ map (!! id) (sRMS s) 
                                    chars = "█▓▒░"
                                    sizes = map (\(s,e) -> (e-s)) $ zip (0:rms) rms
                                    bar = concatMap (\(sz,c) -> replicate (floor sz) c) $ zip sizes chars
                                    str = bar ++ (replicate ((floor rmsw) - (length bar)) ' ')
                                setColor (sColour s)
                                moveCursor (fromIntegral y + offsetY - 1) 0
                                drawString $ (reverse str) ++ str

initState :: Window -> ColorID -> ColorID -> ColorID -> MVar String -> MVar Response -> (ParamPattern -> IO ()) -> Handle -> State
initState w fg bg warn mIn mOut d logFH
  = State {sCode = [Line (Just $ Block 0 True True Normal) "sound \"bd sn\""],
           sPos = (0,0),
           sWindow = w,
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
           sRMS = replicate 10 $ replicate 4 0,
           sScroll = (0,0)
          }

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
listenRMS mvS = do x <- udpServer "127.0.0.1" 6010
                   loop x
  where
    loop x = 
      do m <- recvMessage x
         act m
         loop x
    act (Just m) = do let xs = map (fromMaybe 0 . datum_floating) $ messageDatum m
                      s <- takeMVar mvS
                      putMVar mvS $ s {sRMS = xs:(take 3 $ sRMS s)}
    act _ = return ()

main :: IO ()
main = do runCurses $ do
            setEcho False
            w <- defaultWindow
            updateWindow w clear
            fg <- newColorID ColorWhite ColorBlack 1
            bg <- newColorID ColorBlack ColorWhite 2
            warn <- newColorID ColorRed ColorBlack 3
            mIn <- liftIO newEmptyMVar
            mOut <- liftIO newEmptyMVar
            liftIO $ forkIO $ hintJob (mIn, mOut)
            (_, getNow) <- liftIO cpsUtils
            (d, _) <- liftIO (dirtSetters getNow)
            logFH <- liftIO openLog
            mvS <- (liftIO $ newMVar $ initState w fg bg warn mIn mOut d logFH)
            liftIO $ forkIO $ listenRMS mvS
            draw mvS
            render
            mainLoop mvS

mainLoop mvS = loop where
  loop = do draw mvS
            render
            s <- liftIO (readMVar mvS)
            ev <- getEvent (sWindow s) (Just 50)
            case ev of
             Nothing -> loop
             Just (EventCharacter x) -> if x == chr(27)
                                        then return ()
                                        else keypress mvS x >> loop
             Just (EventSpecialKey KeyUpArrow) -> move mvS (-1,0) >> loop
             Just (EventSpecialKey KeyDownArrow) -> move mvS (1,0) >> loop
             Just (EventSpecialKey KeyLeftArrow) -> move mvS (0,-1) >> loop
             Just (EventSpecialKey KeyRightArrow) -> move mvS (0,1) >> loop
             Just (EventSpecialKey KeyHome) -> moveHome mvS >> loop
             Just (EventSpecialKey KeyEnd) -> moveEnd mvS >> loop
             Just (EventSpecialKey KeyEnter) -> insertBreak mvS >> loop
             Just (EventSpecialKey KeyDeleteCharacter) -> del mvS >> loop
             Just (EventSpecialKey KeyBackspace) -> backspace mvS >> loop
             Just (EventMouse _ ms) -> mouse mvS ms >> loop
             Just e -> do liftIO $ hPutStrLn stderr $ show e
                          loop

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

keyCtrl mvS _ = return ()

mouse mvS (MouseState {mouseCoordinates = (x,y,_), mouseButtons = [(1, ButtonClicked)]}) = moveTo mvS (fromIntegral (max (y-offsetY) 0),fromIntegral (max (x-offsetX) 0))
mouse _ _ = return ()

{-
keyCtrl mvS c = do s <- (liftIO $ readMVar mvS)
                   updateWindow (sWindow s) $ do
                     moveCursor 18 10
                     drawString $ show c
-}

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
     updateWindow (sWindow s) clear

insertChar :: MVar State -> Char -> Curses ()
insertChar mvS c =
  do s <- (liftIO $ takeMVar mvS)
     let (y,x) = sPos s
         (y',x') = (y,x+1)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let change = (insertChange (y,x) [[c]]) {cWhen = now}
     liftIO $ applyChange mvS (s {sXWarp = x'}) change
     updateWindow (sWindow s) clear

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
     updateWindow (sWindow s) clear

del :: MVar State -> Curses ()
del mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), _, l, _, _, _) = cursorContext s
         change | x < (length $ lText l) = Just $ (deleteChange (y,x) (y,x+1) [[charAt ls (y,x)]]) {cWhen = now}
                | y == ((length ls) - 1) = Nothing
                | otherwise = Just $ (deleteChange (y,x) (y+1,0) ["",""]) {cWhen = now}
     liftIO $ maybe (putMVar mvS s) (applyChange mvS s) change
     updateWindow (sWindow s) clear

killLine :: MVar State -> Curses ()
killLine mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), _, l, _, _, postX) = cursorContext s
         change | x < (length $ lText l) = Just $ deleteChange (y,x) (y,(length $ lText l)) [postX]
                | y == ((length ls) - 1) = Nothing
                | otherwise = Just $ deleteChange (y,x) (y+1,0) ["",""]
     liftIO $ maybe (putMVar mvS s) (applyChange mvS s) change
     updateWindow (sWindow s) clear

eval :: MVar State -> Curses ()
eval mvS = 
  do s <- (liftIO $ takeMVar mvS)
     let blocks = activeBlocks 0 $ sCode s
     
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
                              return (s', ps')
  where act id (HintOK p) b = (b {bStatus = Success, bModified = False}, (p # orbit (pure id)):ps)
        act _ (HintError err) b = (b {bStatus = Error}, ps)
        setBlock n block = s {sCode = ls}
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

{-
eval' :: MVar State -> Curses ()
eval' mvS = 
  do s <- (liftIO $ takeMVar mvS)
     let (y,_) = sPos s
         ls = sCode s
         block | hasChar (ls !! y) = findBlock
               | otherwise = []
         findChars = takeWhile (hasChar . (ls !!))
         pre = reverse $ findChars $ reverse [0 .. y]
         post | y == ((length ls) - 1) = []
              | otherwise = findChars [y+1 .. ((length ls) - 1)]
         findBlock = pre ++ post
         codeblock = intercalate "\n" (map (lText . (ls !!)) findBlock)
     liftIO $ putMVar (sHintIn s) codeblock
     response <- liftIO $ takeMVar (sHintOut s)
     ok <- act s response
     liftIO $ putMVar mvS $ s {sHilite = (ok, findBlock)}
     draw mvS
  where
    act s (HintOK p) = do liftIO $ (sDirt s) p
                          return True
    act s (HintError err) =
      do updateWindow (sWindow s) $ do
           moveCursor 15 0
           drawString $ show err
         return False

-}
