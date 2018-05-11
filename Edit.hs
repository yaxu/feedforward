
{- Feedforward (c) Alex McLean 2018
   Text editor for TidalCycles
   https://github.com/yaxu/feedforward
   Distributed under the terms of the GNU Public License 3.0, see LICENSE
-}

{-# LANGUAGE DeriveGeneric #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (foldM, filterM, forever)
import Control.Monad.IO.Class
import Data.Char
import Data.List (intercalate, (\\), elemIndex, inits, sort)
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
import System.Posix.Signals
import System.Environment (getArgs,lookupEnv)
import TidalHint
import UI.NCurses
import Text.Printf
import qualified Network.Socket as N
import qualified Network.WebSockets as WS
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as T

import qualified Data.Aeson as A
import GHC.Generics

type Tag = Int

data Status = Success | Error | Normal
            deriving (Show, Eq)

data Block = Block {bTag :: Tag,
                    bActive :: Bool,
                    bModified :: Bool,
                    bStatus :: Status,
                    bPattern :: Maybe ParamPattern
                   }
             deriving Show

data Line = Line {lBlock :: Maybe Block,
                  lText :: String
                 }
             deriving Show

data Dirt = Classic | Super
          deriving Eq

dirt = Classic

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
setTag l@(Line {lBlock = Nothing}) tag = l {lBlock = Just (Block {bTag = tag, bActive = True, bModified=True,
                                                                  bStatus = Normal, bPattern = Nothing
                                                                 }
                                                          )
                                           }

type Code = [Line]
type Pos = (Int, Int)
type CpsUtils = ((Double -> IO (), (Double -> IO ()), IO Rational))

data Mode = EditMode | FileMode | PlaybackMode

data FileChoice = FileChoice {fcPath :: [FilePath],
                              fcIndex :: Int,
                              fcDirs :: [FilePath],
                              fcFiles :: [FilePath]
                             }

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
                    sMode :: Mode,
                    sFileChoice :: FileChoice,
                    sCircle :: Maybe (Change -> IO ())
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
            | Eval {cWhen :: Double}
            deriving (Show, Generic)

instance A.ToJSON Change
instance A.FromJSON Change

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
applyChange mvS s (change@(Change {})) = do putMVar mvS s'
                                            writeLog s change
  where ls | (cOrigin change) == "+input" = updateTags $ applyInput s change
           | (cOrigin change) == "+delete" = updateTags $ applyDelete s change
           | otherwise = sCode s
        changes = sChangeSet s
        s' = s {sChangeSet = change:changes,
                sCode = ls,
                sPos = cNewPos change
               }

applyChange mvS s change@(Eval {}) = do
  do let blocks = activeBlocks 0 $ sCode s
     hPutStrLn stderr $ "eval"
     do (s',ps) <- foldM evalBlock (s, []) blocks
        (sDirt s) (stack ps)
        writeLog s' change
        putMVar mvS s'
     return ()

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

evalChange :: Change
evalChange = Eval {cWhen = -1}

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

drawEditor :: MVar State -> Curses ()
drawEditor mvS
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

connectCircle :: IO (Maybe (Change -> IO ()))
connectCircle =
  do addr <- fromMaybe "127.0.0.1" <$> lookupEnv "CIRCLE_ADDR"
     port <- fromMaybe "6010" <$> lookupEnv "CIRCLE_PORT"
     name <- lookupEnv "CIRCLE_NAME"
     hPutStrLn stderr "connectcircle"
     if isJust name
       then do mChange <- newEmptyMVar 
               forkIO $ WS.runClient addr (read port) "/" (app (fromJust name) mChange)
               return $ Just $ putMVar (mChange :: MVar Change)
       else (return Nothing)
       where app name mChange conn =
               do -- hPutStrLn stderr "Connected!"
                  let msg = T.pack $ "/name " ++ name
                  WS.sendTextData conn msg
                  
                  forkIO $ forever $ do
                    msg <- WS.receiveData conn
                    liftIO $ T.putStrLn msg
                  let loop = do
                        change <- takeMVar mChange
                        WS.sendTextData conn (T.append (T.pack "/change ") $ decodeUtf8 $ A.encode $ change) >> loop
                  loop
                  WS.sendClose conn (T.pack "/quit")

initState :: [String] -> Curses (MVar State)
initState args
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
       circle <- liftIO connectCircle
       mvS <- liftIO $ newMVar $ State {sCode = [Line (Just $ Block 0 True True Normal Nothing) "sound \"bd sn\""],
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
                                        sMode = EditMode,
                                        sFileChoice = FileChoice {fcPath = [],
                                                                  fcIndex = 0,
                                                                  fcDirs = [],
                                                                  fcFiles = []
                                                                 },
                                        sCircle = circle
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
             let datePath = formatTime defaultTimeLocale ("%Y" </> "%m" </> "%d") t
                 time = formatTime defaultTimeLocale "%H%M%S" t
                 filePath = logDirectory </> datePath </> time ++ "-" ++ (show id) ++ ".txt"
             createDirectoryIfMissing True (logDirectory </> datePath)
             openFile filePath WriteMode

logDirectory = "logs"

defaultLogPath = do t <- getZonedTime
                    let y = formatTime defaultTimeLocale "%Y" t
                        m = formatTime defaultTimeLocale "%m" t
                        d = formatTime defaultTimeLocale "%d" t
                        paths = tail $ inits [y,m,d]

                    exists <- mapM (doesDirectoryExist . joinPath . (logDirectory:)) paths

                    let i = elemIndex False exists
                    return $ case i of
                      Nothing -> last paths
                      Just 0 -> []
                      Just n -> paths !! (n-1)

pathContents path = do let fullPath = joinPath (logDirectory:path)
                       all <- listDirectory fullPath
                       files <- filterM (doesFileExist . (fullPath </>)) all
                       dirs <- filterM (doesDirectoryExist . (fullPath </>)) all
                       return (dirs,files)
                       

writeLog :: State -> Change -> IO ()
writeLog s c = do hPutStrLn (sLogFH s) (show c)
                  hFlush (sLogFH s)
                  sendCircle (sCircle s)
                    where sendCircle Nothing = return ()
                          sendCircle (Just f) = f c

listenRMS :: MVar State -> IO ()
listenRMS mvS = do let port = case dirt of
                               Super -> 0
                               Classic -> 6010
                   udp <- udpServer "127.0.0.1" port
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
main = do installHandler sigINT Ignore Nothing
          installHandler sigTERM Ignore Nothing
          installHandler sigPIPE Ignore Nothing
          installHandler sigHUP Ignore Nothing
          installHandler sigKILL Ignore Nothing
          argv <- getArgs
          runCurses $ do
            setEcho False
            mvS <- initState argv
            liftIO $ forkIO $ listenRMS mvS
            drawEditor mvS
            render
            mainLoop mvS

playbackThread mvS path = do return ()

handleEv mvS PlaybackMode ev =
  do let -- quit = return True
         ok = return False
     -- if (isJust ev) then liftIO $ hPutStrLn stderr $ "pressed: " ++ show ev else return ()
     case ev of
      Nothing -> ok
      Just (EventCharacter x) -> if x == '\ESC'
                                 then do s <- liftIO $ takeMVar mvS
                                         liftIO $ putMVar mvS $ s {sMode = EditMode}
                                         ok
                                 else ok

handleEv mvS EditMode ev =
  do let quit = return True
         ok = return False
     if (isJust ev) then liftIO $ hPutStrLn stderr $ "pressed: " ++ show ev else return ()
     case ev of
      Nothing -> ok
      Just (EventCharacter x) -> if x == '\ESC'
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
handleEv mvS FileMode (Just (EventSpecialKey k)) =
  case k of KeyUpArrow -> fcMove mvS (-1) >> ok
            KeyDownArrow -> fcMove mvS 1 >> ok
            KeyLeftArrow -> do s <- (liftIO $ takeMVar mvS)
                               let fc = sFileChoice s
                                   path = init $ fcPath fc
                               -- liftIO $ hPutStrLn stderr $ "origpath: " ++ (show (fcPath fc)) ++ " new path: " ++ (show path)
                               (dirs,files) <- (liftIO $ pathContents path)
                               let fc' = fc {fcPath = path, fcDirs = sort dirs, fcFiles = sort files}
                                   s' = s {sFileChoice = fc'}
                               liftIO $ putMVar mvS s'
                               ok
            KeyRightArrow -> do s <- (liftIO $ takeMVar mvS)
                                let fc = sFileChoice s
                                    selected = fcDirs fc !! fcIndex fc
                                    path = fcPath fc ++ [selected]
                                -- liftIO $ hPutStrLn stderr $ "origpath: " ++ (show (fcPath fc)) ++ " new path: " ++ (show path)
                                if fcIndex fc < (length (fcDirs fc))
                                  then do (dirs,files) <- (liftIO $ pathContents path)
                                          let fc' = fc {fcPath = path, fcDirs = (sort dirs),
                                                        fcFiles = (sort files),
                                                        fcIndex = 0
                                                       }
                                              s' = s {sFileChoice = fc'}
                                          liftIO $ putMVar mvS s'
                                  else do liftIO $ putMVar mvS s
                                          return ()
                                ok
            _ -> ok

handleEv mvS FileMode (Just (EventCharacter x))
  | x == chr 27 = do s <- liftIO $ takeMVar mvS
                     liftIO $ putMVar mvS $ s {sMode = EditMode}
                     ok
  | otherwise = ok
                
handleEv mvS FileMode (Just e) = do liftIO $ hPutStrLn stderr $ show e
                                    ok
fcMove mvS d = do s <- liftIO $ takeMVar mvS
                  let fileChoice = sFileChoice s
                      maxI = (length $ fcDirs fileChoice) + (length $ fcFiles fileChoice) - 1
                      i = min maxI $ max 0 $ (fcIndex fileChoice) + d
                  liftIO $ hPutStrLn stderr $ "max: " ++ show maxI ++ " i: " ++ show i
                  liftIO $ putMVar mvS $
                    s {sFileChoice = fileChoice {fcIndex = i}}
                  return ()

quit :: Curses Bool
quit = return True

ok :: Curses Bool
ok = return False

mainLoop mvS = loop where
  loop = do s <- liftIO (readMVar mvS)

            case sMode s of
             EditMode     -> drawEditor mvS
             FileMode     -> drawDirs mvS
             PlaybackMode -> drawEditor mvS
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
     

eval :: MVar State -> Curses ()
eval mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let change = evalChange {cWhen = now}
     liftIO $ applyChange mvS s change
     -- updateWindow (sEditWindow s) clear
     return ()

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

fileTime :: FilePath -> String
fileTime fp = h ++ (':':m) ++ (':':s)
  where t = take 6 fp
        h = take 2 t
        m = take 2 $ drop 2 t
        s = take 2 $ drop 4 t

fileMode :: MVar State -> Curses ()
fileMode mvS = do s <- liftIO $ takeMVar mvS
                  defaultPath <- liftIO defaultLogPath
                  (dirs,files) <- (liftIO $ pathContents defaultPath)
                  let s' = s {sMode = FileMode,
                              sFileChoice =
                                FileChoice {fcPath = defaultPath,
                                            fcIndex = 0,
                                            fcDirs = sort dirs,
                                            fcFiles = sort files
                                           }
                             }
                  liftIO $ putMVar mvS s'
                  drawDirs mvS

-- readDir fc 

drawDirs:: MVar State -> Curses ()
drawDirs mvS
  = do s <- (liftIO $ takeMVar mvS)
       let fileWindow = sFileWindow s
           fc = sFileChoice s
           filePath = fcPath fc
           dirs = fcDirs fc
           dirs' = map (++ "/") dirs
           files = fcFiles fc
       let i = min (fromIntegral $ length dirs + length files) $ max 0 $ fromIntegral $ fcIndex fc
       updateWindow fileWindow $
         do clear
            (h,w) <- windowSize
            mapM_ (drawDir i) $ take (fromIntegral h - 1) $ zip [0..] (sort dirs' ++ (map fileTime $ sort files))
            moveCursor 0 0
            drawString $ take 10 $ intercalate "/" filePath
       liftIO $ putMVar mvS s
  where drawDir i (n,dir) = do setAttribute AttributeReverse (n == i)
                               moveCursor (n+1) 0
                               drawString dir
                               setAttribute AttributeReverse False

evalBlock :: (State, [ParamPattern]) -> (Int, Code) -> IO (State, [ParamPattern])
evalBlock (s,ps) (n, ls) = do let code = intercalate "\n" (map lText ls)
                                  id = fromJust $ lTag $ head ls
                              liftIO $ putMVar (sHintIn s) code
                              response <- liftIO $ takeMVar (sHintOut s)
                              
                              let block = fromJust $ lBlock $ (sCode s) !! n
                                  (block', ps') = act id response block
                                  s' = setBlock n block'
                              hPutStrLn stderr $ show $ block
                              hPutStrLn stderr $ ">>>>"
                              hPutStrLn stderr $ show $ block'
                              -- hPutStrLn stderr $ show $ sCode s'
                              return (s', ps')
  where act id (HintOK p) b = (b {bStatus = Success, bModified = False, bPattern = Just p'}, p':ps)
          where p' = p # orbit (pure id)
        act _ (HintError err) b = (b {bStatus = Error}, ps')
          where ps' | isJust $ bPattern b = (fromJust $ bPattern b):ps
                    | otherwise = ps
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
