module Edit where

{- Feedforward (c) Alex McLean 2018
   Text editor for TidalCycles
   https://github.com/yaxu/feedforward
   Distributed under the terms of the GNU Public License 3.0, see LICENSE
-}

import           Control.Concurrent      (ThreadId, forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Monad           (filterM, foldM, forever, unless, when)
import           Control.Monad.IO.Class
import           Data.Char
import           Data.List               (elemIndex, inits, intercalate,
                                          isPrefixOf, sort, stripPrefix, (\\))
import           Data.Maybe              (catMaybes, fromJust, fromMaybe,
                                          isJust, mapMaybe)
import qualified Data.Text.IO            as T
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import qualified Network.Socket          as N
import qualified Network.WebSockets      as WS
import           Sound.OSC.FD
import           Sound.Tidal.Context     (ParamPattern, cpsUtils, cpsUtils',
                                          dirtSetters, orbit, silence, stack,
                                          superDirtSetters, ( # ))
import           System.Directory
import           System.Environment      (getArgs, lookupEnv)
import           System.FilePath
import           System.IO
import           System.Posix.Process
import           System.Posix.Signals
import           Text.Printf
import           UI.NCurses

import qualified Data.Aeson              as A
import           GHC.Generics

import           Change
import           TidalHint

type Tag = Int

data Status = Success | Error | Normal
            deriving (Show, Eq)

data Block = Block {bTag      :: Tag,
                    bModified :: Bool,
                    bStatus   :: Status,
                    bPattern  :: Maybe ParamPattern,
                    bMute     :: Bool,
                    bSolo     :: Bool
                   }
             deriving Show

data Line = Line {lBlock :: Maybe Block,
                  lText  :: String
                 }
             deriving Show

type Code = [Line]

data Dirt = Classic | Super
          deriving Eq

data Playback = Playback {pbOffset  :: Double,
                          pbChanges :: [Change]
                         }

dirt = Super

playbackSpeed = 2

lTag :: Line -> Maybe Tag
lTag l = bTag <$> lBlock l

lMuted :: Line -> Bool
lMuted l = fromMaybe False $ bMute <$> lBlock l

lMute :: Line -> Bool
lMute Line {lBlock = Just Block {bMute = a}} = a
lMute _                                      = False

lStatus :: Line -> Maybe Status
lStatus l = bStatus <$> lBlock l

setTag :: Line -> Tag -> Line
setTag l@(Line {lBlock = Just b}) tag = l {lBlock = Just (b {bTag = tag})}
setTag l@(Line {lBlock = Nothing}) tag
  = l {lBlock = Just (Block {bTag = tag,
                             bModified=True,
                             bStatus = Normal, bPattern = Nothing,
                             bMute = False,
                             bSolo = False
                            }
                     )
      }


type CpsUtils = (Double -> IO (), Double -> IO (), IO Rational)

data Mode = EditMode | FileMode | PlaybackMode

data FileChoice = FileChoice {fcPath  :: [FilePath],
                              fcIndex :: Int,
                              fcDirs  :: [FilePath],
                              fcFiles :: [FilePath]
                             }

data State = State {sCode         :: Code,
                    sPos          :: Pos,
                    sXWarp        :: Int,
                    sEditWindow   :: Window,
                    sFileWindow   :: Window,
                    sColour       :: ColorID,
                    sColourHilite :: ColorID,
                    sColourWarn   :: ColorID,
                    sColourShaded :: ColorID,
                    sHintIn       :: MVar String,
                    sHintOut      :: MVar Response,
                    sDirt         :: ParamPattern -> IO (),
                    sChangeSet    :: ChangeSet,
                    sLogFH        :: Handle,
                    sRMS          :: [Float],
                    sScroll       :: (Int,Int),
                    sCpsUtils     :: CpsUtils,
                    sMode         :: Mode,
                    sFileChoice   :: FileChoice,
                    sCircle       :: Maybe (Change -> IO ()),
                    sPlayback     :: Maybe Playback,
                    sName         :: Maybe String,
                    sRefresh      :: Bool,
                    sLastAlt      :: Double
                   }

topMargin    = 1 :: Integer
bottomMargin = 2 :: Integer
leftMargin   = 3 :: Integer
rightMargin  = 0 :: Integer

hasChar :: Line -> Bool
hasChar = any (/= ' ') . lText

updateTags :: Code -> Code
updateTags ls = assignTags freeTags ls'
  where assignTags :: [Tag] -> Code -> Code
        assignTags [] (l:ls) = l:ls
        assignTags _ [] = []
        assignTags ids (l:ls) | lTag l == Just (-1) = setTag l (head ids):(assignTags (tail ids) ls)
                              | otherwise = l:(assignTags ids ls)
        freeTags = ([1 .. 9]++[0]) \\ tagIds
        tagIds = mapMaybe lTag ls'
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

applyChange :: State -> Change -> IO (State)
applyChange s (change@(Change {})) = do writeLog s' change
                                        return s'
  where ls | (cOrigin change) == "+input" = updateTags $ applyInput s change
           | (cOrigin change) == "+delete" = updateTags $ applyDelete s change
           | otherwise = sCode s
        changes = sChangeSet s
        s' = s {sChangeSet = change:changes,
                sCode = ls,
                sPos = cNewPos change
               }

applyChange s change@(Eval {}) =
  do let blocks = unmutedBlocks $ sCode s
         blocks' = allBlocks 0 $ sCode s
     hPutStrLn stderr $ "unmuted blocks: " ++ show (length blocks) ++ " of " ++ show (length blocks')
     hPutStrLn stderr $ show blocks'
     (s',ps) <- foldM evalBlock (s, []) blocks
     (sDirt s) (stack ps)
     writeLog s' change
     return s'

applyChange s change@(Snapshot {}) =
  do hPutStrLn stderr $ "got a snapshot"
     writeLog s change
     return $ s {sCode = updateTags $ map (Line Nothing) (cText change),
                 sPos = (0,0),
                 sRefresh = True
                }

applyChange s _ =
  do hPutStrLn stderr $ "unhandled change type"
     return s

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

goCursor state = moveCursor ((topMargin + (fromIntegral $ fst $ sPos state))-sY) ((leftMargin + fromIntegral (snd $ sPos state)) - sX)
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
     let name = fromMaybe "" ((\x -> "[" ++ x ++ "] ") <$> sName s)

     updateWindow (sEditWindow s) $
       do (h,w) <- windowSize
          moveCursor (h-2) 0
          setColor $ sColourHilite s
          let str = " " ++ name ++ show (sPos s)
          drawString $ str ++ replicate ((fromIntegral w) - (length str)) ' '

rmsBlocks = " ▁▂▃▄▅▆▇█"

drawEditor :: MVar State -> Curses ()
drawEditor mvS
  = do s <- (liftIO $ takeMVar mvS)
       s'' <- updateWindow (sEditWindow s) $ do
         when (sRefresh s) clear
         (h,w) <- windowSize
         let s' = doScroll s (h,w)
         setColor (sColour s')
         let ls = zip (sCode s) [0 ..]
         mapM_ (drawLine s w) $ zip [topMargin..] $ take (fromIntegral $ h - (topMargin + bottomMargin)) $ drop (fst $ sScroll s') $ ls
         -- HACK: clear trailing line in case one has been deleted
         -- assumes only one line ever deleted at a time (true so far)
         when (length ls < (fromIntegral $ h - (bottomMargin + topMargin))) $
           do moveCursor (1 + (fromIntegral $ length ls)) 0
              drawString $ take (fromIntegral w) $ repeat ' '
         return s'
       drawFooter s''
       updateWindow (sEditWindow s) $ goCursor s''
       liftIO $ putMVar mvS (s'' {sRefresh = False})
  where drawLine :: State -> Integer -> (Integer, (Line, Integer)) -> Update ()
        drawLine s w (y, (l, n)) =
          do let scrollX = snd $ sScroll s
                 skipLeft = drop scrollX $ lText l
                 skipBoth = take (fromIntegral $ w - (leftMargin + rightMargin + 1)) $ skipLeft
             moveCursor y leftMargin
             setColor (sColour s)
             drawString (take (fromIntegral $ w-leftMargin) $ skipBoth ++ repeat ' ')

             setColor $ sColourHilite s
             when (scrollX > 0) $
               do moveCursor y leftMargin
                  drawString "<"
             when ((length skipLeft) > (length skipBoth)) $
               do moveCursor y (w-1)
                  drawString ">"
             moveCursor y 0
             setColor $ sColour s
             lineHead
             drawRMS s w (y-1) l
               where lineHead | isJust (lTag l) = do let c | lMuted l = setColor $ sColourShaded s
                                                           | lStatus l == (Just Error) = setColor $ sColourWarn s
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
                              | otherwise = do moveCursor y 0
                                               drawString "  "
        drawRMS s w y l | hasBlock l = do let rmsMax = (length rmsBlocks) - 1
                                              id = fromJust $ lTag l
                                              rmsL = min rmsMax $ floor $ 50 * ((sRMS s) !! (id*2))
                                              rmsR = min rmsMax $ floor $ 50 * ((sRMS s) !! (id*2+1))
                                              str = (rmsBlocks !! rmsL):(rmsBlocks !! rmsR):[]
                                          setColor (sColour s)
                                          moveCursor (fromIntegral y + topMargin - 1) 0
                                          drawString $ str
                        | otherwise = return ()

connectCircle :: MVar State -> Maybe String -> IO (Maybe (Change -> IO ()))
connectCircle mvS name =
  do addr <- fromMaybe "127.0.0.1" <$> lookupEnv "CIRCLE_ADDR"
     port <- fromMaybe "6010" <$> lookupEnv "CIRCLE_PORT"
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
                    circleAct conn $ T.unpack msg
                    hPutStrLn stderr $ T.unpack msg
                  let loop = do
                        change <- takeMVar mChange
                        WS.sendTextData conn (T.append (T.pack "/change ") $ decodeUtf8 $ A.encode change) >> loop
                  loop
                  WS.sendClose conn (T.pack "/quit")
             circleAct conn msg | isPrefixOf "/takeSnapshot " msg =
                                    do let snapName = fromMaybe "noname" $ stripPrefix "/takeSnapshot " msg
                                       s <- takeMVar mvS
                                       let code = map lText $ sCode s
                                       now <- (realToFrac <$> getPOSIXTime)
                                       writeLog s $ Snapshot {cWhen = now,
                                                              cText = code,
                                                              cName = Just snapName
                                                             }
                                       putMVar mvS s
                                       return ()
                                | isPrefixOf "/change " msg =
                                    do let change = A.decode $ encodeUtf8 $ T.pack $ fromJust $ stripPrefix "/change " msg
                                       if (isJust change)
                                         then do s <- takeMVar mvS
                                                 s' <- applyChange s $ fromJust change
                                                 putMVar mvS s'
                                         else (hPutStrLn stderr $ "bad change: " ++ msg)
                                       return ()
                                | otherwise = return ()

initState :: [String] -> Curses (MVar State)
initState args
  = do w <- defaultWindow
       updateWindow w clear
       setEcho False
       setKeypad w True
       fg <- newColorID ColorWhite ColorDefault 1
       bg <- newColorID ColorBlack ColorWhite 2
       shade <- newColorID ColorBlack ColorBlue 2
       warn <- newColorID ColorWhite ColorRed 3
       fileWindow <- newWindow 10 20 3 3
       mIn <- liftIO newEmptyMVar
       mOut <- liftIO newEmptyMVar
       liftIO $ forkIO $ hintJob (mIn, mOut)
       (_, getNow) <- liftIO cpsUtils
       cpsUtils <- liftIO cpsUtils'
       let setters = case dirt of
                      Classic -> dirtSetters
                      Super   -> superDirtSetters
       (d, _) <- liftIO (setters getNow)
       logFH <- liftIO openLog
       name <- liftIO $ lookupEnv "CIRCLE_NAME"
       mvS <- liftIO $ newEmptyMVar
       circle <- liftIO $ connectCircle mvS name
       liftIO $ putMVar mvS $ State {sCode = [Line Nothing ""],
                                     sPos = (0,0),
                                     sEditWindow = w,
                                     sFileWindow = fileWindow,
                                     sXWarp = 0,
                                     sColour = fg,
                                     sColourHilite = bg,
                                     sColourShaded = shade,
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
                                     sCircle = circle,
                                     sPlayback = Nothing,
                                     sName = name,
                                     sRefresh = False,
                                     sLastAlt = 0
                                    }
       return mvS

moveHome :: MVar State -> Curses ()
moveHome mvS = do s <- liftIO (readMVar mvS)
                  let (_, x) = sPos s
                  move mvS (0, negate x)

moveEnd :: MVar State -> Curses ()
moveEnd mvS = do s <- liftIO (readMVar mvS)
                 let (y, x) = sPos s
                     xTo = length (lText $ sCode s !! y)
                 move mvS (0, xTo-x)

move :: MVar State -> (Int, Int) -> Curses ()
move mvS (yd,xd) = do s <- liftIO (takeMVar mvS)
                      let maxY = length (sCode s) - 1
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
                      Just 0  -> []
                      Just n  -> paths !! (n-1)

pathContents path = do let fullPath = joinPath (logDirectory:path)
                       all <- listDirectory fullPath
                       files <- filterM (doesFileExist . (fullPath </>)) all
                       dirs <- filterM (doesDirectoryExist . (fullPath </>)) all
                       return (dirs,files)


writeLog :: State -> Change -> IO ()
writeLog s c = do hPutStrLn (sLogFH s) (T.unpack $ decodeUtf8 $ A.encode $ c)
                  hFlush (sLogFH s)
                  sendCircle (sCircle s)
                    where sendCircle Nothing  = return ()
                          sendCircle (Just f) = f c

listenRMS :: MVar State -> IO ()
listenRMS mvS = do let port = case dirt of
                               Super   -> 0
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
         mungeOrbit <- mungeOrbitIO
         let orbit = mungeOrbit $ fromMaybe 0 $ datum_integral $ messageDatum m !! 1
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
          installHandler sigSTOP Ignore Nothing
          installHandler sigTSTP Ignore Nothing
          argv <- getArgs
          runCurses $ do
            mvS <- initState argv
            liftIO $ forkIO $ listenRMS mvS
            drawEditor mvS
            render
            mainLoop mvS

handleEv :: MVar State -> Mode -> Maybe Event -> Curses Bool
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
      Just _ -> ok

handleEv mvS EditMode ev =
  do let quit = return True
         ok = return False
     -- if (isJust ev) then liftIO $ hPutStrLn stderr $ "pressed: " ++ show ev else return ()
     case ev of
      Nothing -> ok
      Just (EventCharacter '\ESC') ->
        do liftIO $ do s <- takeMVar mvS
                       now <- (realToFrac <$> getPOSIXTime)
                       putMVar mvS $ s {sLastAlt = now}
           ok
      Just (EventCharacter x) -> do isAlt <- liftIO checkAlt
                                    keypress mvS isAlt x >> ok
       where checkAlt = do s <- readMVar mvS
                           now <- realToFrac <$> getPOSIXTime
                           -- this timeout not actually necessary as
                           -- the ESC will do this anyway, via
                           -- setKeypad..
                           return $ (now - (sLastAlt s)) < altTimeout
             altTimeout = 0.02
      Just (EventSpecialKey KeyUpArrow) -> move mvS (-1,0) >> ok
      Just (EventSpecialKey KeyDownArrow) -> move mvS (1,0) >> ok
      Just (EventSpecialKey KeyLeftArrow) -> move mvS (0,-1) >> ok
      Just (EventSpecialKey KeyRightArrow) -> move mvS (0,1) >> ok
      Just (EventSpecialKey KeyHome) -> moveHome mvS >> ok
      Just (EventSpecialKey KeyEnd) -> moveEnd mvS >> ok
      Just (EventSpecialKey KeyEnter) -> insertBreak mvS >> ok
      Just (EventSpecialKey KeyDeleteCharacter) -> del mvS >> ok
      Just (EventSpecialKey KeyBackspace) -> backspace mvS >> ok
      Just (EventSpecialKey (KeyFunction 2)) -> fileMode mvS >> ok
      Just (EventSpecialKey (KeyFunction 10)) -> quit
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
            KeyRightArrow -> do s <- (liftIO $ readMVar mvS)
                                let fc = sFileChoice s
                                    path = selectedPath fc
                                -- liftIO $ hPutStrLn stderr $ "origpath: " ++ (show (fcPath fc)) ++ " new path: " ++ (show path)
                                if fcIndex fc < (length (fcDirs fc))
                                  then do s <- (liftIO $ takeMVar mvS)
                                          (dirs,files) <- (liftIO $ pathContents path)
                                          let fc' = fc {fcPath = path, fcDirs = (sort dirs),
                                                        fcFiles = (sort files),
                                                        fcIndex = 0
                                                       }
                                              s' = s {sFileChoice = fc'}
                                          liftIO $ putMVar mvS s'
                                  else do -- clear screen
                                          delAll mvS
                                          s <- (liftIO $ takeMVar mvS)
                                          -- liftIO $ hPutStrLn stderr $ "select file: " ++ joinPath path
                                          liftIO $ do s' <- (startPlayback s $ joinPath path)
                                                      putMVar mvS s'
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
                  -- liftIO $ hPutStrLn stderr $ "max: " ++ show maxI ++ " i: " ++ show i
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

            ev <- getEvent (sEditWindow s) (Just (1000 `div` 20))
            done <- handleEv mvS (sMode s) ev
            updateScreen mvS (sMode s)
            unless done loop

updateScreen :: MVar State -> Mode -> Curses ()
updateScreen mvS PlaybackMode
  = do s <- liftIO $ takeMVar mvS
       let (Playback offset cs) = fromJust $ sPlayback s
       now <- liftIO $ (realToFrac <$> getPOSIXTime)
       let (ready, waiting) = takeReady cs (now - offset)
       s' <- liftIO $ foldM applyChange s ready
       liftIO $ putMVar mvS (s' {sPlayback = Just $ Playback offset waiting})
       return ()
         where takeReady cs t = (takeWhile (\c -> (cWhen c) < t) cs,
                                 dropWhile (\c -> (cWhen c) < t) cs
                                )

updateScreen _ _ = return ()

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

-- deprecate?
keyCtrl mvS 'h' = stopAll mvS

keyCtrl mvS 'l' = liftIO $ modifyMVar_ mvS $ \s -> return $ s {sRefresh = True}


keyCtrl mvS _ = return ()

keyAlt mvS '\n' = eval mvS
keyAlt mvS 'h' = stopAll mvS

keyAlt mvS '0' = toggleMute mvS 0
keyAlt mvS '1' = toggleMute mvS 1
keyAlt mvS '2' = toggleMute mvS 2
keyAlt mvS '3' = toggleMute mvS 3
keyAlt mvS '4' = toggleMute mvS 4
keyAlt mvS '5' = toggleMute mvS 5
keyAlt mvS '6' = toggleMute mvS 6
keyAlt mvS '7' = toggleMute mvS 7
keyAlt mvS '8' = toggleMute mvS 8
keyAlt mvS '9' = toggleMute mvS 9


keyAlt mvS c = do liftIO $ hPutStrLn stderr $ "got Alt-" ++ [c]
                  return ()

withTag :: Code -> Tag -> (Line -> Line) -> Code
withTag [] _ _ = []
withTag (l:ls) t f | lTag l == Just t = (f l):ls
                   | otherwise = l:(withTag ls t f)

toggleMute mvS n =
  do liftIO $ do hPutStrLn stderr ("togglemute " ++ show n)
                 s <- takeMVar mvS
                 let ls = sCode s
                     ls' = withTag ls n f
                 putMVar mvS (s {sCode = ls'})
     eval mvS
       where f (l@(Line {lBlock = Just b})) = l {lBlock = Just $ b {bMute = not (bMute b)}}
             f l = l -- can't happen


toggleSolo mvS n =
  liftIO $ do s <- takeMVar mvS
              let ls = sCode s
                  ls' = withTag ls n f
              putMVar mvS (s {sCode = ls'})
                where f (l@(Line {lBlock = Just b})) = l {lBlock = Just $ b {bSolo = not (bSolo b)}}
                      f l = l -- can't happen

{-
keyCtrl mvS c = do s <- (liftIO $ readMVar mvS)
                   updateWindow (sEditWindow s) $ do
                     moveCursor 18 10
                     drawString $ show c
-}

mouse mvS (MouseState {mouseCoordinates = (x,y,_), mouseButtons = [(1, ButtonClicked)]}) = moveTo mvS (fromIntegral (max (y-topMargin) 0),fromIntegral (max (x-leftMargin) 0))
mouse _ _ = return ()

keypress mvS isAlt c | isAlt = keyAlt mvS c
                     | isCtrl = keyCtrl mvS (chr $ (ord c) + 96)
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
  do liftIO $ do s <- (takeMVar mvS)
                 now <- (realToFrac <$> getPOSIXTime)
                 let change = evalChange {cWhen = now}
                 s' <- applyChange s change
                 putMVar mvS s'
     -- updateWindow (sEditWindow s) clear
     return ()

stopAll :: MVar State -> Curses ()
stopAll mvS =
  do liftIO $ do s <- (readMVar mvS)
                 (sDirt s) silence
     -- updateWindow (sEditWindow s) clear
     return ()

insertBreak :: MVar State -> Curses ()
insertBreak mvS =
  do s <- liftIO $ takeMVar mvS
     liftIO $ do let (y,x) = sPos s
                     (y',x') = (y+1,0)
                 now <- (realToFrac <$> getPOSIXTime)
                 let change = (insertChange (y,x) ["",""]) {cWhen = now}
                 s' <- applyChange (s {sXWarp = 0}) change
                 putMVar mvS s'
     -- updateWindow (sEditWindow s) clear

insertChar :: MVar State -> Char -> Curses ()
insertChar mvS c =
  do s <- liftIO $ takeMVar mvS
     liftIO $ do let (y,x) = sPos s
                     (y',x') = (y,x+1)
                 now <- (realToFrac <$> getPOSIXTime)
                 let change = (insertChange (y,x) [[c]]) {cWhen = now}
                 s' <- applyChange (s {sXWarp = x'}) change
                 putMVar mvS s'
     -- updateWindow (sEditWindow s) clear

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
     s' <- liftIO $ maybe (return s) (applyChange s) change
     liftIO $ putMVar mvS s'
     -- updateWindow (sEditWindow s) clear

del :: MVar State -> Curses ()
del mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), _, l, _, _, _) = cursorContext s
         change | x < (length $ lText l) = Just $ (deleteChange (y,x) (y,x+1) [[charAt ls (y,x)]]) {cWhen = now}
                | y == ((length ls) - 1) = Nothing
                | otherwise = Just $ (deleteChange (y,x) (y+1,0) ["",""]) {cWhen = now}
     s' <- liftIO $ maybe (return s) (applyChange s) change
     liftIO $ putMVar mvS s'
     -- updateWindow (sEditWindow s) clear

delAll :: MVar State -> Curses ()
delAll mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let ls = sCode s
         lastY = (length ls) - 1
         lastX = (lineLength ls lastY) - 1
         change | null ls = Nothing
                | otherwise = Just $ (deleteChange (0,0) (lastY,lastX+1) (map lText ls)) {cWhen = now}
     s' <- liftIO $ maybe (return s) (applyChange s) change
     liftIO $ putMVar mvS s'
     updateWindow (sEditWindow s) clear

killLine :: MVar State -> Curses ()
killLine mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), _, l, _, _, postX) = cursorContext s
         change | x < (length $ lText l) = Just $ deleteChange (y,x) (y,(length $ lText l)) [postX]
                | y == ((length ls) - 1) = Nothing
                | otherwise = Just $ deleteChange (y,x) (y+1,0) ["",""]
     s' <- liftIO $ maybe (return s) (applyChange s) change
     liftIO $ putMVar mvS s'
     -- updateWindow (sEditWindow s) clear

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
                              liftIO $ hPutStrLn stderr $ "Response: " ++ show response
                              mungeOrbit <- mungeOrbitIO
                              let block = fromJust $ lBlock $ (sCode s) !! n
                                  (block', ps') = act (mungeOrbit id) response block
                                  s' = setBlock n block'
                              -- hPutStrLn stderr $ show $ block
                              -- hPutStrLn stderr $ ">>>>"
                              -- hPutStrLn stderr $ show $ block'
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

mungeOrbitIO :: IO (Int -> Int)
mungeOrbitIO = do orbitOffset <- (read . fromMaybe "0") <$> lookupEnv "ORBIT_OFFSET"
                  orbitMax <- (read . fromMaybe "10") <$> lookupEnv "ORBIT_MAX"
                  return $ \o -> orbitOffset + (o `mod` orbitMax)

hasBlock :: Line -> Bool
hasBlock = isJust . lBlock

allBlocks :: Int -> Code -> [(Int, Code)]
allBlocks _ [] = []
allBlocks n (l:ls) | hasBlock l = (n,b):(allBlocks (n+(length b)+1) ls')
                   | otherwise = allBlocks (n+1) ls
  where b = takeWhile hasChar (l:ls)
        ls' = drop (length b) ls

unmutedBlocks :: Code -> [(Int, Code)]
unmutedBlocks ls = filter (not . lMuted . (!!0) . snd) $ allBlocks 0 ls

scSub = do udp <- udpServer "127.0.0.1" 0
           remote_addr <- N.inet_addr "127.0.0.1"
           let remote_sockaddr = N.SockAddrInet 57110 remote_addr
           sendTo udp (Message "/notify" []) remote_sockaddr
           loop udp
  where loop udp = do m <- recvMessage udp
                      hPutStrLn stderr $ show m
                      loop udp

selectedPath fc = fcPath fc ++ [selected]
  where selected = (fcDirs fc ++ fcFiles fc) !! fcIndex fc

startPlayback :: State -> FilePath -> IO State
startPlayback s path =
  do now <- (realToFrac <$> getPOSIXTime)
     fh <- openFile (logDirectory </> path) ReadMode
     c <- hGetContents fh
     let ls = lines c
         changes = mapMaybe (A.decode . encodeUtf8 . T.pack) ls
         offset | not (null changes) = now - (cWhen (head changes))
                | otherwise = 0
         playback = Playback {pbChanges = changes,
                              pbOffset = offset
                             }
     return $ s {sPlayback = Just playback,
                 sMode = PlaybackMode
                }


dumpCode :: Code -> String
dumpCode ls = unlines $ map lText ls

unDumpCode :: String -> Code
unDumpCode s = updateTags $ map (Line Nothing) $ lines s
