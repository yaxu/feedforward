{- Feedforward (c) Alex McLean 2018
   Text editor for TidalCycles
   https://github.com/yaxu/feedforward
   Distributed under the terms of the GNU Public License 3.0, see LICENSE
-}

import UI.NCurses
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Char
import TidalHint
import Control.Concurrent (forkIO)
import Sound.Tidal.Context (superDirtSetters, ParamPattern, cpsUtils)
import Data.List (intercalate)
import Data.Time.Clock.POSIX
import Data.Time
import Data.Time.Format
import System.IO
import System.Directory
import System.FilePath
import System.Posix.Process

type Code = [String]
type Pos = (Int, Int)

data State = State {sCode :: Code,
                    sPos :: Pos,
                    sXWarp :: Int,
                    sWindow :: Window,
                    sColour :: ColorID,
                    sColourHilite :: ColorID,
                    sColourWarn :: ColorID,
                    sHilite :: (Bool, [Int]),
                    sHintIn :: MVar String,
                    sHintOut :: MVar Response,
                    sDirt :: ParamPattern -> IO (),
                    sChangeSet :: ChangeSet,
                    sLogFH :: Handle
                   }

offsetY = 2
offsetX = 1

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

applyChange :: MVar State -> State -> Change -> IO ()
applyChange mvS s change = do putMVar mvS s'
                              writeLog s change
  where ls | (cOrigin change) == "+input" = applyInput s change
           | (cOrigin change) == "+delete" = applyDelete s change
           | otherwise = sCode s
        changes = sChangeSet s
        s' = s {sChangeSet = change:changes,
                sCode = ls,
                sPos = cNewPos change
               }

applyInput :: State -> Change -> [String]
applyInput s change = preL ++ added ++ postL
  where (ls, (y,x), preL, l, postL, preX, postX) = cursorContext' s (cFrom change)
        added = addToHead preX $ addToLast postX $ cText change
        addToHead :: [a] -> [[a]] -> [[a]]
        addToHead x xs = (x ++ (head xs)) : tail xs
        addToLast x xs = init xs ++ [(last xs ++ x)]

applyDelete :: State -> Change -> [String]
applyDelete s change = preL ++ ((preX ++ postX):postL)
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

goCursor state = moveCursor (offsetY + (fromIntegral $ fst $ sPos state)) (offsetX + (fromIntegral $ snd $ sPos state))

draw :: MVar State -> Curses ()
draw mvS
  = do s <- (liftIO $ readMVar mvS)
       updateWindow (sWindow s) $ do
         (h,w) <- windowSize
         setColor (sColourHilite s)
         moveCursor 0 0
         let spaces = ((fromIntegral w) - (length "feedforward")) `div` 2
         drawString $ (replicate spaces ' ') ++ "feedforward" ++ (replicate ((fromIntegral w) - spaces - (length "feedforward")) ' ')
         mapM_ (drawLine s) $ zip (sCode s) [0 ..]
         goCursor s
         return ()
  where drawLine s (line, n) =
          do moveCursor (n + offsetY) offsetX
             if elem (fromIntegral n) (snd $ sHilite s)
               then setColor (if (fst $ sHilite s)
                              then sColourHilite s
                              else sColourWarn s
                             )
               else setColor (sColour s)
             drawString line

initState :: Window -> ColorID -> ColorID -> ColorID -> MVar String -> MVar Response -> (ParamPattern -> IO ()) -> Handle -> State
initState w fg bg warn mIn mOut d logFH
  = State {sCode = ["sound \"bd sn\""],
           sPos = (0,0),
           sWindow = w,
           sXWarp = 0,
           sColour = fg,
           sColourHilite = bg,
           sColourWarn = warn,
           sHilite = (False, []),
           sHintIn = mIn,
           sHintOut = mOut,
           sDirt = d,
           sChangeSet = [],
           sLogFH = logFH
          }

moveHome :: MVar State -> Curses ()
moveHome mvS = do s <- liftIO (readMVar mvS)
                  let (_, x) = sPos s
                  move mvS (0, 0-x)

moveEnd mvS = do s <- liftIO (readMVar mvS)
                 let (y, x) = sPos s
                     xTo = length ((sCode s) !! y)
                 move mvS (0, xTo-x)

move :: MVar State -> (Int, Int) -> Curses ()
move mvS (yd,xd) = do s <- liftIO (takeMVar mvS)
                      let maxY = (length $ sCode s) - 1
                          (y,x) = sPos s
                          y' = max 0 $ min maxY (y + yd)
                          maxX | (length $ sCode s) == y' = 0
                               | otherwise = length $ (sCode s) !! y'
                          x' = max 0 $ min maxX (x + xd)
                          xw | xd /= 0 = x'
                             | otherwise = sXWarp s
                          x'' = min xw maxX
                      liftIO $ putMVar mvS $ s {sPos = (y',x''),
                                                sXWarp = xw,
                                                sHilite = (False, [])
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
            (d, _) <- liftIO (superDirtSetters getNow)
            logFH <- liftIO openLog
            mvS <- (liftIO $ newMVar $ initState w fg bg warn mIn mOut d logFH)
            draw mvS
            render
            mainLoop mvS

mainLoop mvS = loop where
  loop = do draw mvS
            render
            s <- liftIO (readMVar mvS)
            ev <- getEvent (sWindow s) Nothing
            case ev of
             Nothing -> loop
             Just (EventCharacter x) -> if x == esc
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
             Just _ -> loop
      where esc = chr(27)

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

{-
keyCtrl mvS c = do s <- (liftIO $ readMVar mvS)
                   updateWindow (sWindow s) $ do
                     moveCursor 18 10
                     drawString $ show c
-}

keypress mvS c | isCtrl = keyCtrl mvS (chr $ (ord c) + 96)
               | otherwise = insertChar mvS c
  where isCtrl = ord(c) >= 1 && ord(c) <= 26


cursorContext s = cursorContext' s (sPos s)

cursorContext' s (y,x) =
  (ls, (y,x), preL, l, postL, preX, postX)
  where  ls = sCode s
         preL = take y ls
         l = head $ drop y ls
         postL = drop (y+1) ls
         preX = take x l
         postX = drop x l
     

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
        l' | x == 0 = postX
           | otherwise = (take ((length preX) - 1) preX) ++ postX
        ls' = preL ++ (l':postL)

charAt :: [String] -> (Int,Int) -> Char
charAt ls (y,x) = (ls !! y) !! x

lineLength :: [String] -> Int -> Int
lineLength ls y = length $ ls !! y

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
     liftIO $ maybe (return ()) (applyChange mvS s) change
     updateWindow (sWindow s) clear

del :: MVar State -> Curses ()
del mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), _, l, _, _, _) = cursorContext s
         change | x < (length l) = Just $ (deleteChange (y,x) (y,x+1) [[charAt ls (y,x)]]) {cWhen = now}
                | y == ((length ls) - 1) = Nothing
                | otherwise = Just $ (deleteChange (y,x) (y+1,0) ["",""]) {cWhen = now}
     liftIO $ maybe (return ()) (applyChange mvS s) change
     updateWindow (sWindow s) clear

killLine :: MVar State -> Curses ()
killLine mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), _, l, _, _, postX) = cursorContext s
         change | x < (length l) = Just $ deleteChange (y,x) (y,(length l)) [postX]
                | y == ((length ls) - 1) = Nothing
                | otherwise = Just $ deleteChange (y,x) (y+1,0) ["",""]
     liftIO $ maybe (return ()) (applyChange mvS s) change
     updateWindow (sWindow s) clear

eval :: MVar State -> Curses ()
eval mvS = 
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
         hasChar = or . map (/= ' ')
         codeblock = intercalate "\n" (map (ls !!) findBlock)
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



waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
