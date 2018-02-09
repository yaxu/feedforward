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

type Code = [String]
type Pos = (Int, Int)

data State = State {codeState :: Code,
                    posState :: Pos,
                    xWarp :: Int,
                    windowState :: Window,
                    fgState :: ColorID,
                    bgState :: ColorID,
                    hiliteState :: [Int],
                    hintIn :: MVar String,
                    hintOut :: MVar Response,
                    dirtState :: ParamPattern -> IO (),
                    changeSetState :: ChangeSet
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
                      cWhen :: Double
                     }
            deriving Show

type ChangeSet = [Change]

writeChanges mvS =
  liftIO $ do s <- readMVar mvS
              writeFile "changes.txt" $ concatMap ((++ "\n\n") . show) $ changeSetState s

addChange :: Double -> Change -> State -> State
addChange now change s =
  s {changeSetState = change':changes}
  where changes = changeSetState s
        change' = change {cWhen = now}
        

insertChange :: Pos -> [String] -> Change
insertChange from str = Change {cFrom = from,
                                cTo = from,
                                cText = str,
                                cRemoved = [""],
                                cOrigin = "+input",
                                cWhen = -1
                               }

deleteChange :: Pos -> Pos -> [String] -> Change
deleteChange from to removed = Change {cFrom = from,
                                       cTo = to,
                                       cText = [""],
                                       cRemoved = removed,
                                       cOrigin = "+input",
                                       cWhen = -1
                                      }

goCursor state = moveCursor (offsetY + (fromIntegral $ fst $ posState state)) (offsetX + (fromIntegral $ snd $ posState state))

draw :: MVar State -> Curses ()
draw mvS
  = do s <- (liftIO $ readMVar mvS)
       updateWindow (windowState s) $ do
         (h,w) <- windowSize
         setColor (bgState s)
         moveCursor 0 0
         let spaces = ((fromIntegral w) - (length "feedforward")) `div` 2
         drawString $ (replicate spaces ' ') ++ "feedforward" ++ (replicate ((fromIntegral w) - spaces - (length "feedforward")) ' ')
         mapM_ (drawLine s) $ zip (codeState s) [0 ..]
         goCursor s
         return ()
  where drawLine s (line, n) =
          do moveCursor (n + offsetY) offsetX
             if elem (fromIntegral n) (hiliteState s)
               then setColor (bgState s)
               else setColor (fgState s)
             drawString line

initState :: Window -> ColorID -> ColorID -> MVar String -> MVar Response -> (ParamPattern -> IO ()) -> State
initState w fg bg mIn mOut d
  = State {codeState = ["every 3 rev $ sound \"bd sn\"", "  where foo = 1"],
           posState = (0,0),
           windowState = w,
           xWarp = 0,
           fgState = fg,
           bgState = bg,
           hiliteState = [],
           hintIn = mIn,
           hintOut = mOut,
           dirtState = d,
           changeSetState = []
          }

moveHome :: MVar State -> Curses ()
moveHome mvS = do s <- liftIO (readMVar mvS)
                  let (_, x) = posState s
                  move mvS (0, 0-x)

moveEnd mvS = do s <- liftIO (readMVar mvS)
                 let (y, x) = posState s
                     xTo = length ((codeState s) !! y)
                 move mvS (0, xTo-x)

move :: MVar State -> (Int, Int) -> Curses ()
move mvS (yd,xd) = do s <- liftIO (takeMVar mvS)
                      let maxY = (length $ codeState s) - 1
                          (y,x) = posState s
                          y' = max 0 $ min maxY (y + yd)
                          maxX | (length $ codeState s) == y' = 0
                               | otherwise = length $ (codeState s) !! y'
                          x' = max 0 $ min maxX (x + xd)
                          xw | xd /= 0 = x'
                             | otherwise = xWarp s
                          x'' = min xw maxX
                      liftIO $ putMVar mvS $ s {posState = (y',x''),
                                                xWarp = xw,
                                                hiliteState = []
                                               }

main :: IO ()
main = do runCurses $ do
            setEcho False
            w <- defaultWindow
            updateWindow w clear
            fg <- newColorID ColorWhite ColorBlack 1
            bg <- newColorID ColorBlack ColorWhite 2
            mIn <- liftIO newEmptyMVar
            mOut <- liftIO newEmptyMVar
            liftIO $ forkIO $ hintJob (mIn, mOut)
            (_, getNow) <- liftIO cpsUtils
            (d, _) <- liftIO (superDirtSetters getNow)
            mvS <- (liftIO $ newMVar $ initState w fg bg mIn mOut d)
            draw mvS
            render
            mainLoop mvS

mainLoop mvS = loop where
  loop = do draw mvS
            render
            s <- liftIO (readMVar mvS)
            ev <- getEvent (windowState s) Nothing
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

keyCtrl mvS 'l' = writeChanges mvS

keyCtrl mvS _ = return ()

{-
keyCtrl mvS c = do s <- (liftIO $ readMVar mvS)
                   updateWindow (windowState s) $ do
                     moveCursor 18 10
                     drawString $ show c
-}

keypress mvS c | isCtrl = keyCtrl mvS (chr $ (ord c) + 96)
               | otherwise = insertChar mvS c
  where isCtrl = ord(c) >= 1 && ord(c) <= 26

cursorContext s =
  (ls, (y,x), preL, l, postL, preX, postX)
  where  ls = codeState s
         (y,x) = posState s
         preL = take y ls
         l = head $ drop y ls
         postL = drop (y+1) ls
         preX = take x l
         postX = drop x l
     

insertBreak :: MVar State -> Curses ()
insertBreak mvS =
  do s <- (liftIO $ takeMVar mvS)
     let (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
         ls' = preL ++ (preX:postX:postL)
         (y',x') = (y+1,0)
         change = insertChange (y,x) ["",""]
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     liftIO $ putMVar mvS $ addChange now change $ s {codeState = ls',
                                                      posState = (y',x'),
                                                      xWarp = 0
                                                     }
     updateWindow (windowState s) clear

insertChar :: MVar State -> Char -> Curses ()
insertChar mvS c =
  do s <- (liftIO $ takeMVar mvS)
     let (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
         (y',x') = (y,x+1)
         l' = preX ++ (c:postX)
         ls' = preL ++ (l':postL)
         change = insertChange (y,x) [[c]]
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     liftIO $ putMVar mvS $ addChange now change $ s {codeState = ls',
                                                      posState = (y',x'),
                                                      xWarp = x'
                                                     }
     updateWindow (windowState s) clear

backspaceChar :: State -> State
backspaceChar s =
  s {codeState = ls',
     posState = (y',x'),
     xWarp = x'
    }
  where (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
        (y',x') = (y,max 0 (x-1))
        l' | x == 0 = postX
           | otherwise = (take ((length preX) - 1) preX) ++ postX
        ls' = preL ++ (l':postL)

joinLines :: State -> State
joinLines s =
  s {codeState = ls',
     posState = (y',x'),
     xWarp = x'
    }
  where (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
        l' = (last preL) ++ l
        ls' = (take ((length preL) - 1) preL) ++ (l':postL)
        (y',x') = (y-1,length $ last preL)
        xWarp = x'

charAt :: [String] -> (Int,Int) -> Char
charAt ls (y,x) = (ls !! y) !! x

lineLength :: [String] -> Int -> Int
lineLength ls y = length $ ls !! y

backspace :: MVar State -> Curses ()
backspace mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (y,x) = posState s
         ls = codeState s
         (s', change) | x > 0 =
                          (backspaceChar s,
                           Just $ deleteChange (y,x-1) (y,x) [[charAt ls (y,x-1)]]
                          )
                      | y == 0 = (s, Nothing)
                      | otherwise = (joinLines s,
                                     Just $ deleteChange (y-1,
                                                          lineLength ls (y-1)
                                                         ) (y, x) ["", ""]
                                    )
     liftIO $ putMVar mvS $ maybe s' (\c -> addChange now c s') change
     updateWindow (windowState s) clear

joinLinesBack :: State -> State
joinLinesBack s =
  s {codeState = ls',
     posState = (y',x'),
     xWarp = x'
    }
  where (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
        l' = l ++ head postL
        ls' = preL ++ (l':(tail postL))
        (y',x') = (y,x)
        xWarp = x'

deleteChar :: State -> State
deleteChar s =
  s {codeState = ls',
     posState = (y',x'),
     xWarp = x'
    }
  where (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
        (y',x') = (y,x)
        l' | x == ((length l)) = preX
           | otherwise = preX ++ (tail postX)
        ls' = preL ++ (l':postL)

deleteToEnd :: State -> State
deleteToEnd s =
  s {codeState = ls',
     posState = (y',x'),
     xWarp = x'
    }
  where (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
        (y',x') = (y,x)
        l' = preX
        ls' = preL ++ (l':postL)

del :: MVar State -> Curses ()
del mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
         (s', change) | x < (length l) = (deleteChar s,
                                          Just $ deleteChange (y,x) (y,x+1) [[charAt ls (y,x)]]
                                         )
                      | y == ((length ls) - 1) = (s, Nothing)
                      | otherwise = (joinLinesBack s, Just $ deleteChange (y,x) (y+1,0) ["",""])
     liftIO $ putMVar mvS $ maybe s' (\c -> addChange now c s') change
     updateWindow (windowState s) clear

killLine :: MVar State -> Curses ()
killLine mvS =
  do s <- (liftIO $ takeMVar mvS)
     now <- (liftIO $ realToFrac <$> getPOSIXTime)
     let (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
         (s',change) | x < (length l) = (deleteToEnd s, Just $ deleteChange (y,x) (y,(length l) -1) [postX])
                     | y == ((length ls) - 1) = (s, Nothing)
                     | otherwise = (joinLinesBack s, Just $ deleteChange (y,x) (y+1,0) ["",""])
     liftIO $ putMVar mvS $ maybe s' (\c -> addChange now c s') change
     updateWindow (windowState s) clear

eval :: MVar State -> Curses ()
eval mvS = 
  do s <- (liftIO $ takeMVar mvS)
     let (y,_) = posState s
         ls = codeState s
         block | hasChar (ls !! y) = findBlock
               | otherwise = []
         findChars = takeWhile (hasChar . (ls !!))
         pre = reverse $ findChars $ reverse [0 .. y]
         post | y == ((length ls) - 1) = []
              | otherwise = findChars [y+1 .. ((length ls) - 1)]
         findBlock = pre ++ post
         hasChar = or . map (/= ' ')
         codeblock = intercalate "\n" (map (ls !!) findBlock)
     liftIO $ putMVar (hintIn s) codeblock
     response <- liftIO $ takeMVar (hintOut s)
     act s response
     liftIO $ putMVar mvS $ s {hiliteState = findBlock}
     draw mvS
  where
    act s (HintOK p) = liftIO $ (dirtState s) p
    act s (HintError err) =
      do updateWindow (windowState s) $ do
           moveCursor 15 0
           drawString $ show err



waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
