import UI.NCurses
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Char

type Code = [String]
type Pos = (Int, Int)

data State = State {codeState :: Code,
                    posState :: Pos,
                    xWarp :: Int,
                    windowState :: Window
                   }

goCursor state = moveCursor (fromIntegral $ fst $ posState state) (fromIntegral $ snd $ posState state)

draw :: MVar State -> Curses ()
draw mvS = do s <- (liftIO $ readMVar mvS)
              updateWindow (windowState s) $ do
                mapM_ drawLine $ zip (codeState s) [0 ..]
                goCursor s
              return ()
  where drawLine (line, n) = do moveCursor n 0
                                drawString line

initState :: Window -> State
initState w = State {codeState = ["every 3 rev $ sound \"bd sn\"", "  where foo = 1", "", "hello world"],
                     posState = (0,0),
                     windowState = w,
                     xWarp = 0
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
                                                xWarp = xw
                                               }

main :: IO ()
main = do runCurses $ do
            setEcho False
            w <- defaultWindow
            updateWindow w clear
            mvS <- (liftIO $ newMVar $ initState w)
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
             Just (EventSpecialKey KeyDeleteCharacter) -> delete mvS >> loop
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

keyCtrl mvS 'd' = delete mvS
keyCtrl mvS 'k' = killLine mvS

keyCtrl mvS 'j' = insertBreak mvS

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
     liftIO $ putMVar mvS $ s {codeState = ls',
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
     liftIO $ putMVar mvS $ s {codeState = ls',
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

backspace :: MVar State -> Curses ()
backspace mvS =
  do s <- (liftIO $ takeMVar mvS)
     let (y,x) = posState s
         s' | x > 0 = backspaceChar s
            | y == 0 = s
            | otherwise = joinLines s
     liftIO $ putMVar mvS s'
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

delete :: MVar State -> Curses ()
delete mvS =
  do s <- (liftIO $ takeMVar mvS)
     let (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
         s' | x < (length l) = deleteChar s
            | y == ((length ls) - 1) = s
            | otherwise = joinLinesBack s
     liftIO $ putMVar mvS s'
     updateWindow (windowState s) clear

killLine :: MVar State -> Curses ()
killLine mvS =
  do s <- (liftIO $ takeMVar mvS)
     let (ls, (y,x), preL, l, postL, preX, postX) = cursorContext s
         s' | x < (length l) = deleteToEnd s
            | y == ((length ls) - 1) = s
            | otherwise = joinLinesBack s
     liftIO $ putMVar mvS s'
     updateWindow (windowState s) clear


waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
