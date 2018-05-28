module TidalHint where

import           Control.Exception
import           Language.Haskell.Interpreter as Hint
import           Sound.Tidal.Context
import           System.IO
import           System.Posix.Signals

data Response = HintOK {parsed :: ParamPattern}
              | HintError {errorMessage :: String}

instance Show Response where
  show (HintOK p)    = "Ok: " ++ show p
  show (HintError s) = "Error: " ++ s

{-
runJob :: String -> IO (Response)
runJob job = do putStrLn $ "Parsing: " ++ job
                result <- hintParamPattern job
                let response = case result of
                      Left err -> Error (show err)
                      Right p -> OK p
                return response
-}

libs = ["Prelude","Sound.Tidal.Context","Sound.OSC.Datum"
        -- ,"Sound.Tidal.Simple"
       ]

{-
hintParamPattern  :: String -> IO (Either InterpreterError ParamPattern)
hintParamPattern s = Hint.runInterpreter $ do
  Hint.set [languageExtensions := [OverloadedStrings]]
  Hint.setImports libs
  Hint.interpret s (Hint.as :: ParamPattern)
-}

hintJob  :: (MVar String, MVar Response) -> IO ()
hintJob (mIn, mOut) =
  do installHandler sigINT Ignore Nothing
     installHandler sigTERM Ignore Nothing
     installHandler sigPIPE Ignore Nothing
     installHandler sigHUP Ignore Nothing
     installHandler sigKILL Ignore Nothing
     installHandler sigSTOP Ignore Nothing
     result <- catch (do Hint.runInterpreter $ do
                           _ <- liftIO $ installHandler sigINT Ignore Nothing
                           Hint.set [languageExtensions := [OverloadedStrings]]
                           --Hint.setImports libs
                           Hint.setImportsQ $ (Prelude.map (\x -> (x, Nothing)) libs) ++ [("Data.Map", Nothing)]
                           hintLoop
                     )
               (\e -> return (Left $ UnknownError $ "exception" ++ show (e :: SomeException)))
     let response = case result of
          Left err -> HintError (parseError err)
          Right p  -> HintOK p -- can happen
         parseError (UnknownError s) = "Unknown error: " ++ s
         parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
         parseError (NotAllowed s) = "NotAllowed error: " ++ s
         parseError (GhcException s) = "GHC Exception: " ++ s
         --parseError _ = "Strange error"

     takeMVar mIn
     putMVar mOut response
     hintJob (mIn, mOut)
     where hintLoop = do s <- liftIO (readMVar mIn)
                         -- check <- typeChecks s
                         --interp check s
                         interp True s
                         hintLoop
           interp True s = do p <- Hint.interpret s (Hint.as :: ParamPattern)
                              liftIO $ putMVar mOut $ HintOK p
                              liftIO $ takeMVar mIn
                              return ()
           interp False _ = do liftIO $ putMVar mOut $ HintError "Didn't typecheck"
                               liftIO $ takeMVar mIn
                               return ()

