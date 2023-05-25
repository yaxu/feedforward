module TidalHint where

import           Control.Concurrent.MVar
import           Control.Exception            (IOException, SomeException)
import           Control.Monad
import           Control.Monad.Catch
import           Data.List                    (intercalate, isPrefixOf)
import           Language.Haskell.Interpreter as Hint
import           Sound.Tidal.Context
import           Sound.Tidal.Utils
import           System.IO
import           System.Posix.Signals

import           Parameters

data Response = HintOK {parsed :: ControlSignal}
              | HintError {errorMessage :: String}

instance Show Response where
  show (HintOK p)    = "Ok: " ++ show p
  show (HintError s) = "Error: " ++ s

imports = [
  ModuleImport "Data.Map" NotQualified (ImportList ["Map"]),
  ModuleImport "Prelude" NotQualified NoImportList,
  ModuleImport "Sound.Tidal.Context" NotQualified NoImportList
  ]

hintJob :: (MVar String, MVar Response) -> Parameters -> IO ()
hintJob (mIn, mOut) parameters =
  do result <- catch (do Hint.runInterpreter $ do
                           Hint.set [languageExtensions := [OverloadedStrings]]
                           Hint.setImportsF imports
                           execScripts (scripts parameters)
                           hintLoop
                     )
               (\e -> do hPutStrLn stderr $ "exception" ++ show (e :: SomeException)
                         return (Left $ UnknownError $ "exception" ++ show (e :: SomeException))
               )
     hPutStrLn stderr $ "hmm: " ++ show result
     takeMVar mIn
     putMVar mOut (toResponse result)
     hintJob (mIn, mOut) parameters
     where hintLoop = do s <- liftIO (readMVar mIn)
                         let munged = deltaMini s
                         liftIO $ hPutStrLn stderr $ "munged! " ++ munged
                         t <- Hint.typeChecksWithDetails munged
                         liftIO $ hPutStrLn stderr $ "typechecked"
                         --interp check s
                         interp t munged
                         hintLoop
           interp (Left errors) _ = do liftIO $ do hPutStrLn stderr $ "error: " ++ (concatMap show errors)
                                                   putMVar mOut $ HintError $ "Didn't typecheck" ++ (concatMap show errors)
                                                   takeMVar mIn
                                       return ()
           interp (Right t) s =
             do
                liftIO $ hPutStrLn stderr $ "Parsing: " ++ s
                p <- try (Hint.interpret s (Hint.as :: ControlSignal)) :: Interpreter (Either InterpreterError ControlSignal)
                case p of
                  Left exc -> liftIO $ do
                    hPutStrLn stderr $ parseError exc
                    putMVar mOut $ HintError (parseError exc)
                  Right pat -> liftIO $ do
                    hPutStrLn stderr "Eval"
                    putMVar mOut $ HintOK pat

                liftIO $ takeMVar mIn
                return ()

execScripts :: [String] -> Interpreter ()
execScripts paths = do
  forM_ paths $ \path -> do
    liftIO $ hPutStrLn stderr ("Loading script... " ++ path)
    readResult <- liftIO $ try (readFile path) :: Interpreter (Either IOException String)
    case readResult of
      Left exc -> liftIO $ hPutStrLn stderr ("Error loading script " ++ show exc)
      Right script -> do
        execResult <- try (runStmt script) :: Interpreter (Either InterpreterError ())
        case execResult of
          Left exc -> liftIO $ hPutStrLn stderr $ parseError exc
          Right () -> return ()

toResponse :: Either InterpreterError ControlSignal -> Response
toResponse (Left err) = HintError (parseError err)
toResponse (Right p)  = HintOK p

parseError :: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s
