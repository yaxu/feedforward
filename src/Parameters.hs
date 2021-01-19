module Parameters where

import Options.Applicative

newtype Parameters = Parameters {scripts :: [String]}

parameters :: Parser Parameters
parameters = Parameters
      <$> many ( strOption
          ( long "scripts"
         <> short 's'
         <> metavar "FILE"
         <> help "Script file to be executed at start" ))