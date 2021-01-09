module Parameters where

-- TODO: better define a data Parameters containing all the parameters
type PScript = String

parseScripts :: [String] -> [PScript]
parseScripts [] = []
parseScripts ("-s":v:xs) = v : parseScripts xs
parseScripts (_:_:xs) = parseScripts xs
