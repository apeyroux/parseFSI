-- analyse des logs de la ForgeSI

import Text.ParserCombinators.Parsec
import Data.Function
import Data.List

data LogLine = LogLine {
      getIP     :: String
    , getIdent  :: String
    , getUser   :: String
    , getDate   :: String
    , getReq    :: String
    , getStatus :: String
    , getBytes  :: String
    , getRef    :: String
    , getUA     :: String
} deriving (Ord, Show, Eq)

plainValue :: Parser String
plainValue = many1 (noneOf " \n")

bracketedValue :: Parser String
bracketedValue = do
    char '['
    content <- many (noneOf "]")
    char ']'
    return content

quotedValue :: Parser String
quotedValue = do
    char '"'
    content <- many (noneOf "\"")
    char '"'
    return content

logLine :: Parser LogLine
logLine = do
    ip <- plainValue
    space -- parse and throw away a space
    ident <- plainValue
    space
    user <- plainValue
    space
    date <- bracketedValue
    space
    req <- quotedValue
    space
    status <- plainValue
    space
    bytes <- plainValue
    space
    ref <- quotedValue
    space
    ua <- quotedValue
    return $ LogLine ip ident user date req status bytes ref ua 

parseLine :: String -> Either ParseError LogLine
parseLine input = parse logLine "(unknown)" input 

-- info sur les stats ip : 
-- let ips = map (\l -> case l of Right res -> getIP res) r
-- reverse . sort $ map (\i -> (length i, head i)) (groupBy ((==)) ips)

main = do 
	ln <- readFile "access.log"
	let r = map parseLine (lines ln)
	let u = map (\l -> case l of Right res -> getUser res) r
	let t = reverse . sort $ map (\i -> (length i, head i)) (groupBy ((==)) u)
	mapM_ (\(x, y) -> putStrLn $ "Count: " ++ show x ++ " IP: " ++ y) t
