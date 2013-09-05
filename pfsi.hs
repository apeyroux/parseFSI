-- analyse des logs de la ForgeSI

import Text.ParserCombinators.Parsec
import Control.Applicative
import System.Environment
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

valueBetween:: Char -> Char -> Parser String
valueBetween o c = between (char o) (char c) (Control.Applicative.many $ noneOf [c])

plainV, bracketedV, quotedV, finalQuote :: Parser String
plainV     = many1 (noneOf " \n") <* space
bracketedV = valueBetween '[' ']' <* space
quotedV    = valueBetween '"' '"' <* space
finalQuote = valueBetween '"' '"'

logLine :: Parser LogLine
logLine = do
    ip     <- plainV
    ident  <- plainV
    user   <- plainV
    date   <- bracketedV
    req    <- quotedV
    status <- plainV
    bytes  <- plainV
    ref    <- quotedV
    ua     <- finalQuote
    return $ LogLine ip ident user date req status bytes ref ua 

parseLine :: String -> Either ParseError LogLine
parseLine input = parse logLine "(unknown)" input 

main = do 
	args <- getArgs
	ln <- readFile (args!!0)
	mapM_ (\(hit, user) -> putStrLn $ "User : " ++ user ++ "\taction : " ++ show hit) $ reverse . sort . map (\r -> (length r, head r)) $ group . sort . map (\l -> case parseLine l of Right res -> getUser res)$ lines ln

