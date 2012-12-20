module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show (Atom name)       = "Atom: " ++ name
  show (Number i)        = "Number: " ++ show i
  show (String str)      = "String: " ++ str
  show (Bool a)          = "Bool: " ++ show a
  show (List xs)         = "List[ " ++ (unwords (map show xs)) ++ " ]"
  show (DottedList xs x) = "DList[ " ++ (unwords (map show xs)) ++ " " ++ show x ++ " ]"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err           -> "No match: " ++ show err
                   Right val          -> "Found Value " ++ show val

parseReplacers = try . choice . map (\(p, r) -> p >> return (r p))

escapeReplacers = [(string "\\\"", const '\"'), (string "\\\n", const '\n'),(string "\\\b", const '\b'),(string "'\\\f", const '\f'),
                   (string "\\\r", const '\r'), (string "\\\t", const '\t'),(string "\\\\", const '\\'),(string "\\/", const '/')]

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ parseReplacers escapeReplacers <|> (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many $ letter <|> digit <|> symbol
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseHex :: Parser LispVal
parseHex = do num <- many1 $ digit <|> oneOf "abcdefABCDEF"
              return . Number . fst . head . readHex $ num

parseBinary :: Parser LispVal
parseBinary = do num <- many1 $ oneOf "01"
                 return . Number . foldl (\acc cur -> acc * 2 + bint cur) 0 $ num
  where bint c = if c == '0' then 0 else 1

numberReplacers = [(try $ char '#' >> oneOf "bB", const "#b"), (try $ char '#' >> oneOf "xX", const "#x")]


parseNumber :: Parser LispVal
parseNumber = do val <- many1 digit <|> (parseReplacers numberReplacers)
                 case val of
                   "#x" -> parseHex
                   "#b" -> parseBinary
                   _    -> return $ Number . read $ val

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseNumber
            <|> parseAtom
            <|> parseString
            <|> parseQuoted
            <|> do char '('
                   x <- (try parseList) <|> parseDottedList
                   char ')'
                   return x

main :: IO ()
main = do args <- getArgs
          case args of
            [x:xs] -> do let arg = args !! 0
                         putStrLn arg
                         putStrLn . readExpr $ arg
            _      -> do putStrLn "Enter expression: "
                         expression <- getLine
                         putStrLn . readExpr $ expression
