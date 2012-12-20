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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal



readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> String $ "No match: " ++ show err
                   Right val -> val

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

isSymbol :: [LispVal] -> LispVal
isSymbol [(Atom _)] = Bool True
isSymbol _ = Bool False

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _ = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [(Number _)] = Bool True
isNumber _ = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean [(Bool _)] = Bool True
isBoolean _ = Bool False

isList :: [LispVal] -> LispVal
isList [(List _)] = Bool True
isList _ = Bool False

isPair :: [LispVal] -> LispVal
isPair [(DottedList _ _)] = Bool True
isPair _ = Bool False

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [(String str)] = Atom str

symbolToString :: [LispVal] -> LispVal
symbolToString [(Atom a)] = String a

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("number?", isNumber),
              ("boolean?", isBoolean),
              ("list?", isList),
              ("pair?", isPair),
              ("string->symbol", stringToSymbol),
              ("symbol->string", symbolToString)]



apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
