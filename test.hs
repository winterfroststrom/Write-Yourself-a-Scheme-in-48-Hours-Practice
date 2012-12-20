module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Numeric

-- | Lisp syntax constructs
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

-- | Error

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where
  show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- | Parser

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> throwError $ Parser err
                   Right val -> return val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

-- | Eval

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [(Atom _)] = return $ Bool True
isSymbol [(_)] = return $ Bool False
isSymbol p = throwError $ NumArgs 1 p

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString [(_)] = return $ Bool False
isString p = throwError $ NumArgs 1 p

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber [(_)] = return $ Bool False
isNumber p = throwError $ NumArgs 1 p

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [(Bool _)] = return $ Bool True
isBoolean [(_)] = return $ Bool False
isBoolean p = throwError $ NumArgs 1 p

isList :: [LispVal] -> ThrowsError LispVal
isList [(List _)] = return $ Bool True
isList [(_)] = return $ Bool False
isList p = throwError $ NumArgs 1 p

isPair :: [LispVal] -> ThrowsError LispVal
isPair [(DottedList _ _)] = return $ Bool True
isPair [(_)] = return $ Bool False
isPair p = throwError $ NumArgs 1 p

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String str)] = return $ Atom str
stringToSymbol [(s)] = throwError $ TypeMismatch "string" s
stringToSymbol p = throwError $ NumArgs 1 p

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom a)] = return $ String a
symbolToString [(v)] = throwError $ TypeMismatch "symbol" v
symbolToString p = throwError $ NumArgs 1 p

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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



apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | Main

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
