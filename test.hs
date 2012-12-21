{-# LANGUAGE ExistentialQuantification #-}
module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Numeric
import System.IO

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

throwOneArgError :: [LispVal] -> ThrowsError LispVal
throwOneArgError =  throwError . NumArgs 1

throwTypeError :: String -> LispVal -> ThrowsError LispVal
throwTypeError t v = throwError . TypeMismatch t $ v

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

-- | Unpackers

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do unpacked1 <- unpacker arg1
                                                   unpacked2 <- unpacker arg2
                                                   return $ unpacked1 == unpacked2
                                                `catchError` (const $ return False)

-- | Eval

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [_] = return $ Bool False
isSymbol p = throwOneArgError p

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString p = throwOneArgError p

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber p = throwOneArgError p

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [Bool _] = return $ Bool True
isBoolean [_] = return $ Bool False
isBoolean p = throwOneArgError p

isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return $ Bool True
isList [_] = return $ Bool False
isList p = throwOneArgError p

isPair :: [LispVal] -> ThrowsError LispVal
isPair [DottedList _ _] = return $ Bool True
isPair [_] = return $ Bool False
isPair p = throwOneArgError p

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String str] = return $ Atom str
stringToSymbol [s] = throwTypeError "string" s
stringToSymbol p = throwOneArgError p

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom a] = return $ String a
symbolToString [v] = throwTypeError "symbol" v
symbolToString p = throwOneArgError p

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwTypeError "pair" badArg
car badArgList = throwOneArgError badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList _ x] = return x
cdr [badArg] = throwTypeError "pair" badArg
cdr p = throwOneArgError p

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

looseEqual :: LispVal -> LispVal -> ThrowsError LispVal
looseEqual arg1 arg2 = do eq <- liftM or $ mapM (unpackEquals arg1 arg2)
                                [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
                          return $ Bool eq

equal :: [LispVal] -> ThrowsError LispVal
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (and $ map equalPair $ zip arg1 arg2)
  where equalPair (x1, x2) = case equal [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
equal [arg1, arg2] = looseEqual arg1 arg2
equal badArgList = throwError $ NumArgs 2 badArgList

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
              ("symbol->string", symbolToString),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", pred, conseq, alt]) = do result <- eval pred
                                                case result of
                                                  Bool False -> eval alt
                                                  Bool True -> eval conseq
                                                  otherwise -> throwTypeError "bool" pred
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | Repl

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do result <- prompt
                               if pred result
                                 then return ()
                                 else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (liftM2 (||) (== "exit") (== "quit")) (readPrompt "Lisp>>> ") evalAndPrint

-- | Main

main :: IO ()
main = do args <- getArgs
          case length args of
              0 -> runRepl
              1 -> evalAndPrint $ args !! 0
              otherwise -> putStrLn "Program takes only 0 or 1 argument"
