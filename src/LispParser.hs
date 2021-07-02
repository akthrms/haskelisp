{-# LANGUAGE ExistentialQuantification #-}

module LispParser (runRepl, evalAndPoint) where

import Control.Monad.Except
import System.IO (hFlush, stdout)
import Text.ParserCombinators.Parsec hiding (spaces)

-- Types

data LispValue
  = Atom String
  | List [LispValue]
  | DottedList [LispValue] LispValue
  | Number Integer
  | String String
  | Bool Bool

instance Show LispValue where
  show (Atom name) = name
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Number contents) = show contents
  show (String contents) = "\"" ++ contents ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"

unwordsList :: [LispValue] -> String
unwordsList = unwords . map show

data LispError
  = NumArgs Integer [LispValue]
  | TypeMismatch String LispValue
  | Parser ParseError
  | BadSpecialForm String LispValue
  | NotFunction String String
  | UnboundVar String String

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseError) = "Parse error at " ++ show parseError
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message fun) = message ++ ": " ++ fun
  show (UnboundVar message varName) = message ++ ": " ++ varName

type ThrowsError = Either LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispValue -> ThrowsError a)

-- Parsers

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispValue
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      pure x

parseString :: Parser LispValue
parseString = do
  char '"'
  x <- many $ noneOf "\""
  char '"'
  pure $ String x

parseAtom :: Parser LispValue
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  pure $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispValue
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispValue
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispValue
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  pure $ DottedList head tail

parseQuoted :: Parser LispValue
parseQuoted = do
  char '\''
  x <- parseExpr
  pure $ List [Atom "quote", x]

-- Evals

eval :: LispValue -> ThrowsError LispValue
eval val@(String _) = pure val
eval val@(Number _) = pure val
eval val@(Bool _) = pure val
eval (List [Atom "quote", val]) = pure val
eval (List [Atom "if", pred, thenVal, elseVal]) = do
  result <- eval pred
  case result of
    Bool False -> eval elseVal
    _ -> eval thenVal
eval (List (Atom fun : args)) = mapM eval args >>= apply fun
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispValue] -> ThrowsError LispValue
apply fun args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" fun)
    ($ args)
    (lookup fun primitives)

primitives :: [(String, [LispValue] -> ThrowsError LispValue)]
primitives =
  [ ("+", numBinOperation (+)),
    ("-", numBinOperation (-)),
    ("*", numBinOperation (*)),
    ("/", numBinOperation div),
    ("mod", numBinOperation mod),
    ("quotient", numBinOperation quot),
    ("remainder", numBinOperation rem),
    ("=", boolBinOperation unpackNum (==)),
    ("<", boolBinOperation unpackNum (<)),
    (">", boolBinOperation unpackNum (>)),
    ("/=", boolBinOperation unpackNum (/=)),
    ("<=", boolBinOperation unpackNum (<=)),
    (">=", boolBinOperation unpackNum (>=)),
    ("&&", boolBinOperation unpackBool (&&)),
    ("||", boolBinOperation unpackBool (||)),
    ("string=?", boolBinOperation unpackString (==)),
    ("string<?", boolBinOperation unpackString (<)),
    ("string>?", boolBinOperation unpackString (>)),
    ("string<=?", boolBinOperation unpackString (<=)),
    ("string>=?", boolBinOperation unpackString (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

numBinOperation :: (Integer -> Integer -> Integer) -> [LispValue] -> ThrowsError LispValue
numBinOperation operation arg@[_] = throwError $ NumArgs 2 arg
numBinOperation operation args = Number . foldl1 operation <$> mapM unpackNum args

boolBinOperation :: (LispValue -> ThrowsError a) -> (a -> a -> Bool) -> [LispValue] -> ThrowsError LispValue
boolBinOperation unpacker operation args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      pure $ Bool $ left `operation` right

unpackNum :: LispValue -> ThrowsError Integer
unpackNum (Number n) = pure n
unpackNum (String n) =
  if null parsed
    then throwError $ TypeMismatch "number" (String n)
    else pure $ (fst . head) parsed
  where
    parsed = reads n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackString :: LispValue -> ThrowsError String
unpackString (String s) = pure s
unpackString (Number s) = pure $ show s
unpackString (Bool s) = pure $ show s
unpackString notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispValue -> ThrowsError Bool
unpackBool (Bool b) = pure b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispValue] -> ThrowsError LispValue
car [List (x : _)] = pure x
car [DottedList (x : _) _] = pure x
car [badArgs] = throwError $ TypeMismatch "pair" badArgs
car badArgs = throwError $ NumArgs 1 badArgs

cdr :: [LispValue] -> ThrowsError LispValue
cdr [List (_ : xs)] = pure $ List xs
cdr [DottedList [_] x] = pure x
cdr [DottedList (_ : xs) x] = pure $ DottedList xs x
cdr [badArgs] = throwError $ TypeMismatch "pair" badArgs
cdr badArgs = throwError $ NumArgs 1 badArgs

cons :: [LispValue] -> ThrowsError LispValue
cons [x, List []] = pure $ List [x]
cons [x, List xs] = pure $ List $ x : xs
cons [x, DottedList xs y] = pure $ DottedList (x : xs) y
cons [x, y] = pure $ DottedList [x] y
cons badArgs = throwError $ NumArgs 2 badArgs

eqv :: [LispValue] -> ThrowsError LispValue
eqv [Bool arg1, Bool arg2] = pure $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = pure $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = pure $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = pure $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List args1, List args2] =
  pure $ Bool $ (length args1 == length args2) && all eqvPair (zip args1 args2)
  where
    eqvPair (x, y) = case eqv [x, y] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = pure $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

unpackEquals :: LispValue -> LispValue -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    pure $ unpacked1 == unpacked2
    `catchError` const (pure False)

equal :: [LispValue] -> ThrowsError LispValue
equal [arg1, arg2] = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [ AnyUnpacker unpackNum,
          AnyUnpacker unpackString,
          AnyUnpacker unpackBool
        ]
  eqvEquals <- eqv [arg1, arg2]
  pure $ Bool $ primitiveEquals || let (Bool x) = eqvEquals in x
equal badArgs = throwError $ NumArgs 2 badArgs

-- IO

readExpr :: String -> ThrowsError LispValue
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> pure val

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = action `catchError` (pure . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

flushString :: String -> IO ()
flushString s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushString prompt >> getLine

evalString :: String -> IO String
evalString expr = pure $ extractValue $ trapError $ fmap show $ eval =<< readExpr expr

evalAndPoint :: String -> IO ()
evalAndPoint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then pure ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>>") evalAndPoint
