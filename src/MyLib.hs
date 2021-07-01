module MyLib where

import Control.Monad.Except
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
  | Default String

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseError) = "Parse error at " ++ show parseError
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message fun) = message ++ ": " ++ fun
  show (UnboundVar message varName) = message ++ ": " ++ varName

type ThrowsError = Either LispError

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
    ("remainder", numBinOperation rem)
  ]

numBinOperation :: (Integer -> Integer -> Integer) -> [LispValue] -> ThrowsError LispValue
numBinOperation operation param@[_] = throwError $ NumArgs 2 param
numBinOperation operation params = Number . foldl1 operation <$> mapM unpackNum params

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

-- Run

run :: String -> String
run input = extractValue . trapError . fmap show $ readExpr input >>= eval

readExpr :: String -> ThrowsError LispValue
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> pure val

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = action `catchError` (pure . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
