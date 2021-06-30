module MyLib where

import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispValue
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

data LispValue
  = Atom String
  | List [LispValue]
  | DottedList [LispValue] LispValue
  | Number Integer
  | String String
  | Bool Bool

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

showLispValue :: LispValue -> String
showLispValue (String contents) = "\"" ++ contents ++ "\""
showLispValue (Atom name) = name
showLispValue (Number contents) = show contents
showLispValue (Bool True) = "#t"
showLispValue (Bool False) = "#f"
showLispValue (List contents) = "(" ++ unwordsList contents ++ ")"
showLispValue (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showLispValue tail ++ ")"

unwordsList :: [LispValue] -> String
unwordsList = unwords . map showLispValue

instance Show LispValue where
  show = showLispValue

eval :: LispValue -> LispValue
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom fun : args)) = apply fun $ map eval args

apply :: String -> [LispValue] -> LispValue
apply fun args = maybe (Bool False) ($ args) (lookup fun primitives)

primitives :: [(String, [LispValue] -> LispValue)]
primitives =
  [ ("+", numericBinOperation (+)),
    ("-", numericBinOperation (-)),
    ("*", numericBinOperation (*)),
    ("/", numericBinOperation div),
    ("mod", numericBinOperation mod),
    ("quotient", numericBinOperation quot),
    ("remainder", numericBinOperation rem)
  ]

numericBinOperation :: (Integer -> Integer -> Integer) -> [LispValue] -> LispValue
numericBinOperation operation params = Number $ foldl1 operation $ map unpackNum params

unpackNum :: LispValue -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in if null parsed then 0 else fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- >>> eval . readExpr $ "'atom"
-- atom
-- >>> eval . readExpr $ "2"
-- 2
-- >>> eval . readExpr $ "\"a string\""
-- "a string"
-- >>> eval . readExpr $ "(+ 2 2)"
-- 4
-- >>> eval . readExpr $ "(+ 2 (-4 1))"
-- 2
-- >>> eval . readExpr $ "(+ 2 (- 4 1))"
-- 5
-- >>> eval . readExpr $ "(- (+ 4 6 3) 3 5 2)"
-- 3
