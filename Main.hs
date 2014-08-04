module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad (liftM)

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
  show = showVal

showVal :: LispVal -> String
showVal (String s)                     = unwords ["\"", s, "\""]
showVal (Bool True)                    = "#t"
showVal (Bool False)                   = "#f"
showVal (Atom name)                    = name
showVal (Number n)                     = show n
showVal (List lispVals)                = "(" ++ unwordsList lispVals ++ ")"
showVal (DottedList headVals tailVals) = "(" ++ unwordsList headVals ++ "." ++ showVal tailVals ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | A string begins with a "
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

-- | An atom is a letter or a symbol followed by any number of letters, digits or symbols.
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
             "#t"      -> Bool True
             "#f"      -> Bool False
             otherwise -> Atom atom

-- | A number is any number of digits
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- | A list is a LispVal expression space separated
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- | A dotted list is a list of space separated lisp expression and
-- a . to define the tail. The tial is also a lisp expression.
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- | Parse quote expression
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- | Parse LispVal expression
parseExpr :: Parser LispVal
parseExpr =   parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "No match: " ++ show err
  Right val -> val

eval :: LispVal -> LispVal
eval v@(String _)             = v
eval v@(Number _)             = v
eval v@(Bool   _)             = v
eval (List [Atom "quote", v]) = v
eval (List (Atom fn : args)) = apply fn $ map eval args

apply :: String -> [LispVal] -> LispVal
apply = undefined

main :: IO ()
main =
  getArgs >>= print . eval . readExpr . unwords
