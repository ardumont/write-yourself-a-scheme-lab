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
             deriving Show

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

-- | Parse LispVal expression
parseExpr :: Parser LispVal
parseExpr =   parseAtom
          <|> parseString
          <|> parseNumber
          <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> Maybe LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> Nothing
  Right val -> Just val

main :: IO ()
main = do
  args <- getArgs
  print $ readExpr (unwords args)
