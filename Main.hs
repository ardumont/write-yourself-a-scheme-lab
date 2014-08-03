module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad (liftM)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value!"

data LispVal = Atom String
             | List [LispVal]
             | DottedLisp [LispVal] LispVal
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

main :: IO ()
main = do
  (arg0:_) <- getArgs
  putStrLn $ readExpr arg0
