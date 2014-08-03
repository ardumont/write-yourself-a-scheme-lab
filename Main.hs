module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

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

main :: IO ()
main = do
  (arg0:_) <- getArgs
  putStrLn $ readExpr arg0
