module Main where

import System.Environment

main :: IO ()
main = do
  (arg0:_) <- getArgs
  putStrLn ("Hello, " ++ arg0)
