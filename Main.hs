module Main where

import System.Environment

main :: IO ()
main = do
  (arg0:arg1:_) <- getArgs
  mapM_ putStr ["Hello, ", arg0, " ", arg1]
