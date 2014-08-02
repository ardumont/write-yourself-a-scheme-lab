module Main where

import System.Environment

main :: IO ()
main = do
  (arg0:arg1:_) <- getArgs
  display arg0 arg1

display :: String -> String -> IO ()
display arg0 arg1 = mapM_ putStr ["Hello, ", arg0, " ", arg1]
