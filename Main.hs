module Main where

import System.Environment

main :: IO ()
main = do
  name <- getLine
  putStrLn $ "Hello, " ++ name

display :: String -> String -> IO ()
display arg0 arg1 = mapM_ putStr ["Hello, ", arg0, " ", arg1]

compute :: Num a => a -> a -> a
compute num0 num1 = num0 + num1
