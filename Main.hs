module Main where

import System.Environment

main :: IO ()
main = do
  (arg0:arg1:_) <- getArgs
  let n0 = read arg0 :: Int
      n1 = read arg1 :: Int
  putStrLn $ arg0 ++ " + " ++ arg1 ++ " = " ++ show (compute n0 n1)

display :: String -> String -> IO ()
display arg0 arg1 = mapM_ putStr ["Hello, ", arg0, " ", arg1]

compute :: Num a => a -> a -> a
compute num0 num1 = num0 + num1
