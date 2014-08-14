module SchemeTests where

import Test.HUnit

tests :: Test.HUnit.Test
tests = TestList []

main :: IO ()
main = runTestTT tests >>= print
