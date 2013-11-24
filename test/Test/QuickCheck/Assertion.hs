module Test.QuickCheck.Assertion where

import Test.QuickCheck.Property

(?==) :: (Eq a, Show a) => a -> a -> Result
a ?== b 
  | a == b    = succeeded
  | otherwise = failed { reason = "expected: " ++ show b ++ "\nbut got:  " ++ show a ++ "\n" }
