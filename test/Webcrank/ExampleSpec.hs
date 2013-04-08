{-# LANGUAGE ScopedTypeVariables #-}
module Webcrank.ExampleSpec where

import Webcrank
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "example" $ do
    it "identity" $ property $ \(n :: Int) ->
      bletch n == n
