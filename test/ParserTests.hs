{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParserTests where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Webcrank.Internal

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

instance (Arbitrary s, CI.FoldCase s) => Arbitrary (CI s) where
  arbitrary = CI.mk <$> arbitrary

parserTests :: TestTree
parserTests = testGroup "parser tests"
  [ csl1Tests
  ]

testParse :: (Eq a, Show a) => Parser a -> ByteString -> Maybe a -> TestTree
testParse p s e = testCase (show s) (either (const Nothing) Just (parseOnly p s) @?= e)

csl1Tests :: TestTree
csl1Tests = testGroup "csl1"
  [ testCsl1 "foo,bar" (Just ["foo", "bar"])
  , testCsl1 "foo ,bar," (Just ["foo", "bar"])
  , testCsl1 "foo, ,bar,charlie  " (Just ["foo", "bar", "charlie"])
  , testCsl1 ", foo, ,bar" (Just ["foo", "bar"])
  , testCsl1 "" Nothing
  , testCsl1 "," Nothing
  , testCsl1 ",  ," Nothing
  ]

testCsl1 :: ByteString -> Maybe [ByteString] -> TestTree
testCsl1 = testParse (csl1 tokenP)

