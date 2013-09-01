{-# LANGUAGE OverloadedStrings #-}

module ParserTests where

import Control.Applicative ((<$>))
import Data.ByteString as B
import Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Builder as BB
import Data.ByteString.Lazy.Builder.ASCII (doubleDec)
import Data.List (sortBy)
import Network.HTTP.Types
import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Webcrank

parserTests = testGroup "parser tests"
  [ testProperty "parseAcceptHeader" parseAcceptHeaderProp
  , parseMediaTypeTests
  ]

newtype QVal = QVal { qval :: Double } deriving (Show, Eq, Ord)

instance Arbitrary QVal where
  arbitrary = QVal <$> arbitrary `suchThat` (\q -> q >= 0.0 && q <= 1.0)

instance Arbitrary MediaType where
  arbitrary = elements 
    [ textJson
    , applicationJson
    , textHtml
    , applicationXml
    , textXml 
    , MediaType "text" "*" []
    , MediaType "application" "*" []
    ]

parseAcceptHeaderProp :: [(MediaType, Maybe QVal)] -> Bool
parseAcceptHeaderProp xs = sorted == parsed where
  sorted = fst <$> sortBy ord (norm <$> xs) where
    norm (mt, Nothing) = (mt, QVal 1.0)
    norm (mt, Just q) = (mt, q)
    ord (MediaType prix subx _, qx) (MediaType priy suby _, qy) 
      | qx > qy = GT
      | qx < qy = LT
      | prix == priy && suby == "*" = GT
      | prix == priy && subx == "*" = LT
      | otherwise = EQ
  parsed = parseAcceptHeader (toHeader combined) where
    combined = combine <$> xs where
      combine (MediaType pri sub ps, q) = MediaType pri sub (maybe ps ((:ps) . addQ) q)
      addQ = ((,) "q") . LB.toStrict . BB.toLazyByteString . doubleDec . qval
    toHeader = B.intercalate "," . (renderMediaType <$>)

parseMediaTypeTests = testGroup "parseMediaType"
  [ testCase "with extra whitespace" testParseMediaTypeWithExtraWS
  ]

testParseMediaTypeWithExtraWS = Just mtype @=? parseMediaType accept
  where accept = "application/x-www-form-urlencoded          ;      charset      =       utf8"
        mtype  = MediaType "application" "x-www-form-urlencoded" [("charset", "utf8")]
