{-# LANGUAGE OverloadedStrings #-}

module ParserTests where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Builder as BB
import Data.ByteString.Lazy.Builder.ASCII (doubleDec)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char (chr)
import Data.List (sortBy)
import Network.HTTP.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Webcrank

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

instance (Arbitrary s, CI.FoldCase s) => Arbitrary (CI s) where
  arbitrary = CI.mk <$> arbitrary

parserTests = testGroup "parser tests"
  [ testProperty "parseAcceptHeader" parseAcceptHeaderProp
  , testProperty "parseMediaType" parseMediaTypeProp
--   , parseConnegHeaderTests
--   , parseEtags
  ]

newtype QVal = QVal { qval :: Double } deriving (Show, Eq, Ord)

instance Arbitrary QVal where
  arbitrary = QVal <$> arbitrary `suchThat` (\q -> q >= 0.0 && q <= 1.0)

instance Arbitrary MediaType where
  arbitrary = do 
    (MediaType pri sub _) <- elements 
      [ textJson
      , applicationJson
      , textHtml
      , applicationXml
      , textXml 
      , MediaType "text" "*" []
      , MediaType "application" "*" []
      ]
    ps <- listOf genParam
    return (MediaType pri sub ps)

genParam = (,) <$> name <*> value where
  name = (CI.mk <$> genToken) `suchThat` (/= "q")
  value = oneof [genToken, genQuotedString]

genToken = B.pack <$> gen where
  gen = listOf1 $ elements as
  as = [ a | a <- ['\33'..'\126'], a `notElem` "()<>@,;:\\\"/[]?={}" ]

genQuotedString = B.pack <$> gen where
  gen = listOf $ elements as
  as = '\13' : '\10' : '\9' : [ a | a <- ['\32'..'\126'], a /= '"' ]

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
      addQ = (,) "q" . LB.toStrict . BB.toLazyByteString . doubleDec . qval
    toHeader = B.intercalate "," . (renderMediaType <$>)

parseMediaTypeProp :: MediaType -> Bool
parseMediaTypeProp m = Just m == parsed where
  parsed = parseMediaType (renderMediaType m)

testParseMediaTypeWithExtraWS = Just mtype @=? parseMediaType accept
  where accept = "application/x-www-form-urlencoded          ;      charset      =       utf8"
        mtype  = MediaType "application" "x-www-form-urlencoded" [("charset", "utf8")]
