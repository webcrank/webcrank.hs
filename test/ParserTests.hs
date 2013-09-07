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
import Data.Monoid
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

newtype HttpToken = HttpToken { unToken :: ByteString }
  deriving (Eq, Show)

newtype HttpQuotedString = HttpQuotedString { unQuotedString :: ByteString }
  deriving (Eq, Show)

data GenMediaType = GenMediaType (CI ByteString) (CI ByteString) [(CI ByteString, Either HttpToken HttpQuotedString)]
  deriving (Eq, Show)

renderGenMediaType :: GenMediaType -> ByteString
renderGenMediaType (GenMediaType pri sub ps) = B.concat [ CI.original pri, "/", CI.original sub, params ] where
  params = B.concat (renderP <$> ps)
  renderP (k, v) = B.concat [ ";", CI.original k, "=", renderV v ] where
  renderV = either unToken (renderQuotedString . unQuotedString)

mkMediaType :: GenMediaType -> MediaType
mkMediaType (GenMediaType pri sub ps) = MediaType pri sub ps' where
  ps' = (mkVal <$>) <$> ps
  mkVal = either unToken unQuotedString

newtype QVal = QVal { qval :: Double } deriving (Show, Eq, Ord)

instance Arbitrary QVal where
  arbitrary = QVal <$> arbitrary `suchThat` (\q -> q >= 0.0 && q <= 1.0)

instance Arbitrary GenMediaType where
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
    return $ GenMediaType pri sub ps
  shrink (GenMediaType pri sub ps) = (\ps' -> GenMediaType pri sub ps') <$> shrink ps

genParam = (,) <$> name <*> value where
  name = CI.mk . unToken <$> arbitrary `suchThat` (\n -> n /= HttpToken "q" && n /= HttpToken "Q")
  value = oneof [Left <$> arbitrary, Right <$> arbitrary]

instance Arbitrary HttpToken where
  arbitrary = HttpToken . B.pack <$> gen where
		gen = listOf1 $ elements as
		as = [ a | a <- ['\33'..'\126'], a `notElem` "()<>@,;:\\\"/[]?={}" ]

instance Arbitrary HttpQuotedString where
  arbitrary = HttpQuotedString . B.pack <$> gen where
		gen = listOf $ elements as
		as = '\13' : '\10' : '\9' : [ a | a <- ['\32'..'\126'], a /= '"' ]

parseAcceptHeaderProp :: [(GenMediaType, Maybe QVal)] -> Bool
parseAcceptHeaderProp xs = sorted == parsed where
  sorted = fst <$> sortBy ord (norm <$> xs) where
    norm (mt, Nothing) = (mkMediaType mt, QVal 1.0)
    norm (mt, Just q) = (mkMediaType mt, q)
    ord (MediaType prix subx _, qx) (MediaType priy suby _, qy) 
      | qx > qy = GT
      | qx < qy = LT
      | prix == priy && suby == "*" = GT
      | prix == priy && subx == "*" = LT
      | otherwise = EQ
  parsed = parseAcceptHeader header
  header = B.intercalate "," (render <$> xs) where
    render (gmt, q) = renderGenMediaType gmt <> qparam q
    qparam = maybe "" ((";q=" <>) . LB.toStrict . BB.toLazyByteString . doubleDec . qval)

parseMediaTypeProp :: GenMediaType -> Bool
parseMediaTypeProp m = Just (mkMediaType m) == parsed where
  parsed = parseMediaType (renderGenMediaType m)

testParseMediaTypeWithExtraWS = Just mtype @=? parseMediaType accept
  where accept = "application/x-www-form-urlencoded          ;      charset      =       utf8"
        mtype  = MediaType "application" "x-www-form-urlencoded" [("charset", "utf8")]
