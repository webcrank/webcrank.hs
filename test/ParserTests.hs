{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserTests where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck
import Text.Printf

import Webcrank
import Test.QuickCheck.Assertion

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

instance (Arbitrary s, CI.FoldCase s) => Arbitrary (CI s) where
  arbitrary = CI.mk <$> arbitrary

parserTests :: TestTree
parserTests = testGroup "parser tests"
  [ cslTests
  , csl1Tests
  , testProperty "parseAccept " parseAcceptProp
  , testProperty "parseMediaType" parseMediaTypeProp
  , testProperty "parseAcceptLang" parseAcceptLangProp
	, testProperty "parseAcceptEnc" parseAcceptEncProp
  ]

cslTests = testGroup "csl"
  [ testCsl "foo,bar" (Just ["foo", "bar"])
  , testCsl "foo ,bar," (Just ["foo", "bar"])
  , testCsl "foo, ,bar,charlie  " (Just ["foo", "bar", "charlie"])
  , testCsl ", foo, ,bar" (Just ["foo", "bar"])
  , testCsl "" (Just [])
  , testCsl "," (Just [])
  , testCsl ",  ," (Just [])
  ]

testParse :: (Eq a, Show a) => Parser a -> ByteString -> Maybe a -> TestTree
testParse p s e = testCase (show s) (either (const Nothing) Just (parseOnly p s) @?= e)

testCsl :: ByteString -> Maybe [ByteString] -> TestTree
testCsl = testParse (csl tokenP) 

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

newtype HttpToken = HttpToken { unToken :: ByteString }
  deriving (Eq, Show)

newtype HttpQuotedString = HttpQuotedString { unQuotedString :: ByteString }
  deriving (Eq, Show)

data GenMediaType = GenMediaType 
  { mediaTypePri:: CI ByteString
  , mediaTypeSub :: CI ByteString
  , mediaTypeParams :: [(CI ByteString, Either HttpToken HttpQuotedString)]
  , mediaTypeQVal :: Maybe QVal
  }

instance Show GenMediaType where
  show = show . renderGenMediaType

renderGenMediaType :: GenMediaType -> ByteString
renderGenMediaType (GenMediaType pri sub ps q) = B.concat [ CI.original pri, "/", CI.original sub, params ] where
  params = B.concat (renderP <$> ps) <> qparam
  renderP (k, v) = B.concat [ ";", CI.original k, "=", renderV v ] where
    renderV = either unToken (renderQuotedString . unQuotedString)
  qparam = fromMaybe "" ((";q=" <>) . B.pack . printf "%.3f" . qval <$> q)

mkMediaType :: GenMediaType -> (MediaType, Double)
mkMediaType (GenMediaType pri sub ps q) = (MediaType pri sub ps', q') where
  ps' = (mkVal <$>) <$> ps
  mkVal = either unToken unQuotedString
  q' = maybe 1.0 qval q

newtype QVal = QVal { qval :: Double } deriving (Show, Eq, Ord)

instance Arbitrary QVal where
  arbitrary = QVal . (/1000.0) . fromIntegral <$> gen where
    gen = frequency [(5, pure 0), (5, pure 1000), (90, choose (0, 1000))] :: Gen Int

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
    q <- arbitrary
    return $ GenMediaType pri sub ps q
  shrink (GenMediaType pri sub ps q) = GenMediaType pri sub <$> shrink ps <*> shrink q

genParam :: Gen (CI ByteString, Either HttpToken HttpQuotedString)
genParam = (,) <$> pname <*> pval where
  pname = CI.mk . unToken <$> arbitrary `suchThat` (\n -> n /= HttpToken "q" && n /= HttpToken "Q")
  pval = oneof [Left <$> arbitrary, Right <$> arbitrary]

instance Arbitrary HttpToken where
  arbitrary = HttpToken . B.pack <$> gen where
  	gen = listOf1 $ elements as
  	as = [ a | a <- ['\33'..'\126'], a `notElem` "()<>@,;:\\\"/[]?={}" ]

instance Arbitrary HttpQuotedString where
  arbitrary = HttpQuotedString . B.pack <$> gen where
    gen = listOf $ elements as
    as = "\t !" ++ ['\x23'..'\x5B'] ++ ['\x5D'..'\x7E'] ++ ['\x80'..'\FF']

parseAcceptProp xs = sorted ?== parsed where
  sorted = fst <$> sortBy ord (mkMediaType <$> xs) where
    ord (MediaType prix subx _, qx) (MediaType priy suby _, qy) 
      | qx > qy = GT
      | qx < qy = LT
      | prix == priy && suby == "*" = GT
      | prix == priy && subx == "*" = LT
      | otherwise = EQ
  parsed = parseAccept header
  header = B.intercalate "," (renderGenMediaType <$> xs)

parseMediaTypeProp m = Just (mkMediaType m) ?== parsed where
  parsed = parseMediaType (renderGenMediaType m)

instance Show [(HttpToken, Maybe QVal)] where
  show = show . B.intercalate "," . (renderAcceptVal <$>)

renderAcceptVal :: (HttpToken, Maybe QVal) -> ByteString
renderAcceptVal (t, q) = unToken t <> qparam q where
  qparam q = fromMaybe "" ((";q=" <>) . B.pack . printf "%.3f" . qval <$> q)

renderAcceptVals :: [(HttpToken, Maybe QVal)] -> ByteString
renderAcceptVals = B.intercalate "," . (renderAcceptVal <$>)

parseAcceptLangProp xs = parseAcceptLang a ?== e where
  a = renderAcceptVals xs
  e = un <$> xs where
    un (t, q) = (CI.mk (unToken t), maybe 1.0 qval q)

parseAcceptEncProp xs = parseAcceptEnc a ?== e where
  a = renderAcceptVals xs
  e = un <$> xs where
    un (t, q) = (CI.mk (unToken t), maybe 1.0 qval q)
