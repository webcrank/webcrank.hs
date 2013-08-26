{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module TestData where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Builder (byteString, toLazyByteString)
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty(..))
import Network.HTTP.Types
import Webcrank

data TestRq = TestRq
  { testRqMethod :: Method
  , testRqHeaders :: RequestHeaders
  } deriving (Eq, Show)

testRq :: TestRq
testRq = TestRq
  { testRqMethod = methodGet
  , testRqHeaders = []
  }

testResp :: Status -> ResponseHeaders -> Maybe (ResponseBody ByteString) -> Response ByteString
testResp s hs b = (s, hs, b)

testBody :: ByteString -> Maybe (ResponseBody ByteString)
testBody = Just . StreamResponseBody

instance HasRequestInfo TestRq where
  rqMethod = testRqMethod
  rqHeader h r = snd <$> find ((h ==) . fst) (testRqHeaders r)

type TestRes = Response ByteString

instance Show (ResponseBody ByteString) where
  show (FileResponseBody fp r) = "FileResponseBody " ++ fp ++ " (" ++ show r ++ ")"
  show (StreamResponseBody b) = "StreamResponseBody " ++ show b
  show (BuilderResponseBody sb) = "BuilderResponseBody " ++ show (toLazyByteString sb)

instance Eq (ResponseBody ByteString) where
  (FileResponseBody fp r) == (FileResponseBody fp' r') = fp == fp' && r == r'
  (StreamResponseBody b) == (StreamResponseBody b') = b == b'
  (BuilderResponseBody sb) == (BuilderResponseBody sb') = toLazyByteString sb == toLazyByteString sb'
  (StreamResponseBody b) == (BuilderResponseBody sb) = b == toStrict (toLazyByteString sb)
  (BuilderResponseBody sb) == (StreamResponseBody b) = b == toStrict (toLazyByteString sb)
  _ == _ = False

type TestResource = Resource TestRq ByteString IO ()

testResource :: TestResource
testResource = resource' (return cts) where
  cts = (textHtml, toHtml) :| []
  toHtml = value $ StreamResponseBody "<html><body>Test</body></html"
