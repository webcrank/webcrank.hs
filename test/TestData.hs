{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}

module TestData where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (toLazyByteString)
import Data.Foldable (find)
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

testResp :: Status -> ResponseHeaders -> ResponseBody ByteString -> Response ByteString
testResp s hs b = (s, hs, b)

instance HasRequestInfo TestRq where
  rqMethod = testRqMethod
  rqHeader h r = snd <$> find ((h ==) . fst) (testRqHeaders r)

type TestRes = Response ByteString

instance Show (ResponseBody ByteString) where
  show (FileResponseBody fp r) = "FileResponseBody " ++ fp ++ " (" ++ show r ++ ")"
  show (StreamResponseBody b) = "StreamResponseBody " ++ show b
  show (BuilderResponseBody sb) = "BuilderResponseBody " ++ (show $ toLazyByteString sb)

instance Eq (ResponseBody ByteString) where
  (FileResponseBody fp r) == (FileResponseBody fp' r') = fp == fp' && r == r'
  (StreamResponseBody b) == (StreamResponseBody b') = b == b'
  (BuilderResponseBody sb) == (BuilderResponseBody sb') = toLazyByteString sb == toLazyByteString sb'

type TestResource = Resource TestRq ByteString IO ()

