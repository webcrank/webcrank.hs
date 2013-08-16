{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (byteString)
import Network.HTTP.Types
import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit
import TestData
import Webcrank

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [resourceTests]

resourceTests = testGroup "runResource" 
  [ testCase "service is unavailable" testServiceUnavailable
  , testCase "not implemented" testNotImplemented 
  , testCase "uri too long" testUriTooLong
  ]

(@=?>) = (=<<) . (@=?)

testServiceUnavailable = resp503 @=?> runResource rs testRq where
  rs = resource' { serviceAvailable = return False }
  resp503 = testResp serviceUnavailable503 [(hContentType, "text/html")] (BuilderResponseBody $ byteString "<html><head><title>503 Service Unavailable</title></head><body><h1>Service Unavailable</h1>The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.<br><p><hr><address>webcrank web server</address></body></html>")

testNotImplemented = resp501 @=?> runResource resource' rq where
  rq = testRq { testRqMethod = "MOVE" }
  resp501 = testResp notImplemented501 [(hContentType, "text/html")] (BuilderResponseBody $ byteString "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>The server does not support the MOVE method.<br><p><hr><address>webcrank web server</address></body></html>")

testUriTooLong = resp414 @=?> runResource rs testRq where
  rs = resource' { uriTooLong = return True }
  resp414 = testResp requestURITooLong414 [(hContentType, "text/html")] (BuilderResponseBody $ byteString "<html><head><title>414 Request-URI Too Large</title></head><body><h1>Request-URI Too Large</h1><p><hr><address>webcrank web server</address></body></html>")

