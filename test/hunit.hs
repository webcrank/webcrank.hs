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
  ]

(@=?>) = (=<<) . (@=?)

testServiceUnavailable = resp503 @=?> runResource rs testRq where
  rs = resource' { serviceAvailable = return False }
  resp503 = testResp serviceUnavailable503 [(hContentType, "text/html")] (BuilderResponseBody $ byteString "<html><head><title>503 Service Unavailable</title></head><body><h1>Service Unavailable</h1>The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.<br><p><hr><address>webcrank web server</address></body></html>")

