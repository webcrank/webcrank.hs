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
  , testCase "method not allowed" testMethodNotAllowed
  , testCase "bad request" testBadRequest
  , testCase "unauthorized" testUnauthorized
  ]

(@=?>) = (=<<) . (@=?)

testServiceUnavailable = resp503 @=?> runResource rs testRq where
  rs = resource' { serviceAvailable = value False }
  resp503 = testResp serviceUnavailable503 
                     [(hContentType, "text/html")] 
                     (BuilderResponseBody $ byteString "<html><head><title>503 Service Unavailable</title></head><body><h1>Service Unavailable</h1>The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.<br><p><hr><address>webcrank web server</address></body></html>")

testNotImplemented = resp501 @=?> runResource resource' rq where
  rq = testRq { testRqMethod = "MOVE" }
  resp501 = testResp notImplemented501 
                     [(hContentType, "text/html")] 
                     (BuilderResponseBody $ byteString "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>The server does not support the MOVE method.<br><p><hr><address>webcrank web server</address></body></html>")

testUriTooLong = resp414 @=?> runResource rs testRq where
  rs = resource' { uriTooLong = value True }
  resp414 = testResp requestURITooLong414 
                     [(hContentType, "text/html")] 
                     (BuilderResponseBody $ byteString "<html><head><title>414 Request-URI Too Long</title></head><body><h1>Request-URI Too Long</h1>Request-URI Too Long<p><hr><address>webcrank web server</address></body></html>")

testMethodNotAllowed = resp405 @=?> runResource resource' rq where
  rq = testRq { testRqMethod = "POST" }
  resp405 = testResp methodNotAllowed405 
                     [(hContentType, "text/html"), ("Allow", "GET, HEAD")]
                     (BuilderResponseBody $ byteString "<html><head><title>405 Method Not Allowed</title></head><body><h1>Method Not Allowed</h1>Method Not Allowed<p><hr><address>webcrank web server</address></body></html>")

testBadRequest = resp400 @=?> runResource rs testRq where
  rs = resource' { malformedRequest = value True }
  resp400 = testResp badRequest400 
                     [(hContentType, "text/html")] 
                     (BuilderResponseBody $ byteString "<html><head><title>400 Bad Request</title></head><body><h1>Bad Request</h1>Bad Request<p><hr><address>webcrank web server</address></body></html>")

testUnauthorized = resp401 @=?> runResource rs testRq where
  realm = "Basic realm=\"Webcrank\""
  rs = testResource { isAuthorized = unauthorized realm }
  resp401 = testResp unauthorized401 
                     [(hContentType, "text/html"), ("WWW-Authenticate", realm)] 
                     (BuilderResponseBody $ byteString "<html><head><title>401 Unauthorized</title></head><body><h1>Unauthorized</h1>Unauthorized<p><hr><address>webcrank web server</address></body></html>")

testForbidden = resp403 @=?> runResource rs testRq where
  rs = resource' { forbidden = value True }
  resp403 = testResp forbidden403
                     [(hContentType, "text/html")] 
                     (BuilderResponseBody $ byteString "<html><head><title>403 Forbidden</title></head><body><h1>Forbidden</h1>Forbidden<p><hr><address>webcrank web server</address></body></html>")

testInvalidContentHeaders = resp501 @=?> runResource rs testRq where
  rs = resource' { validContentHeaders = value False }
  resp501 = testResp notImplemented501
                     [(hContentType, "text/html")] 
                     (BuilderResponseBody $ byteString "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>Not Implemented<p><hr><address>webcrank web server</address></body></html>")

testUnknownContentType = resp415 @=?> runResource rs testRq where
  rs = resource' { knownContentType = value False }
  resp415 = testResp unsupportedMediaType415
                     [(hContentType, "text/html")] 
                     (BuilderResponseBody $ byteString "<html><head><title>415 Unsupported Media Type</title></head><body><h1>Unsupported Media Type</h1>Unsupported Media Type<p><hr><address>webcrank web server</address></body></html>")
