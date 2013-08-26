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
tests = testGroup "Unit tests" 
  [ connegTests
  , parserTests
  , runResourceTests
  ]

connegTests = testGroup "conneg tests"
  [ chooseMediaTypeTests
  ]

chooseMediaTypeTests = testGroup "chooseMediaType"
  [ testMediaTypeChosen [textHtml] [MediaType "*" "*" []] textHtml
  , testMediaTypeChosen [textHtml] [MediaType "text" "*" []] textHtml
  , testMediaTypeChosen [textHtml] [textHtml] textHtml
  , testMediaTypeNotChosen [textHtml] [MediaType "foo" "*" []]
  , testMediaTypeNotChosen [textHtml] [textXml]
  , testMediaTypeNotChosen [textHtml] [textXml]
  , testMediaTypeChosen [textHtml, imageJpeg] [imageJpeg, textHtml] imageJpeg
  , testMediaTypeChosen [textHtml, imageJpeg] [MediaType "image" "*" [], MediaType "text" "*" []] imageJpeg
  , testMediaTypeChosen [textHtml, imageJpeg] [MediaType "image" "*" [], MediaType "image" "png" []] imageJpeg
  ]

testMediaTypeChosen ps as m = testCase n $ test where
  n = show ps ++ " matches " ++ show m
  test = chooseMediaType ps as @?= Just m

testMediaTypeNotChosen ps as = testCase n $ test where
  n = show ps ++ " does not match " ++ show as
  test = chooseMediaType ps as @?= Nothing

parserTests = testGroup "parser tests"
  [ parseAcceptHeaderTests
  , parseMediaTypeTests
  ]

parseAcceptHeaderTests = testGroup "parseAcceptHeader"
  [ testCase "with q vals" testParseAcceptHeaderWithQs
  ]

testParseAcceptHeaderWithQs = expected @=? parseAcceptHeader hdr
  where expected = [MediaType "application" "json" [("charset","utf8")], textHtml, applicationXml]
        hdr = "text/html;q=0.8, application/xml;q=0.5, application/json;charset=utf8"

parseMediaTypeTests = testGroup "parseMediaType"
  [ testCase "with extra whitespace" testParseMediaTypeWithExtraWS
  ]

testParseMediaTypeWithExtraWS = Just mtype @=? parseMediaType accept
  where accept = "application/x-www-form-urlencoded          ;      charset      =       utf8"
        mtype  = MediaType "application" "x-www-form-urlencoded" [("charset", "utf8")]

runResourceTests = testGroup "runResource" 
  [ testCase "service is unavailable" testServiceUnavailable
  , testCase "not implemented" testNotImplemented 
  , testCase "uri too long" testUriTooLong
  , testCase "method not allowed" testMethodNotAllowed
  , testCase "bad request" testBadRequest
  , testCase "unauthorized" testUnauthorized
  , testCase "forbidden" testForbidden
  , testCase "unknown or unsupported Content-* headers" testInvalidContentHeaders 
  , testCase "unknown Content-Type" testUnknownContentType 
  , testCase "entity too large" testEntityTooLarge
  , testCase "OPTIONS" testOptions 
  , testCase "not acceptable" testNotAcceptable 
  ]

(<@=?>) = (=<<) . (@=?)

testServiceUnavailable = resp503 <@=?> runResource rs testRq where
  rs = testResource { serviceAvailable = value False }
  resp503 = testResp serviceUnavailable503 
                     [(hContentType, "text/html")] 
                     (testBody "<html><head><title>503 Service Unavailable</title></head><body><h1>Service Unavailable</h1>The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.<p><hr><address>webcrank web server</address></body></html>")

testNotImplemented = resp501 <@=?> runResource testResource rq where
  rq = testRq { testRqMethod = "MOVE" }
  resp501 = testResp notImplemented501 
                     [(hContentType, "text/html")] 
                     (testBody "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>The server does not support the MOVE method.<p><hr><address>webcrank web server</address></body></html>")

testUriTooLong = resp414 <@=?> runResource rs testRq where
  rs = testResource { uriTooLong = value True }
  resp414 = testResp requestURITooLong414 
                     [(hContentType, "text/html")] 
                     (testBody "<html><head><title>414 Request-URI Too Long</title></head><body><h1>Request-URI Too Long</h1>Request-URI Too Long<p><hr><address>webcrank web server</address></body></html>")

testMethodNotAllowed = resp405 <@=?> runResource testResource rq where
  rq = testRq { testRqMethod = "POST" }
  resp405 = testResp methodNotAllowed405 
                     [(hContentType, "text/html"), ("Allow", "GET, HEAD")]
                     (testBody "<html><head><title>405 Method Not Allowed</title></head><body><h1>Method Not Allowed</h1>Method Not Allowed<p><hr><address>webcrank web server</address></body></html>")

testBadRequest = resp400 <@=?> runResource rs testRq where
  rs = testResource { malformedRequest = value True }
  resp400 = testResp badRequest400 
                     [(hContentType, "text/html")] 
                     (testBody "<html><head><title>400 Bad Request</title></head><body><h1>Bad Request</h1>Bad Request<p><hr><address>webcrank web server</address></body></html>")

testUnauthorized = resp401 <@=?> runResource rs testRq where
  realm = "Basic realm=\"Webcrank\""
  rs = testResource { isAuthorized = unauthorized realm }
  resp401 = testResp unauthorized401 
                     [(hContentType, "text/html"), ("WWW-Authenticate", realm)] 
                     (testBody "<html><head><title>401 Unauthorized</title></head><body><h1>Unauthorized</h1>Unauthorized<p><hr><address>webcrank web server</address></body></html>")

testForbidden = resp403 <@=?> runResource rs testRq where
  rs = testResource { forbidden = value True }
  resp403 = testResp forbidden403
                     [(hContentType, "text/html")] 
                     (testBody "<html><head><title>403 Forbidden</title></head><body><h1>Forbidden</h1>Forbidden<p><hr><address>webcrank web server</address></body></html>")

testInvalidContentHeaders = resp501 <@=?> runResource rs testRq where
  rs = testResource { validContentHeaders = value False }
  resp501 = testResp notImplemented501
                     [(hContentType, "text/html")] 
                     (testBody "<html><head><title>501 Not Implemented</title></head><body><h1>Not Implemented</h1>The server does not support the GET method.<p><hr><address>webcrank web server</address></body></html>")

testUnknownContentType = resp415 <@=?> runResource rs testRq where
  rs = testResource { knownContentType = value False }
  resp415 = testResp unsupportedMediaType415
                     [(hContentType, "text/html")] 
                     (testBody "<html><head><title>415 Unsupported Media Type</title></head><body><h1>Unsupported Media Type</h1>Unsupported Media Type<p><hr><address>webcrank web server</address></body></html>")

testEntityTooLarge = resp413 <@=?> runResource rs testRq where
  rs = testResource { validEntityLength = value False }
  resp413 = testResp requestEntityTooLarge413
                     [(hContentType, "text/html")] 
                     (testBody "<html><head><title>413 Request Entity Too Large</title></head><body><h1>Request Entity Too Large</h1>Request Entity Too Large<p><hr><address>webcrank web server</address></body></html>")

testOptions = resp200 <@=?> runResource rs rq where
  hdrs = [("X-Test", "1, 2, 3")]
  rs = testResource { allowedMethods = return [methodGet, methodOptions], options = return hdrs }
  rq = testRq { testRqMethod = methodOptions }
  resp200 = testResp ok200 hdrs Nothing

testNotAcceptable = resp406 <@=?> runResource testResource rq where
  rq = testRq { testRqHeaders = [(hAccept, "text/plain")] }
  resp406 = testResp notAcceptable406
                     [(hContentType, "text/html")]
                     (testBody "<html><head><title>406 Not Acceptable</title></head><body><h1>Not Acceptable</h1>Not Acceptable<p><hr><address>webcrank web server</address></body></html>")

