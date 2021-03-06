{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DecisionTests where

import Control.Applicative
import Control.Lens
import Control.Monad.Catch.Pure
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types
import Test.Tasty
import Test.Tasty.HUnit

import Webcrank
import Webcrank.Internal
import Webcrank.Internal.DecisionCore
import Webcrank.Internal.Halt

import TestServerAPI

decisionTests :: TestTree
decisionTests = testGroup "Decision tests"
  [ b13Tests
  , b12Tests
  , b11Tests
  , b10Tests
  , b9Tests
  , b8Tests
  , b7Tests
  , b6Tests
  , b5Tests
  , b4Tests
  , b3Tests
  , c3Tests
  , c4Tests
  , d4Tests
  , d5Tests
  , e5Tests
  , e6Tests
  , f6Tests
  , f7Tests
  , g7Tests
  , g8Tests
  , g9Tests
  , g11Tests
  , h7Tests
  , h10Tests
  , h11Tests
  , h12Tests
  , i4Tests
  , i7Tests
  , i12Tests
  , i13Tests
  , j18Tests
  , k5Tests
  , k7Tests
  , k13Tests
  , l5Tests
  , l7Tests
  , l13Tests
  , l14Tests
  , l15Tests
  , l17Tests
  , m5Tests
  , m7Tests
  , m16Tests
  , m20Tests
  , n5Tests
  , n11Tests
  , n16Tests
  , o14Tests
  , o16Tests
  , o18Tests
  , o20Tests
  , p3Tests
  , p11Tests
  ]

b13Tests :: TestTree
b13Tests = decisionTestGroup "b13" "Service available?"
  [ testCase "True ==> b12" $
      after' b13 resource req @?= Decision' "b12"
  , testCase "False ==> 503 Service Unavailable" $
      let r = resource { serviceAvailable = return False }
      in after' b13 r req @?= error' serviceUnavailable503
  ]

b12Tests :: TestTree
b12Tests = decisionTestGroup "b12" "Known method?" ts where
  ts = mk <$> ms
  mk (m, r) = testCase (mconcat [show m, " ==> ", show r]) $
    after' b12 resource (req { reqMethod = m }) @?= r
  ms = ("QWERTY", error' notImplemented501) : ((, Decision' "b11") <$> [methodGet, methodHead, methodPost, methodPut, methodDelete, methodTrace, methodConnect, methodOptions])

b11Tests :: TestTree
b11Tests = decisionTestGroup "b11" "URI too long?"
  [ testCase "True ==> 414 Request-URI Too Long" $
      let r = resource { uriTooLong = return True }
      in after' b11 r req @?= error' requestURITooLong414
  , testCase "False ==> b11" $
      after' b11 resource req @?= Decision' "b10"
 ]

b10Tests :: TestTree
b10Tests = decisionTestGroup "b10" "Method allowed?"
  [ testCase "True ==> b9" $
      after' b10 resource req @?= Decision' "b9"
  , testCase "False ==> 405 Method Not Allowed" $
      let rq = req { reqMethod = methodPost }
      in afterHdrs b10 resource rq @?=
        (error' methodNotAllowed405, HashMap.singleton hAllow ["GET, HEAD"])
  ]

b9Tests :: TestTree
b9Tests = decisionTestGroup "b9" "Malformed?"
  [ testCase "True ==> 400 Malformed Request" $
      let r = resource { malformedRequest = return True }
      in after' b9 r req @?= error' badRequest400
  , testCase "False ==> b8" $
      after' b9 resource req @?= Decision' "b8"
 ]

b8Tests :: TestTree
b8Tests = decisionTestGroup "b8" "Authorized?"
  [ testCase "True ==> b7" $
      after' b8 resource req @?= Decision' "b7"
  , testCase "False ==> 401 Unauthorized" $
      let r = resource { isAuthorized = return $ Unauthorized "Basic realm=\"W\"" }
      in afterHdrs b8 r req @?=
        (error' unauthorized401, HashMap.singleton hWWWAuthenticate ["Basic realm=\"W\""])
  ]

b7Tests :: TestTree
b7Tests = decisionTestGroup "b7" "Forbidden?"
  [ testCase "True ==> 403 Forbidden" $
      let r = resource { forbidden = return True }
      in after' b7 r req @?= error' forbidden403
  , testCase "False ==> b6" $
      after' b7 resource req @?= Decision' "b6"
 ]

b6Tests :: TestTree
b6Tests = decisionTestGroup "b6" "Okay Content-* Headers?"
  [ testCase "True ==> b5" $
      after' b6 resource req @?= Decision' "b5"
  , testCase "False ==> 501 Not Implemented" $
    let r = resource { validContentHeaders = return False }
    in after' b6 r req @?= error' notImplemented501
  ]

b5Tests :: TestTree
b5Tests = decisionTestGroup "b5" "Known Content-Type?"
  [ testCase "True ==> b4" $
      after' b5 resource req @?= Decision' "b4"
  , testCase "False ==> 415 Unsupported Media Type" $
      let r = resource { knownContentType = return False }
      in after' b5 r req @?= error' unsupportedMediaType415
  ]

b4Tests :: TestTree
b4Tests = decisionTestGroup "b4" "Req Entity Too Large?"
  [ testCase "True ==> b3" $
      after' b4 resource req @?= Decision' "b3"
  , testCase "False ==> 413 Request Entity Too Large" $
      let r = resource { validEntityLength = return False }
      in after' b4 r req @?= error' requestEntityTooLarge413
  ]

b3Tests :: TestTree
b3Tests = decisionTestGroup "b3" "OPTIONS?"
  [ testCase "True ==> 200 Ok" $
      let
        r = resource { options = return [("X-Tra", "Webcrank")] }
        rq = req { reqMethod = methodOptions }
      in afterHdrs b3 r rq @?= (Done' ok200, HashMap.singleton "X-Tra" ["Webcrank"])
  , testCase "False ==> c3" $
      after' b3 resource req @?= Decision' "c3"
  ]

c3Tests :: TestTree
c3Tests = decisionTestGroup "c3" "Accept exists?"
  [ testCase "True ==> c4 + default media type" $
      let rq = req { reqHeaders = HashMap.singleton hAccept ["text/html"] }
      in afterMediaType c3 resource rq @?= (Decision' "c4", "application" // "octet-stream")
  , testCase "False ==> c4 w/o media type" $
      let
        r = resource { contentTypesProvided = return [("text" // "html", return "")] }
      in afterMediaType c3 r req @?= (Decision' "d4", "text" // "html")
  ] where
    afterMediaType s r rq = case after s r rq of
      (d, rd) -> (d, _reqDataRespMediaType rd)

c4Tests :: TestTree
c4Tests = decisionTestGroup "c4" "Acceptable media type available?"
  [ testCase "True ==> d4" $
      let r = resource { contentTypesProvided = return [("text" // "html", return "")] }
      in after' (c4 "text/html") r req @?= Decision' "d4"
  , testCase "False ==> 406 Not Acceptable" $
      after' (c4 "text/html") resource req @?= Error' notAcceptable406 "No acceptable media type available"
  ]

d4Tests :: TestTree
d4Tests = decisionTestGroup "d4" "Accept-Language exists?"
  [ testCase "True ==> d5" $
      let rq = req { reqHeaders = HashMap.singleton hAcceptLanguage ["en"] }
      in after' d4 resource rq @?= Decision' "d5"
  , testCase "False ==> e5" $
      after' d4 resource req @?= Decision' "e5"
  ]

-- just a placeholder
d5Tests :: TestTree
d5Tests = decisionTestGroup "d5" "Acceptable language available?"
  [ testCase "Always ==> e5" $
      after' (d5 "en") resource req @?= Decision' "e5"
  ]

e5Tests :: TestTree
e5Tests = decisionTestGroup "e5" "Accept-Charset exists?"
  [ testCase "True ==> e6" $
      let rq = req { reqHeaders = HashMap.singleton hAcceptCharset ["utf-8"] }
      in after' e5 resource rq @?= Decision' "e6"
  , testCase "False + no charsets provided ==> f6 w/o charset" $
      afterCharset e5 resource req @?= (Decision' "f6", Nothing)
  , testCase "False + charset provided ==> f6 + charset" $
      let r = resource { charsetsProvided = provideCharsets $ return ("utf-8", id) }
      in afterCharset e5 r req @?= (Decision' "f6", Just "utf-8")
  ]

e6Tests :: TestTree
e6Tests = decisionTestGroup "e6" "Acceptable Charset available?"
  [ testCase "True ==> f6" $
      let r = resource { charsetsProvided = provideCharsets $ return ("utf-8", id) }
      in afterCharset (e6 "utf-8") r req @?= (Decision' "f6", Just "utf-8")
  , testCase "no charsets provided ==> f6 w/o charset" $
      afterCharset (e6 "utf-8") resource req @?= (Decision' "f6", Nothing)
  , testCase "False ==> 406 Not Acceptable" $
      let r = resource { charsetsProvided = provideCharsets $ return ("wtf-9", id) }
      in after' (e6 "utf-8") r req @?= Error' notAcceptable406 "No acceptable charset available"
  ]

f6Tests :: TestTree
f6Tests = decisionTestGroup "f6" "Accept-Encoding exists?"
  [ testCase "True ==> f7" $
      let rq = req { reqHeaders = HashMap.singleton hAcceptEncoding ["gzip"] }
      in after' f6 resource rq @?= Decision' "f7"
  , testCase "False ==> g7" $
      after' f6 resource req @?= Decision' "g7"
  ]

f7Tests :: TestTree
f7Tests = decisionTestGroup "f7" "Acceptable encoding available?"
  [ testCase "True ==> g7 + encoding" $
      let r = resource { encodingsProvided = return [("gzip", id)] }
      in afterEnc (f7 "gzip") r req @?= (Decision' "g7", Just "gzip")
  , testCase "False ==> g7 + identity" $
      afterEnc (f7 "gzip") resource req @?= (Decision' "g7", Nothing)
  ] where
    afterEnc s r rq = case after s r rq of
      (d, rd) -> (d, _reqDataRespEncoding rd)

g7Tests :: TestTree
g7Tests = decisionTestGroup "g7" "Resource exists?"
  [ testCase "True ==> g8" $
      after' g7 resource req @?= Decision' "g8"
  , testCase "False ==> h7" $
      let r = resource { resourceExists = return False }
      in after' g7 r req @?= Decision' "h7"
  ]

g8Tests :: TestTree
g8Tests = decisionTestGroup "g8" "If-Match exists?"
  [ testCase "True ==> g9" $
      let rq = req { reqHeaders = HashMap.singleton hIfMatch ["webcrank"] }
      in after' g8 resource rq @?= Decision' "g9"
  , testCase "False ==> h10" $
      after' g8 resource req @?= Decision' "h10"
  ]

g9Tests :: TestTree
g9Tests = decisionTestGroup "g9" "If-Match: * exists?"
  [ testCase "True ==> h10" $
      after' (g9 "*") resource req @?= Decision' "h10"
  , testCase "False ==> g11" $
      let rq = req { reqHeaders = HashMap.singleton hIfMatch ["webcrank"] }
      in after' (g9 "webcrank") resource rq @?= Decision' "g11"
  ]

g11Tests :: TestTree
g11Tests = decisionTestGroup "g11" "ETag in If-Match?"
  [ testCase "True ==> h10" $
      let r = resource { generateETag = return $ StrongETag "webcrank" }
      in after' (g11 "\"webcrank\"") r req @?= Decision' "h10"
  , testCase "False (no ETag) ==> 412 Precondition Failed" $
      after' (g11 "\"webcrank\"") resource req @?= error' preconditionFailed412
  , testCase "False (mismatch ETag) ==> 412 Precondition Failed" $
      let r = resource { generateETag = return $ StrongETag "webcrank123" }
      in after' (g11 "\"webcrank456\"") r req @?= error' preconditionFailed412
  ]

h7Tests :: TestTree
h7Tests = decisionTestGroup "h7" "If-Match exists? (no existing resource)"
  [ testCase "True ==> 412 Precondition Failed" $
      let rq = req { reqHeaders = HashMap.singleton hIfMatch ["webcrank"] }
      in after' h7 resource rq @?= error' preconditionFailed412
  , testCase "False ==> i7" $
      after' h7 resource req @?= Decision' "i7"
  ]

h10Tests :: TestTree
h10Tests = decisionTestGroup "h10" "If-Unmodified-Since exists?"
  [ testCase "True ==> h11" $
      let rq = req { reqHeaders = HashMap.singleton hIfUnmodifiedSince [dateStr] }
      in after' h10 resource rq @?= Decision' "h11"
  , testCase "False ==> i12" $
      after' h10 resource req @?= Decision' "i12"
  ]

h11Tests :: TestTree
h11Tests = decisionTestGroup "h11" "If-Unmodified-Since is valid date?"
  [ testCase "True ==> h12" $
      after' (h11 dateStr) resource req @?= Decision' "h12"
  , testCase "False ==> i12" $
      after' (h11 "not a date") resource req @?= Decision' "i12"
  ]

h12Tests :: TestTree
h12Tests = decisionTestGroup "h12" "Last-Modified > If-Unmodified-Since?"
  [ testCase "True ==> 412 Precondition Failed" $
      after' (h12 $ date { hdDay = 10 }) r req @?= error' preconditionFailed412
  , testCase "False ==> i12" $
      after' (h12 $ date { hdDay = 20 }) r req @?= Decision' "i12"
  ] where
    r = resource { lastModified = return date }

i4Tests :: TestTree
i4Tests = decisionTestGroup "i4" "Moved permanently? (apply PUT to different URI)"
  [ testCase "True ==> 301 Moved Permanently" $
      let
        loc = "http://example.com/abc"
        r = resource { movedPermanently = return loc }
      in afterHdrs i4 r req @?= (Done' movedPermanently301, HashMap.singleton hLocation [loc])
  , testCase "False ==> p3" $
      after' i4 resource req @?= Decision' "p3"
  ]

i7Tests :: TestTree
i7Tests = decisionTestGroup "i7" "PUT?"
  [ testCase "True ==> i4" $
      let rq = req { reqMethod = methodPut }
      in after' i7 resource rq @?= Decision' "i4"
  , testCase "False ==> k7" $
      after' i7 resource req @?= Decision' "k7"
  ]

i12Tests :: TestTree
i12Tests = decisionTestGroup "i12" "If-None-Match exists?"
  [ testCase "True ==> i13" $
      let rq = req { reqHeaders = HashMap.singleton hIfNoneMatch ["webcrank"] }
      in after' i12 resource rq @?= Decision' "i13"
  , testCase "False ==> l13" $
      after' i12 resource req @?= Decision' "l13"
  ]

i13Tests :: TestTree
i13Tests = decisionTestGroup "i13" "If-None-Match: * exists?"
  [ testCase "True ==> j18" $
      after' (i13 "*") resource req @?= Decision' "j18"
  , testCase "False ==> k13" $
      after' (i13 "webcrank") resource req @?= Decision' "k13"
  ]

j18Tests :: TestTree
j18Tests = decisionTestGroup "j18" "GET or HEAD? (resource exists)"
  [ testCase "True (GET) ==> 304 Not Modified" $
      after' j18 resource req @?= Done' notModified304
  , testCase "True (HEAD) ==> 304 Not Modified" $
      let rq = req { reqMethod = methodHead }
      in after' j18 resource rq @?= Done' notModified304
  , testCase "False ==> 412 Precondition Failed" $
      let rq = req { reqMethod = methodPost }
      in after' j18 resource rq @?= error' preconditionFailed412
  ]

k5Tests :: TestTree
k5Tests = decisionTestGroup "k5" "Moved permanently? (non-PUT variant)"
  [ testCase "True ==> 301 Moved Permanently" $
      let
        uri = "http://example.com/abc"
        r = resource { movedPermanently = return uri }
      in afterHdrs k5 r req @?= (Done' movedPermanently301, HashMap.singleton hLocation [uri])
  , testCase "False ==> l5" $
      after' k5 resource req @?= Decision' "l5"
  ]

k7Tests :: TestTree
k7Tests = decisionTestGroup "k7" "Previously existed?"
  [ testCase "True ==> k5" $
      let r = resource { previouslyExisted = return True }
      in after' k7 r req @?= Decision' "k5"
  , testCase "False ==> l7" $
      after' k7 resource req @?= Decision' "l7"
  ]

k13Tests :: TestTree
k13Tests = decisionTestGroup "k13" "ETag in If-None-Match?"
  [ testCase "True ==> j18" $
      let r = resource { generateETag = return $ StrongETag "webcrank" }
      in after' (k13 "\"webcrank\"") r req @?= Decision' "j18"
  , testCase "False (no ETag) ==> l13" $
      after' (k13 "\"webcrank\"") resource req @?= Decision' "l13"
  , testCase "False (mismatch ETag) ==> l13" $
      let r = resource { generateETag = return $ StrongETag "webcrank123" }
      in after' (k13 "\"webcrank456\"") r req @?= Decision' "l13"
  ]

l5Tests :: TestTree
l5Tests = decisionTestGroup "l5" "Moved temporarily?"
  [ testCase "True ==> 307 Moved Temporarily" $
      let
        uri = "http://example.com/abc"
        r = resource { movedTemporarily = return uri }
      in afterHdrs l5 r req @?= (Done' temporaryRedirect307, HashMap.singleton hLocation [uri])
  , testCase "False ==> m5" $
      after' l5 resource req @?= Decision' "m5"
  ]

l7Tests :: TestTree
l7Tests = decisionTestGroup "l7" "POST? (no existing resource)"
  [ testCase "True ==> m7" $
      let rq = req { reqMethod = methodPost }
      in after' l7 resource rq @?= Decision' "m7"
  , testCase "False ==> 404 Not Found" $
      after' l7 resource req @?= error' notFound404
  ]

l13Tests :: TestTree
l13Tests = decisionTestGroup "l17" "If-Modified-Since exists?"
  [ testCase "True ==> l14" $
      let rq = req { reqHeaders = HashMap.singleton hIfModifiedSince [dateStr] }
      in after' l13 resource rq @?= Decision' "l14"
  , testCase "False ==> m16" $
      after' l13 resource req @?= Decision' "m16"
  ]

l14Tests :: TestTree
l14Tests = decisionTestGroup "l14" "If-Modified-Since is a valid date?"
  [ testCase "True ==> l15" $
      after' (l14 dateStr) resource req @?= Decision' "l15"
  , testCase "False ==> m16" $
      after' (l14 "not a date") resource req @?= Decision' "m16"
  ]

l15Tests :: TestTree
l15Tests = decisionTestGroup "l15" "If-Modified-Since > Now?"
  [ testCase "True ==> m16" $
      let rq = req { reqTime = date { hdDay = 10 } }
      in after' (l15 date) resource rq @?= Decision' "m16"
  , testCase "False ==> l17" $
      after' (l15 date) resource req @?= Decision' "l17"
  ]

l17Tests :: TestTree
l17Tests = decisionTestGroup "l17" "Last-Modified > If-Modified-Since?"
  [ testCase "True ==> m16" $
      let r = resource { lastModified = return $ date { hdDay = 20 } }
      in after' (l17 date) r req @?= Decision' "m16"
  , testCase "No last modified ==> m16" $
      after' (l17 date) resource req @?= Decision' "m16"
  , testCase "False ==> 304 Not Modified" $
      let r = resource { lastModified = return $ date { hdDay = 10 } }
      in after' (l17 date) r req @?= Done' notModified304
  ]

m5Tests :: TestTree
m5Tests = decisionTestGroup "m5" "POST? (resource previously existed)"
  [ testCase "True ==> n5" $
      let rq = req { reqMethod = methodPost }
      in after' m5 resource rq @?= Decision' "n5"
  , testCase "False ==> 410 Gone" $
      after' m5 resource req @?= error' gone410
  ]

m7Tests :: TestTree
m7Tests = decisionTestGroup "m7" "Server allows POST to missing resource?"
  [ testCase "True ==> n11" $
      let r = resource { allowMissingPost = return True }
      in after' m7 r req @?= Decision' "n11"
  , testCase "False ==> 404 Not Found" $
      after' m7 resource req @?= error' notFound404
  ]

m16Tests :: TestTree
m16Tests = decisionTestGroup "m16" "DELETE?"
  [ testCase "True ==> m20" $
      let rq = req { reqMethod = methodDelete }
      in after' m16 resource rq @?= Decision' "m20"
  , testCase "False ==> n16" $
      after' m16 resource req @?= Decision' "n16"
  ]

m20Tests :: TestTree
m20Tests = decisionTestGroup "m20" "Delete enacted?"
  [ testCase "True + completed ==> n11" $
      let r = resource { deleteResource = return True }
      in after' m20 r req @?= Decision' "n11"
  , testCase "True + not completed ==> 202 Accepted" $
      let r = resource { deleteResource = return True, deleteCompleted = return False }
      in after' m20 r req @?= Done' accepted202
  , testCase "False ==> 500 Internal Server Error" $
      after' m20 resource req @?= error' internalServerError500
  ]

n5Tests :: TestTree
n5Tests = decisionTestGroup "n5" "Server allows POST to missing resource? (resource did not exist previously)"
  [ testCase "True ==> n11" $
      let r = resource { allowMissingPost = return True }
      in after' n5 r req @?= Decision' "n11"
  , testCase "False ==> 410 Gone" $
      after' n5 resource req @?= error' gone410
  ]

n11Tests :: TestTree
n11Tests = decisionTestGroup "n11" "Redirect?"
  [ testCase "True + created + content type accepted ==> 303 See Other" $
      let
        r = resource
          { postAction = return . PostCreateRedir $ ["webcrank"]
          , contentTypesAccepted = return [("text" // "html", return ())]
          }
        rq = req { reqHeaders = HashMap.singleton hContentType ["text/html"] }
        aft = case after n11 r rq of
          (d, rd) -> (d, HashMap.lookup hLocation $_reqDataRespHeaders rd, _reqDataDispPath rd)
      in aft @?= (Done' seeOther303, Just ["http://example.com/webcrank"], ["webcrank"])
   , testCase "True + created + no content type accepted ==> 415 Unsupported Media Type" $
      let
        r = resource
          { postAction = return . PostCreateRedir $ ["webcrank"]
          , contentTypesAccepted = return [("text" // "html", return ())]
          }
        rq = req { reqHeaders = HashMap.singleton hContentType ["text/plain"] }
      in after' n11 r rq @?= error' unsupportedMediaType415
  , testCase "True + process ==> 303 See Other" $
      let r = resource { postAction = return $ PostProcessRedir $ return "http://example.com/webcrank" }
      in afterHdrs n11 r req @?=
        (Done' seeOther303, HashMap.singleton hLocation ["http://example.com/webcrank"])
  , testCase "False + created + content type accepted ==> p11" $
      let
        r = resource
          { postAction = return . PostCreate $ ["webcrank"]
          , contentTypesAccepted = return [("text" // "html", return ())]
          }
        rq = req { reqHeaders = HashMap.singleton hContentType ["text/html"] }
      in afterHdrs n11 r rq @?=
        (Decision' "p11", HashMap.singleton hLocation ["http://example.com/webcrank"])
  , testCase "False + created + no content type accepted ==> p11" $
      let
        r = resource
          { postAction = return . PostCreate $ ["webcrank"]
          , contentTypesAccepted = return [("text" // "html", return ())]
          }
        rq = req { reqHeaders = HashMap.singleton hContentType ["text/plain"] }
      in afterHdrs n11 r rq @?=
        (error' unsupportedMediaType415, HashMap.singleton hLocation ["http://example.com/webcrank"])
  , testCase "False + process ==> p11" $
      let r = resource { postAction = return $ PostProcess $ return () }
      in after' n11 r req @?= Decision' "p11"
  ]

n16Tests :: TestTree
n16Tests = decisionTestGroup "n16" "POST? (resource exists)"
  [ testCase "True ==> n11" $
      let rq = req { reqMethod = methodPost }
      in after' n16 resource rq @?= Decision' "n11"
  , testCase "False ==> o16" $
      after' n16 resource req @?= Decision' "o16"
  ]

o14Tests :: TestTree
o14Tests = decisionTestGroup "o14" "Conflict? (resource exists)"
  [ testCase "True ==> 409 Conflict" $
      let r = resource { isConflict = return True }
      in after' o14 r req @?= error' conflict409
  , testCase "False + no acceptable content type ==> 415 Unsupported Media Type" $
      after' o14 resource req @?= error' unsupportedMediaType415
  , testCase "False + acceptable content type ==> p11" $
      let
        r = resource { contentTypesAccepted = return [("text" // "html", return ())] }
        rq = req { reqHeaders = HashMap.singleton hContentType ["text/html"] }
      in after' o14 r rq @?= Decision' "p11"
  ]

o16Tests :: TestTree
o16Tests = decisionTestGroup "o16" "PUT? (resource exists)"
  [ testCase "True ==> o14" $
      let rq = req { reqMethod = methodPut }
      in after' o16 resource rq @?= Decision' "o14"
  , testCase "False ==> o18" $
      after' o16 resource req @?= Decision' "o18"
  ]

o18Tests :: TestTree
o18Tests = decisionTestGroup "o18" "Multiple representations?"
  [ testCase "True ==> 300 Multiple Choices" $
      let r = resource { multipleChoices = return True }
      in after' o18 r req @?= Done' multipleChoices300
  , testCase "False ==> 200 Ok" $
      after' o18 resource req @?= Done' ok200
  ]

o20Tests :: TestTree
o20Tests = decisionTestGroup "o20" "Response includes an entity?"
  [ testCase "True ==> o18" $
      let
        body rd = rd { _reqDataRespBody = Just "webcrank" }
        aft = fst $ afterWith o20 resource req body
      in aft @?= Decision' "o18"
  , testCase "False ==> 204 No Content" $
      after' o20 resource req @?= Done' noContent204
  ]

p3Tests :: TestTree
p3Tests = decisionTestGroup "p3" "Conflict? (resource doesn't exist)"
  [ testCase "True ==> 409 Conflict" $
      let r = resource { isConflict = return True }
      in after' p3 r req @?= error' conflict409
  , testCase "False + no acceptable content types ==> 415 Unsupported Media Type" $
      after' p3 resource req @?= error' unsupportedMediaType415
  , testCase "False + acceptable content type ==> p11" $
      let
        r = resource { contentTypesAccepted = return [("text" // "html", return ())] }
        rq = req { reqHeaders = HashMap.singleton hContentType ["text/html"] }
      in after' p3 r rq @?= Decision' "p11"
  ]

p11Tests :: TestTree
p11Tests = decisionTestGroup "p11" "New resource?"
  [ testCase "True ==> 201 Created" $
      let
        loc rd = rd { _reqDataRespHeaders = HashMap.singleton hLocation ["http://example.com/abc"] }
        aft = fst $ afterWith p11 resource req loc
      in aft @?= Done' created201
  , testCase "False ==> o20" $
      after' p11 resource req @?= Decision' "o20"
  ]

decisionTestGroup :: TestName -> TestName -> [TestTree] -> TestTree
decisionTestGroup s n = testGroup (s <> ". " <> n)

date :: HTTPDate
date = defaultHTTPDate
  { hdYear = 1994
  , hdMonth = 11
  , hdDay = 15
  , hdHour = 8
  , hdMinute = 12
  , hdSecond = 31
  , hdWkday = 2
  }

dateStr :: ByteString
dateStr = formatHTTPDate date

after
  :: FlowChart (HaltT TestCrank) Status
  -> Resource TestCrank
  -> Req
  -> (Decision', ReqData)
after s r rq = afterWith s r rq id

afterWith
  :: FlowChart (HaltT TestCrank) Status
  -> Resource TestCrank
  -> Req
  -> (ReqData -> ReqData)
  -> (Decision', ReqData)
afterWith s r rq f = run where
  run = case runReader (runCatchT $ step s r f) rq of
    Left e -> error $ show e
    Right a -> a

step
  :: FlowChart (HaltT TestCrank) Status
  -> Resource TestCrank
  -> (ReqData -> ReqData)
  -> TestState (Decision', ReqData)
step s r f = do
  let rd = f newReqData
  case s of
    Decision _ a -> runTestCrank (runHaltT a) r rd >>= \case
      (Left (Halt sc), rd', _) -> return (Done' sc, rd')
      (Left (Error sc e), rd', _) -> return (Error' sc e, rd')
      (Right (Decision l _), rd', _) -> return (Decision' l, rd')
      (Right (Done a'), rd', _) -> runTestCrank (runHaltT a') r rd' <&> \case
        (Left (Halt sc), rd'', _) -> (Done' sc, rd'')
        (Left (Error sc e), rd'', _) -> (Error' sc e, rd'')
        (Right sc, rd'', _) -> (Done' sc, rd'')
    Done _ -> error "can't look past end state of flow chart"

after'
  :: FlowChart (HaltT TestCrank) Status
  -> Resource TestCrank
  -> Req
  -> Decision'
after' s r = fst . after s r

afterHdrs
  :: FlowChart (HaltT TestCrank) Status
  -> Resource TestCrank
  -> Req
  -> (Decision', HeadersMap)
afterHdrs s r rq = case after s r rq of
  (d, rd) -> (d, _reqDataRespHeaders rd)

afterCharset
  :: FlowChart (HaltT TestCrank) Status
  -> Resource TestCrank
  -> Req
  -> (Decision', Maybe Charset)
afterCharset s r rq = case after s r rq of
  (d, rd) -> (d, _reqDataRespCharset rd)

data Decision'
  = Error' Status LB.ByteString
  | Done' Status
  | Decision' String
  deriving Eq

error' :: Status -> Decision'
error' s = Error' s (LB.fromStrict $ statusMessage s)

instance Show Decision' where
  show = \case
    Error' sc e -> mconcat
      [ show $ statusCode sc
      , " "
      , B.unpack $ statusMessage sc
      , " - "
      , show . B.unpack . LB.toStrict $ e
      ]
    Done' sc -> mconcat [show $ statusCode sc, " ", B.unpack $ statusMessage sc ]
    Decision' l -> l
