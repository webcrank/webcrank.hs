{-# LANGUAGE OverloadedStrings #-}

module HandleRequestTests where

import qualified Data.HashMap.Strict as HashMap
import Test.Tasty
import Test.Tasty.HUnit

import Webcrank
import TestServerAPI

handleRequestTests :: TestTree
handleRequestTests = testGroup "handleRequest"
  [ getTest
  , postTest
  , putTest
  , deleteTest
  , notModifiedTest
  , serviceUnavailableTest
  ]

getTest :: TestTree
getTest = testCase "GET" $
  let r = resource { contentTypesProvided = return [("plain" // "text", return "webcrank")] }
  in handleTestReq r req @?=
    Res ok200
      (HashMap.singleton hContentType ["plain/text"])
      (Just "webcrank")

postTest :: TestTree
postTest = testCase "POST" $
  let
    r = resource
      { contentTypesAccepted =
          return [("plain" // "text", putResponseHeader hContentType "plain/text" >> writeLBS "webcrank")]
      , postAction = return . PostCreate $ ["new"]
      , allowedMethods = return [methodGet, methodHead, methodPost]
      }
    rq = req
      { reqMethod = methodPost
      , reqHeaders = HashMap.singleton hContentType ["plain/text"]
      }
  in handleTestReq r rq @?=
    Res created201
      (HashMap.fromList [(hLocation, ["http://example.com/new"]), (hContentType, ["plain/text"])])
      (Just "webcrank")

putTest :: TestTree
putTest = testCase "PUT" $
  let
    r = resource
      { contentTypesAccepted =
          return [("plain" // "text", putResponseHeader hContentType "plain/text" >> writeLBS "webcrank")]
      , allowedMethods = return [methodGet, methodHead, methodPut]
      }
    rq = req
      { reqMethod = methodPut
      , reqHeaders = HashMap.singleton hContentType ["plain/text"]
      }
  in handleTestReq r rq @?=
    Res ok200
      (HashMap.singleton hContentType ["plain/text"])
      (Just "webcrank")

deleteTest :: TestTree
deleteTest = testCase "DELETE" $
  let
    r = resource
      { allowedMethods = return [methodGet, methodHead, methodDelete]
      , deleteResource = return True
      }
    rq = req { reqMethod = methodDelete }
  in handleTestReq r rq @?=
    Res noContent204
      (HashMap.singleton hContentType ["application/octet-stream"])
      Nothing

notModifiedTest :: TestTree
notModifiedTest = testCase "Not modified" $
  let
    r = resource
      { lastModified = return defaultHTTPDate
      , generateETag = return $ StrongETag "webcrank"
      , expires = return defaultHTTPDate
      }
    rq = req { reqHeaders = HashMap.singleton hIfModifiedSince [ formatHTTPDate defaultHTTPDate ] }
  in handleTestReq r rq @?=
    Res notModified304
      (HashMap.fromList [(hETag, ["webcrank"]), (hExpires, [formatHTTPDate defaultHTTPDate])])
      Nothing

serviceUnavailableTest :: TestTree
serviceUnavailableTest = testCase "Service unavailable" $
  let r = resource { serviceAvailable = return False }
  in resStatus (handleTestReq r req) @?= serviceUnavailable503

