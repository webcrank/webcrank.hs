{-# LANGUAGE OverloadedStrings #-}

module HandleRequestTests where

import qualified Data.Map as Map
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types
import Test.Tasty
import Test.Tasty.HUnit

import Webcrank
import Webcrank.Internal

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
  let r = resource' { contentTypesProvided = return [("plain" // "text", return "webcrank")] }
  in snd (handleTestReq r req) @?=
    Res ok200
      (Map.singleton hContentType ["plain/text"])
      "webcrank"

postTest :: TestTree
postTest = testCase "POST" $
  let
    r = resource'
      { contentTypesAccepted =
          return [("plain" // "text", putHeader hContentType "plain/text" >> writeLBS "webcrank")]
      , postAction = return $ PostCreate ["new"]
      , allowedMethods = return [methodGet, methodHead, methodPost]
      }
    rq = req
      { reqMethod = methodPost
      , reqHeaders = Map.singleton hContentType ["plain/text"]
      }
  in snd (handleTestReq r rq) @?=
    Res created201
      (Map.fromList [(hLocation, ["http://example.com/new"]), (hContentType, ["plain/text"])])
      "webcrank"

putTest :: TestTree
putTest = testCase "PUT" $
  let
    r = resource'
      { contentTypesAccepted =
          return [("plain" // "text", putHeader hContentType "plain/text" >> writeLBS "webcrank")]
      , allowedMethods = return [methodGet, methodHead, methodPut]
      }
    rq = req
      { reqMethod = methodPut
      , reqHeaders = Map.singleton hContentType ["plain/text"]
      }
  in snd (handleTestReq r rq) @?=
    Res ok200
      (Map.singleton hContentType ["plain/text"])
      "webcrank"

deleteTest :: TestTree
deleteTest = testCase "DELETE" $
  let
    r = resource'
      { allowedMethods = return [methodGet, methodHead, methodDelete]
      , deleteResource = return True
      }
    rq = req { reqMethod = methodDelete }
  in snd (handleTestReq r rq) @?=
    Res noContent204
      (Map.singleton hContentType ["application/octet-stream"])
      ""

notModifiedTest :: TestTree
notModifiedTest = testCase "Not modified" $
  let
    r = resource'
      { lastModified = return defaultHTTPDate
      , generateETag = return $ StrongETag "webcrank"
      , expires = return defaultHTTPDate
      }
    rq = req { reqHeaders = Map.singleton hIfModifiedSince [ formatHTTPDate defaultHTTPDate ] }
  in snd (handleTestReq r rq) @?=
    Res notModified304
      (Map.fromList [(hETag, ["webcrank"]), (hExpires, [formatHTTPDate defaultHTTPDate])])
      ""

serviceUnavailableTest :: TestTree
serviceUnavailableTest = testCase "Service unavailable" $
  let r = resource' { serviceAvailable = return False }
  in resStatus (snd (handleTestReq r req)) @?= serviceUnavailable503

