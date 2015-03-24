{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestServerAPI where

import Control.Applicative
import Control.Monad.Catch.Pure
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Network.HTTP.Date
import Network.HTTP.Types

import Webcrank
import Webcrank.ServerAPI
import Webcrank.ServerAPI.WebcrankT

data Req = Req
  { reqMethod :: Method
  , reqURI :: ByteString
  , reqHeaders :: HeadersMap
  , reqTime :: HTTPDate
  } deriving Show

data Res = Res
  { resStatus :: Status
  , resHeaders :: HeadersMap
  , resBody :: Maybe LB.ByteString
  } deriving (Show, Eq)

req :: Req
req = Req
  { reqMethod = methodGet
  , reqURI = "http://example.com"
  , reqHeaders = HashMap.empty
  , reqTime = defaultHTTPDate
      { hdYear = 1994
      , hdMonth = 11
      , hdDay = 15
      , hdHour = 8
      , hdMinute = 12
      , hdSecond = 31
      , hdWkday = 2
      }
  }

res :: Res
res = Res ok200 HashMap.empty Nothing

type TestState = CatchT (Reader Req)
type TestCrank = WebcrankT TestState

runTestCrank :: TestCrank a -> Resource TestCrank -> ReqData -> TestState (a, ReqData, LogData)
runTestCrank a r = runWebcrankT a testAPI r

testAPI :: ServerAPI TestCrank
testAPI = ServerAPI
  { srvGetRequestMethod = lift $ asks reqMethod
  , srvGetRequestURI = lift $ asks reqURI
  , srvGetRequestHeader = \h -> lift $ asks ((listToMaybe =<<) . HashMap.lookup h . reqHeaders)
  , srvGetRequestTime = lift $ asks reqTime
  }

handleTestReq :: Resource TestCrank -> Req -> Res
handleTestReq r rq = runReader run rq where
  run = handleE <$> run'
  handleE = \case
    Left e -> error $ show e
    Right (s, hs, b) -> Res s hs b
  run' = runCatchT (handleRequest (\a -> runTestCrank a r newReqData))

