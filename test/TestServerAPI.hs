{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestServerAPI where

import Control.Monad.Catch.Pure
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Network.HTTP.Date
import Network.HTTP.Types

import Webcrank.Internal

data Req = Req
  { reqMethod :: Method
  , reqURI :: ByteString
  , reqHeaders :: Map HeaderName [ByteString]
  , reqTime :: HTTPDate
  } deriving Show

data Res = Res
  { resStatus :: Status
  , resHeaders :: Map HeaderName [ByteString]
  , resBody :: LB.ByteString
  } deriving (Show, Eq)

req :: Req
req = Req
  { reqMethod = methodGet
  , reqURI = "http://example.com"
  , reqHeaders = Map.empty
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
res = Res ok200 Map.empty LB.empty

type TestState = CatchT (State (Req, Res))

testAPI :: ServerAPI TestState
testAPI = ServerAPI
  { srvGetRequestMethod = gets (reqMethod . fst)
  , srvGetRequestURI = gets (reqURI . fst)
  , srvGetRequestHeader = \h -> gets ((listToMaybe =<<) . Map.lookup h . reqHeaders . fst)
  , srvGetRequestTime = gets (reqTime . fst)
  , srvPutResponseStatus = \s ->
      modify $ \(rq, rs) -> (rq, rs { resStatus = s })
  , srvPutResponseHeaders = \hs ->
      modify $ \(rq, rs) -> (rq, rs { resHeaders = hs })
  , srvPutResponseBody = \b ->
      modify $ \(rq, rs) -> (rq, rs { resBody = b })
  }

handleTestReq :: Resource TestState -> Req -> (Req, Res)
handleTestReq r rq = execState run (rq, res) where
  run = runCatchT (handleRequest testAPI r)

