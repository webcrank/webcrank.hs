{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestServerAPI where

import Control.Applicative
import Control.Monad.Catch.Pure
import Control.Monad.Reader
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
  , resBody :: Maybe LB.ByteString
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
res = Res ok200 Map.empty Nothing

type TestState = CatchT (Reader Req)

testAPI :: ServerAPI TestState
testAPI = ServerAPI
  { srvGetRequestMethod = asks reqMethod
  , srvGetRequestURI = asks reqURI
  , srvGetRequestHeader = \h -> asks ((listToMaybe =<<) . Map.lookup h . reqHeaders)
  , srvGetRequestTime = asks reqTime
  }

handleTestReq :: Resource TestState -> Req -> Res
handleTestReq r rq = runReader run rq where
  run = handleE <$> run'
  handleE = \case
    Left e -> error $ show e
    Right (s, hs, b) -> Res s hs b
  run' = runCatchT (handleRequest testAPI r)

