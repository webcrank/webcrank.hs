{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Internal.ReqData where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal.Types

initReqData :: ServerAPI m -> ReqData m
initReqData api = ReqData
  { _reqDataServerAPI = api
  , _reqDataDispPath = []
  , _reqDataRespMediaType = "application" // "octet-stream"
  , _reqDataRespCharset = Nothing
  , _reqDataRespEncoding = Nothing
  , _reqDataRespHeaders = Map.empty
  , _reqDataRespBody = Nothing
  }

getResponseHeader
  :: (Functor m, MonadState s m, HasRespHeaders s (Map HeaderName [ByteString]))
  => HeaderName
  -> m (Maybe ByteString)
getResponseHeader h = (listToMaybe =<<) . Map.lookup h <$> use respHeaders

putResponseHeader
  :: (MonadState s m, HasRespHeaders s (Map HeaderName [ByteString]))
  => HeaderName
  -> ByteString
  -> m ()
putResponseHeader h v = respHeaders %= Map.insert h [v]

putResponseHeaders
  :: (MonadState s m, HasRespHeaders s (Map HeaderName [ByteString]))
  => ResponseHeaders
  -> m ()
putResponseHeaders = mapM_ (uncurry putResponseHeader)

removeResponseHeader
  :: (MonadState s m, HasRespHeaders s (Map HeaderName [ByteString]))
  => HeaderName
  -> m ()
removeResponseHeader h = respHeaders %= Map.delete h

getResponseLocation
  :: (Functor m, MonadState s m, HasRespHeaders s (Map HeaderName [ByteString]))
  => m (Maybe ByteString)
getResponseLocation = getResponseHeader hLocation

putResponseLocation
  :: (MonadState s m, HasRespHeaders s (Map HeaderName [ByteString]))
  => ByteString
  -> m ()
putResponseLocation = putResponseHeader hLocation

