{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Internal.ReqData where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HashMap
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
  , _reqDataRespHeaders = HashMap.empty
  , _reqDataRespBody = Nothing
  }

getResponseHeader
  :: (Functor m, MonadState s m, HasRespHeaders s HeadersMap)
  => HeaderName
  -> m (Maybe ByteString)
getResponseHeader h = (listToMaybe =<<) . HashMap.lookup h <$> use respHeaders

putResponseHeader
  :: (MonadState s m, HasRespHeaders s HeadersMap)
  => HeaderName
  -> ByteString
  -> m ()
putResponseHeader h v = respHeaders %= HashMap.insert h [v]

putResponseHeaders
  :: (MonadState s m, HasRespHeaders s HeadersMap)
  => ResponseHeaders
  -> m ()
putResponseHeaders = mapM_ (uncurry putResponseHeader)

removeResponseHeader
  :: (MonadState s m, HasRespHeaders s HeadersMap)
  => HeaderName
  -> m ()
removeResponseHeader h = respHeaders %= HashMap.delete h

getResponseLocation
  :: (Functor m, MonadState s m, HasRespHeaders s HeadersMap)
  => m (Maybe ByteString)
getResponseLocation = getResponseHeader hLocation

putResponseLocation
  :: (MonadState s m, HasRespHeaders s HeadersMap)
  => ByteString
  -> m ()
putResponseLocation = putResponseHeader hLocation

writeLBS
  :: (MonadState s m, HasRespBody s (Maybe Body))
  => LB.ByteString
  -> m ()
writeLBS = (respBody ?=)

