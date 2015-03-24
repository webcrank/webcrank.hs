{-# LANGUAGE FlexibleContexts #-}

module Webcrank.Internal.ResourceData where

import Control.Lens
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Network.HTTP.Date
import Network.HTTP.Types

import Webcrank.Internal.Types

newResourceData :: ServerAPI m -> Resource m -> ResourceData m
newResourceData = ResourceData

callr
  :: (MonadReader r m, HasResourceData r m)
  => (Resource m -> HaltT m b)
  -> HaltT m b
callr = (view resourceDataResource >>=)

callr'
  :: (MonadReader r m, HasResourceData r m)
  => (Resource m -> m b)
  -> HaltT m b
callr' = lift . (view resourceDataResource >>=)

callr''
  :: (MonadReader r m, HasResourceData r m)
  => (Resource m -> m b)
  -> m b
callr'' = (view resourceDataResource >>=)

callAPI
  :: (MonadTrans t, Monad m, MonadReader r (t m), HasResourceData r m)
  => (ServerAPI m -> m b)
  -> t m b
callAPI f = view resourceDataServerAPI >>= lift . f

callAPI'
  :: (MonadReader r m, HasResourceData r m)
  => (ServerAPI m -> m b)
  -> m b
callAPI' f = view resourceDataServerAPI >>= f

getRequestMethod
  :: (MonadTrans t, Monad m, MonadReader r (t m), HasResourceData r m)
  => t m Method
getRequestMethod = callAPI srvGetRequestMethod

getRequestMethod'
  :: (MonadReader r m, HasResourceData r m)
  => m Method
getRequestMethod' = callAPI' srvGetRequestMethod

getRequestHeader
  :: (MonadTrans t, Monad m, MonadReader r (t m), HasResourceData r m)
  => HeaderName
  -> t m (Maybe ByteString)
getRequestHeader = callAPI . flip srvGetRequestHeader

getRequestTime
  :: (MonadTrans t, Monad m, MonadReader r (t m), HasResourceData r m)
  => t m HTTPDate
getRequestTime = callAPI srvGetRequestTime

getRequestURI
  :: (MonadTrans t, Monad m, MonadReader r (t m), HasResourceData r m)
  => t m ByteString
getRequestURI = callAPI srvGetRequestURI

