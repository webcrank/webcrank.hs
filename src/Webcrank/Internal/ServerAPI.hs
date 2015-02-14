{-# LANGUAGE FlexibleContexts #-}

module Webcrank.Internal.ServerAPI where

import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import Network.HTTP.Date
import Network.HTTP.Types

import Webcrank.Internal.Types

callAPI
  :: (MonadState s (t m), Monad m, HasServerAPI s (ServerAPI m), MonadTrans t)
  => (ServerAPI m -> m b)
  -> t m b
callAPI f = use serverAPI >>= lift . f

getRequestMethod
  :: (MonadState s (t m), Monad m, HasServerAPI s (ServerAPI m), MonadTrans t)
  => t m Method
getRequestMethod = callAPI srvGetRequestMethod

getRequestHeader
  :: (MonadState s (t m), Monad m, HasServerAPI s (ServerAPI m), MonadTrans t)
  => HeaderName
  -> t m  (Maybe ByteString)
getRequestHeader = callAPI . flip srvGetRequestHeader

getRequestTime
  :: (MonadState s (t m), Monad m, HasServerAPI s (ServerAPI m), MonadTrans t)
  => t m HTTPDate
getRequestTime = callAPI srvGetRequestTime

getRequestURI
  :: (MonadState s (t m), Monad m, HasServerAPI s (ServerAPI m), MonadTrans t)
  => t m ByteString
getRequestURI = callAPI srvGetRequestURI

