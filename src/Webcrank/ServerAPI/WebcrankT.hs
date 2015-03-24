{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Webcrank.ServerAPI.WebcrankT where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Error
import Control.Monad.RWS

import Webcrank.Internal.Types

-- | Monad transformer can be used by server API providers as part of
-- their stack.
-- Provides tracking of the request state and logging of the decisions
-- made so far.
newtype WebcrankT m a =
  WebcrankT { unWebcrankT :: RWST (ResourceData (WebcrankT m)) LogData ReqData m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ResourceData (WebcrankT m))
    , MonadState ReqData
    , MonadWriter LogData
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadTrans WebcrankT where
  lift = WebcrankT . lift

runWebcrankT
  :: WebcrankT m a
  -> ServerAPI (WebcrankT m)
  -> Resource (WebcrankT m)
  -> ReqData
  -> m (a, ReqData, LogData)
runWebcrankT w s r rd = runRWST (unWebcrankT w) (ResourceData s r) rd

