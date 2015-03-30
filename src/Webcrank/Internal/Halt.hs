{-# LANGUAGE FlexibleContexts #-}

module Webcrank.Internal.Halt where

import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Types

import Webcrank.Internal.Types

runHaltT :: HaltT m a -> m (Either Halt a)
runHaltT = runEitherT . unHaltT
{-# INLINE runHaltT #-}

-- | Immediately end processing of the request, returning the response
-- @Status@. It is the responsibility of the resource to ensure that all
-- necessary response header and body elements have been added in
-- order to make that response code valid.
halt :: Monad m => Status -> HaltT m a
halt = HaltT . left . Halt
{-# INLINE halt #-}

-- | Immediately end processing of this request, returning a
-- @500 Internal Server Error@ response. The response body will contain the
-- reason.
werror :: Monad m => LB.ByteString -> HaltT m a
werror = werrorWith internalServerError500
{-# INLINE werror #-}

-- | Immediately end processing of this request, returning a response
-- with the given @Status@. The response body will contain the reason.
werrorWith :: Monad m => Status -> LB.ByteString -> HaltT m a
werrorWith s = HaltT . left . Error s
{-# INLINE werrorWith #-}
