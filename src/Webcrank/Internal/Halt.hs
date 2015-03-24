{-# LANGUAGE FlexibleContexts #-}

module Webcrank.Internal.Halt where

import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Types

import Webcrank.Internal.Types

runHaltT :: HaltT m a -> m (Either Halt a)
runHaltT = runEitherT . unHaltT
{-# INLINE runHaltT #-}

halt :: Monad m => Status -> HaltT m a
halt = HaltT . left . Halt
{-# INLINE halt #-}

werror :: Monad m => Status -> LB.ByteString -> HaltT m a
werror s = HaltT . left . Error s . Just
{-# INLINE werror #-}

werror' :: Monad m => Status -> HaltT m a
werror' = HaltT . left . flip Error Nothing
{-# INLINE werror' #-}

