module Webcrank.Internal.WebcrankT where

import Control.Monad.RWS
import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Types

import Webcrank.Internal.Types

runWebcrankT'
  :: WebcrankT' m a
  -> Resource m
  -> ReqData m
  -> m (a, ReqData m, ())
runWebcrankT' = runRWST . unWebcrankT'

runWebcrankT
  :: WebcrankT m a
  -> Resource m
  -> ReqData m
  -> m (Either Halt a, ReqData m, ())
runWebcrankT = runWebcrankT' . runEitherT . unWebcrankT

halt :: Monad m => Status -> WebcrankT m a
halt = WebcrankT . left . Halt

werror :: Monad m => Status -> LB.ByteString -> WebcrankT m a
werror s = WebcrankT . left . Error s . Just

werror' :: Monad m => Status -> WebcrankT m a
werror' = WebcrankT . left . flip Error Nothing

