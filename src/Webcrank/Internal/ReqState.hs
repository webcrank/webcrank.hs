module Webcrank.Internal.ReqState where

import Control.Monad.RWS
import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Types

import Webcrank.Internal.Types

runReqState' :: ReqState' s m a -> Resource s m -> ReqData s m -> m (a, ReqData s m, ())
runReqState' = runRWST . unReqState'

runReqState :: ReqState s m a -> Resource s m -> ReqData s m -> m (Either Halt a, ReqData s m, ())
runReqState = runReqState' . runEitherT . unReqState

halt :: Monad m => Status -> ReqState s m ()
halt = ReqState . left . Halt

werror :: Monad m => Status -> LB.ByteString -> ReqState s m a
werror s = ReqState . left . Error s . Just

werror' :: Monad m => Status -> ReqState s m a
werror' = ReqState . left . flip Error Nothing

