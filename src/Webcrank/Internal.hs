{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Webcrank.Internal where 

import Control.Applicative
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder)
import Network.HTTP.Types (Method, Status)
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)

class HasRequestInfo r where
  rqMethod :: r -> Method
  rqHeader :: HeaderName -> r -> Maybe ByteString

data ResponseBody b
  = FileResponseBody FilePath (Maybe (Int, Int))
  | StreamResponseBody b
  | BuilderResponseBody Builder

type ErrorRenderer rq rb s m = Status -> ResourceFn rq rb s m (ResponseBody rb)

data RqData rq rb s m = RqData
  { rqInfo :: rq
  , rqState :: s
  , errorRenderer :: ErrorRenderer rq rb s m
  , respHdrs :: ResponseHeaders
  , respBody :: Maybe (ResponseBody rb)
  }

instance HasRequestInfo rq => HasRequestInfo (RqData rq rb s m) where
  rqMethod = rqMethod . rqInfo
  rqHeader h = rqHeader h . rqInfo

data ResourceFnResult rb a
  = ResourceFnValue a
  | ResourceFnError (ResponseBody rb)
  | ResourceFnHalt Status

newtype ResourceFn rq rb s m a = ResourceFn { runResourceFn :: StateT (RqData rq rb s m) m (ResourceFnResult rb a) }

instance (Monad m) => Functor (ResourceFn rq rb s m) where
  fmap f r = ResourceFn $ runResourceFn r >>= return . f' where
    f' (ResourceFnValue a) = ResourceFnValue $ f a
    f' (ResourceFnError e) = ResourceFnError e
    f' (ResourceFnHalt s) = ResourceFnHalt s

instance (Monad m) => Applicative (ResourceFn rq rb s m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (ResourceFn rq rb s m) where
  return = ResourceFn . return . ResourceFnValue
  f >>= g = ResourceFn $ runResourceFn f >>= g' where
    g' (ResourceFnValue a) = runResourceFn $ g a
    g' (ResourceFnError e) = return $ ResourceFnError e
    g' (ResourceFnHalt s) = return $ ResourceFnHalt s

instance (Monad m) => MonadState s (ResourceFn rq rb s m) where
  get = rgets rqState
  put s = rmodify (\rqd -> rqd { rqState = s })

getErrorRenderer :: Monad m => ResourceFn rq rb s m (ErrorRenderer rq rb s m)
getErrorRenderer = rgets errorRenderer

getRqMethod :: (Monad m, HasRequestInfo rq) => ResourceFn rq rb s m Method
getRqMethod = rgets (rqMethod . rqInfo)

rgets :: Monad m => (RqData rq rb s m -> a) -> ResourceFn rq rb s m a
rgets f = ResourceFn $ gets (ResourceFnValue . f)

rmodify :: Monad m => (RqData rq rb s m -> RqData rq rb s m) -> ResourceFn rq rb s m ()
rmodify f = ResourceFn $ modify f >> return (ResourceFnValue ())

