{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Webcrank.Types.Internal where 

import Control.Applicative
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder)
import Data.CaseInsensitive (CI)
import Network.HTTP.Types (Method, Status)
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Webcrank.Types.MediaType

class HasRequestInfo r where
  rqMethod :: r -> Method
  rqHeader :: HeaderName -> r -> Maybe ByteString

data ResponseBody b
  = FileResponseBody FilePath (Maybe (Int, Int))
  | StreamResponseBody b
  | BuilderResponseBody Builder

type ErrorRenderer rq rb s m = (Status, Maybe Builder) -> ResourceFn rq rb s m (Maybe (ResponseBody rb))

type Charset = CI ByteString

-- TODO adding tracing option
newtype Init m s = Init { runInit :: m s }

data RqData rq rb s m = RqData
  { rqInfo :: rq
  , rqState :: s
  , errorRenderer :: ErrorRenderer rq rb s m
  , respMediaType  :: MediaType
  , respCharset :: Maybe Charset
  , respHdrs :: ResponseHeaders
  , respBody :: Maybe (ResponseBody rb)
  }

instance HasRequestInfo rq => HasRequestInfo (RqData rq rb s m) where
  rqMethod = rqMethod . rqInfo
  rqHeader h = rqHeader h . rqInfo

data Result a
  = Value a
  | Error Builder -- | Immediately end processing of this request, returning a 500 Internal Server Error response. The response body will contain the term.
  | Halt Status   -- | Immediately end processing of this request, returning response status. It is the responsibility of the resource to ensure that all necessary response header and body elements are filled in order to make that response code valid.
  deriving (Functor)

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return = Value
  (Value a) >>= g = g a
  (Error e) >>= _ = Error e
  (Halt s) >>= _ = Halt s

newtype ResourceFn rq rb s m a = ResourceFn { unResourceFn :: StateT (RqData rq rb s m) m a }
  deriving (Monad)

instance Monad m => Functor (ResourceFn rq rb s m) where
  fmap = liftM

instance Monad m => Applicative (ResourceFn rq rb s m) where
  pure = return
  (<*>) = ap

instance Monad m => MonadState s (ResourceFn rq rb s m) where
  get = rgets rqState
  put s = rmodify (\rqd -> rqd { rqState = s })

instance MonadTrans (ResourceFn rq rb s) where
  lift = ResourceFn . lift

rgets :: Monad m => (RqData rq rb s m -> a) -> ResourceFn rq rb s m a
rgets = ResourceFn . gets

rmodify :: Monad m => (RqData rq rb s m -> RqData rq rb s m) -> ResourceFn rq rb s m ()
rmodify = void . ResourceFn . modify 

getErrorRenderer :: Monad m => ResourceFn rq rb s m (ErrorRenderer rq rb s m)
getErrorRenderer = rgets errorRenderer

getRqMethod :: (Monad m, HasRequestInfo rq) => ResourceFn rq rb s m Method
getRqMethod = rgets (rqMethod . rqInfo)

getRqHeader :: (Monad m, HasRequestInfo rq) => HeaderName -> ResourceFn rq rb s m (Maybe ByteString)
getRqHeader h = rgets (rqHeader h . rqInfo)

getRespMediaType :: Monad m => ResourceFn rq rb s m MediaType
getRespMediaType = rgets respMediaType

putRespMediaType :: Monad m => MediaType -> ResourceFn rq rb s m ()
putRespMediaType t = rmodify (\rd -> rd { respMediaType = t })

getRespCharset :: Monad m => ResourceFn rq rb s m (Maybe Charset)
getRespCharset = rgets respCharset

putRespCharset :: Monad m => Maybe Charset -> ResourceFn rq rb s m ()
putRespCharset c = rmodify (\rd -> rd { respCharset = c })

data Authorized = Authorized | Unauthorized ByteString

