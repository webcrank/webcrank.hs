module Webcrank.Types 
  ( HasRequestInfo(..)
  , ResponseBody(..)
  , Init
  , initOk
  , initError
  , ErrorRenderer
  , ResourceFn
  , error
  , halt
  , getRespBody
  , putRespBody
  , getRespHeaders
  , putRespHeaders
  , modifyRespHeaders 
  , addRespHeader
  , Resource(..)
  , resource
  , resource'
  ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Prelude hiding (error)
import Webcrank.Internal

type Init m s = MaybeT m s

initOk :: Monad m => s -> Init m s
initOk = return

initError :: Monad m => Init m s
initError = MaybeT $ return Nothing

error :: Monad m => ResponseBody rb -> ResourceFn rq rb s m a
error = ResourceFn . return . ResourceFnError

halt :: Monad m => Status -> ResourceFn rq rb s m a
halt = ResourceFn . return . ResourceFnHalt

getRespBody :: Monad m => ResourceFn rq rb s m (Maybe (ResponseBody rb))
getRespBody = rgets respBody

putRespBody :: Monad m => ResponseBody rb -> ResourceFn rq rb s m ()
putRespBody b = rmodify (\rqd -> rqd { respBody = Just b })

getRespHeaders :: Monad m => ResourceFn rq rb s m ResponseHeaders
getRespHeaders = rgets respHdrs

putRespHeaders :: Monad m => ResponseHeaders -> ResourceFn rq rb s m ()
putRespHeaders hdrs = rmodify (\rqd -> rqd { respHdrs = hdrs })

modifyRespHeaders :: Monad m => (ResponseHeaders -> ResponseHeaders) -> ResourceFn rq rb s m ()
modifyRespHeaders f = do
  hs <- getRespHeaders
  putRespHeaders (f hs)

addRespHeader :: Monad m => HeaderName -> ByteString -> ResourceFn rq rb s m ()
addRespHeader h v = modifyRespHeaders ((h, v) :)

data Resource rq rb m s = Resource
  { -- | Perform some initialization for the request and return a state, @s@,
    -- which will be threaded through all of the other resource functions.
    rqInit :: Init m s

    -- | @False@ will result in @503 Not Found@. Defaults to @True@.
  , serviceAvailable :: ResourceFn rq rb s m Bool

    -- | @False@ will result in @404 Not Found@. Defaults to @True@.
--   , resourceExists :: ResourceFn rb s m Bool
  }

-- | Constructs a @Resource@ with the given initializer and defaults for all the other other properties.
resource :: Monad m => Init m s -> Resource rq rb m s
resource i = Resource
  { rqInit               = i
  , serviceAvailable     = return True
--   , resourceExists       = return $ Result True
--   , isAuthorized         = return $ Result Authz
--   , forbidden            = return $ Result False
--   , allowMissingPost     = return $ Result False
  }

resource' :: Monad m => Resource rq rb m ()
resource' = resource $ initOk ()

