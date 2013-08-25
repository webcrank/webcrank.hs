module Webcrank.Types 
  ( HasRequestInfo(..)
  , ResponseBody(..)
  , ErrorRenderer
  , ResourceFn
  , initOk
  , value
  , error
  , halt
  , getRespBody
  , putRespBody
  , getRespHeaders
  , putRespHeaders
  , modifyRespHeaders 
  , addRespHeader
  , removeRespHeader
  , setRespHeader
  , Resource(..)
  , resource
  , resource'
  , authorized
  , unauthorized
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder)
import Network.HTTP.Types (Status, Method, methodGet, methodHead)
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Prelude hiding (error)
import Webcrank.Internal

data Resource rq rb m s = Resource
  { -- | Perform some initialization for the request and return a state, @s@,
    -- which will be threaded through all of the other resource functions.
    rqInit :: Init m s

    -- | @False@ will result in @503 Not Found@. Defaults to @True@.
  , serviceAvailable :: ResourceFn rq rb s m (Result Bool)

    -- | @True@ will result in @414 Request Too Long@. Defaults to @False@.
  , uriTooLong :: ResourceFn rq rb s m (Result Bool)

    -- | If a @Method@ not in this list is requested, then a @405 Method Not Allowed@ will be sent. Defaults to @GET@ and @HEAD@.
  , allowedMethods :: ResourceFn rq rb s m [Method]

    -- | @True@ will result in @400 Bad Request@. Defaults to @False@.
  , malformedRequest :: ResourceFn rq rb s m (Result Bool)

    -- | If this is @NoAuthz@, the response will be @401 Unauthorized@. 
    -- @NoAuthz@ will be used as the challenge WWW-Authenticate header.
    -- Defaults to @Authz@.
  , isAuthorized :: ResourceFn rq rb s m (Result Authorized)

    -- | @True@ will result in @403 Forbidden@. Defaults to @False@.
  , forbidden :: ResourceFn rq rb s m (Result Bool)

    -- | @False@ will result in @501 Not Implemented@. Defaults to @True@.
  , validContentHeaders :: ResourceFn rq rb s m (Result Bool)


    -- | @False@ will result in @404 Not Found@. Defaults to @True@.
--   , resourceExists :: ResourceFn rb s m Bool
  }

-- | Constructs a @Resource@ with the given initializer and defaults for all the other other properties.
resource :: Monad m => Init m s -> Resource rq rb m s
resource i = Resource
  { rqInit               = i
  , serviceAvailable     = value True
  , uriTooLong           = value False
  , allowedMethods       = return [methodGet, methodHead]
  , malformedRequest     = value False
  , isAuthorized         = value Authorized
  , forbidden            = value False
  , validContentHeaders  = value True
--   , resourceExists       = value True
--   , isAuthorized         = value Authz
--   , allowMissingPost     = value False
  }

resource' :: Monad m => Resource rq rb m ()
resource' = resource $ initOk ()

initOk :: Monad m => s -> Init m s
initOk = Init . return

value :: Monad m => a -> ResourceFn rq rb s m (Result a)
value = return . Value

error :: Monad m => Builder -> ResourceFn rq rb s m (Result a)
error = return . Error

halt :: Monad m => Status -> ResourceFn rq rb s m (Result a)
halt = return . Halt

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

removeRespHeader :: Monad m => HeaderName -> ResourceFn rq rb s m ()
removeRespHeader h = modifyRespHeaders (filter ((h ==) . fst))

setRespHeader :: Monad m => HeaderName -> ByteString -> ResourceFn rq rb s m ()
setRespHeader h v = removeRespHeader h >> addRespHeader h v 

authorized :: Monad m => ResourceFn rq rb s m (Result Authorized)
authorized = return $ return Authorized

unauthorized :: Monad m => ByteString -> ResourceFn rq rb s m (Result Authorized)
unauthorized = return . return . Unauthorized

