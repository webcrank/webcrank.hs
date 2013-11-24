{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Types.Resource
  ( Resource(..)
  , ResourceFn
  , ErrorRenderer
  , ResponseBody(..)
  , ContentTypesProvided
  , Charset
  , CharsetsProvided(..)
  , Encoding
  , resource
  , resource'
  , initOk
  , value
  , error
  , halt
  , authorized
  , unauthorized
  , getRespBody
  , putRespBody
  , getRespHeaders
  , putRespHeaders
  , modifyRespHeaders 
  , addRespHeader
  , removeRespHeader
  , putRespHeader
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder)
import Data.List.NonEmpty (NonEmpty)
import Network.HTTP.Types (Status, Method, methodGet, methodHead)
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Prelude hiding (error, head)
import Webcrank.Types.Internal
import Webcrank.Types.MediaType

type ContentTypesProvided rq rb s m = NonEmpty (MediaType, ResourceFn rq rb s m (Result (ResponseBody rb)))

data CharsetsProvided rb
  = NoCharset 
  | CharsetsProvided (NonEmpty (Charset, rb -> rb))

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

    -- | @False@ will result in @415 Unsupported Media Type@. Defaults to @True@.
  , knownContentType :: ResourceFn rq rb s m (Result Bool)

    -- | @False@ will result in @413 Request Entity Too Large@. Defaults to @True@.
  , validEntityLength :: ResourceFn rq rb s m (Result Bool)

    -- | If the OPTIONS method is supported and is used, the headers that should appear in the response.
  , options :: ResourceFn rq rb s m ResponseHeaders

    -- | Content negotiation is driven by this function. For example, if a client request includes an @Accept@
    -- header with a value that does not appear as a @MediaType@ in any of the tuples, then a @406 Not Acceptable@ will be sent.
    -- If there is a matching @MediaType@, that function is used to create the entity when a response should include one.
  , contentTypesProvided :: ResourceFn rq rb s m (ContentTypesProvided rq rb s m)

    -- | Used on GET requests to ensure that the entity is in @Charset@.
  , charsetsProvided :: ResourceFn rq rb s m (CharsetsProvided rb)

    -- | Used on GET requests to ensure that the body is encoded. 
    -- One useful setting is to have the function check on method, and on GET requests
    -- return @[("identity", id), ("gzip", compress)]@ as this is all that is needed 
    -- to support gzip content encoding.
  , encodingsProvided :: ResourceFn rq rb s m [ (Encoding, rb -> rb) ]

  }

-- | Constructs a @Resource@ with the given initializer and defaults for all the other other properties.
resource :: Monad m => Init m s 
                    -> ResourceFn rq rb s m (NonEmpty (MediaType, ResourceFn rq rb s m (Result (ResponseBody rb))))
                    -> Resource rq rb m s
resource i cs = Resource
  { rqInit               = i
  , serviceAvailable     = value True
  , uriTooLong           = value False
  , allowedMethods       = return [methodGet, methodHead]
  , malformedRequest     = value False
  , isAuthorized         = value Authorized
  , forbidden            = value False
  , validContentHeaders  = value True
  , knownContentType     = value True
  , validEntityLength    = value True
  , options              = return []
  , contentTypesProvided = cs
  , charsetsProvided     = return NoCharset
  , encodingsProvided    = return [ ("identity", id) ]
  }

resource' :: Monad m => ResourceFn rq rb () m (NonEmpty (MediaType, ResourceFn rq rb () m (Result (ResponseBody rb))))
                     -> Resource rq rb m ()
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

putRespHeader :: Monad m => HeaderName -> ByteString -> ResourceFn rq rb s m ()
putRespHeader h v = removeRespHeader h >> addRespHeader h v 

authorized :: Monad m => ResourceFn rq rb s m (Result Authorized)
authorized = return $ return Authorized

unauthorized :: Monad m => ByteString -> ResourceFn rq rb s m (Result Authorized)
unauthorized = return . return . Unauthorized

