{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Webcrank.Internal.Types where

import Control.Applicative
import Control.Monad.RWS
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as B
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types

data ServerAPI m = ServerAPI
  { srvGetRequestMethod :: m Method
    -- ^ Get the request method of the current request.

  , srvGetRequestURI :: m ByteString
    -- ^ The full URI of the request.

  , srvGetRequestHeader :: HeaderName -> m (Maybe ByteString)
    -- ^ Get the request header of the current request.

  , srvGetRequestTime :: m HTTPDate
    -- ^ Get the time the request was received.

  , srvPutResponseStatus :: Status -> m ()
    -- ^ Put the status of the response.
    --
  , srvPutResponseHeaders :: Map HeaderName [ByteString] -> m ()
    -- ^ Put the header key-value-pair in the response. If a header with the same
    -- name already exists, it is overwritten with the new value

  , srvPutResponseBody :: LB.ByteString -> m ()
    -- ^ Sets the lazy @ByteString@ as the body of the response.
  }

type Encoding = CI ByteString

type Charset = CI ByteString

data ReqData s m = ReqData
  { _reqDataServerAPI :: ServerAPI m
  , _reqDataState :: s
  , _reqDataRespMediaType :: MediaType
  , _reqDataRespCharset :: Maybe Charset
  , _reqDataRespEncoding :: Encoding
  , _reqDataDispPath :: [Text]
  , _reqDataRespHeaders :: Map HeaderName [ByteString]
  , _reqDataRespBody :: Maybe LB.ByteString
  }

newtype ReqState' s m a = ReqState' { unReqState' :: RWST (Resource s m) () (ReqData s m) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (ReqState' s) where
  lift = ReqState' . lift

instance Monad m => MonadReader (Resource s m) (ReqState' s m) where
  ask = ReqState' ask
  local f = ReqState' . local f . unReqState'

instance Monad m => MonadState (ReqData s m) (ReqState' s m) where
  get = ReqState' get
  put = ReqState' . put

data Halt = Halt Status | Error Status (Maybe LB.ByteString)
  deriving (Eq, Show)

newtype ReqState s m a = ReqState { unReqState :: EitherT Halt (ReqState' s m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (ReqState s) where
  lift = ReqState . lift . lift

instance Monad m => MonadReader (Resource s m) (ReqState s m) where
  ask = ReqState ask
  local f = ReqState . local f . unReqState

instance Monad m => MonadState (ReqData s m) (ReqState s m) where
  get = ReqState get
  put = ReqState . put

data Authorized = Authorized | Unauthorized ByteString

data CharsetsProvided
  = NoCharset
  | CharsetsProvided (NonEmpty (Charset, LB.ByteString -> LB.ByteString))

instance RenderHeader (CI ByteString) where
  renderHeader = CI.original

instance Accept (CI ByteString) where
  parseAccept = Just . CI.mk
  matches a b = case b of
    "*" -> True
    _ -> a == b
  moreSpecificThan _ b = b == "*"

data ETag = StrongETag ByteString | WeakETag ByteString deriving Eq

instance Show ETag where
  show e = B.toString $ case e of
    StrongETag v -> "\"" <> v <> "\""
    WeakETag v -> "W/\"" <> v <> "\""

data PostAction s m
  = PostCreate [Text]
  | PostCreateRedir [Text]
  | PostProcess (ReqState s m ())
  | PostProcessRedir (ReqState s m ByteString)

data Resource s m = Resource
  { initRequest :: m s
    -- ^ Perform some initialization for the request and return a state, @s@,
    -- which will be threaded through all of the other resource functions.

  , serviceAvailable :: ReqState s m Bool
    -- ^ @False@ will result in @503 Service Unavailable@. Defaults to @True@.

  , uriTooLong :: ReqState s m Bool
    -- ^ @True@ will result in @414 Request Too Long@. Defaults to @False@.

  , allowedMethods :: ReqState' s m [Method]
    -- ^ If a @Method@ not in this list is requested, then a @405 Method Not
    -- Allowed@ will be sent. Defaults to @GET@ and @HEAD@.

  , malformedRequest :: ReqState s m Bool
    -- ^ @True@ will result in @400 Bad Request@. Defaults to @False@.

  , isAuthorized :: ReqState s m Authorized
    -- ^ If this is @NoAuthz@, the response will be @401 Unauthorized@.
    -- @NoAuthz@ will be used as the challenge WWW-Authenticate header.
    -- Defaults to @Authz@.

  , forbidden :: ReqState s m Bool
    -- ^ @True@ will result in @403 Forbidden@. Defaults to @False@.

  , validContentHeaders :: ReqState s m Bool
    -- ^ @False@ will result in @501 Not Implemented@. Defaults to @True@.

  , knownContentType :: ReqState s m Bool
    -- ^ @False@ will result in @415 Unsupported Media Type@. Defaults to
    -- @True@.

  , validEntityLength :: ReqState s m Bool
    -- ^ @False@ will result in @413 Request Entity Too Large@. Defaults to
    -- @True@.

  , options :: ReqState' s m ResponseHeaders
    -- ^ If the OPTIONS method is supported and is used, the headers that
    -- should appear in the response.

  , contentTypesProvided :: ReqState' s m [(MediaType, ReqState s m LB.ByteString)]
    -- ^ Content negotiation is driven by this function. For example, if a
    -- client request includes an @Accept@ header with a value that does not
    -- appear as a @MediaType@ in any of the tuples, then a @406 Not
    -- Acceptable@ will be sent. If there is a matching @MediaType@, that
    -- function is used to create the entity when a response should include one.

  , charsetsProvided :: ReqState' s m CharsetsProvided
    -- ^ Used on GET requests to ensure that the entity is in @Charset@.

  , encodingsProvided :: ReqState' s m [(Encoding, LB.ByteString -> LB.ByteString)]
    -- ^ Used on GET requests to ensure that the body is encoded.
    -- One useful setting is to have the function check on method, and on GET
    -- requests return @[("identity", id), ("gzip", compress)]@ as this is all
    -- that is needed to support gzip content encoding.

  , resourceExists :: ReqState s m Bool
    -- ^ @False@ will result in @404 Not Found@. Defaults to @True@.

  , generateETag :: MaybeT (ReqState' s m) ETag
    -- ^ If this returns an @ETag@, it will be used for the ETag header and for
    -- comparison in conditional requests.

  , lastModified :: MaybeT (ReqState' s m) HTTPDate
    -- ^ If this returns a @HTTPDate@, it will be used for the Last-Modified header
    -- and for comparison in conditional requests.

  , expires :: MaybeT (ReqState' s m) HTTPDate
    -- ^ If this returns a @HTTPDate@, it will be used for the Expires header.

  , movedPermanently :: MaybeT (ReqState s m) ByteString
    -- ^ If this returns a URI, the client will receive a 301 Moved Permanently
    -- with the URI in the Location header.

  , movedTemporarily :: MaybeT (ReqState s m) ByteString
    -- ^ If this returns a URI, the client will receive a 307 Temporary Redirect
    -- with URI in the Location header.

  , previouslyExisted :: ReqState s m Bool
    -- ^ If this returns @True@, the @movedPermanently@ and @movedTemporarily@
    -- callbacks will be invoked to determine whether the response should be
    -- 301 Moved Permanently, 307 Temporary Redirect, or 410 Gone.

  , allowMissingPost :: ReqState s m Bool
    -- ^ If the resource accepts POST requests to nonexistent resources, then
    -- this should return @True@. Defaults to @False@.

  , deleteResource :: ReqState s m Bool
    -- ^ This is called when a DELETE request should be enacted, and should return
    -- @True@ if the deletion succeeded or has been accepted.

  , deleteCompleted :: ReqState s m Bool
    -- ^ This is only called after a successful @deleteResource@ call, and should
    -- return @False@ if the deletion was accepted but cannot yet be guaranteed to
    -- have finished.

  , postAction :: ReqState' s m (PostAction s m)
    -- ^ If POST requests should be treated as a request to put content into a
    -- (potentially new) resource as opposed to being a generic submission for
    -- processing, then this function should return @PostCreate path@. If it
    -- does return @PostCreate path@, then the rest of the request will be
    -- treated much like a PUT to the path entry. Otherwise, if it returns
    -- @PostProcess a@, then the action @a@ will be run.

  , contentTypesAccepted :: ReqState' s m [(MediaType, ReqState s m ())]

  , variances :: ReqState' s m [HeaderName]
    -- ^ This function should return a list of strings with header names that
    -- should be included in a given response's Vary header. The standard
    -- conneg headers (Accept, Accept-Encoding, Accept-Charset,
    -- Accept-Language) do not need to be specified here as Webcrank will add
    -- the correct elements of those automatically depending on resource
    -- behavior.

  , multipleChoices :: ReqState s m Bool
    -- ^ If this returns @True@, then it is assumed that multiple
    -- representations of the response are possible and a single one cannot
    -- be automatically chosen, so a @300 Multiple Choices@ will be sent
    -- instead of a @200 OK@.

  , isConflict :: ReqState' s m Bool
    -- ^ If this returns @True@, the client will receive a 409 Conflict.

  , finishRequest :: ReqState' s m ()
    -- ^ Called just before the final response is constructed and sent.
  }

