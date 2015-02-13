{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Webcrank
  ( Resource(..)
  , resource
  , resource'
  , ReqState
  , halt
  , werror
  , ReqState'
  , Authorized(..)
  , Charset
  , CharsetsProvided(..)
  , provideCharsets
  , Encoding
  , ETag(..)
  , PostAction(..)
  , Halt(..)
  , addHeader
  , putHeader
  , writeLBS
  ) where

import Control.Monad
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty
import qualified Data.Map as Map
import Data.Monoid
import Network.HTTP.Types

import Webcrank.Internal

resource :: Monad m => m s -> Resource s m
resource initReq = Resource
  { initRequest = initReq
  , serviceAvailable = return True
  , uriTooLong = return False
  , allowedMethods = return [methodGet, methodHead]
  , malformedRequest = return False
  , isAuthorized = return Authorized
  , forbidden = return False
  , validContentHeaders = return True
  , knownContentType = return True
  , validEntityLength = return True
  , options = return []
  , contentTypesProvided = return []
  , charsetsProvided = return NoCharset
  , encodingsProvided = return []
  , resourceExists = return True
  , generateETag = mzero
  , lastModified = mzero
  , expires = mzero
  , movedPermanently = mzero
  , movedTemporarily = mzero
  , previouslyExisted = return False
  , allowMissingPost = return False
  , deleteResource = return False
  , deleteCompleted = return True
  , postAction = return $ PostProcess $ return ()
  , contentTypesAccepted = return []
  , variances = return []
  , multipleChoices = return False
  , isConflict = return False
  , finishRequest = return ()
  }

resource' :: Monad m => Resource () m
resource' = resource $ return ()

provideCharsets
  :: Monad m
  => NonEmpty (Charset, Body -> Body)
  -> ReqState' s m CharsetsProvided
provideCharsets = return . CharsetsProvided

putHeader :: MonadState (ReqData s n) m => HeaderName -> ByteString -> m ()
putHeader h v = modifyResponseHeaders (Map.insert h [v])

addHeader :: MonadState (ReqData s n) m => HeaderName -> ByteString -> m ()
addHeader h v = modifyResponseHeaders (Map.insertWith (<>) h [v])

writeLBS :: MonadState (ReqData s n) m => LB.ByteString -> m ()
writeLBS = putResponseBody . Just

