{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Webcrank
  ( -- * Resources
    Resource(..)
  , resource
    -- * Monad
  , WebcrankT
  , halt
  , werror
  , WebcrankT'
    -- * Charsets
  , Charset
  , CharsetsProvided(..)
  , provideCharsets
    -- * Headers
  , HasRespHeaders(..)
  , addResponseHeader
  , putResponseHeader
    -- * Body
  , HasRespBody(..)
  , Body
  , writeLBS
    -- * Other
  , Encoding
  , Authorized(..)
  , ETag(..)
  , PostAction(..)
  , module Network.HTTP.Date
  , module Network.HTTP.Media
  , module Network.HTTP.Types
  , hAcceptCharset, hAcceptEncoding, hAllow, hETag, hExpires, hIfMatch, hIfNoneMatch, hIfUnmodifiedSince, hTransferEncoding, hVary, hWWWAuthenticate
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Network.HTTP.Date
import Network.HTTP.Media
import Network.HTTP.Types

import Webcrank.Internal

resource :: Monad m => Resource m
resource = Resource
  { serviceAvailable = return True
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

provideCharsets
  :: Monad m
  => NonEmpty (Charset, Body -> Body)
  -> WebcrankT' m CharsetsProvided
provideCharsets = return . CharsetsProvided

addResponseHeader
  :: (MonadState s m, HasRespHeaders s (Map HeaderName [ByteString]))
  => HeaderName
  -> ByteString
  -> m ()
addResponseHeader h v = respHeaders %= Map.insertWith (<>) h [v]

writeLBS
  :: (MonadState s m, HasRespBody s (Maybe Body))
  => LB.ByteString
  -> m ()
writeLBS = (respBody ?=)

