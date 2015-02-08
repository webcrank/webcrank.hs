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
  ) where

import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty
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
  => NonEmpty (Charset, LB.ByteString -> LB.ByteString)
  -> ReqState' s m CharsetsProvided
provideCharsets = return . CharsetsProvided
