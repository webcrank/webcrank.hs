module Webcrank.Prelude (
  module X
) where

-- Add as required, just don't add anything specific that could be
-- pulled from Control.* or alike.
import Prelude as X (Int, Eq, Ord, Show)
import Control.Applicative as X ((<$>), (<*>), (*>), (<*), pure)
import Control.Monad as X (void, when, unless, liftM)
import Data.Traversable as X (mapM)
