module Scrod.Core.Identifier where

import qualified Data.Text as Text
import qualified Scrod.Core.Namespace as Namespace

-- | An identifier reference in documentation.
data Identifier = MkIdentifier
  { namespace :: Maybe Namespace.Namespace,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)
