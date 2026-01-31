module Scrod.Type.Identifier where

import qualified Data.Text as Text
import qualified Scrod.Type.Namespace as Namespace

-- | An identifier reference in documentation.
-- Combines an optional namespace (Value or Type) with the identifier text.
-- 'Nothing' namespace corresponds to plain 'identifier' syntax.
data Identifier = MkIdentifier
  { namespace :: Maybe Namespace.Namespace,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)
