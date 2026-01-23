module Scrod.Unstable.Type.Subordinates where

import qualified Scrod.Unstable.Type.ExportName as ExportName

-- | Subordinate exports for a type or class.
-- Represents the contents of parentheses in exports like @Foo(..)@ or @Foo(Bar, Baz)@.
data Subordinates = MkSubordinates
  { -- | Whether a @(..)@ wildcard is present.
    wildcard :: Bool,
    -- | Explicitly listed children.
    explicit :: [ExportName.ExportName]
  }
  deriving (Eq, Ord, Show)
