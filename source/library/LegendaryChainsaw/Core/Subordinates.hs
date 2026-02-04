module LegendaryChainsaw.Core.Subordinates where

import qualified LegendaryChainsaw.Core.ExportName as ExportName

-- | Subordinate exports for a type or class.
data Subordinates = MkSubordinates
  { -- | Whether a @(..)@ wildcard is present.
    wildcard :: Bool,
    -- | Explicitly listed children.
    explicit :: [ExportName.ExportName]
  }
  deriving (Eq, Ord, Show)
