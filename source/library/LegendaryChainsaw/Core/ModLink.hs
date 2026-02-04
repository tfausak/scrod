module LegendaryChainsaw.Core.ModLink where

import qualified LegendaryChainsaw.Core.ModuleName as ModuleName

-- | A link to a module with an optional label.
data ModLink doc = MkModLink
  { name :: ModuleName.ModuleName,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)
