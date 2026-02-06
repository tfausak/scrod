module Scrod.Core.ModLink where

import qualified Scrod.Core.ModuleName as ModuleName

-- | A link to a module with an optional label.
data ModLink doc = MkModLink
  { name :: ModuleName.ModuleName,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)
