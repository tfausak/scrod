module Scrod.Type.ModLink where

import qualified Scrod.Type.ModuleName as ModuleName

-- | A link to a module with an optional label.
-- Mirrors 'Documentation.Haddock.Types.ModLink' from haddock-library,
-- but uses 'ModuleName' instead of 'String' for the module name.
data ModLink doc = MkModLink
  { name :: ModuleName.ModuleName,
    label :: Maybe doc
  }
  deriving (Eq, Ord, Show)
