module Scrod.Core.Import where

import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.PackageName as PackageName

data Import = MkImport
  { name :: ModuleName.ModuleName,
    package :: Maybe PackageName.PackageName,
    alias :: Maybe ModuleName.ModuleName
  }
  deriving (Eq, Ord, Show)
