module Scrod.Core.Since where

import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Core.Version as Version

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Version.Version
  }
  deriving (Eq, Ord, Show)
