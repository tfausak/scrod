module Scrod.Type.Since where

import qualified Scrod.Type.PackageName as PackageName
import qualified Scrod.Type.Version as Version

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Version.Version
  }
  deriving (Eq, Ord, Show)
