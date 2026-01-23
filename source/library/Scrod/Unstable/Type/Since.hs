module Scrod.Unstable.Type.Since where

import qualified Scrod.Unstable.Type.PackageName as PackageName
import qualified Scrod.Unstable.Type.Version as Version

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Maybe Version.Version -- TODO: Make this required.
  }
  deriving (Eq, Ord, Show)

empty :: Since
empty =
  MkSince
    { package = Nothing,
      version = Nothing
    }
