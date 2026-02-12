{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Since where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Core.Version as Version

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Version.Version
  }
  deriving (Eq, Generics.Generic, Ord, Show)
