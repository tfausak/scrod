{-# LANGUAGE DeriveGeneric #-}

module Scrod.Unstable.Type.Since where

import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Scrod.Unstable.Type.PackageName as PackageName
import qualified Scrod.Unstable.Type.Version as Version

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Version.Version
  }
  deriving (Eq, Ord, Show, Generics.Generic)

instance Aeson.ToJSON Since where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = id}
