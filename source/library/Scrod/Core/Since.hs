{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Since where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Core.Version as Version
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

data Since = MkSince
  { package :: Maybe PackageName.PackageName,
    version :: Version.Version
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Since
