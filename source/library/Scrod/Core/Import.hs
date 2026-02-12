{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Import where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

data Import = MkImport
  { name :: ModuleName.ModuleName,
    package :: Maybe PackageName.PackageName,
    alias :: Maybe ModuleName.ModuleName
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically Import
