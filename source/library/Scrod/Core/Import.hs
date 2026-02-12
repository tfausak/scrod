{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Import where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.PackageName as PackageName
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

data Import = MkImport
  { name :: ModuleName.ModuleName,
    package :: Maybe PackageName.PackageName,
    alias :: Maybe ModuleName.ModuleName
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Import instance ToJson Import

deriving via Generics.Generically Import instance ToSchema Import
