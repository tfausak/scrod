{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

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

deriving via Generics.Generically Since instance ToJson.ToJson Since

deriving via Generics.Generically Since instance Schema.ToSchema Since
