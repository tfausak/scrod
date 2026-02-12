{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Located where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Location as Location
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

data Located a = MkLocated
  { location :: Location.Location,
    value :: a
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically (Located a) instance (ToJson.ToJson a) => ToJson.ToJson (Located a)

deriving via Generics.Generically (Located a) instance (Schema.ToSchema a) => Schema.ToSchema (Located a)
