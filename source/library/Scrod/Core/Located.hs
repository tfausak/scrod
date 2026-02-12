{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Located where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Location as Location
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

data Located a = MkLocated
  { location :: Location.Location,
    value :: a
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically (Located a) instance (ToJson a) => ToJson (Located a)

deriving via Generics.Generically (Located a) instance (ToSchema a) => ToSchema (Located a)
