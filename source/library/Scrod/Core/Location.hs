{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Location where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Column as Column
import qualified Scrod.Core.Line as Line
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

data Location = MkLocation
  { line :: Line.Line,
    column :: Column.Column
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically Location instance ToJson.ToJson Location

deriving via Generics.Generically Location instance Schema.ToSchema Location
