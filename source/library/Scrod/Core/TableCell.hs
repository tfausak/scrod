{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.TableCell where

import qualified GHC.Generics as Generics
import qualified Numeric.Natural as Natural
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A table cell with colspan, rowspan, and contents.
data Cell doc = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically (Cell doc) instance (ToJson.ToJson doc) => ToJson.ToJson (Cell doc)

deriving via Generics.Generically (Cell doc) instance (Schema.ToSchema doc) => Schema.ToSchema (Cell doc)
