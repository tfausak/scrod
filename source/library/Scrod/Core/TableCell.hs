{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

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
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically (Cell doc)
