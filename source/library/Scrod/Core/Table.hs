{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Scrod.Core.Table where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.TableCell as TableCell
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Schema as Schema

-- | A table with header and body rows.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Generics.Generic, Ord, Show)
  deriving (ToJson.ToJson, Schema.ToSchema) via Generics.Generically (Table doc)
