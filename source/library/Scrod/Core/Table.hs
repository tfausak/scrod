{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

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

deriving via Generics.Generically (Table doc) instance (ToJson.ToJson doc) => ToJson.ToJson (Table doc)

deriving via Generics.Generically (Table doc) instance (Schema.ToSchema doc) => Schema.ToSchema (Table doc)
