{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.Table where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.TableCell as TableCell
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | A table with header and body rows.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically (Table doc) instance (ToJson doc) => ToJson (Table doc)

deriving via Generics.Generically (Table doc) instance (ToSchema doc) => ToSchema (Table doc)
