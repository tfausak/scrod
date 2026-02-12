{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Scrod.Core.TableCell where

import qualified GHC.Generics as Generics
import qualified Numeric.Natural as Natural
import Scrod.Json.ToJson (ToJson)
import Scrod.Schema (ToSchema)

-- | A table cell with colspan, rowspan, and contents.
data Cell doc = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)

deriving via Generics.Generically (Cell doc) instance (ToJson doc) => ToJson (Cell doc)

deriving via Generics.Generically (Cell doc) instance (ToSchema doc) => ToSchema (Cell doc)
