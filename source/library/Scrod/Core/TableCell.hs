{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.TableCell where

import qualified GHC.Generics as Generics
import qualified Numeric.Natural as Natural

-- | A table cell with colspan, rowspan, and contents.
data Cell doc = MkCell
  { colspan :: Natural.Natural,
    rowspan :: Natural.Natural,
    contents :: doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)
