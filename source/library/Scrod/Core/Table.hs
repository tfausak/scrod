{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Table where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.TableCell as TableCell

-- | A table with header and body rows.
data Table doc = MkTable
  { headerRows :: [[TableCell.Cell doc]],
    bodyRows :: [[TableCell.Cell doc]]
  }
  deriving (Eq, Generics.Generic, Ord, Show)
