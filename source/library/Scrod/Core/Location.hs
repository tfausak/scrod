{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Location where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Column as Column
import qualified Scrod.Core.Line as Line

data Location = MkLocation
  { line :: Line.Line,
    column :: Column.Column
  }
  deriving (Eq, Generics.Generic, Ord, Show)
