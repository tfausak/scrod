{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Located where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Location as Location

data Located a = MkLocated
  { location :: Location.Location,
    value :: a
  }
  deriving (Eq, Generics.Generic, Ord, Show)
