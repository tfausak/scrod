{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Header where

import qualified GHC.Generics as Generics
import qualified Scrod.Core.Level as Level

-- | A section header with a level and title.
data Header doc = MkHeader
  { level :: Level.Level,
    title :: doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)
