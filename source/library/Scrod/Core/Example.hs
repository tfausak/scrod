{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Example where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics

-- | An example expression with its expected result.
data Example = MkExample
  { expression :: Text.Text,
    result :: [Text.Text]
  }
  deriving (Eq, Generics.Generic, Ord, Show)
