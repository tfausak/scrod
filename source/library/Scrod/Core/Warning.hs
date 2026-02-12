{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Warning where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Scrod.Core.Category as Category

data Warning = MkWarning
  { category :: Category.Category,
    value :: Text.Text
  }
  deriving (Eq, Generics.Generic, Ord, Show)
