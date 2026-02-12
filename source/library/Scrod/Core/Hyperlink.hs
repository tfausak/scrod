{-# LANGUAGE DeriveGeneric #-}

module Scrod.Core.Hyperlink where

import qualified Data.Text as Text
import qualified GHC.Generics as Generics

-- | A hyperlink with an optional label.
data Hyperlink doc = MkHyperlink
  { url :: Text.Text,
    label :: Maybe doc
  }
  deriving (Eq, Generics.Generic, Ord, Show)
