{-# LANGUAGE DerivingVia #-}

module Scrod.Unstable.Type.ItemName where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

newtype ItemName = MkItemName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via Text.Text
