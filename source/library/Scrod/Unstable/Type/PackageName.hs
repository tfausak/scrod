{-# LANGUAGE DerivingVia #-}

module Scrod.Unstable.Type.PackageName where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

newtype PackageName = MkPackageName
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via Text.Text

fromString :: String -> PackageName
fromString =
  MkPackageName
    . Text.pack
