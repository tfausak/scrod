{-# LANGUAGE DerivingVia #-}

module Scrod.Unstable.Type.Language where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Driver.Flags as Flags

newtype Language = MkLanguage
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.ToJSON) via Text.Text

fromString :: String -> Language
fromString =
  MkLanguage
    . Text.pack

fromGhc :: Flags.Language -> Language
fromGhc =
  fromString
    . show
