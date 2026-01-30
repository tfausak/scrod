{-# LANGUAGE LambdaCase #-}

module Scrod.Type.Language where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified GHC.Driver.Flags as Flags

newtype Language = MkLanguage
  { value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromString :: String -> Language
fromString =
  MkLanguage
    . Text.pack

fromGhc :: Flags.Language -> Language
fromGhc =
  fromString
    . show

fromJson :: Aeson.Value -> Either String Language
fromJson = \case
  Aeson.String txt -> Right (MkLanguage txt)
  _ -> Left "Language must be a string"

toJson :: Language -> Aeson.Value
toJson (MkLanguage txt) = Aeson.String txt
