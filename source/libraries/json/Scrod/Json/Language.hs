{-# LANGUAGE LambdaCase #-}

module Scrod.Json.Language where

import qualified Data.Aeson as Aeson
import qualified Scrod.Type.Language as Type

fromJson :: Aeson.Value -> Either String Type.Language
fromJson = \case
  Aeson.String txt -> Right (Type.MkLanguage txt)
  _ -> Left "Language must be a string"

toJson :: Type.Language -> Aeson.Value
toJson (Type.MkLanguage txt) = Aeson.String txt
