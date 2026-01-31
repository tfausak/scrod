{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Located where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.Location as Location
import qualified Scrod.Type.Located as Type

fromJson :: (Aeson.Value -> Either String a) -> Aeson.Value -> Either String (Type.Located a)
fromJson fromJsonA value = case value of
  Aeson.Object obj -> do
    locJson <- Helpers.lookupField obj "location"
    loc <- Location.fromJson locJson
    valJson <- Helpers.lookupField obj "value"
    val <- fromJsonA valJson
    Right $ Type.MkLocated {Type.location = loc, Type.value = val}
  _ -> Left "Located must be an object"

toJson :: (a -> Aeson.Value) -> Type.Located a -> Aeson.Value
toJson toJsonA (Type.MkLocated loc val) =
  Aeson.object
    [ "location" Aeson..= Location.toJson loc,
      "value" Aeson..= toJsonA val
    ]
