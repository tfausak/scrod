{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Subordinates where

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Scrod.Json.ExportName as ExportName
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Type.Subordinates as Type

fromJson :: Aeson.Value -> Either String Type.Subordinates
fromJson = \case
  Aeson.Object obj -> do
    wildcardJson <- Helpers.lookupField obj "wildcard"
    w <- case wildcardJson of
      Aeson.Bool b -> Right b
      _ -> Left "wildcard must be a boolean"
    explicitJson <- Helpers.lookupField obj "explicit"
    e <- case explicitJson of
      Aeson.Array vec -> traverse ExportName.fromJson (Vector.toList vec)
      _ -> Left "explicit must be an array"
    Right $ Type.MkSubordinates {Type.wildcard = w, Type.explicit = e}
  _ -> Left "Subordinates must be an object"

toJson :: Type.Subordinates -> Aeson.Value
toJson (Type.MkSubordinates w e) =
  Aeson.object
    [ "wildcard" Aeson..= w,
      "explicit" Aeson..= fmap ExportName.toJson e
    ]
