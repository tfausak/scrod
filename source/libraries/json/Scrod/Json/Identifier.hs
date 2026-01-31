{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Identifier where

import qualified Data.Aeson as Aeson
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.Namespace as Namespace
import qualified Scrod.Type.Identifier as Type

fromJson :: Aeson.Value -> Either String Type.Identifier
fromJson value = case value of
  Aeson.Object obj -> do
    nsJson <- Helpers.lookupField obj "namespace"
    ns <- case nsJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Namespace.fromJson nsJson)
    valJson <- Helpers.lookupField obj "value"
    val <- case valJson of
      Aeson.String t -> Right t
      _ -> Left "value must be a string"
    Right $ Type.MkIdentifier {Type.namespace = ns, Type.value = val}
  _ -> Left "Identifier must be an object"

toJson :: Type.Identifier -> Aeson.Value
toJson (Type.MkIdentifier ns val) =
  Aeson.object
    [ "namespace" Aeson..= maybe Aeson.Null Namespace.toJson ns,
      "value" Aeson..= val
    ]
