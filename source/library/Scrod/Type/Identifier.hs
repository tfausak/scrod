{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Type.Identifier where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Type.Namespace as Namespace

-- | An identifier reference in documentation.
-- Combines an optional namespace (Value or Type) with the identifier text.
-- 'Nothing' namespace corresponds to plain 'identifier' syntax.
data Identifier = MkIdentifier
  { namespace :: Maybe Namespace.Namespace,
    value :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Identifier
fromJson = \case
  Aeson.Object obj -> do
    nsJson <- JsonHelpers.lookupField obj "namespace"
    ns <- case nsJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Namespace.fromJson nsJson)
    valJson <- JsonHelpers.lookupField obj "value"
    val <- case valJson of
      Aeson.String t -> Right t
      _ -> Left "value must be a string"
    Right $ MkIdentifier {namespace = ns, value = val}
  _ -> Left "Identifier must be an object"

toJson :: Identifier -> Aeson.Value
toJson (MkIdentifier ns val) =
  Aeson.object
    [ "namespace" Aeson..= maybe Aeson.Null Namespace.toJson ns,
      "value" Aeson..= val
    ]
