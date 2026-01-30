{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Type.Namespace where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Type.JsonHelpers as JsonHelpers

-- | The namespace qualification for an identifier.
-- Mirrors 'Documentation.Haddock.Types.Namespace' from haddock-library,
-- but without the 'None' case (represented as 'Nothing' in 'Identifier').
data Namespace
  = -- | v'identifier' syntax
    Value
  | -- | t'identifier' syntax
    Type
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Namespace
fromJson = \case
  Aeson.Object obj -> do
    tagJson <- JsonHelpers.lookupField obj "tag"
    tag <- case tagJson of
      Aeson.String t -> Right t
      _ -> Left "tag must be a string"
    case tag of
      "Value" -> Right Value
      "Type" -> Right Type
      _ -> Left $ "unknown Namespace tag: " <> Text.unpack tag
  _ -> Left "Namespace must be an object"

toJson :: Namespace -> Aeson.Value
toJson ns = Aeson.object ["tag" Aeson..= tag]
  where
    tag :: Text.Text
    tag = case ns of
      Value -> "Value"
      Type -> "Type"
