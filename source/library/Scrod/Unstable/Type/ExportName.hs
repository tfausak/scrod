{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.ExportName where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Scrod.Unstable.Type.ExportNameKind as ExportNameKind
import qualified Scrod.Unstable.Type.JsonHelpers as JsonHelpers

-- | A name in an export list, possibly annotated with 'pattern' or 'type'.
-- Mirrors GHC's IEWrappedName but simplified.
data ExportName = MkExportName
  { kind :: Maybe ExportNameKind.ExportNameKind,
    name :: Text.Text
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String ExportName
fromJson = \case
  Aeson.Object obj -> do
    kindJson <- JsonHelpers.lookupField obj "kind"
    k <- case kindJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (ExportNameKind.fromJson kindJson)
    nameJson <- JsonHelpers.lookupField obj "name"
    n <- case nameJson of
      Aeson.String t -> Right t
      _ -> Left "name must be a string"
    Right $ MkExportName {kind = k, name = n}
  _ -> Left "ExportName must be an object"

toJson :: ExportName -> Aeson.Value
toJson (MkExportName k n) =
  Aeson.object
    [ "kind" Aeson..= maybe Aeson.Null ExportNameKind.toJson k,
      "name" Aeson..= n
    ]
