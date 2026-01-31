{-# LANGUAGE OverloadedStrings #-}

module Scrod.Json.Interface where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Scrod.Json.Doc as Doc
import qualified Scrod.Json.Export as Export
import qualified Scrod.Json.Extension as JsonExtension
import qualified Scrod.Json.Helpers as Helpers
import qualified Scrod.Json.Item as Item
import qualified Scrod.Json.Language as Language
import qualified Scrod.Json.Located as Located
import qualified Scrod.Json.ModuleName as ModuleName
import qualified Scrod.Json.Since as Since
import qualified Scrod.Json.Version as Version
import qualified Scrod.Json.Warning as Warning
import qualified Scrod.Type.Extension as Extension
import qualified Scrod.Type.Interface as Type

fromJson :: Aeson.Value -> Either String Type.Interface
fromJson value = case value of
  Aeson.Object obj -> do
    verJson <- Helpers.lookupField obj "version"
    ver <- Version.fromJson verJson
    langJson <- Helpers.lookupField obj "language"
    lang <- case langJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Language.fromJson langJson)
    extsJson <- Helpers.lookupField obj "extensions"
    exts <- extensionsFromJson extsJson
    docJson <- Helpers.lookupField obj "documentation"
    doc <- Doc.fromJson docJson
    sinceJson <- Helpers.lookupField obj "since"
    s <- case sinceJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Since.fromJson sinceJson)
    nameJson <- Helpers.lookupField obj "name"
    n <- case nameJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Located.fromJson ModuleName.fromJson nameJson)
    warnJson <- Helpers.lookupField obj "warning"
    w <- case warnJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Warning.fromJson warnJson)
    exportsJson <- Helpers.lookupField obj "exports"
    e <- case exportsJson of
      Aeson.Null -> Right Nothing
      Aeson.Array vec -> fmap Just (traverse Export.fromJson (Vector.toList vec))
      _ -> Left "exports must be an array or null"
    itemsJson <- Helpers.lookupField obj "items"
    i <- case itemsJson of
      Aeson.Array vec -> traverse (Located.fromJson Item.fromJson) (Vector.toList vec)
      _ -> Left "items must be an array"
    Right $
      Type.MkInterface
        { Type.version = ver,
          Type.language = lang,
          Type.extensions = exts,
          Type.documentation = doc,
          Type.since = s,
          Type.name = n,
          Type.warning = w,
          Type.exports = e,
          Type.items = i
        }
  _ -> Left "Interface must be an object"

toJson :: Type.Interface -> Aeson.Value
toJson (Type.MkInterface ver lang exts doc s n w e i) =
  Aeson.object
    [ "version" Aeson..= Version.toJson ver,
      "language" Aeson..= maybe Aeson.Null Language.toJson lang,
      "extensions" Aeson..= extensionsToJson exts,
      "documentation" Aeson..= Doc.toJson doc,
      "since" Aeson..= maybe Aeson.Null Since.toJson s,
      "name" Aeson..= maybe Aeson.Null (Located.toJson ModuleName.toJson) n,
      "warning" Aeson..= maybe Aeson.Null Warning.toJson w,
      "exports" Aeson..= maybe Aeson.Null (Aeson.Array . Vector.fromList . fmap Export.toJson) e,
      "items" Aeson..= Aeson.Array (Vector.fromList (fmap (Located.toJson Item.toJson) i))
    ]

extensionsFromJson :: Aeson.Value -> Either String (Map.Map Extension.Extension Bool)
extensionsFromJson value = case value of
  Aeson.Array vec -> do
    pairs <- traverse parseExtPair (Vector.toList vec)
    Right (Map.fromList pairs)
  _ -> Left "extensions must be an array"
  where
    parseExtPair pairValue = case pairValue of
      Aeson.Object obj -> do
        extJson <- Helpers.lookupField obj "extension"
        ext <- JsonExtension.fromJson extJson
        enabledJson <- Helpers.lookupField obj "enabled"
        enabled <- case enabledJson of
          Aeson.Bool b -> Right b
          _ -> Left "enabled must be a boolean"
        Right (ext, enabled)
      _ -> Left "extension entry must be an object"

extensionsToJson :: Map.Map Extension.Extension Bool -> Aeson.Value
extensionsToJson =
  Aeson.Array
    . Vector.fromList
    . fmap
      ( \(ext, enabled) ->
          Aeson.object
            [ "extension" Aeson..= JsonExtension.toJson ext,
              "enabled" Aeson..= enabled
            ]
      )
    . Map.toList
