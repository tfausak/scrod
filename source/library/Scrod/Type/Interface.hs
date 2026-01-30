{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrod.Type.Interface where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Scrod.Type.Doc as Doc
import qualified Scrod.Type.Export as Export
import qualified Scrod.Type.Extension as Extension
import qualified Scrod.Type.Item as Item
import qualified Scrod.Type.JsonHelpers as JsonHelpers
import qualified Scrod.Type.Language as Language
import qualified Scrod.Type.Located as Located
import qualified Scrod.Type.ModuleName as ModuleName
import qualified Scrod.Type.Since as Since
import qualified Scrod.Type.Version as Version
import qualified Scrod.Type.Warning as Warning

data Interface = MkInterface
  { version :: Version.Version,
    language :: Maybe Language.Language,
    extensions :: Map.Map Extension.Extension Bool,
    documentation :: Doc.Doc,
    since :: Maybe Since.Since,
    name :: Maybe (Located.Located ModuleName.ModuleName),
    warning :: Maybe Warning.Warning,
    exports :: Maybe [Export.Export],
    items :: [Located.Located Item.Item]
  }
  deriving (Eq, Ord, Show)

fromJson :: Aeson.Value -> Either String Interface
fromJson = \case
  Aeson.Object obj -> do
    verJson <- JsonHelpers.lookupField obj "version"
    ver <- Version.fromJson verJson
    langJson <- JsonHelpers.lookupField obj "language"
    lang <- case langJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Language.fromJson langJson)
    extsJson <- JsonHelpers.lookupField obj "extensions"
    exts <- extensionsFromJson extsJson
    docJson <- JsonHelpers.lookupField obj "documentation"
    doc <- Doc.fromJson docJson
    sinceJson <- JsonHelpers.lookupField obj "since"
    s <- case sinceJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Since.fromJson sinceJson)
    nameJson <- JsonHelpers.lookupField obj "name"
    n <- case nameJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Located.fromJson ModuleName.fromJson nameJson)
    warnJson <- JsonHelpers.lookupField obj "warning"
    w <- case warnJson of
      Aeson.Null -> Right Nothing
      _ -> fmap Just (Warning.fromJson warnJson)
    exportsJson <- JsonHelpers.lookupField obj "exports"
    e <- case exportsJson of
      Aeson.Null -> Right Nothing
      Aeson.Array vec -> fmap Just (traverse Export.fromJson (Vector.toList vec))
      _ -> Left "exports must be an array or null"
    itemsJson <- JsonHelpers.lookupField obj "items"
    i <- case itemsJson of
      Aeson.Array vec -> traverse (Located.fromJson Item.fromJson) (Vector.toList vec)
      _ -> Left "items must be an array"
    Right $
      MkInterface
        { version = ver,
          language = lang,
          extensions = exts,
          documentation = doc,
          since = s,
          name = n,
          warning = w,
          exports = e,
          items = i
        }
  _ -> Left "Interface must be an object"

toJson :: Interface -> Aeson.Value
toJson (MkInterface ver lang exts doc s n w e i) =
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
extensionsFromJson = \case
  Aeson.Array vec -> do
    pairs <- traverse parseExtPair (Vector.toList vec)
    Right (Map.fromList pairs)
  _ -> Left "extensions must be an array"
  where
    parseExtPair = \case
      Aeson.Object obj -> do
        extJson <- JsonHelpers.lookupField obj "extension"
        ext <- Extension.fromJson extJson
        enabledJson <- JsonHelpers.lookupField obj "enabled"
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
            [ "extension" Aeson..= Extension.toJson ext,
              "enabled" Aeson..= enabled
            ]
      )
    . Map.toList
