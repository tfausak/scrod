{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Interface where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Types
import qualified Data.Map as Map
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Export as Export
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Item as Item
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Version as Version
import qualified Scrod.Unstable.Type.Warning as Warning

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

instance Aeson.FromJSON Interface where
  parseJSON = Aeson.withObject "Interface" $ \obj -> do
    ver <- obj Aeson..: "version"
    lang <- obj Aeson..: "language"
    extsJson <- obj Aeson..: "extensions"
    exts <- extensionsFromJson extsJson
    doc <- obj Aeson..: "documentation"
    s <- obj Aeson..: "since"
    n <- obj Aeson..: "name"
    w <- obj Aeson..: "warning"
    e <- obj Aeson..: "exports"
    i <- obj Aeson..: "items"
    pure $
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

instance Aeson.ToJSON Interface where
  toJSON (MkInterface ver lang exts doc s n w e i) =
    Aeson.object
      [ "version" Aeson..= ver,
        "language" Aeson..= lang,
        "extensions" Aeson..= extensionsToJson exts,
        "documentation" Aeson..= doc,
        "since" Aeson..= s,
        "name" Aeson..= n,
        "warning" Aeson..= w,
        "exports" Aeson..= e,
        "items" Aeson..= i
      ]

extensionsFromJson :: [Aeson.Value] -> Types.Parser (Map.Map Extension.Extension Bool)
extensionsFromJson = fmap Map.fromList . traverse parseExt
  where
    parseExt :: Aeson.Value -> Types.Parser (Extension.Extension, Bool)
    parseExt = Aeson.withObject "Extension" $ \obj -> do
      ext <- obj Aeson..: "extension"
      enabled <- obj Aeson..: "enabled"
      pure (ext, enabled)

extensionsToJson :: Map.Map Extension.Extension Bool -> [Aeson.Value]
extensionsToJson =
  fmap
    ( \(ext, enabled) ->
        Aeson.object
          [ "extension" Aeson..= ext,
            "enabled" Aeson..= enabled
          ]
    )
    . Map.toList
