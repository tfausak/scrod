{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Interface where

import qualified Data.Aeson as Aeson
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
