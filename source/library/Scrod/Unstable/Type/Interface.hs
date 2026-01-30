{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.Interface where

import qualified Data.Map as Map
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Export as Export
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Item as Item
import qualified Scrod.Unstable.Type.Json as Json
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

toJson :: Interface -> Json.Json
toJson (MkInterface ver lang exts doc s n w e i) =
  Json.object
    [ ("version", Version.toJson ver),
      ("language", maybe Json.Null Language.toJson lang),
      ("extensions", extensionsToJson exts),
      ("documentation", Doc.toJson doc),
      ("since", maybe Json.Null Since.toJson s),
      ("name", maybe Json.Null (Located.toJson ModuleName.toJson) n),
      ("warning", maybe Json.Null Warning.toJson w),
      ("exports", maybe Json.Null (Json.fromList . fmap Export.toJson) e),
      ("items", Json.fromList $ fmap (Located.toJson Item.toJson) i)
    ]
  where
    extensionsToJson =
      Json.fromList
        . fmap
          ( \(ext, enabled) ->
              Json.object
                [ ("extension", Extension.toJson ext),
                  ("enabled", Json.fromBool enabled)
                ]
          )
        . Map.toList
