module Scrod.Unstable.Type.Interface where

import qualified Data.Map as Map
import qualified Data.Text as Text
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
    [ (Text.pack "version", Version.toJson ver),
      (Text.pack "language", maybe Json.Null Language.toJson lang),
      (Text.pack "extensions", extensionsToJson exts),
      (Text.pack "documentation", Doc.toJson doc),
      (Text.pack "since", maybe Json.Null Since.toJson s),
      (Text.pack "name", maybe Json.Null (Located.toJson ModuleName.toJson) n),
      (Text.pack "warning", maybe Json.Null Warning.toJson w),
      (Text.pack "exports", maybe Json.Null (Json.fromList . fmap Export.toJson) e),
      (Text.pack "items", Json.fromList $ fmap (Located.toJson Item.toJson) i)
    ]
  where
    extensionsToJson =
      Json.fromList
        . fmap
          ( \(ext, enabled) ->
              Json.object
                [ (Text.pack "extension", Extension.toJson ext),
                  (Text.pack "enabled", Json.fromBool enabled)
                ]
          )
        . Map.toList
