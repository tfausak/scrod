module Scrod.Core.Module where

import qualified Data.Map as Map
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Export as Export
import qualified Scrod.Core.Extension as Extension
import qualified Scrod.Core.Import as Import
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.Language as Language
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.Since as Since
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Warning as Warning
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Json.Value as Json
import qualified Scrod.Schema as Schema

data Module = MkModule
  { version :: Version.Version,
    language :: Maybe Language.Language,
    extensions :: Map.Map Extension.Extension Bool,
    documentation :: Doc.Doc,
    since :: Maybe Since.Since,
    signature :: Bool,
    name :: Maybe (Located.Located ModuleName.ModuleName),
    warning :: Maybe Warning.Warning,
    exports :: Maybe [Export.Export],
    imports :: [Import.Import],
    items :: [Located.Located Item.Item]
  }
  deriving (Eq, Ord, Show)

instance ToJson.ToJson Module where
  toJson m =
    Json.object
      . filter (\(_, v) -> v /= Json.null)
      $ [ ("version", ToJson.toJson $ version m),
          ("language", ToJson.toJson $ language m),
          ("extensions", extensionsToJson $ extensions m),
          ("documentation", ToJson.toJson $ documentation m),
          ("since", ToJson.toJson $ since m),
          ("signature", ToJson.toJson $ signature m),
          ("name", ToJson.toJson $ name m),
          ("warning", ToJson.toJson $ warning m),
          ("exports", ToJson.toJson $ exports m),
          ("imports", ToJson.toJson $ imports m),
          ("items", ToJson.toJson $ items m)
        ]

extensionsToJson :: Map.Map Extension.Extension Bool -> Json.Value
extensionsToJson =
  Json.object
    . fmap (\(k, v) -> (Text.unpack $ Extension.unwrap k, ToJson.toJson v))
    . Map.toList

instance Schema.ToSchema Module where
  toSchema _ = do
    versionS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Version.Version)
    languageS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Language.Language)
    extensionsS <- extensionsSchema
    documentationS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Doc.Doc)
    sinceS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Since.Since)
    nameS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy (Located.Located ModuleName.ModuleName))
    warningS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Warning.Warning)
    exportsS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy [Export.Export])
    importsS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy [Import.Import])
    itemsS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy [Located.Located Item.Item])
    let allProps =
          [ ("version", Schema.unwrap versionS),
            ("language", Schema.unwrap languageS),
            ("extensions", Schema.unwrap extensionsS),
            ("documentation", Schema.unwrap documentationS),
            ("since", Schema.unwrap sinceS),
            ("signature", Json.object [("type", Json.string "boolean")]),
            ("name", Schema.unwrap nameS),
            ("warning", Schema.unwrap warningS),
            ("exports", Schema.unwrap exportsS),
            ("imports", Schema.unwrap importsS),
            ("items", Schema.unwrap itemsS)
          ]
    let reqNames =
          [ Json.string "version",
            Json.string "extensions",
            Json.string "documentation",
            Json.string "signature",
            Json.string "imports",
            Json.string "items"
          ]
    pure . Schema.MkSchema $
      Json.object
        [ ("type", Json.string "object"),
          ("properties", Json.object allProps),
          ("required", Json.array reqNames),
          ("additionalProperties", Json.boolean False)
        ]

extensionsSchema :: Schema.SchemaM Schema.Schema
extensionsSchema =
  pure . Schema.MkSchema $
    Json.object
      [ ("type", Json.string "object"),
        ("additionalProperties", Json.object [("type", Json.string "boolean")])
      ]
