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
import Scrod.Json.ToJson (ToJson (toJson))
import qualified Scrod.Json.Value as Json
import Scrod.Schema (Schema (MkSchema, unwrap), SchemaM, ToSchema (toSchema))

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

instance ToJson Module where
  toJson m =
    Json.object
      . filter (\(_, v) -> v /= Json.null)
      $ [ ("version", toJson $ version m),
          ("language", toJson $ language m),
          ("extensions", extensionsToJson $ extensions m),
          ("documentation", toJson $ documentation m),
          ("since", toJson $ since m),
          ("signature", toJson $ signature m),
          ("name", toJson $ name m),
          ("warning", toJson $ warning m),
          ("exports", toJson $ exports m),
          ("imports", toJson $ imports m),
          ("items", toJson $ items m)
        ]

extensionsToJson :: Map.Map Extension.Extension Bool -> Json.Value
extensionsToJson =
  Json.object
    . fmap (\(k, v) -> (Text.unpack $ Extension.unwrap k, toJson v))
    . Map.toList

instance ToSchema Module where
  toSchema _ = do
    versionS <- toSchema (Proxy.Proxy :: Proxy.Proxy Version.Version)
    languageS <- toSchema (Proxy.Proxy :: Proxy.Proxy Language.Language)
    extensionsS <- extensionsSchema
    documentationS <- toSchema (Proxy.Proxy :: Proxy.Proxy Doc.Doc)
    sinceS <- toSchema (Proxy.Proxy :: Proxy.Proxy Since.Since)
    nameS <- toSchema (Proxy.Proxy :: Proxy.Proxy (Located.Located ModuleName.ModuleName))
    warningS <- toSchema (Proxy.Proxy :: Proxy.Proxy Warning.Warning)
    exportsS <- toSchema (Proxy.Proxy :: Proxy.Proxy [Export.Export])
    importsS <- toSchema (Proxy.Proxy :: Proxy.Proxy [Import.Import])
    itemsS <- toSchema (Proxy.Proxy :: Proxy.Proxy [Located.Located Item.Item])
    let allProps =
          [ ("version", unwrap versionS),
            ("language", unwrap languageS),
            ("extensions", unwrap extensionsS),
            ("documentation", unwrap documentationS),
            ("since", unwrap sinceS),
            ("signature", Json.object [("type", Json.string "boolean")]),
            ("name", unwrap nameS),
            ("warning", unwrap warningS),
            ("exports", unwrap exportsS),
            ("imports", unwrap importsS),
            ("items", unwrap itemsS)
          ]
    let reqNames =
          [ Json.string "version",
            Json.string "extensions",
            Json.string "documentation",
            Json.string "signature",
            Json.string "imports",
            Json.string "items"
          ]
    pure . MkSchema $
      Json.object
        [ ("type", Json.string "object"),
          ("properties", Json.object allProps),
          ("required", Json.array reqNames),
          ("additionalProperties", Json.boolean False)
        ]

extensionsSchema :: SchemaM Schema
extensionsSchema =
  pure . MkSchema $
    Json.object
      [ ("type", Json.string "object"),
        ("additionalProperties", Json.object [("type", Json.string "boolean")])
      ]
