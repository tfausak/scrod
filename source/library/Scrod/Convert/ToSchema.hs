{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Provide 'ToSchema' instances for Scrod core types.
--
-- Simple newtype wrappers use @deriving via@ the underlying type.
-- Record types, enum types, and tagged sum types all use
-- @deriving via 'Generics.Generically'@ to get instances derived
-- generically. Other types have hand-written instances. Import this
-- module to bring all instances into scope.
module Scrod.Convert.ToSchema
  ( module Scrod.Schema,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Numeric.Natural as Natural
import qualified Scrod.Core.Category as Category
import qualified Scrod.Core.Column as Column
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Example as Example
import qualified Scrod.Core.Export as Export
import qualified Scrod.Core.ExportIdentifier as ExportIdentifier
import qualified Scrod.Core.ExportName as ExportName
import qualified Scrod.Core.ExportNameKind as ExportNameKind
import qualified Scrod.Core.Extension as Extension
import qualified Scrod.Core.Header as Header
import qualified Scrod.Core.Hyperlink as Hyperlink
import qualified Scrod.Core.Identifier as Identifier
import qualified Scrod.Core.Import as Import
import qualified Scrod.Core.Item as Item
import qualified Scrod.Core.ItemKey as ItemKey
import qualified Scrod.Core.ItemKind as ItemKind
import qualified Scrod.Core.ItemName as ItemName
import qualified Scrod.Core.Language as Language
import qualified Scrod.Core.Level as Level
import qualified Scrod.Core.Line as Line
import qualified Scrod.Core.Located as Located
import qualified Scrod.Core.Location as Location
import qualified Scrod.Core.ModLink as ModLink
import qualified Scrod.Core.Module as Module
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.Namespace as Namespace
import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Core.Picture as Picture
import qualified Scrod.Core.Section as Section
import qualified Scrod.Core.Since as Since
import qualified Scrod.Core.Subordinates as Subordinates
import qualified Scrod.Core.Table as Table
import qualified Scrod.Core.TableCell as TableCell
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Warning as Warning
import qualified Scrod.Json.Value as Json
import Scrod.Schema (Schema (MkSchema, unwrap), SchemaM, ToSchema (isOptional, toSchema), define)
import qualified Scrod.Schema as ToSchema

-- * Simple newtype wrappers use @deriving via@ to get their instances

-- from the underlying type.

deriving via Text.Text instance ToSchema Category.Category

deriving via Natural.Natural instance ToSchema Column.Column

deriving via Text.Text instance ToSchema Extension.Extension

deriving via Text.Text instance ToSchema ItemName.ItemName

deriving via Natural.Natural instance ToSchema ItemKey.ItemKey

deriving via Text.Text instance ToSchema Language.Language

deriving via Natural.Natural instance ToSchema Line.Line

deriving via Text.Text instance ToSchema ModuleName.ModuleName

deriving via Text.Text instance ToSchema PackageName.PackageName

deriving via Header.Header Doc.Doc instance ToSchema Section.Section

deriving via NonEmpty.NonEmpty Natural.Natural instance ToSchema Version.Version

-- * Record types, enum types, and tagged sum types use

-- @Generics.Generically@ to derive their instances generically.

deriving via Generics.Generically Example.Example instance ToSchema Example.Example

deriving via Generics.Generically ExportIdentifier.ExportIdentifier instance ToSchema ExportIdentifier.ExportIdentifier

deriving via Generics.Generically ExportName.ExportName instance ToSchema ExportName.ExportName

deriving via Generics.Generically (Header.Header doc) instance (ToSchema doc) => ToSchema (Header.Header doc)

deriving via Generics.Generically (Hyperlink.Hyperlink doc) instance (ToSchema doc) => ToSchema (Hyperlink.Hyperlink doc)

deriving via Generics.Generically Identifier.Identifier instance ToSchema Identifier.Identifier

deriving via Generics.Generically Import.Import instance ToSchema Import.Import

deriving via Generics.Generically Item.Item instance ToSchema Item.Item

deriving via Generics.Generically (Located.Located a) instance (ToSchema a) => ToSchema (Located.Located a)

deriving via Generics.Generically Location.Location instance ToSchema Location.Location

deriving via Generics.Generically (ModLink.ModLink doc) instance (ToSchema doc) => ToSchema (ModLink.ModLink doc)

deriving via Generics.Generically Picture.Picture instance ToSchema Picture.Picture

deriving via Generics.Generically Since.Since instance ToSchema Since.Since

deriving via Generics.Generically Subordinates.Subordinates instance ToSchema Subordinates.Subordinates

deriving via Generics.Generically (Table.Table doc) instance (ToSchema doc) => ToSchema (Table.Table doc)

deriving via Generics.Generically (TableCell.Cell doc) instance (ToSchema doc) => ToSchema (TableCell.Cell doc)

deriving via Generics.Generically Warning.Warning instance ToSchema Warning.Warning

deriving via Generics.Generically ExportNameKind.ExportNameKind instance ToSchema ExportNameKind.ExportNameKind

deriving via Generics.Generically ItemKind.ItemKind instance ToSchema ItemKind.ItemKind

deriving via Generics.Generically Namespace.Namespace instance ToSchema Namespace.Namespace

deriving via Generics.Generically Export.Export instance ToSchema Export.Export

-- * Hand-written instances for types that require special encoding.

instance ToSchema Module.Module where
  toSchema _ = do
    version <- toSchema (Proxy.Proxy :: Proxy.Proxy Version.Version)
    language <- toSchema (Proxy.Proxy :: Proxy.Proxy Language.Language)
    extensions <- extensionsSchema
    documentation <- toSchema (Proxy.Proxy :: Proxy.Proxy Doc.Doc)
    since <- toSchema (Proxy.Proxy :: Proxy.Proxy Since.Since)
    name <- toSchema (Proxy.Proxy :: Proxy.Proxy (Located.Located ModuleName.ModuleName))
    warning <- toSchema (Proxy.Proxy :: Proxy.Proxy Warning.Warning)
    exports <- toSchema (Proxy.Proxy :: Proxy.Proxy [Export.Export])
    imports <- toSchema (Proxy.Proxy :: Proxy.Proxy [Import.Import])
    items <- toSchema (Proxy.Proxy :: Proxy.Proxy [Located.Located Item.Item])
    let allProps =
          [ ("version", unwrap version),
            ("language", unwrap language),
            ("extensions", unwrap extensions),
            ("documentation", unwrap documentation),
            ("since", unwrap since),
            ("signature", Json.object [("type", Json.string "boolean")]),
            ("name", unwrap name),
            ("warning", unwrap warning),
            ("exports", unwrap exports),
            ("imports", unwrap imports),
            ("items", unwrap items)
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

-- | 'Doc.Doc' is recursive, so its schema uses 'define' to register a
-- named definition in @$defs@ and return a @$ref@. The schema value is
-- built purely with inline sub-schemas and a self-reference for @Doc@.
instance ToSchema Doc.Doc where
  toSchema _ = define "doc" $ pure (MkSchema docSchemaValue)

-- | Pure schema value for 'Doc.Doc'. Uses @$ref \"#/$defs/doc\"@ for
-- self-references and inlines all other sub-schemas.
docSchemaValue :: Json.Value
docSchemaValue =
  let self = Json.object [("$ref", Json.string "#/$defs/doc")]
      str = Json.object [("type", Json.string "string")]
      nullSchema = Json.object [("type", Json.string "null")]
      int = Json.object [("type", Json.string "integer")]
      tagged tag valueSchema =
        Json.object
          [ ("type", Json.string "object"),
            ( "properties",
              Json.object
                [ ("type", Json.object [("const", Json.string tag)]),
                  ("value", valueSchema)
                ]
            ),
            ("required", Json.array [Json.string "type", Json.string "value"]),
            ("additionalProperties", Json.boolean False)
          ]
      tupleOf items =
        Json.object
          [ ("type", Json.string "array"),
            ("prefixItems", Json.array items),
            ("items", Json.boolean False),
            ("minItems", Json.integral $ length items),
            ("maxItems", Json.integral $ length items)
          ]
      -- Inline sub-schemas (non-recursive types, computed purely)
      identifierSchema = pureSchema (Proxy.Proxy :: Proxy.Proxy Identifier.Identifier)
      pictureSchema = pureSchema (Proxy.Proxy :: Proxy.Proxy Picture.Picture)
      exampleSchema = pureSchema (Proxy.Proxy :: Proxy.Proxy Example.Example)
      -- Sub-schemas parameterized by Doc (use self-reference)
      modLinkSchema =
        objectSchemaOptPure
          [("name", str)]
          [("label", self)]
      hyperlinkSchema =
        objectSchemaOptPure
          [("url", str)]
          [("label", self)]
      headerSchema =
        objectSchemaPure
          [ ("level", pureSchema (Proxy.Proxy :: Proxy.Proxy Level.Level)),
            ("title", self)
          ]
      cellSchema =
        objectSchemaPure
          [ ("colspan", pureSchema (Proxy.Proxy :: Proxy.Proxy Natural.Natural)),
            ("rowspan", pureSchema (Proxy.Proxy :: Proxy.Proxy Natural.Natural)),
            ("contents", self)
          ]
      tableSchema =
        objectSchemaPure
          [ ("headerRows", Json.object [("type", Json.string "array"), ("items", Json.object [("type", Json.string "array"), ("items", cellSchema)])]),
            ("bodyRows", Json.object [("type", Json.string "array"), ("items", Json.object [("type", Json.string "array"), ("items", cellSchema)])])
          ]
   in Json.object
        [ ( "oneOf",
            Json.array
              [ tagged "Empty" nullSchema,
                tagged "Append" $
                  Json.object
                    [ ("type", Json.string "array"),
                      ("items", self),
                      ("minItems", Json.integer 2),
                      ("maxItems", Json.integer 2)
                    ],
                tagged "String" str,
                tagged "Paragraph" self,
                tagged "Identifier" identifierSchema,
                tagged "Module" modLinkSchema,
                tagged "Emphasis" self,
                tagged "Monospaced" self,
                tagged "Bold" self,
                tagged "UnorderedList" $
                  Json.object
                    [ ("type", Json.string "array"),
                      ("items", self)
                    ],
                tagged "OrderedList" $
                  Json.object
                    [ ("type", Json.string "array"),
                      ("items", tupleOf [int, self])
                    ],
                tagged "DefList" $
                  Json.object
                    [ ("type", Json.string "array"),
                      ("items", tupleOf [self, self])
                    ],
                tagged "CodeBlock" self,
                tagged "Hyperlink" hyperlinkSchema,
                tagged "Pic" pictureSchema,
                tagged "MathInline" str,
                tagged "MathDisplay" str,
                tagged "AName" str,
                tagged "Property" str,
                tagged "Examples" $
                  Json.object
                    [ ("type", Json.string "array"),
                      ("items", exampleSchema)
                    ],
                tagged "Header" headerSchema,
                tagged "Table" tableSchema
              ]
          )
        ]

-- | Extract the schema value from a non-recursive 'ToSchema' instance
-- without the monadic context. Only safe for types whose 'toSchema'
-- does not depend on accumulated definitions.
pureSchema :: (ToSchema a) => Proxy.Proxy a -> Json.Value
pureSchema p = unwrap . fst $ ToSchema.runSchemaM (toSchema p)

-- | Build an object schema from required properties only (pure helper).
objectSchemaPure :: [(String, Json.Value)] -> Json.Value
objectSchemaPure props =
  Json.object
    [ ("type", Json.string "object"),
      ("properties", Json.object props),
      ("required", Json.array $ fmap (Json.string . fst) props),
      ("additionalProperties", Json.boolean False)
    ]

-- | Build an object schema with required and optional properties (pure
-- helper).
objectSchemaOptPure :: [(String, Json.Value)] -> [(String, Json.Value)] -> Json.Value
objectSchemaOptPure required optional =
  Json.object
    [ ("type", Json.string "object"),
      ("properties", Json.object (required <> optional)),
      ("required", Json.array $ fmap (Json.string . fst) required),
      ("additionalProperties", Json.boolean False)
    ]

instance ToSchema Level.Level where
  toSchema _ =
    pure . MkSchema $
      Json.object
        [ ("type", Json.string "integer"),
          ("minimum", Json.integer 1),
          ("maximum", Json.integer 6)
        ]
