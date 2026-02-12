module Scrod.Core.Doc where

import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Scrod.Core.Example as Example
import qualified Scrod.Core.Header as Header
import qualified Scrod.Core.Hyperlink as Hyperlink
import qualified Scrod.Core.Identifier as Identifier
import qualified Scrod.Core.Level as Level
import qualified Scrod.Core.ModLink as ModLink
import qualified Scrod.Core.Picture as Picture
import qualified Scrod.Core.Table as Table
import Scrod.Json.ToJson (ToJson (toJson))
import qualified Scrod.Json.Value as Json
import Scrod.Schema (Schema (MkSchema, unwrap), ToSchema (toSchema), define)
import qualified Scrod.Schema as ToSchema

-- | Documentation AST.
data Doc
  = Empty
  | Append Doc Doc
  | String Text.Text
  | Paragraph Doc
  | Identifier Identifier.Identifier
  | Module (ModLink.ModLink Doc)
  | Emphasis Doc
  | Monospaced Doc
  | Bold Doc
  | UnorderedList [Doc]
  | OrderedList [(Int, Doc)]
  | DefList [(Doc, Doc)]
  | CodeBlock Doc
  | Hyperlink (Hyperlink.Hyperlink Doc)
  | Pic Picture.Picture
  | MathInline Text.Text
  | MathDisplay Text.Text
  | AName Text.Text
  | Property Text.Text
  | Examples [Example.Example]
  | Header (Header.Header Doc)
  | Table (Table.Table Doc)
  deriving (Eq, Ord, Show)

instance ToJson Doc where
  toJson doc = case doc of
    Empty -> Json.tagged "Empty" Json.null
    Append a b -> Json.tagged "Append" $ Json.arrayOf toJson [a, b]
    String t -> Json.tagged "String" $ Json.text t
    Paragraph d -> Json.tagged "Paragraph" $ toJson d
    Identifier i -> Json.tagged "Identifier" $ toJson i
    Module ml -> Json.tagged "Module" $ toJson ml
    Emphasis d -> Json.tagged "Emphasis" $ toJson d
    Monospaced d -> Json.tagged "Monospaced" $ toJson d
    Bold d -> Json.tagged "Bold" $ toJson d
    UnorderedList ds -> Json.tagged "UnorderedList" $ toJson ds
    OrderedList items ->
      Json.tagged "OrderedList" $
        Json.arrayOf (\(n, d) -> Json.array [Json.integral n, toJson d]) items
    DefList defs ->
      Json.tagged "DefList" $
        Json.arrayOf (\(t, d) -> Json.arrayOf toJson [t, d]) defs
    CodeBlock d -> Json.tagged "CodeBlock" $ toJson d
    Hyperlink h -> Json.tagged "Hyperlink" $ toJson h
    Pic p -> Json.tagged "Pic" $ toJson p
    MathInline t -> Json.tagged "MathInline" $ Json.text t
    MathDisplay t -> Json.tagged "MathDisplay" $ Json.text t
    AName t -> Json.tagged "AName" $ Json.text t
    Property t -> Json.tagged "Property" $ Json.text t
    Examples es -> Json.tagged "Examples" $ toJson es
    Header h -> Json.tagged "Header" $ toJson h
    Table t -> Json.tagged "Table" $ toJson t

-- | 'Doc' is recursive, so its schema uses 'define' to register a
-- named definition in @$defs@ and return a @$ref@. The schema value is
-- built purely with inline sub-schemas and a self-reference for @Doc@.
instance ToSchema Doc where
  toSchema _ = define "doc" $ pure (MkSchema docSchemaValue)

-- | Pure schema value for 'Doc'. Uses @$ref \"#/$defs/doc\"@ for
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
