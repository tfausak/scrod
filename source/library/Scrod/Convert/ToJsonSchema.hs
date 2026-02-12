{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Generate a JSON Schema describing the output of 'ToJson.toJson'.
--
-- Produces a JSON Schema (2020-12) as a 'Json.Value'. The root schema
-- describes the module object, and all sub-schemas are defined in @$defs@.
module Scrod.Convert.ToJsonSchema where

import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Json.Value as Json
import qualified Scrod.JsonPointer.Evaluate as Pointer
import qualified Scrod.JsonPointer.Pointer as Pointer
import qualified Scrod.Spec as Spec

-- | See 'moduleSchema'.
toJsonSchema :: Json.Value
toJsonSchema = moduleSchema

-- * Helpers

-- | Reference a definition in @$defs@.
ref :: String -> Json.Value
ref name = Json.object [("$ref", Json.string $ "#/$defs/" <> name)]

-- | Allow a schema to also accept @null@.
nullable :: Json.Value -> Json.Value
nullable schema = Json.object [("oneOf", Json.array [schema, Json.object [("type", Json.string "null")]])]

-- | Create a tagged-object variant for use in @oneOf@.
--
-- This mirrors the runtime @{"type": tag, "value": ...}@ pattern produced
-- by 'Json.tagged'.
taggedVariant :: String -> Json.Value -> Json.Value
taggedVariant tag valueSchema =
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

-- | Create an object schema with all properties required and no additional
-- properties.
objectSchema :: [(String, Json.Value)] -> Json.Value
objectSchema props =
  Json.object
    [ ("type", Json.string "object"),
      ("properties", Json.object props),
      ("required", Json.array $ fmap (Json.string . fst) props),
      ("additionalProperties", Json.boolean False)
    ]

-- | Create a fixed-length array (tuple) schema.
tupleSchema :: [Json.Value] -> Json.Value
tupleSchema items =
  Json.object
    [ ("type", Json.string "array"),
      ("prefixItems", Json.array items),
      ("items", Json.boolean False),
      ("minItems", Json.integral $ length items),
      ("maxItems", Json.integral $ length items)
    ]

-- * Root

-- | Schema for the top-level module object.
moduleSchema :: Json.Value
moduleSchema =
  Json.object
    [ ("$schema", Json.string "https://json-schema.org/draft/2020-12/schema"),
      ("$id", Json.string "https://scrod.fyi/schema.json"),
      ("title", Json.string "Scrod"),
      ("description", Json.string "JSON output of the Scrod Haskell documentation tool."),
      ("type", Json.string "object"),
      ( "properties",
        Json.object
          [ ("version", ref "version"),
            ("language", nullable $ ref "language"),
            ("extensions", ref "extensions"),
            ("documentation", ref "doc"),
            ("since", nullable $ ref "since"),
            ("signature", Json.object [("type", Json.string "boolean")]),
            ("name", nullable $ ref "locatedModuleName"),
            ("warning", nullable $ ref "warning"),
            ("exports", nullable $ Json.object [("type", Json.string "array"), ("items", ref "export")]),
            ("imports", Json.object [("type", Json.string "array"), ("items", ref "import")]),
            ("items", Json.object [("type", Json.string "array"), ("items", ref "locatedItem")])
          ]
      ),
      ( "required",
        Json.array
          [ Json.string "version",
            Json.string "language",
            Json.string "extensions",
            Json.string "documentation",
            Json.string "since",
            Json.string "signature",
            Json.string "name",
            Json.string "warning",
            Json.string "exports",
            Json.string "imports",
            Json.string "items"
          ]
      ),
      ("additionalProperties", Json.boolean False),
      ("$defs", defs)
    ]

-- * Definitions

defs :: Json.Value
defs =
  Json.object
    [ ("version", versionSchema),
      ("language", languageSchema),
      ("extensions", extensionsSchema),
      ("doc", docSchema),
      ("since", sinceSchema),
      ("locatedModuleName", locatedModuleNameSchema),
      ("location", locationSchema),
      ("warning", warningSchema),
      ("export", exportSchema),
      ("import", importSchema),
      ("locatedItem", locatedItemSchema),
      ("item", itemSchema),
      ("itemKind", itemKindSchema),
      ("exportIdentifier", exportIdentifierSchema),
      ("exportName", exportNameSchema),
      ("exportNameKind", exportNameKindSchema),
      ("subordinates", subordinatesSchema),
      ("identifier", identifierSchema),
      ("namespace", namespaceSchema),
      ("example", exampleSchema),
      ("header", headerSchema),
      ("hyperlink", hyperlinkSchema),
      ("modLink", modLinkSchema),
      ("picture", pictureSchema),
      ("table", tableSchema),
      ("cell", cellSchema)
    ]

-- | @[integer, ...]@ with at least one element. Corresponds to
-- 'Scrod.Core.Version.Version' (a @NonEmpty Natural@).
versionSchema :: Json.Value
versionSchema =
  Json.object
    [ ("type", Json.string "array"),
      ("items", Json.object [("type", Json.string "integer"), ("minimum", Json.integer 0)]),
      ("minItems", Json.integer 1)
    ]

-- | A Haskell language edition string (e.g. @"Haskell2010"@, @"GHC2024"@).
languageSchema :: Json.Value
languageSchema = Json.object [("type", Json.string "string")]

-- | An object mapping extension names to booleans.
extensionsSchema :: Json.Value
extensionsSchema =
  Json.object
    [ ("type", Json.string "object"),
      ("additionalProperties", Json.object [("type", Json.string "boolean")])
    ]

-- | The documentation AST. This is a tagged union using @{"type": ...,
-- "value": ...}@ and is recursive.
docSchema :: Json.Value
docSchema =
  Json.object
    [ ( "oneOf",
        Json.array
          [ taggedVariant "Empty" $ Json.object [("type", Json.string "null")],
            taggedVariant "Append" $
              Json.object
                [ ("type", Json.string "array"),
                  ("items", ref "doc"),
                  ("minItems", Json.integer 2),
                  ("maxItems", Json.integer 2)
                ],
            taggedVariant "String" $ Json.object [("type", Json.string "string")],
            taggedVariant "Paragraph" $ ref "doc",
            taggedVariant "Identifier" $ ref "identifier",
            taggedVariant "Module" $ ref "modLink",
            taggedVariant "Emphasis" $ ref "doc",
            taggedVariant "Monospaced" $ ref "doc",
            taggedVariant "Bold" $ ref "doc",
            taggedVariant "UnorderedList" $
              Json.object
                [ ("type", Json.string "array"),
                  ("items", ref "doc")
                ],
            taggedVariant "OrderedList" $
              Json.object
                [ ("type", Json.string "array"),
                  ("items", tupleSchema [Json.object [("type", Json.string "integer")], ref "doc"])
                ],
            taggedVariant "DefList" $
              Json.object
                [ ("type", Json.string "array"),
                  ("items", tupleSchema [ref "doc", ref "doc"])
                ],
            taggedVariant "CodeBlock" $ ref "doc",
            taggedVariant "Hyperlink" $ ref "hyperlink",
            taggedVariant "Pic" $ ref "picture",
            taggedVariant "MathInline" $ Json.object [("type", Json.string "string")],
            taggedVariant "MathDisplay" $ Json.object [("type", Json.string "string")],
            taggedVariant "AName" $ Json.object [("type", Json.string "string")],
            taggedVariant "Property" $ Json.object [("type", Json.string "string")],
            taggedVariant "Examples" $
              Json.object
                [ ("type", Json.string "array"),
                  ("items", ref "example")
                ],
            taggedVariant "Header" $ ref "header",
            taggedVariant "Table" $ ref "table"
          ]
      )
    ]

sinceSchema :: Json.Value
sinceSchema =
  objectSchema
    [ ("package", nullable $ Json.object [("type", Json.string "string")]),
      ("version", ref "version")
    ]

locatedModuleNameSchema :: Json.Value
locatedModuleNameSchema =
  objectSchema
    [ ("location", ref "location"),
      ("value", Json.object [("type", Json.string "string")])
    ]

locationSchema :: Json.Value
locationSchema =
  objectSchema
    [ ("line", Json.object [("type", Json.string "integer"), ("minimum", Json.integer 1)]),
      ("column", Json.object [("type", Json.string "integer"), ("minimum", Json.integer 1)])
    ]

warningSchema :: Json.Value
warningSchema =
  objectSchema
    [ ("category", Json.object [("type", Json.string "string")]),
      ("value", Json.object [("type", Json.string "string")])
    ]

-- | Tagged union: @Identifier@, @Group@, @Doc@, @DocNamed@.
exportSchema :: Json.Value
exportSchema =
  Json.object
    [ ( "oneOf",
        Json.array
          [ taggedVariant "Identifier" $ ref "exportIdentifier",
            taggedVariant "Group" $ ref "header",
            taggedVariant "Doc" $ ref "doc",
            taggedVariant "DocNamed" $ Json.object [("type", Json.string "string")]
          ]
      )
    ]

importSchema :: Json.Value
importSchema =
  objectSchema
    [ ("name", Json.object [("type", Json.string "string")]),
      ("package", nullable $ Json.object [("type", Json.string "string")]),
      ("alias", nullable $ Json.object [("type", Json.string "string")])
    ]

locatedItemSchema :: Json.Value
locatedItemSchema =
  objectSchema
    [ ("location", ref "location"),
      ("value", ref "item")
    ]

itemSchema :: Json.Value
itemSchema =
  objectSchema
    [ ("key", Json.object [("type", Json.string "integer"), ("minimum", Json.integer 0)]),
      ("kind", ref "itemKind"),
      ("parentKey", nullable $ Json.object [("type", Json.string "integer"), ("minimum", Json.integer 0)]),
      ("name", nullable $ Json.object [("type", Json.string "string")]),
      ("documentation", ref "doc"),
      ("signature", nullable $ Json.object [("type", Json.string "string")])
    ]

itemKindSchema :: Json.Value
itemKindSchema =
  Json.object
    [ ( "oneOf",
        Json.array $
          fmap
            (\name -> taggedVariant name $ Json.object [("type", Json.string "null")])
            [ "Annotation",
              "Class",
              "ClassInstance",
              "ClassMethod",
              "ClosedTypeFamily",
              "DataConstructor",
              "DataFamily",
              "DataFamilyInstance",
              "DataType",
              "Default",
              "DerivedInstance",
              "FixitySignature",
              "ForeignExport",
              "ForeignImport",
              "Function",
              "GADTConstructor",
              "InlineSignature",
              "Newtype",
              "OpenTypeFamily",
              "PatternBinding",
              "PatternSynonym",
              "RecordField",
              "Rule",
              "SpecialiseSignature",
              "Splice",
              "StandaloneDeriving",
              "StandaloneKindSig",
              "TypeData",
              "TypeFamilyInstance",
              "TypeSynonym"
            ]
      )
    ]

exportIdentifierSchema :: Json.Value
exportIdentifierSchema =
  objectSchema
    [ ("name", ref "exportName"),
      ("subordinates", nullable $ ref "subordinates"),
      ("warning", nullable $ ref "warning"),
      ("doc", nullable $ ref "doc")
    ]

exportNameSchema :: Json.Value
exportNameSchema =
  objectSchema
    [ ("kind", nullable $ ref "exportNameKind"),
      ("name", Json.object [("type", Json.string "string")])
    ]

exportNameKindSchema :: Json.Value
exportNameKindSchema =
  Json.object
    [ ( "oneOf",
        Json.array $
          fmap
            (\name -> taggedVariant name $ Json.object [("type", Json.string "null")])
            ["Module", "Pattern", "Type"]
      )
    ]

subordinatesSchema :: Json.Value
subordinatesSchema =
  objectSchema
    [ ("wildcard", Json.object [("type", Json.string "boolean")]),
      ("explicit", Json.object [("type", Json.string "array"), ("items", ref "exportName")])
    ]

identifierSchema :: Json.Value
identifierSchema =
  objectSchema
    [ ("namespace", nullable $ ref "namespace"),
      ("value", Json.object [("type", Json.string "string")])
    ]

namespaceSchema :: Json.Value
namespaceSchema =
  Json.object
    [ ( "oneOf",
        Json.array $
          fmap
            (\name -> taggedVariant name $ Json.object [("type", Json.string "null")])
            ["Type", "Value"]
      )
    ]

exampleSchema :: Json.Value
exampleSchema =
  objectSchema
    [ ("expression", Json.object [("type", Json.string "string")]),
      ("result", Json.object [("type", Json.string "array"), ("items", Json.object [("type", Json.string "string")])])
    ]

headerSchema :: Json.Value
headerSchema =
  objectSchema
    [ ("level", Json.object [("type", Json.string "integer"), ("minimum", Json.integer 1), ("maximum", Json.integer 6)]),
      ("title", ref "doc")
    ]

hyperlinkSchema :: Json.Value
hyperlinkSchema =
  objectSchema
    [ ("url", Json.object [("type", Json.string "string")]),
      ("label", nullable $ ref "doc")
    ]

modLinkSchema :: Json.Value
modLinkSchema =
  objectSchema
    [ ("name", Json.object [("type", Json.string "string")]),
      ("label", nullable $ ref "doc")
    ]

pictureSchema :: Json.Value
pictureSchema =
  objectSchema
    [ ("uri", Json.object [("type", Json.string "string")]),
      ("title", nullable $ Json.object [("type", Json.string "string")])
    ]

tableSchema :: Json.Value
tableSchema =
  objectSchema
    [ ("headerRows", Json.object [("type", Json.string "array"), ("items", Json.object [("type", Json.string "array"), ("items", ref "cell")])]),
      ("bodyRows", Json.object [("type", Json.string "array"), ("items", Json.object [("type", Json.string "array"), ("items", ref "cell")])])
    ]

cellSchema :: Json.Value
cellSchema =
  objectSchema
    [ ("colspan", Json.object [("type", Json.string "integer"), ("minimum", Json.integer 1)]),
      ("rowspan", Json.object [("type", Json.string "integer"), ("minimum", Json.integer 1)]),
      ("contents", ref "doc")
    ]

-- * Tests

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'toJsonSchema $ do
    Spec.it s "round-trips through JSON encode/decode" $ do
      let encoded = Builder.toString $ Json.encode toJsonSchema
      Spec.assertNe s (Parsec.parseString Json.decode encoded) Nothing

    Spec.it s "has the expected $schema" $ do
      at s "/$schema" $ Json.string "https://json-schema.org/draft/2020-12/schema"

    Spec.it s "has the expected title" $ do
      at s "/title" $ Json.string "Scrod"

    Spec.it s "has type object" $ do
      at s "/type" $ Json.string "object"

    Spec.it s "defines doc" $ do
      at s "/$defs/doc/oneOf" Json.null

    Spec.it s "defines version" $ do
      at s "/$defs/version/type" $ Json.string "array"

    Spec.it s "defines itemKind" $ do
      at s "/$defs/itemKind/oneOf" Json.null

    Spec.it s "defines location" $ do
      at s "/$defs/location/type" $ Json.string "object"

-- | Assert that a JSON Pointer path resolves to the expected value in the
-- schema.
at :: (Applicative m) => Spec.Spec m n -> String -> Json.Value -> m ()
at s path expected = do
  let json = toJsonSchema
  case Parsec.parseString Pointer.decode path of
    Nothing -> Spec.assertFailure s $ "invalid pointer: " <> path
    Just pointer -> case Pointer.evaluate pointer json of
      Nothing -> Spec.assertFailure s $ "path not found: " <> path
      Just actual ->
        -- For the $defs check where we just want to confirm existence, we
        -- pass null as expected and skip the equality check.
        if expected == Json.null
          then pure ()
          else Spec.assertEq s actual expected
