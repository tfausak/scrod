module Scrod.Core.Doc where

import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Scrod.Core.Definition as Definition
import qualified Scrod.Core.Example as Example
import qualified Scrod.Core.Header as Header
import qualified Scrod.Core.Hyperlink as Hyperlink
import qualified Scrod.Core.Identifier as Identifier
import qualified Scrod.Core.ModLink as ModLink
import qualified Scrod.Core.NumberedItem as NumberedItem
import qualified Scrod.Core.Picture as Picture
import qualified Scrod.Core.Table as Table
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Json.Value as Json
import qualified Scrod.Schema as Schema

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
  | OrderedList [NumberedItem.NumberedItem Doc]
  | DefList [Definition.Definition Doc]
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

instance ToJson.ToJson Doc where
  toJson doc = case doc of
    Empty -> Json.tagged "Empty" Json.null
    Append a b -> Json.tagged "Append" $ Json.arrayOf ToJson.toJson [a, b]
    String t -> Json.tagged "String" $ Json.text t
    Paragraph d -> Json.tagged "Paragraph" $ ToJson.toJson d
    Identifier i -> Json.tagged "Identifier" $ ToJson.toJson i
    Module ml -> Json.tagged "Module" $ ToJson.toJson ml
    Emphasis d -> Json.tagged "Emphasis" $ ToJson.toJson d
    Monospaced d -> Json.tagged "Monospaced" $ ToJson.toJson d
    Bold d -> Json.tagged "Bold" $ ToJson.toJson d
    UnorderedList ds -> Json.tagged "UnorderedList" $ ToJson.toJson ds
    OrderedList items -> Json.tagged "OrderedList" $ ToJson.toJson items
    DefList defs -> Json.tagged "DefList" $ ToJson.toJson defs
    CodeBlock d -> Json.tagged "CodeBlock" $ ToJson.toJson d
    Hyperlink h -> Json.tagged "Hyperlink" $ ToJson.toJson h
    Pic p -> Json.tagged "Pic" $ ToJson.toJson p
    MathInline t -> Json.tagged "MathInline" $ Json.text t
    MathDisplay t -> Json.tagged "MathDisplay" $ Json.text t
    AName t -> Json.tagged "AName" $ Json.text t
    Property t -> Json.tagged "Property" $ Json.text t
    Examples es -> Json.tagged "Examples" $ ToJson.toJson es
    Header h -> Json.tagged "Header" $ ToJson.toJson h
    Table t -> Json.tagged "Table" $ ToJson.toJson t

-- | 'Doc' is recursive, so its schema uses 'define' to register a
-- named definition in @$defs@ and return a @$ref@. Sub-type schemas
-- are obtained via 'toSchema', which registers each as a named
-- definition in @$defs@ and returns a @$ref@ pointer.
instance Schema.ToSchema Doc where
  toSchema _ = Schema.define "doc" docSchema

docSchema :: Schema.SchemaM Schema.Schema
docSchema = do
  Schema.MkSchema self <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Doc)
  Schema.MkSchema identifierS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Identifier.Identifier)
  Schema.MkSchema modLinkS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy (ModLink.ModLink Doc))
  Schema.MkSchema hyperlinkS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy (Hyperlink.Hyperlink Doc))
  Schema.MkSchema numberedItemS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy (NumberedItem.NumberedItem Doc))
  Schema.MkSchema definitionS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy (Definition.Definition Doc))
  Schema.MkSchema pictureS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Picture.Picture)
  Schema.MkSchema exampleS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy Example.Example)
  Schema.MkSchema headerS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy (Header.Header Doc))
  Schema.MkSchema tableS <- Schema.toSchema (Proxy.Proxy :: Proxy.Proxy (Table.Table Doc))
  let str = Json.object [("type", Json.string "string")]
      nullSchema = Json.object [("type", Json.string "null")]
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
  pure . Schema.MkSchema $
    Json.object
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
              tagged "Identifier" identifierS,
              tagged "Module" modLinkS,
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
                    ("items", numberedItemS)
                  ],
              tagged "DefList" $
                Json.object
                  [ ("type", Json.string "array"),
                    ("items", definitionS)
                  ],
              tagged "CodeBlock" self,
              tagged "Hyperlink" hyperlinkS,
              tagged "Pic" pictureS,
              tagged "MathInline" str,
              tagged "MathDisplay" str,
              tagged "AName" str,
              tagged "Property" str,
              tagged "Examples" $
                Json.object
                  [ ("type", Json.string "array"),
                    ("items", exampleS)
                  ],
              tagged "Header" headerS,
              tagged "Table" tableS
            ]
        )
      ]
