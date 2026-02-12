{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Render Scrod core types as JSON values via the 'ToJson' class.
--
-- Simple newtype wrappers use @deriving via@ the underlying type.
-- Record types, enum types, and tagged sum types all use
-- @deriving via 'Generics.Generically'@ to get instances derived
-- generically. Other types have hand-written instances. Import this
-- module to bring all instances into scope.
module Scrod.Convert.ToJson where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
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
import qualified Scrod.Json.ToJson as ToJson
import qualified Scrod.Json.Value as Json

-- Simple newtype wrappers use @deriving via@ to get their instances
-- from the underlying type.

deriving via Text.Text instance ToJson.ToJson Category.Category

deriving via Natural.Natural instance ToJson.ToJson Column.Column

deriving via Text.Text instance ToJson.ToJson Extension.Extension

deriving via Text.Text instance ToJson.ToJson ItemName.ItemName

deriving via Natural.Natural instance ToJson.ToJson ItemKey.ItemKey

deriving via Text.Text instance ToJson.ToJson Language.Language

deriving via Natural.Natural instance ToJson.ToJson Line.Line

deriving via Text.Text instance ToJson.ToJson ModuleName.ModuleName

deriving via Text.Text instance ToJson.ToJson PackageName.PackageName

deriving via Header.Header Doc.Doc instance ToJson.ToJson Section.Section

deriving via NonEmpty.NonEmpty Natural.Natural instance ToJson.ToJson Version.Version

-- Record types, enum types, and tagged sum types use
-- @Generics.Generically@ to derive their instances generically.

deriving via Generics.Generically Example.Example instance ToJson.ToJson Example.Example

deriving via Generics.Generically ExportIdentifier.ExportIdentifier instance ToJson.ToJson ExportIdentifier.ExportIdentifier

deriving via Generics.Generically ExportName.ExportName instance ToJson.ToJson ExportName.ExportName

deriving via Generics.Generically (Header.Header doc) instance (ToJson.ToJson doc) => ToJson.ToJson (Header.Header doc)

deriving via Generics.Generically (Hyperlink.Hyperlink doc) instance (ToJson.ToJson doc) => ToJson.ToJson (Hyperlink.Hyperlink doc)

deriving via Generics.Generically Identifier.Identifier instance ToJson.ToJson Identifier.Identifier

deriving via Generics.Generically Import.Import instance ToJson.ToJson Import.Import

deriving via Generics.Generically Item.Item instance ToJson.ToJson Item.Item

deriving via Generics.Generically (Located.Located a) instance (ToJson.ToJson a) => ToJson.ToJson (Located.Located a)

deriving via Generics.Generically Location.Location instance ToJson.ToJson Location.Location

deriving via Generics.Generically (ModLink.ModLink doc) instance (ToJson.ToJson doc) => ToJson.ToJson (ModLink.ModLink doc)

deriving via Generics.Generically Picture.Picture instance ToJson.ToJson Picture.Picture

deriving via Generics.Generically Since.Since instance ToJson.ToJson Since.Since

deriving via Generics.Generically Subordinates.Subordinates instance ToJson.ToJson Subordinates.Subordinates

deriving via Generics.Generically (Table.Table doc) instance (ToJson.ToJson doc) => ToJson.ToJson (Table.Table doc)

deriving via Generics.Generically (TableCell.Cell doc) instance (ToJson.ToJson doc) => ToJson.ToJson (TableCell.Cell doc)

deriving via Generics.Generically Warning.Warning instance ToJson.ToJson Warning.Warning

deriving via Generics.Generically ExportNameKind.ExportNameKind instance ToJson.ToJson ExportNameKind.ExportNameKind

deriving via Generics.Generically ItemKind.ItemKind instance ToJson.ToJson ItemKind.ItemKind

deriving via Generics.Generically Namespace.Namespace instance ToJson.ToJson Namespace.Namespace

deriving via Generics.Generically Export.Export instance ToJson.ToJson Export.Export

-- Hand-written instances for types that require special encoding.

instance ToJson.ToJson Module.Module where
  toJson m =
    Json.object
      . filter (\(_, v) -> v /= Json.null)
      $ [ ("version", ToJson.toJson $ Module.version m),
          ("language", ToJson.toJson $ Module.language m),
          ("extensions", extensionsToJson $ Module.extensions m),
          ("documentation", ToJson.toJson $ Module.documentation m),
          ("since", ToJson.toJson $ Module.since m),
          ("signature", ToJson.toJson $ Module.signature m),
          ("name", ToJson.toJson $ Module.name m),
          ("warning", ToJson.toJson $ Module.warning m),
          ("exports", ToJson.toJson $ Module.exports m),
          ("imports", ToJson.toJson $ Module.imports m),
          ("items", ToJson.toJson $ Module.items m)
        ]

extensionsToJson :: Map.Map Extension.Extension Bool -> Json.Value
extensionsToJson =
  Json.object
    . fmap (\(k, v) -> (Text.unpack $ Extension.unwrap k, ToJson.toJson v))
    . Map.toList

instance ToJson.ToJson Doc.Doc where
  toJson doc = case doc of
    Doc.Empty -> Json.tagged "Empty" Json.null
    Doc.Append a b -> Json.tagged "Append" $ Json.arrayOf ToJson.toJson [a, b]
    Doc.String t -> Json.tagged "String" $ Json.text t
    Doc.Paragraph d -> Json.tagged "Paragraph" $ ToJson.toJson d
    Doc.Identifier i -> Json.tagged "Identifier" $ ToJson.toJson i
    Doc.Module ml -> Json.tagged "Module" $ ToJson.toJson ml
    Doc.Emphasis d -> Json.tagged "Emphasis" $ ToJson.toJson d
    Doc.Monospaced d -> Json.tagged "Monospaced" $ ToJson.toJson d
    Doc.Bold d -> Json.tagged "Bold" $ ToJson.toJson d
    Doc.UnorderedList ds -> Json.tagged "UnorderedList" $ ToJson.toJson ds
    Doc.OrderedList items ->
      Json.tagged "OrderedList" $
        Json.arrayOf (\(n, d) -> Json.array [Json.integral n, ToJson.toJson d]) items
    Doc.DefList defs ->
      Json.tagged "DefList" $
        Json.arrayOf (\(t, d) -> Json.arrayOf ToJson.toJson [t, d]) defs
    Doc.CodeBlock d -> Json.tagged "CodeBlock" $ ToJson.toJson d
    Doc.Hyperlink h -> Json.tagged "Hyperlink" $ ToJson.toJson h
    Doc.Pic p -> Json.tagged "Pic" $ ToJson.toJson p
    Doc.MathInline t -> Json.tagged "MathInline" $ Json.text t
    Doc.MathDisplay t -> Json.tagged "MathDisplay" $ Json.text t
    Doc.AName t -> Json.tagged "AName" $ Json.text t
    Doc.Property t -> Json.tagged "Property" $ Json.text t
    Doc.Examples es -> Json.tagged "Examples" $ ToJson.toJson es
    Doc.Header h -> Json.tagged "Header" $ ToJson.toJson h
    Doc.Table t -> Json.tagged "Table" $ ToJson.toJson t

instance ToJson.ToJson Level.Level where
  toJson l = Json.integer $ case l of
    Level.One -> 1
    Level.Two -> 2
    Level.Three -> 3
    Level.Four -> 4
    Level.Five -> 5
    Level.Six -> 6
