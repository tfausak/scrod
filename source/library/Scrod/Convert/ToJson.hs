{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Render Scrod core types as JSON values via the 'ToJson' class.
--
-- Simple newtype wrappers use @deriving via@ to get instances for free.
-- More complex types have hand-written instances. Import this module to
-- bring all instances into scope.
module Scrod.Convert.ToJson
  ( module Scrod.Json.ToJson,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
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
import Scrod.Json.ToJson (ToJson (toJson))
import qualified Scrod.Json.Value as Json

-- Simple newtype wrappers use @deriving via@ to get their instances
-- from the underlying type.

deriving via Text.Text instance ToJson Category.Category

deriving via Natural.Natural instance ToJson Column.Column

deriving via Text.Text instance ToJson Extension.Extension

deriving via Text.Text instance ToJson ItemName.ItemName

deriving via Natural.Natural instance ToJson ItemKey.ItemKey

deriving via Text.Text instance ToJson Language.Language

deriving via Natural.Natural instance ToJson Line.Line

deriving via Text.Text instance ToJson ModuleName.ModuleName

deriving via Text.Text instance ToJson PackageName.PackageName

deriving via Header.Header Doc.Doc instance ToJson Section.Section

deriving via NonEmpty.NonEmpty Natural.Natural instance ToJson Version.Version

instance ToJson Module.Module where
  toJson m =
    Json.object
      [ ("version", toJson $ Module.version m),
        ("language", toJson $ Module.language m),
        ("extensions", extensionsToJson $ Module.extensions m),
        ("documentation", toJson $ Module.documentation m),
        ("since", toJson $ Module.since m),
        ("signature", toJson $ Module.signature m),
        ("name", toJson $ Module.name m),
        ("warning", toJson $ Module.warning m),
        ("exports", toJson $ Module.exports m),
        ("imports", toJson $ Module.imports m),
        ("items", toJson $ Module.items m)
      ]

extensionsToJson :: Map.Map Extension.Extension Bool -> Json.Value
extensionsToJson =
  Json.object
    . fmap (\(k, v) -> (Text.unpack $ Extension.unwrap k, toJson v))
    . Map.toList

instance ToJson Doc.Doc where
  toJson doc = case doc of
    Doc.Empty -> Json.tagged "Empty" Json.null
    Doc.Append a b -> Json.tagged "Append" $ Json.arrayOf toJson [a, b]
    Doc.String t -> Json.tagged "String" $ Json.text t
    Doc.Paragraph d -> Json.tagged "Paragraph" $ toJson d
    Doc.Identifier i -> Json.tagged "Identifier" $ toJson i
    Doc.Module ml -> Json.tagged "Module" $ toJson ml
    Doc.Emphasis d -> Json.tagged "Emphasis" $ toJson d
    Doc.Monospaced d -> Json.tagged "Monospaced" $ toJson d
    Doc.Bold d -> Json.tagged "Bold" $ toJson d
    Doc.UnorderedList ds -> Json.tagged "UnorderedList" $ toJson ds
    Doc.OrderedList items ->
      Json.tagged "OrderedList" $
        Json.arrayOf (\(n, d) -> Json.array [Json.integral n, toJson d]) items
    Doc.DefList defs ->
      Json.tagged "DefList" $
        Json.arrayOf (\(t, d) -> Json.arrayOf toJson [t, d]) defs
    Doc.CodeBlock d -> Json.tagged "CodeBlock" $ toJson d
    Doc.Hyperlink h -> Json.tagged "Hyperlink" $ toJson h
    Doc.Pic p -> Json.tagged "Pic" $ toJson p
    Doc.MathInline t -> Json.tagged "MathInline" $ Json.text t
    Doc.MathDisplay t -> Json.tagged "MathDisplay" $ Json.text t
    Doc.AName t -> Json.tagged "AName" $ Json.text t
    Doc.Property t -> Json.tagged "Property" $ Json.text t
    Doc.Examples es -> Json.tagged "Examples" $ toJson es
    Doc.Header h -> Json.tagged "Header" $ toJson h
    Doc.Table t -> Json.tagged "Table" $ toJson t

instance ToJson Import.Import where
  toJson i =
    Json.object
      [ ("name", toJson $ Import.name i),
        ("package", toJson $ Import.package i),
        ("alias", toJson $ Import.alias i)
      ]

instance ToJson Since.Since where
  toJson s =
    Json.object
      [ ("package", toJson $ Since.package s),
        ("version", toJson $ Since.version s)
      ]

instance (ToJson a) => ToJson (Located.Located a) where
  toJson l =
    Json.object
      [ ("location", toJson $ Located.location l),
        ("value", toJson $ Located.value l)
      ]

instance ToJson Warning.Warning where
  toJson w =
    Json.object
      [ ("category", toJson $ Warning.category w),
        ("value", Json.text $ Warning.value w)
      ]

instance ToJson Export.Export where
  toJson e = case e of
    Export.Identifier ei -> Json.tagged "Identifier" $ toJson ei
    Export.Group s -> Json.tagged "Group" $ toJson s
    Export.Doc d -> Json.tagged "Doc" $ toJson d
    Export.DocNamed t -> Json.tagged "DocNamed" $ Json.text t

instance ToJson Item.Item where
  toJson i =
    Json.object
      [ ("key", toJson $ Item.key i),
        ("kind", toJson $ Item.kind i),
        ("parentKey", toJson $ Item.parentKey i),
        ("name", toJson $ Item.name i),
        ("documentation", toJson $ Item.documentation i),
        ("signature", toJson $ Item.signature i)
      ]

instance ToJson Location.Location where
  toJson loc =
    Json.object
      [ ("line", toJson $ Location.line loc),
        ("column", toJson $ Location.column loc)
      ]

instance ToJson ExportIdentifier.ExportIdentifier where
  toJson ei =
    Json.object
      [ ("name", toJson $ ExportIdentifier.name ei),
        ("subordinates", toJson $ ExportIdentifier.subordinates ei),
        ("warning", toJson $ ExportIdentifier.warning ei),
        ("doc", toJson $ ExportIdentifier.doc ei)
      ]

instance ToJson ItemKind.ItemKind where
  toJson k = Json.string $ case k of
    ItemKind.Annotation -> "Annotation"
    ItemKind.Class -> "Class"
    ItemKind.ClassInstance -> "ClassInstance"
    ItemKind.ClassMethod -> "ClassMethod"
    ItemKind.ClosedTypeFamily -> "ClosedTypeFamily"
    ItemKind.DataConstructor -> "DataConstructor"
    ItemKind.DataFamily -> "DataFamily"
    ItemKind.DataFamilyInstance -> "DataFamilyInstance"
    ItemKind.DataType -> "DataType"
    ItemKind.Default -> "Default"
    ItemKind.DerivedInstance -> "DerivedInstance"
    ItemKind.FixitySignature -> "FixitySignature"
    ItemKind.ForeignExport -> "ForeignExport"
    ItemKind.ForeignImport -> "ForeignImport"
    ItemKind.Function -> "Function"
    ItemKind.GADTConstructor -> "GADTConstructor"
    ItemKind.InlineSignature -> "InlineSignature"
    ItemKind.Newtype -> "Newtype"
    ItemKind.OpenTypeFamily -> "OpenTypeFamily"
    ItemKind.PatternBinding -> "PatternBinding"
    ItemKind.PatternSynonym -> "PatternSynonym"
    ItemKind.RecordField -> "RecordField"
    ItemKind.Rule -> "Rule"
    ItemKind.SpecialiseSignature -> "SpecialiseSignature"
    ItemKind.Splice -> "Splice"
    ItemKind.StandaloneDeriving -> "StandaloneDeriving"
    ItemKind.StandaloneKindSig -> "StandaloneKindSig"
    ItemKind.TypeData -> "TypeData"
    ItemKind.TypeFamilyInstance -> "TypeFamilyInstance"
    ItemKind.TypeSynonym -> "TypeSynonym"

instance ToJson ExportName.ExportName where
  toJson en =
    Json.object
      [ ("kind", toJson $ ExportName.kind en),
        ("name", Json.text $ ExportName.name en)
      ]

instance ToJson Subordinates.Subordinates where
  toJson s =
    Json.object
      [ ("wildcard", toJson $ Subordinates.wildcard s),
        ("explicit", toJson $ Subordinates.explicit s)
      ]

instance ToJson ExportNameKind.ExportNameKind where
  toJson k = Json.string $ case k of
    ExportNameKind.Module -> "Module"
    ExportNameKind.Pattern -> "Pattern"
    ExportNameKind.Type -> "Type"

instance ToJson Namespace.Namespace where
  toJson ns = Json.string $ case ns of
    Namespace.Type -> "Type"
    Namespace.Value -> "Value"

instance ToJson Example.Example where
  toJson ex =
    Json.object
      [ ("expression", Json.text $ Example.expression ex),
        ("result", Json.arrayOf Json.text $ Example.result ex)
      ]

instance (ToJson doc) => ToJson (Header.Header doc) where
  toJson h =
    Json.object
      [ ("level", toJson $ Header.level h),
        ("title", toJson $ Header.title h)
      ]

instance (ToJson doc) => ToJson (Hyperlink.Hyperlink doc) where
  toJson h =
    Json.object
      [ ("url", Json.text $ Hyperlink.url h),
        ("label", toJson $ Hyperlink.label h)
      ]

instance ToJson Identifier.Identifier where
  toJson i =
    Json.object
      [ ("namespace", toJson $ Identifier.namespace i),
        ("value", Json.text $ Identifier.value i)
      ]

instance (ToJson doc) => ToJson (ModLink.ModLink doc) where
  toJson ml =
    Json.object
      [ ("name", toJson $ ModLink.name ml),
        ("label", toJson $ ModLink.label ml)
      ]

instance ToJson Picture.Picture where
  toJson p =
    Json.object
      [ ("uri", Json.text $ Picture.uri p),
        ("title", toJson $ Picture.title p)
      ]

instance (ToJson doc) => ToJson (Table.Table doc) where
  toJson t =
    Json.object
      [ ("headerRows", toJson $ Table.headerRows t),
        ("bodyRows", toJson $ Table.bodyRows t)
      ]

instance (ToJson doc) => ToJson (TableCell.Cell doc) where
  toJson c =
    Json.object
      [ ("colspan", Json.integral $ TableCell.colspan c),
        ("rowspan", Json.integral $ TableCell.rowspan c),
        ("contents", toJson $ TableCell.contents c)
      ]

instance ToJson Level.Level where
  toJson l = Json.integer $ case l of
    Level.One -> 1
    Level.Two -> 2
    Level.Three -> 3
    Level.Four -> 4
    Level.Five -> 5
    Level.Six -> 6
