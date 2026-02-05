module LegendaryChainsaw.Convert.ToJson where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified LegendaryChainsaw.Core.Category as Category
import qualified LegendaryChainsaw.Core.Column as Column
import qualified LegendaryChainsaw.Core.Doc as Doc
import qualified LegendaryChainsaw.Core.Example as Example
import qualified LegendaryChainsaw.Core.Export as Export
import qualified LegendaryChainsaw.Core.ExportIdentifier as ExportIdentifier
import qualified LegendaryChainsaw.Core.ExportName as ExportName
import qualified LegendaryChainsaw.Core.ExportNameKind as ExportNameKind
import qualified LegendaryChainsaw.Core.Extension as Extension
import qualified LegendaryChainsaw.Core.Header as Header
import qualified LegendaryChainsaw.Core.Hyperlink as Hyperlink
import qualified LegendaryChainsaw.Core.Identifier as Identifier
import qualified LegendaryChainsaw.Core.Item as Item
import qualified LegendaryChainsaw.Core.ItemKey as ItemKey
import qualified LegendaryChainsaw.Core.ItemKind as ItemKind
import qualified LegendaryChainsaw.Core.ItemName as ItemName
import qualified LegendaryChainsaw.Core.Language as Language
import qualified LegendaryChainsaw.Core.Level as Level
import qualified LegendaryChainsaw.Core.Line as Line
import qualified LegendaryChainsaw.Core.Located as Located
import qualified LegendaryChainsaw.Core.Location as Location
import qualified LegendaryChainsaw.Core.ModLink as ModLink
import qualified LegendaryChainsaw.Core.Module as Module
import qualified LegendaryChainsaw.Core.ModuleName as ModuleName
import qualified LegendaryChainsaw.Core.Namespace as Namespace
import qualified LegendaryChainsaw.Core.PackageName as PackageName
import qualified LegendaryChainsaw.Core.Picture as Picture
import qualified LegendaryChainsaw.Core.Section as Section
import qualified LegendaryChainsaw.Core.Since as Since
import qualified LegendaryChainsaw.Core.Subordinates as Subordinates
import qualified LegendaryChainsaw.Core.Table as Table
import qualified LegendaryChainsaw.Core.TableCell as TableCell
import qualified LegendaryChainsaw.Core.Version as Version
import qualified LegendaryChainsaw.Core.Warning as Warning
import qualified LegendaryChainsaw.Json.Value as Json

-- | Convert a Module to a JSON Json.
toJson :: Module.Module -> Json.Value
toJson = moduleToJson

-- | Convert Module to JSON (record)
moduleToJson :: Module.Module -> Json.Value
moduleToJson m =
  Json.object
    [ ("version", versionToJson $ Module.version m),
      ("language", Json.optional languageToJson $ Module.language m),
      ("extensions", extensionsToJson $ Module.extensions m),
      ("documentation", docToJson $ Module.documentation m),
      ("since", Json.optional sinceToJson $ Module.since m),
      ("name", Json.optional (locatedToJson moduleNameToJson) $ Module.name m),
      ("warning", Json.optional warningToJson $ Module.warning m),
      ("exports", Json.optional (Json.arrayOf exportToJson) $ Module.exports m),
      ("items", Json.arrayOf (locatedToJson itemToJson) $ Module.items m)
    ]

-- | Convert Version to JSON (newtype with NonEmpty Natural)
versionToJson :: Version.Version -> Json.Value
versionToJson = Json.arrayOf Json.integral . NonEmpty.toList . Version.unwrap

-- | Convert Language to JSON (newtype with Text)
languageToJson :: Language.Language -> Json.Value
languageToJson = Json.text . Language.unwrap

-- | Convert Extension map to JSON
extensionsToJson :: Map.Map Extension.Extension Bool -> Json.Value
extensionsToJson = Json.object . fmap (\(k, v) -> (Text.unpack $ Extension.unwrap k, Json.boolean v)) . Map.toList

-- | Convert Doc to JSON (sum type)
docToJson :: Doc.Doc -> Json.Value
docToJson doc = case doc of
  Doc.Empty -> Json.tagged "Empty" Json.null
  Doc.Append a b -> Json.tagged "Append" $ Json.arrayOf docToJson [a, b]
  Doc.String t -> Json.tagged "String" $ Json.text t
  Doc.Paragraph d -> Json.tagged "Paragraph" $ docToJson d
  Doc.Identifier i -> Json.tagged "Identifier" $ identifierToJson i
  Doc.Module ml -> Json.tagged "Module" $ modLinkToJson docToJson ml
  Doc.Emphasis d -> Json.tagged "Emphasis" $ docToJson d
  Doc.Monospaced d -> Json.tagged "Monospaced" $ docToJson d
  Doc.Bold d -> Json.tagged "Bold" $ docToJson d
  Doc.UnorderedList ds -> Json.tagged "UnorderedList" $ Json.arrayOf docToJson ds
  Doc.OrderedList items -> Json.tagged "OrderedList" $ Json.arrayOf (\(n, d) -> Json.array [Json.integral n, docToJson d]) items
  Doc.DefList defs -> Json.tagged "DefList" $ Json.arrayOf (\(t, d) -> Json.arrayOf docToJson [t, d]) defs
  Doc.CodeBlock d -> Json.tagged "CodeBlock" $ docToJson d
  Doc.Hyperlink h -> Json.tagged "Hyperlink" $ hyperlinkToJson docToJson h
  Doc.Pic p -> Json.tagged "Pic" $ pictureToJson p
  Doc.MathInline t -> Json.tagged "MathInline" $ Json.text t
  Doc.MathDisplay t -> Json.tagged "MathDisplay" $ Json.text t
  Doc.AName t -> Json.tagged "AName" $ Json.text t
  Doc.Property t -> Json.tagged "Property" $ Json.text t
  Doc.Examples es -> Json.tagged "Examples" $ Json.arrayOf exampleToJson es
  Doc.Header h -> Json.tagged "Header" $ headerToJson docToJson h
  Doc.Table t -> Json.tagged "Table" $ tableToJson docToJson t

-- | Convert Since to JSON (record)
sinceToJson :: Since.Since -> Json.Value
sinceToJson s =
  Json.object
    [ ("package", Json.optional packageNameToJson $ Since.package s),
      ("version", versionToJson $ Since.version s)
    ]

-- | Convert Located to JSON (record with type parameter)
locatedToJson :: (a -> Json.Value) -> Located.Located a -> Json.Value
locatedToJson f l =
  Json.object
    [ ("location", locationToJson $ Located.location l),
      ("value", f $ Located.value l)
    ]

-- | Convert ModuleName to JSON (newtype with Text)
moduleNameToJson :: ModuleName.ModuleName -> Json.Value
moduleNameToJson = Json.text . ModuleName.unwrap

-- | Convert Warning to JSON (record)
warningToJson :: Warning.Warning -> Json.Value
warningToJson w =
  Json.object
    [ ("category", categoryToJson $ Warning.category w),
      ("value", Json.text $ Warning.value w)
    ]

-- | Convert Export to JSON (sum type)
exportToJson :: Export.Export -> Json.Value
exportToJson e = case e of
  Export.Identifier ei -> Json.tagged "Identifier" $ exportIdentifierToJson ei
  Export.Group s -> Json.tagged "Group" $ sectionToJson s
  Export.Doc d -> Json.tagged "Doc" $ docToJson d
  Export.DocNamed t -> Json.tagged "DocNamed" $ Json.text t

-- | Convert Item to JSON (record)
itemToJson :: Item.Item -> Json.Value
itemToJson i =
  Json.object
    [ ("key", itemKeyToJson $ Item.key i),
      ("kind", itemKindToJson $ Item.kind i),
      ("parentKey", Json.optional itemKeyToJson $ Item.parentKey i),
      ("name", Json.optional itemNameToJson $ Item.name i),
      ("documentation", docToJson $ Item.documentation i),
      ("signature", Json.optional Json.text $ Item.signature i)
    ]

-- | Convert Location to JSON (record)
locationToJson :: Location.Location -> Json.Value
locationToJson loc =
  Json.object
    [ ("line", lineToJson $ Location.line loc),
      ("column", columnToJson $ Location.column loc)
    ]

-- | Convert PackageName to JSON (newtype with Text)
packageNameToJson :: PackageName.PackageName -> Json.Value
packageNameToJson = Json.text . PackageName.unwrap

-- | Convert Line to JSON (newtype with Natural)
lineToJson :: Line.Line -> Json.Value
lineToJson = Json.integral . Line.unwrap

-- | Convert Column to JSON (newtype with Natural)
columnToJson :: Column.Column -> Json.Value
columnToJson = Json.integral . Column.unwrap

-- | Convert Category to JSON (newtype with Text)
categoryToJson :: Category.Category -> Json.Value
categoryToJson = Json.text . Category.unwrap

-- | Convert Section to JSON (newtype with Header Doc.Doc)
sectionToJson :: Section.Section -> Json.Value
sectionToJson = headerToJson docToJson . Section.header

-- | Convert ExportIdentifier to JSON (record)
exportIdentifierToJson :: ExportIdentifier.ExportIdentifier -> Json.Value
exportIdentifierToJson ei =
  Json.object
    [ ("name", exportNameToJson $ ExportIdentifier.name ei),
      ("subordinates", Json.optional subordinatesToJson $ ExportIdentifier.subordinates ei),
      ("warning", Json.optional warningToJson $ ExportIdentifier.warning ei),
      ("doc", Json.optional docToJson $ ExportIdentifier.doc ei)
    ]

-- | Convert ItemKey to JSON (newtype with Natural)
itemKeyToJson :: ItemKey.ItemKey -> Json.Value
itemKeyToJson = Json.integral . ItemKey.unwrap

-- | Convert ItemKind to JSON (sum type with no payload)
itemKindToJson :: ItemKind.ItemKind -> Json.Value
itemKindToJson k = Json.string $ case k of
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
  ItemKind.StandaloneDeriving -> "StandaloneDeriving"
  ItemKind.StandaloneKindSig -> "StandaloneKindSig"
  ItemKind.TypeData -> "TypeData"
  ItemKind.TypeFamilyInstance -> "TypeFamilyInstance"
  ItemKind.TypeSynonym -> "TypeSynonym"

-- | Convert ItemName to JSON (newtype with Text)
itemNameToJson :: ItemName.ItemName -> Json.Value
itemNameToJson = Json.text . ItemName.unwrap

-- | Convert ExportName to JSON (record)
exportNameToJson :: ExportName.ExportName -> Json.Value
exportNameToJson en =
  Json.object
    [ ("kind", Json.optional exportNameKindToJson $ ExportName.kind en),
      ("name", Json.text $ ExportName.name en)
    ]

-- | Convert Subordinates to JSON (record)
subordinatesToJson :: Subordinates.Subordinates -> Json.Value
subordinatesToJson s =
  Json.object
    [ ("wildcard", Json.boolean $ Subordinates.wildcard s),
      ("explicit", Json.arrayOf exportNameToJson $ Subordinates.explicit s)
    ]

-- | Convert ExportNameKind to JSON (sum type with no payload)
exportNameKindToJson :: ExportNameKind.ExportNameKind -> Json.Value
exportNameKindToJson k = Json.string $ case k of
  ExportNameKind.Module -> "Module"
  ExportNameKind.Pattern -> "Pattern"
  ExportNameKind.Type -> "Type"

-- | Convert Namespace to JSON (sum type with no payload)
namespaceToJson :: Namespace.Namespace -> Json.Value
namespaceToJson ns = Json.string $ case ns of
  Namespace.Type -> "Type"
  Namespace.Value -> "Value"

-- | Convert Example to JSON (record)
exampleToJson :: Example.Example -> Json.Value
exampleToJson ex =
  Json.object
    [ ("expression", Json.text $ Example.expression ex),
      ("result", Json.arrayOf Json.text $ Example.result ex)
    ]

-- | Convert Header to JSON (record with type parameter)
headerToJson :: (doc -> Json.Value) -> Header.Header doc -> Json.Value
headerToJson f h =
  Json.object
    [ ("level", levelToJson $ Header.level h),
      ("title", f $ Header.title h)
    ]

-- | Convert Hyperlink to JSON (record with type parameter)
hyperlinkToJson :: (doc -> Json.Value) -> Hyperlink.Hyperlink doc -> Json.Value
hyperlinkToJson f h =
  Json.object
    [ ("url", Json.text $ Hyperlink.url h),
      ("label", Json.optional f $ Hyperlink.label h)
    ]

-- | Convert Identifier to JSON (record)
identifierToJson :: Identifier.Identifier -> Json.Value
identifierToJson i =
  Json.object
    [ ("namespace", Json.optional namespaceToJson $ Identifier.namespace i),
      ("value", Json.text $ Identifier.value i)
    ]

-- | Convert ModLink to JSON (record with type parameter)
modLinkToJson :: (doc -> Json.Value) -> ModLink.ModLink doc -> Json.Value
modLinkToJson f ml =
  Json.object
    [ ("name", moduleNameToJson $ ModLink.name ml),
      ("label", Json.optional f $ ModLink.label ml)
    ]

-- | Convert Picture to JSON (record)
pictureToJson :: Picture.Picture -> Json.Value
pictureToJson p =
  Json.object
    [ ("uri", Json.text $ Picture.uri p),
      ("title", Json.optional Json.text $ Picture.title p)
    ]

-- | Convert Table to JSON (record with type parameter)
tableToJson :: (doc -> Json.Value) -> Table.Table doc -> Json.Value
tableToJson f t =
  Json.object
    [ ("headerRows", Json.arrayOf (Json.arrayOf $ cellToJson f) $ Table.headerRows t),
      ("bodyRows", Json.arrayOf (Json.arrayOf $ cellToJson f) $ Table.bodyRows t)
    ]

-- | Convert Cell to JSON (record with type parameter)
cellToJson :: (doc -> Json.Value) -> TableCell.Cell doc -> Json.Value
cellToJson f c =
  Json.object
    [ ("colspan", Json.integral $ TableCell.colspan c),
      ("rowspan", Json.integral $ TableCell.rowspan c),
      ("contents", f $ TableCell.contents c)
    ]

-- | Convert Level to JSON (sum type with no payload)
levelToJson :: Level.Level -> Json.Value
levelToJson l = Json.integer $ case l of
  Level.One -> 1
  Level.Two -> 2
  Level.Three -> 3
  Level.Four -> 4
  Level.Five -> 5
  Level.Six -> 6
