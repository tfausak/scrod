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
import qualified LegendaryChainsaw.Json.Value as Value

-- | Convert a Module to a JSON Value.
toJson :: Module.Module -> Value.Value
toJson = moduleToJson

-- | Convert Module to JSON (record)
moduleToJson :: Module.Module -> Value.Value
moduleToJson m =
  Value.object
    [ ("version", versionToJson $ Module.version m),
      ("language", Value.optional languageToJson $ Module.language m),
      ("extensions", extensionsToJson $ Module.extensions m),
      ("documentation", docToJson $ Module.documentation m),
      ("since", Value.optional sinceToJson $ Module.since m),
      ("name", Value.optional (locatedToJson moduleNameToJson) $ Module.name m),
      ("warning", Value.optional warningToJson $ Module.warning m),
      ("exports", Value.optional (Value.arrayOf exportToJson) $ Module.exports m),
      ("items", Value.arrayOf (locatedToJson itemToJson) $ Module.items m)
    ]

-- | Convert Version to JSON (newtype with NonEmpty Natural)
versionToJson :: Version.Version -> Value.Value
versionToJson = Value.arrayOf Value.integral . NonEmpty.toList . Version.value

-- | Convert Language to JSON (newtype with Text)
languageToJson :: Language.Language -> Value.Value
languageToJson = Value.text . Language.value

-- | Convert Extension map to JSON
extensionsToJson :: Map.Map Extension.Extension Bool -> Value.Value
extensionsToJson = Value.object . fmap (\(k, v) -> (Text.unpack $ Extension.value k, Value.boolean v)) . Map.toList

-- | Convert Doc to JSON (sum type)
docToJson :: Doc.Doc -> Value.Value
docToJson doc = case doc of
  Doc.Empty -> Value.tagged "Empty" Value.null
  Doc.Append a b -> Value.tagged "Append" $ Value.arrayOf docToJson [a, b]
  Doc.String t -> Value.tagged "String" $ Value.text t
  Doc.Paragraph d -> Value.tagged "Paragraph" $ docToJson d
  Doc.Identifier i -> Value.tagged "Identifier" $ identifierToJson i
  Doc.Module ml -> Value.tagged "Module" $ modLinkToJson docToJson ml
  Doc.Emphasis d -> Value.tagged "Emphasis" $ docToJson d
  Doc.Monospaced d -> Value.tagged "Monospaced" $ docToJson d
  Doc.Bold d -> Value.tagged "Bold" $ docToJson d
  Doc.UnorderedList ds -> Value.tagged "UnorderedList" $ Value.arrayOf docToJson ds
  Doc.OrderedList items -> Value.tagged "OrderedList" $ Value.arrayOf (\(n, d) -> Value.array [Value.integral n, docToJson d]) items
  Doc.DefList defs -> Value.tagged "DefList" $ Value.arrayOf (\(t, d) -> Value.arrayOf docToJson [t, d]) defs
  Doc.CodeBlock d -> Value.tagged "CodeBlock" $ docToJson d
  Doc.Hyperlink h -> Value.tagged "Hyperlink" $ hyperlinkToJson docToJson h
  Doc.Pic p -> Value.tagged "Pic" $ pictureToJson p
  Doc.MathInline t -> Value.tagged "MathInline" $ Value.text t
  Doc.MathDisplay t -> Value.tagged "MathDisplay" $ Value.text t
  Doc.AName t -> Value.tagged "AName" $ Value.text t
  Doc.Property t -> Value.tagged "Property" $ Value.text t
  Doc.Examples es -> Value.tagged "Examples" $ Value.arrayOf exampleToJson es
  Doc.Header h -> Value.tagged "Header" $ headerToJson docToJson h
  Doc.Table t -> Value.tagged "Table" $ tableToJson docToJson t

-- | Convert Since to JSON (record)
sinceToJson :: Since.Since -> Value.Value
sinceToJson s =
  Value.object
    [ ("package", Value.optional packageNameToJson $ Since.package s),
      ("version", versionToJson $ Since.version s)
    ]

-- | Convert Located to JSON (record with type parameter)
locatedToJson :: (a -> Value.Value) -> Located.Located a -> Value.Value
locatedToJson f l =
  Value.object
    [ ("location", locationToJson $ Located.location l),
      ("value", f $ Located.value l)
    ]

-- | Convert ModuleName to JSON (newtype with Text)
moduleNameToJson :: ModuleName.ModuleName -> Value.Value
moduleNameToJson = Value.text . ModuleName.value

-- | Convert Warning to JSON (record)
warningToJson :: Warning.Warning -> Value.Value
warningToJson w =
  Value.object
    [ ("category", categoryToJson $ Warning.category w),
      ("value", Value.text $ Warning.value w)
    ]

-- | Convert Export to JSON (sum type)
exportToJson :: Export.Export -> Value.Value
exportToJson e = case e of
  Export.Identifier ei -> Value.tagged "Identifier" $ exportIdentifierToJson ei
  Export.Group s -> Value.tagged "Group" $ sectionToJson s
  Export.Doc d -> Value.tagged "Doc" $ docToJson d
  Export.DocNamed t -> Value.tagged "DocNamed" $ Value.text t

-- | Convert Item to JSON (record)
itemToJson :: Item.Item -> Value.Value
itemToJson i =
  Value.object
    [ ("key", itemKeyToJson $ Item.key i),
      ("kind", itemKindToJson $ Item.kind i),
      ("parentKey", Value.optional itemKeyToJson $ Item.parentKey i),
      ("name", Value.optional itemNameToJson $ Item.name i),
      ("documentation", docToJson $ Item.documentation i),
      ("signature", Value.optional Value.text $ Item.signature i)
    ]

-- | Convert Location to JSON (record)
locationToJson :: Location.Location -> Value.Value
locationToJson loc =
  Value.object
    [ ("line", lineToJson $ Location.line loc),
      ("column", columnToJson $ Location.column loc)
    ]

-- | Convert PackageName to JSON (newtype with Text)
packageNameToJson :: PackageName.PackageName -> Value.Value
packageNameToJson = Value.text . PackageName.value

-- | Convert Line to JSON (newtype with Natural)
lineToJson :: Line.Line -> Value.Value
lineToJson = Value.integral . Line.value

-- | Convert Column to JSON (newtype with Natural)
columnToJson :: Column.Column -> Value.Value
columnToJson = Value.integral . Column.value

-- | Convert Category to JSON (newtype with Text)
categoryToJson :: Category.Category -> Value.Value
categoryToJson = Value.text . Category.value

-- | Convert Section to JSON (newtype with Header Doc.Doc)
sectionToJson :: Section.Section -> Value.Value
sectionToJson = headerToJson docToJson . Section.header

-- | Convert ExportIdentifier to JSON (record)
exportIdentifierToJson :: ExportIdentifier.ExportIdentifier -> Value.Value
exportIdentifierToJson ei =
  Value.object
    [ ("name", exportNameToJson $ ExportIdentifier.name ei),
      ("subordinates", Value.optional subordinatesToJson $ ExportIdentifier.subordinates ei),
      ("warning", Value.optional warningToJson $ ExportIdentifier.warning ei),
      ("doc", Value.optional docToJson $ ExportIdentifier.doc ei)
    ]

-- | Convert ItemKey to JSON (newtype with Natural)
itemKeyToJson :: ItemKey.ItemKey -> Value.Value
itemKeyToJson = Value.integral . ItemKey.value

-- | Convert ItemKind to JSON (sum type with no payload)
itemKindToJson :: ItemKind.ItemKind -> Value.Value
itemKindToJson k = Value.string $ case k of
  ItemKind.Function -> "Function"
  ItemKind.PatternBinding -> "PatternBinding"
  ItemKind.PatternSynonym -> "PatternSynonym"
  ItemKind.DataType -> "DataType"
  ItemKind.Newtype -> "Newtype"
  ItemKind.TypeData -> "TypeData"
  ItemKind.TypeSynonym -> "TypeSynonym"
  ItemKind.DataConstructor -> "DataConstructor"
  ItemKind.GADTConstructor -> "GADTConstructor"
  ItemKind.RecordField -> "RecordField"
  ItemKind.Class -> "Class"
  ItemKind.ClassMethod -> "ClassMethod"
  ItemKind.ClassInstance -> "ClassInstance"
  ItemKind.StandaloneDeriving -> "StandaloneDeriving"
  ItemKind.DerivedInstance -> "DerivedInstance"
  ItemKind.OpenTypeFamily -> "OpenTypeFamily"
  ItemKind.ClosedTypeFamily -> "ClosedTypeFamily"
  ItemKind.DataFamily -> "DataFamily"
  ItemKind.TypeFamilyInstance -> "TypeFamilyInstance"
  ItemKind.DataFamilyInstance -> "DataFamilyInstance"
  ItemKind.ForeignImport -> "ForeignImport"
  ItemKind.ForeignExport -> "ForeignExport"
  ItemKind.FixitySignature -> "FixitySignature"
  ItemKind.InlineSignature -> "InlineSignature"
  ItemKind.SpecialiseSignature -> "SpecialiseSignature"
  ItemKind.StandaloneKindSig -> "StandaloneKindSig"
  ItemKind.Rule -> "Rule"
  ItemKind.Default -> "Default"
  ItemKind.Annotation -> "Annotation"

-- | Convert ItemName to JSON (newtype with Text)
itemNameToJson :: ItemName.ItemName -> Value.Value
itemNameToJson = Value.text . ItemName.value

-- | Convert ExportName to JSON (record)
exportNameToJson :: ExportName.ExportName -> Value.Value
exportNameToJson en =
  Value.object
    [ ("kind", Value.optional exportNameKindToJson $ ExportName.kind en),
      ("name", Value.text $ ExportName.name en)
    ]

-- | Convert Subordinates to JSON (record)
subordinatesToJson :: Subordinates.Subordinates -> Value.Value
subordinatesToJson s =
  Value.object
    [ ("wildcard", Value.boolean $ Subordinates.wildcard s),
      ("explicit", Value.arrayOf exportNameToJson $ Subordinates.explicit s)
    ]

-- | Convert ExportNameKind to JSON (sum type with no payload)
exportNameKindToJson :: ExportNameKind.ExportNameKind -> Value.Value
exportNameKindToJson k = Value.string $ case k of
  ExportNameKind.Pattern -> "Pattern"
  ExportNameKind.Type -> "Type"
  ExportNameKind.Module -> "Module"

-- | Convert Namespace to JSON (sum type with no payload)
namespaceToJson :: Namespace.Namespace -> Value.Value
namespaceToJson ns = Value.string $ case ns of
  Namespace.Value -> "Value"
  Namespace.Type -> "Type"

-- | Convert Example to JSON (record)
exampleToJson :: Example.Example -> Value.Value
exampleToJson ex =
  Value.object
    [ ("expression", Value.text $ Example.expression ex),
      ("result", Value.arrayOf Value.text $ Example.result ex)
    ]

-- | Convert Header to JSON (record with type parameter)
headerToJson :: (doc -> Value.Value) -> Header.Header doc -> Value.Value
headerToJson f h =
  Value.object
    [ ("level", levelToJson $ Header.level h),
      ("title", f $ Header.title h)
    ]

-- | Convert Hyperlink to JSON (record with type parameter)
hyperlinkToJson :: (doc -> Value.Value) -> Hyperlink.Hyperlink doc -> Value.Value
hyperlinkToJson f h =
  Value.object
    [ ("url", Value.text $ Hyperlink.url h),
      ("label", Value.optional f $ Hyperlink.label h)
    ]

-- | Convert Identifier to JSON (record)
identifierToJson :: Identifier.Identifier -> Value.Value
identifierToJson i =
  Value.object
    [ ("namespace", Value.optional namespaceToJson $ Identifier.namespace i),
      ("value", Value.text $ Identifier.value i)
    ]

-- | Convert ModLink to JSON (record with type parameter)
modLinkToJson :: (doc -> Value.Value) -> ModLink.ModLink doc -> Value.Value
modLinkToJson f ml =
  Value.object
    [ ("name", moduleNameToJson $ ModLink.name ml),
      ("label", Value.optional f $ ModLink.label ml)
    ]

-- | Convert Picture to JSON (record)
pictureToJson :: Picture.Picture -> Value.Value
pictureToJson p =
  Value.object
    [ ("uri", Value.text $ Picture.uri p),
      ("title", Value.optional Value.text $ Picture.title p)
    ]

-- | Convert Table to JSON (record with type parameter)
tableToJson :: (doc -> Value.Value) -> Table.Table doc -> Value.Value
tableToJson f t =
  Value.object
    [ ("headerRows", Value.arrayOf (Value.arrayOf $ cellToJson f) $ Table.headerRows t),
      ("bodyRows", Value.arrayOf (Value.arrayOf $ cellToJson f) $ Table.bodyRows t)
    ]

-- | Convert Cell to JSON (record with type parameter)
cellToJson :: (doc -> Value.Value) -> TableCell.Cell doc -> Value.Value
cellToJson f c =
  Value.object
    [ ("colspan", Value.integral $ TableCell.colspan c),
      ("rowspan", Value.integral $ TableCell.rowspan c),
      ("contents", f $ TableCell.contents c)
    ]

-- | Convert Level to JSON (sum type with no payload)
levelToJson :: Level.Level -> Value.Value
levelToJson l = Value.integer $ case l of
  Level.One -> 1
  Level.Two -> 2
  Level.Three -> 3
  Level.Four -> 4
  Level.Five -> 5
  Level.Six -> 6
