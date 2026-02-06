{-# LANGUAGE TemplateHaskellQuotes #-}

module Scrod.Convert.ToJson where

import qualified Control.Monad as Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHC.Stack as Stack
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
import qualified Scrod.Extra.Parsec as Parsec
import qualified Scrod.Json.Value as Json
import qualified Scrod.JsonPointer.Evaluate as Pointer
import qualified Scrod.JsonPointer.Pointer as Pointer
import qualified Scrod.Spec as Spec

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

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'toJson $ do
    let located :: Natural.Natural -> Natural.Natural -> a -> Located.Located a
        located l c v =
          Located.MkLocated
            { Located.location =
                Location.MkLocation
                  { Location.line = Line.MkLine l,
                    Location.column = Column.MkColumn c
                  },
              Located.value = v
            }

    Spec.describe s "version" $ do
      Spec.it s "defaults to [0]" $ do
        assertPointers s [("/version", "[0]")] id

      Spec.it s "converts multi-part version" $ do
        assertPointers s [("/version", "[1, 23]")] $ \m -> m {Module.version = Version.MkVersion $ NonEmpty.fromList [1, 23]}

    Spec.describe s "language" $ do
      Spec.it s "defaults to null" $ do
        assertPointers s [("/language", "null")] id

      Spec.it s "converts language name" $ do
        assertPointers s [("/language", "\"Haskell98\"")] $ \m -> m {Module.language = Just . Language.MkLanguage $ Text.pack "Haskell98"}

    Spec.describe s "extensions" $ do
      Spec.it s "defaults to empty object" $ do
        assertPointers s [("/extensions", "{}")] id

      Spec.it s "converts enabled extension" $ do
        assertPointers s [("/extensions/CPP", "true")] $ \m -> m {Module.extensions = Map.singleton (Extension.MkExtension $ Text.pack "CPP") True}

      Spec.it s "converts disabled extension" $ do
        assertPointers s [("/extensions/CPP", "false")] $ \m -> m {Module.extensions = Map.singleton (Extension.MkExtension $ Text.pack "CPP") False}

    Spec.describe s "documentation" $ do
      Spec.it s "defaults to Empty" $ do
        assertPointers s [("/documentation/type", "\"Empty\"")] id

      Spec.it s "converts String doc" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"String\""),
            ("/documentation/value", "\"hello\"")
          ]
          $ \m -> m {Module.documentation = Doc.String $ Text.pack "hello"}

      Spec.it s "converts Append" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Append\""),
            ("/documentation/value/0/type", "\"String\""),
            ("/documentation/value/0/value", "\"a\""),
            ("/documentation/value/1/type", "\"String\""),
            ("/documentation/value/1/value", "\"b\"")
          ]
          $ \m -> m {Module.documentation = Doc.Append (Doc.String $ Text.pack "a") (Doc.String $ Text.pack "b")}

      Spec.it s "converts Paragraph" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Paragraph\""),
            ("/documentation/value/type", "\"String\""),
            ("/documentation/value/value", "\"p\"")
          ]
          $ \m -> m {Module.documentation = Doc.Paragraph (Doc.String $ Text.pack "p")}

      Spec.it s "converts Identifier" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Identifier\""),
            ("/documentation/value/namespace", "null"),
            ("/documentation/value/value", "\"foo\"")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Identifier
                    Identifier.MkIdentifier
                      { Identifier.namespace = Nothing,
                        Identifier.value = Text.pack "foo"
                      }
              }

      Spec.it s "converts Identifier with namespace" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Identifier\""),
            ("/documentation/value/namespace", "\"Type\""),
            ("/documentation/value/value", "\"Foo\"")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Identifier
                    Identifier.MkIdentifier
                      { Identifier.namespace = Just Namespace.Type,
                        Identifier.value = Text.pack "Foo"
                      }
              }

      Spec.it s "converts Module link" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Module\""),
            ("/documentation/value/name", "\"Data.List\""),
            ("/documentation/value/label", "null")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Module
                    ModLink.MkModLink
                      { ModLink.name = ModuleName.MkModuleName $ Text.pack "Data.List",
                        ModLink.label = Nothing
                      }
              }

      Spec.it s "converts Module link with label" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Module\""),
            ("/documentation/value/name", "\"Data.List\""),
            ("/documentation/value/label/type", "\"String\""),
            ("/documentation/value/label/value", "\"lists\"")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Module
                    ModLink.MkModLink
                      { ModLink.name = ModuleName.MkModuleName $ Text.pack "Data.List",
                        ModLink.label = Just (Doc.String $ Text.pack "lists")
                      }
              }

      Spec.it s "converts Emphasis" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Emphasis\""),
            ("/documentation/value/type", "\"String\""),
            ("/documentation/value/value", "\"em\"")
          ]
          $ \m -> m {Module.documentation = Doc.Emphasis (Doc.String $ Text.pack "em")}

      Spec.it s "converts Monospaced" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Monospaced\""),
            ("/documentation/value/type", "\"String\""),
            ("/documentation/value/value", "\"code\"")
          ]
          $ \m -> m {Module.documentation = Doc.Monospaced (Doc.String $ Text.pack "code")}

      Spec.it s "converts Bold" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Bold\""),
            ("/documentation/value/type", "\"String\""),
            ("/documentation/value/value", "\"bold\"")
          ]
          $ \m -> m {Module.documentation = Doc.Bold (Doc.String $ Text.pack "bold")}

      Spec.it s "converts UnorderedList" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"UnorderedList\""),
            ("/documentation/value/0/type", "\"String\""),
            ("/documentation/value/0/value", "\"a\""),
            ("/documentation/value/1/type", "\"String\""),
            ("/documentation/value/1/value", "\"b\"")
          ]
          $ \m -> m {Module.documentation = Doc.UnorderedList [Doc.String $ Text.pack "a", Doc.String $ Text.pack "b"]}

      Spec.it s "converts OrderedList" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"OrderedList\""),
            ("/documentation/value/0/0", "1"),
            ("/documentation/value/0/1/type", "\"String\""),
            ("/documentation/value/0/1/value", "\"first\"")
          ]
          $ \m -> m {Module.documentation = Doc.OrderedList [(1, Doc.String $ Text.pack "first")]}

      Spec.it s "converts DefList" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"DefList\""),
            ("/documentation/value/0/0/type", "\"String\""),
            ("/documentation/value/0/0/value", "\"term\""),
            ("/documentation/value/0/1/type", "\"String\""),
            ("/documentation/value/0/1/value", "\"def\"")
          ]
          $ \m -> m {Module.documentation = Doc.DefList [(Doc.String $ Text.pack "term", Doc.String $ Text.pack "def")]}

      Spec.it s "converts CodeBlock" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"CodeBlock\""),
            ("/documentation/value/type", "\"String\""),
            ("/documentation/value/value", "\"x = 1\"")
          ]
          $ \m -> m {Module.documentation = Doc.CodeBlock (Doc.String $ Text.pack "x = 1")}

      Spec.it s "converts Hyperlink without label" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Hyperlink\""),
            ("/documentation/value/url", "\"https://example.com\""),
            ("/documentation/value/label", "null")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Hyperlink
                    Hyperlink.MkHyperlink
                      { Hyperlink.url = Text.pack "https://example.com",
                        Hyperlink.label = Nothing
                      }
              }

      Spec.it s "converts Hyperlink with label" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Hyperlink\""),
            ("/documentation/value/url", "\"https://example.com\""),
            ("/documentation/value/label/type", "\"String\""),
            ("/documentation/value/label/value", "\"link\"")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Hyperlink
                    Hyperlink.MkHyperlink
                      { Hyperlink.url = Text.pack "https://example.com",
                        Hyperlink.label = Just (Doc.String $ Text.pack "link")
                      }
              }

      Spec.it s "converts Pic without title" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Pic\""),
            ("/documentation/value/uri", "\"image.png\""),
            ("/documentation/value/title", "null")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Pic
                    Picture.MkPicture
                      { Picture.uri = Text.pack "image.png",
                        Picture.title = Nothing
                      }
              }

      Spec.it s "converts Pic with title" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Pic\""),
            ("/documentation/value/uri", "\"image.png\""),
            ("/documentation/value/title", "\"A picture\"")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Pic
                    Picture.MkPicture
                      { Picture.uri = Text.pack "image.png",
                        Picture.title = Just $ Text.pack "A picture"
                      }
              }

      Spec.it s "converts MathInline" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"MathInline\""),
            ("/documentation/value", "\"x^2\"")
          ]
          $ \m -> m {Module.documentation = Doc.MathInline $ Text.pack "x^2"}

      Spec.it s "converts MathDisplay" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"MathDisplay\""),
            ("/documentation/value", "\"x^2\"")
          ]
          $ \m -> m {Module.documentation = Doc.MathDisplay $ Text.pack "x^2"}

      Spec.it s "converts AName" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"AName\""),
            ("/documentation/value", "\"anchor\"")
          ]
          $ \m -> m {Module.documentation = Doc.AName $ Text.pack "anchor"}

      Spec.it s "converts Property" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Property\""),
            ("/documentation/value", "\"prop\"")
          ]
          $ \m -> m {Module.documentation = Doc.Property $ Text.pack "prop"}

      Spec.it s "converts Examples" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Examples\""),
            ("/documentation/value/0/expression", "\"1 + 1\""),
            ("/documentation/value/0/result/0", "\"2\"")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Examples
                    [ Example.MkExample
                        { Example.expression = Text.pack "1 + 1",
                          Example.result = [Text.pack "2"]
                        }
                    ]
              }

      Spec.it s "converts Header" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Header\""),
            ("/documentation/value/level", "2"),
            ("/documentation/value/title/type", "\"String\""),
            ("/documentation/value/title/value", "\"heading\"")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Header
                    Header.MkHeader
                      { Header.level = Level.Two,
                        Header.title = Doc.String $ Text.pack "heading"
                      }
              }

      Spec.it s "converts Table" $ do
        assertPointers
          s
          [ ("/documentation/type", "\"Table\""),
            ("/documentation/value/headerRows/0/0/colspan", "1"),
            ("/documentation/value/headerRows/0/0/rowspan", "1"),
            ("/documentation/value/headerRows/0/0/contents/type", "\"String\""),
            ("/documentation/value/headerRows/0/0/contents/value", "\"h\""),
            ("/documentation/value/bodyRows/0/0/contents/type", "\"String\""),
            ("/documentation/value/bodyRows/0/0/contents/value", "\"b\"")
          ]
          $ \m ->
            m
              { Module.documentation =
                  Doc.Table
                    Table.MkTable
                      { Table.headerRows =
                          [ [ TableCell.MkCell
                                { TableCell.colspan = 1,
                                  TableCell.rowspan = 1,
                                  TableCell.contents = Doc.String $ Text.pack "h"
                                }
                            ]
                          ],
                        Table.bodyRows =
                          [ [ TableCell.MkCell
                                { TableCell.colspan = 1,
                                  TableCell.rowspan = 1,
                                  TableCell.contents = Doc.String $ Text.pack "b"
                                }
                            ]
                          ]
                      }
              }

    Spec.describe s "since" $ do
      let since =
            Since.MkSince
              { Since.package = Nothing,
                Since.version = Version.MkVersion $ NonEmpty.singleton 0
              }

      Spec.it s "defaults to null" $ do
        assertPointers s [("/since", "null")] id

      Spec.it s "converts since with version" $ do
        assertPointers s [("/since/version", "[0]")] $ \m -> m {Module.since = Just since}

      Spec.it s "converts since with package" $ do
        assertPointers s [("/since/package", "\"base\"")] $ \m -> m {Module.since = Just since {Since.package = Just . PackageName.MkPackageName $ Text.pack "base"}}

    Spec.describe s "name" $ do
      Spec.it s "defaults to null" $ do
        assertPointers s [("/name", "null")] id

      Spec.it s "converts located module name" $ do
        assertPointers
          s
          [ ("/name/location/line", "1"),
            ("/name/location/column", "8"),
            ("/name/value", "\"Data.List\"")
          ]
          $ \m -> m {Module.name = Just . located 1 8 . ModuleName.MkModuleName $ Text.pack "Data.List"}

    Spec.describe s "warning" $ do
      Spec.it s "defaults to null" $ do
        assertPointers s [("/warning", "null")] id

      Spec.it s "converts warning" $ do
        assertPointers
          s
          [ ("/warning/category", "\"deprecated\""),
            ("/warning/value", "\"Use foo instead\"")
          ]
          $ \m ->
            m
              { Module.warning =
                  Just
                    Warning.MkWarning
                      { Warning.category = Category.MkCategory $ Text.pack "deprecated",
                        Warning.value = Text.pack "Use foo instead"
                      }
              }

    Spec.describe s "exports" $ do
      Spec.it s "defaults to null" $ do
        assertPointers s [("/exports", "null")] id

      Spec.it s "converts empty list" $ do
        assertPointers s [("/exports", "[]")] $ \m -> m {Module.exports = Just []}

      Spec.it s "converts Identifier export" $ do
        assertPointers
          s
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "null"),
            ("/exports/0/value/name/name", "\"foo\""),
            ("/exports/0/value/subordinates", "null"),
            ("/exports/0/value/warning", "null"),
            ("/exports/0/value/doc", "null")
          ]
          $ \m ->
            m
              { Module.exports =
                  Just
                    [ Export.Identifier
                        ExportIdentifier.MkExportIdentifier
                          { ExportIdentifier.name =
                              ExportName.MkExportName
                                { ExportName.kind = Nothing,
                                  ExportName.name = Text.pack "foo"
                                },
                            ExportIdentifier.subordinates = Nothing,
                            ExportIdentifier.warning = Nothing,
                            ExportIdentifier.doc = Nothing
                          }
                    ]
              }

      Spec.it s "converts Identifier export with subordinates" $ do
        assertPointers
          s
          [ ("/exports/0/type", "\"Identifier\""),
            ("/exports/0/value/name/kind", "\"Type\""),
            ("/exports/0/value/name/name", "\"Foo\""),
            ("/exports/0/value/subordinates/wildcard", "true"),
            ("/exports/0/value/subordinates/explicit", "[]")
          ]
          $ \m ->
            m
              { Module.exports =
                  Just
                    [ Export.Identifier
                        ExportIdentifier.MkExportIdentifier
                          { ExportIdentifier.name =
                              ExportName.MkExportName
                                { ExportName.kind = Just ExportNameKind.Type,
                                  ExportName.name = Text.pack "Foo"
                                },
                            ExportIdentifier.subordinates =
                              Just
                                Subordinates.MkSubordinates
                                  { Subordinates.wildcard = True,
                                    Subordinates.explicit = []
                                  },
                            ExportIdentifier.warning = Nothing,
                            ExportIdentifier.doc = Nothing
                          }
                    ]
              }

      Spec.it s "converts Group export" $ do
        assertPointers
          s
          [ ("/exports/0/type", "\"Group\""),
            ("/exports/0/value/level", "1"),
            ("/exports/0/value/title/type", "\"String\""),
            ("/exports/0/value/title/value", "\"Section\"")
          ]
          $ \m ->
            m
              { Module.exports =
                  Just
                    [ Export.Group
                        Section.MkSection
                          { Section.header =
                              Header.MkHeader
                                { Header.level = Level.One,
                                  Header.title = Doc.String $ Text.pack "Section"
                                }
                          }
                    ]
              }

      Spec.it s "converts Doc export" $ do
        assertPointers
          s
          [ ("/exports/0/type", "\"Doc\""),
            ("/exports/0/value/type", "\"String\""),
            ("/exports/0/value/value", "\"some doc\"")
          ]
          $ \m ->
            m
              { Module.exports =
                  Just [Export.Doc (Doc.String $ Text.pack "some doc")]
              }

      Spec.it s "converts DocNamed export" $ do
        assertPointers
          s
          [ ("/exports/0/type", "\"DocNamed\""),
            ("/exports/0/value", "\"chunkName\"")
          ]
          $ \m ->
            m
              { Module.exports =
                  Just [Export.DocNamed $ Text.pack "chunkName"]
              }

    Spec.describe s "items" $ do
      Spec.it s "defaults to empty array" $ do
        assertPointers s [("/items", "[]")] id

      Spec.it s "converts item kind" $ do
        assertPointers
          s
          [ ("/items/0/location/line", "1"),
            ("/items/0/location/column", "2"),
            ("/items/0/value/key", "3"),
            ("/items/0/value/kind", "\"Function\""),
            ("/items/0/value/parentKey", "null"),
            ("/items/0/value/name", "null"),
            ("/items/0/value/documentation/type", "\"Empty\""),
            ("/items/0/value/signature", "null")
          ]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      2
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 3,
                          Item.kind = ItemKind.Function,
                          Item.parentKey = Nothing,
                          Item.name = Nothing,
                          Item.documentation = Doc.Empty,
                          Item.signature = Nothing
                        }
                  ]
              }

      Spec.it s "converts DataType kind" $ do
        assertPointers
          s
          [("/items/0/value/kind", "\"DataType\"")]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      1
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 0,
                          Item.kind = ItemKind.DataType,
                          Item.parentKey = Nothing,
                          Item.name = Nothing,
                          Item.documentation = Doc.Empty,
                          Item.signature = Nothing
                        }
                  ]
              }

      Spec.it s "converts Class kind" $ do
        assertPointers
          s
          [("/items/0/value/kind", "\"Class\"")]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      1
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 0,
                          Item.kind = ItemKind.Class,
                          Item.parentKey = Nothing,
                          Item.name = Nothing,
                          Item.documentation = Doc.Empty,
                          Item.signature = Nothing
                        }
                  ]
              }

      Spec.it s "converts Newtype kind" $ do
        assertPointers
          s
          [("/items/0/value/kind", "\"Newtype\"")]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      1
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 0,
                          Item.kind = ItemKind.Newtype,
                          Item.parentKey = Nothing,
                          Item.name = Nothing,
                          Item.documentation = Doc.Empty,
                          Item.signature = Nothing
                        }
                  ]
              }

      Spec.it s "converts TypeSynonym kind" $ do
        assertPointers
          s
          [("/items/0/value/kind", "\"TypeSynonym\"")]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      1
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 0,
                          Item.kind = ItemKind.TypeSynonym,
                          Item.parentKey = Nothing,
                          Item.name = Nothing,
                          Item.documentation = Doc.Empty,
                          Item.signature = Nothing
                        }
                  ]
              }

      Spec.it s "converts item with parentKey" $ do
        assertPointers
          s
          [ ("/items/0/value/parentKey", "7")
          ]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      1
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 0,
                          Item.kind = ItemKind.ClassMethod,
                          Item.parentKey = Just $ ItemKey.MkItemKey 7,
                          Item.name = Nothing,
                          Item.documentation = Doc.Empty,
                          Item.signature = Nothing
                        }
                  ]
              }

      Spec.it s "converts item with name" $ do
        assertPointers
          s
          [ ("/items/0/value/name", "\"myFunc\"")
          ]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      1
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 0,
                          Item.kind = ItemKind.Function,
                          Item.parentKey = Nothing,
                          Item.name = Just . ItemName.MkItemName $ Text.pack "myFunc",
                          Item.documentation = Doc.Empty,
                          Item.signature = Nothing
                        }
                  ]
              }

      Spec.it s "converts item with documentation" $ do
        assertPointers
          s
          [ ("/items/0/value/documentation/type", "\"String\""),
            ("/items/0/value/documentation/value", "\"A function\"")
          ]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      1
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 0,
                          Item.kind = ItemKind.Function,
                          Item.parentKey = Nothing,
                          Item.name = Nothing,
                          Item.documentation = Doc.String $ Text.pack "A function",
                          Item.signature = Nothing
                        }
                  ]
              }

      Spec.it s "converts item with signature" $ do
        assertPointers
          s
          [ ("/items/0/value/signature", "\"Int -> Bool\"")
          ]
          $ \m ->
            m
              { Module.items =
                  [ located
                      1
                      1
                      Item.MkItem
                        { Item.key = ItemKey.MkItemKey 0,
                          Item.kind = ItemKind.Function,
                          Item.parentKey = Nothing,
                          Item.name = Nothing,
                          Item.documentation = Doc.Empty,
                          Item.signature = Just $ Text.pack "Int -> Bool"
                        }
                  ]
              }

assertPointers :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> [(String, String)] -> (Module.Module -> Module.Module) -> m ()
assertPointers s pjs f = do
  let m =
        toJson $
          f
            Module.MkModule
              { Module.version = Version.MkVersion $ NonEmpty.singleton 0,
                Module.language = Nothing,
                Module.extensions = Map.empty,
                Module.documentation = Doc.Empty,
                Module.since = Nothing,
                Module.name = Nothing,
                Module.warning = Nothing,
                Module.exports = Nothing,
                Module.items = []
              }
  Monad.forM_ pjs $ \(p, j) -> do
    pointer <- maybe (Spec.assertFailure s $ "invalid pointer: " <> show p) pure $ Parsec.parseString Pointer.decode p
    json <- maybe (Spec.assertFailure s $ "invalid json: " <> show j) pure $ Parsec.parseString Json.decode j
    Spec.assertEq s (Pointer.evaluate pointer m) $ Just json
