module Scrod.Convert.ToHtml where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
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
import qualified Scrod.Css.Declaration as CssDeclaration
import qualified Scrod.Css.Item as CssItem
import qualified Scrod.Css.Name as CssName
import qualified Scrod.Css.Rule as CssRule
import qualified Scrod.Css.Selector as CssSelector
import qualified Scrod.Css.Stylesheet as CssStylesheet
import qualified Scrod.Extra.Builder as Builder
import qualified Scrod.Spec as Spec
import qualified Scrod.Xml.Attribute as Attribute
import qualified Scrod.Xml.Content as Content
import qualified Scrod.Xml.Declaration as XmlDeclaration
import qualified Scrod.Xml.Document as Xml
import qualified Scrod.Xml.Element as Element
import qualified Scrod.Xml.Misc as Misc
import qualified Scrod.Xml.Name as XmlName

-- | Convert a Module to a complete HTML document.
toHtml :: Module.Module -> Xml.Document
toHtml m =
  Xml.MkDocument
    { Xml.prolog =
        [ Misc.Declaration $
            XmlDeclaration.MkDeclaration
              (XmlName.MkName $ Text.pack "doctype")
              (Text.pack "html")
        ],
      Xml.root =
        Xml.element
          "html"
          []
          [ Content.Element (headElement m),
            Content.Element (bodyElement m)
          ]
    }

moduleTitle :: Module.Module -> Text.Text
moduleTitle m = case Module.name m of
  Nothing -> Text.pack "Documentation"
  Just (Located.MkLocated _ (ModuleName.MkModuleName n)) -> n

headElement :: Module.Module -> Element.Element
headElement m =
  Xml.element
    "head"
    []
    [ Content.Element $
        Xml.element "meta" [Xml.attribute "charset" "utf-8"] [],
      Content.Element $
        Xml.element
          "meta"
          [ Xml.attribute "name" "viewport",
            Xml.attribute "content" "width=device-width, initial-scale=1"
          ]
          [],
      Content.Element $
        Xml.element "title" [] [Xml.text (moduleTitle m)],
      Content.Element $
        Xml.element "style" [] [Xml.text . Builder.toText $ CssStylesheet.encode stylesheet]
    ]

bodyElement :: Module.Module -> Element.Element
bodyElement m =
  Xml.element
    "body"
    []
    ( [Content.Element (headerSection m)]
        <> [Content.Element (metadataSection m)]
        <> exportsContents (Module.exports m)
        <> extensionsContents (Module.extensions m)
        <> itemsContents (Module.items m)
    )

-- Header section

headerSection :: Module.Module -> Element.Element
headerSection m =
  Xml.element
    "header"
    [Xml.attribute "class" "module-header"]
    ( [Content.Element $ Xml.element "h1" [] [Xml.text (moduleTitle m)]]
        <> warningContents (Module.warning m)
        <> moduleDocContents (Module.documentation m)
    )

warningContents :: Maybe Warning.Warning -> [Content.Content Element.Element]
warningContents Nothing = []
warningContents (Just w) = [Content.Element (warningToHtml w)]

warningToHtml :: Warning.Warning -> Element.Element
warningToHtml (Warning.MkWarning (Category.MkCategory cat) val) =
  Xml.element
    "div"
    [Xml.attribute "class" "warning"]
    [ Content.Element $
        Xml.element
          "span"
          [Xml.attribute "class" "warning-category"]
          [Xml.text cat],
      Xml.text (Text.pack ": " <> val)
    ]

moduleDocContents :: Doc.Doc -> [Content.Content Element.Element]
moduleDocContents Doc.Empty = []
moduleDocContents doc =
  [Content.Element $ Xml.element "div" [Xml.attribute "class" "module-doc"] (docToContents doc)]

-- Metadata section

metadataSection :: Module.Module -> Element.Element
metadataSection m =
  Xml.element
    "section"
    [Xml.attribute "class" "metadata"]
    [ Content.Element $
        Xml.element
          "dl"
          []
          ( versionItem (Module.version m)
              <> languageItem (Module.language m)
              <> sinceItem (Module.since m)
          )
    ]

versionItem :: Version.Version -> [Content.Content Element.Element]
versionItem v =
  [ Content.Element $ Xml.element "dt" [] [Xml.string "Version"],
    Content.Element $ Xml.element "dd" [] [Xml.text (versionToText v)]
  ]

versionToText :: Version.Version -> Text.Text
versionToText (Version.MkVersion parts) =
  Text.intercalate (Text.pack ".") . fmap (Text.pack . show) $ NonEmpty.toList parts

languageItem :: Maybe Language.Language -> [Content.Content Element.Element]
languageItem Nothing = []
languageItem (Just (Language.MkLanguage lang)) =
  [ Content.Element $ Xml.element "dt" [] [Xml.string "Language"],
    Content.Element $ Xml.element "dd" [] [Xml.text lang]
  ]

sinceItem :: Maybe Since.Since -> [Content.Content Element.Element]
sinceItem Nothing = []
sinceItem (Just since) =
  [ Content.Element $ Xml.element "dt" [] [Xml.string "Since"],
    Content.Element $
      Xml.element
        "dd"
        [Xml.attribute "class" "since"]
        [Xml.text (sinceToText since)]
  ]

sinceToText :: Since.Since -> Text.Text
sinceToText (Since.MkSince maybePackage version) =
  packageText <> versionToText version
  where
    packageText :: Text.Text
    packageText = case maybePackage of
      Nothing -> Text.empty
      Just (PackageName.MkPackageName pkg) -> pkg <> Text.pack "-"

-- Exports section

exportsContents :: Maybe [Export.Export] -> [Content.Content Element.Element]
exportsContents Nothing = []
exportsContents (Just []) = []
exportsContents (Just exports) =
  [ Content.Element $
      Xml.element
        "section"
        [Xml.attribute "class" "exports"]
        ( [Content.Element $ Xml.element "h2" [] [Xml.string "Exports"]]
            <> [ Content.Element $
                   Xml.element
                     "ul"
                     [Xml.attribute "class" "export-list"]
                     (concatMap exportToContents exports)
               ]
        )
  ]

exportToContents :: Export.Export -> [Content.Content Element.Element]
exportToContents export = case export of
  Export.Identifier ident ->
    [Content.Element $ Xml.element "li" [] [Content.Element (exportIdentifierToHtml ident)]]
  Export.Group section ->
    [Content.Element $ Xml.element "li" [] [Content.Element (sectionToHtml section)]]
  Export.Doc doc ->
    [ Content.Element $
        Xml.element
          "li"
          []
          [Content.Element $ Xml.element "div" [Xml.attribute "class" "export-doc"] (docToContents doc)]
    ]
  Export.DocNamed name ->
    [ Content.Element $
        Xml.element
          "li"
          []
          [ Content.Element $
              Xml.element
                "div"
                [Xml.attribute "class" "export-doc-named"]
                [Xml.text (Text.pack "\x00a7" <> name)]
          ]
    ]

exportIdentifierToHtml :: ExportIdentifier.ExportIdentifier -> Element.Element
exportIdentifierToHtml (ExportIdentifier.MkExportIdentifier name subs maybeWarning maybeDoc) =
  Xml.element
    "div"
    [Xml.attribute "class" "export-item"]
    ( foldMap (\w -> [Content.Element (warningToHtml w)]) maybeWarning
        <> [ Content.Element $
               Xml.element
                 "code"
                 [Xml.attribute "class" "export-name"]
                 ( [Xml.text (exportNameToText name)]
                     <> subordinatesToContents subs
                 )
           ]
        <> foldMap
          ( \doc ->
              [Content.Element $ Xml.element "div" [Xml.attribute "class" "export-doc"] (docToContents doc)]
          )
          maybeDoc
    )

exportNameToText :: ExportName.ExportName -> Text.Text
exportNameToText (ExportName.MkExportName maybeKind name) =
  kindPrefix <> name
  where
    kindPrefix :: Text.Text
    kindPrefix = case maybeKind of
      Nothing -> Text.empty
      Just ExportNameKind.Pattern -> Text.pack "pattern "
      Just ExportNameKind.Type -> Text.pack "type "
      Just ExportNameKind.Module -> Text.pack "module "

subordinatesToContents :: Maybe Subordinates.Subordinates -> [Content.Content Element.Element]
subordinatesToContents Nothing = []
subordinatesToContents (Just (Subordinates.MkSubordinates wildcard explicit)) =
  let wildcardText :: Text.Text
      wildcardText = Text.pack ".."
      explicitTexts :: [Text.Text]
      explicitTexts = fmap (\(ExportName.MkExportName _ n) -> n) explicit
      allTexts :: [Text.Text]
      allTexts = if wildcard then wildcardText : explicitTexts else explicitTexts
      combined :: Text.Text
      combined = Text.intercalate (Text.pack ", ") allTexts
   in [Xml.text (Text.pack "(" <> combined <> Text.pack ")")]

sectionToHtml :: Section.Section -> Element.Element
sectionToHtml (Section.MkSection (Header.MkHeader level title)) =
  Xml.element
    "div"
    [Xml.attribute "class" "export-group"]
    [ Content.Element $
        Xml.element
          (sectionLevelToName level)
          [Xml.attribute "class" "export-group-title"]
          (docToContents title)
    ]

sectionLevelToName :: Level.Level -> String
sectionLevelToName l = case l of
  Level.One -> "h3"
  Level.Two -> "h4"
  Level.Three -> "h5"
  Level.Four -> "h6"
  Level.Five -> "h6"
  Level.Six -> "h6"

-- Extensions section

extensionsContents :: Map.Map Extension.Extension Bool -> [Content.Content Element.Element]
extensionsContents extensions
  | Map.null extensions = []
  | otherwise =
      [ Content.Element $
          Xml.element
            "section"
            [Xml.attribute "class" "extensions"]
            ( [Content.Element $ Xml.element "h2" [] [Xml.string "Extensions"]]
                <> [ Content.Element $
                       Xml.element
                         "div"
                         [Xml.attribute "class" "extension-list"]
                         (concatMap extToContents $ Map.toList extensions)
                   ]
            )
      ]

extToContents :: (Extension.Extension, Bool) -> [Content.Content Element.Element]
extToContents (Extension.MkExtension name, enabled) =
  let cls :: String
      cls = if enabled then "extension" else "extension extension-disabled"
   in [ Content.Element $
          Xml.element "span" [Xml.attribute "class" cls] [Xml.text name],
        Xml.string " "
      ]

-- Items section

itemsContents :: [Located.Located Item.Item] -> [Content.Content Element.Element]
itemsContents [] = []
itemsContents items =
  [ Content.Element $
      Xml.element
        "section"
        [Xml.attribute "class" "items"]
        ( [Content.Element $ Xml.element "h2" [] [Xml.string "Declarations"]]
            <> concatMap renderItemWithChildren topLevelItems
        )
  ]
  where
    childrenMap :: Map.Map Natural.Natural [Located.Located Item.Item]
    childrenMap = foldr addChild Map.empty items

    addChild :: Located.Located Item.Item -> Map.Map Natural.Natural [Located.Located Item.Item] -> Map.Map Natural.Natural [Located.Located Item.Item]
    addChild li acc = case Item.parentKey (Located.value li) of
      Nothing -> acc
      Just (ItemKey.MkItemKey pk) -> Map.insertWith (<>) pk [li] acc

    topLevelItems :: [Located.Located Item.Item]
    topLevelItems = filter (isTopLevel . Located.value) items

    isTopLevel :: Item.Item -> Bool
    isTopLevel item = case Item.parentKey item of
      Nothing -> True
      Just _ -> False

    renderItemWithChildren :: Located.Located Item.Item -> [Content.Content Element.Element]
    renderItemWithChildren li =
      let k = ItemKey.unwrap (Item.key (Located.value li))
          children = Map.findWithDefault [] k childrenMap
       in [Content.Element (itemToHtml li)]
            <> if null children
              then []
              else
                [ Content.Element $
                    Xml.element
                      "div"
                      [Xml.attribute "class" "item-children"]
                      (concatMap renderItemWithChildren children)
                ]

itemToHtml :: Located.Located Item.Item -> Element.Element
itemToHtml (Located.MkLocated loc (Item.MkItem key itemKind _parentKey maybeName doc maybeSig)) =
  Xml.element
    "div"
    [ Xml.attribute "class" "item",
      Xml.attribute "id" ("item-" <> show (ItemKey.unwrap key))
    ]
    ( nameContents
        <> [Content.Element kindElement]
        <> signatureContents
        <> [Content.Element keyElement]
        <> [Content.Element (locationElement loc)]
        <> docContents'
    )
  where
    nameContents :: [Content.Content Element.Element]
    nameContents = case maybeName of
      Nothing -> []
      Just (ItemName.MkItemName n) ->
        [Content.Element $ Xml.element "span" [Xml.attribute "class" "item-name"] [Xml.text n]]

    kindElement :: Element.Element
    kindElement =
      Xml.element
        "span"
        [Xml.attribute "class" "item-kind"]
        [Xml.text (Text.pack " [" <> kindToText itemKind <> Text.pack "]")]

    signatureContents :: [Content.Content Element.Element]
    signatureContents = case maybeSig of
      Nothing -> []
      Just sig ->
        [ Content.Element $
            Xml.element
              "span"
              [Xml.attribute "class" "item-signature"]
              [Xml.text (Text.pack " :: " <> sig)]
        ]

    keyElement :: Element.Element
    keyElement =
      Xml.element
        "span"
        [Xml.attribute "class" "item-key"]
        [Xml.text (Text.pack "#" <> Text.pack (show (ItemKey.unwrap key)))]

    docContents' :: [Content.Content Element.Element]
    docContents' = case doc of
      Doc.Empty -> []
      _ -> [Content.Element $ Xml.element "div" [Xml.attribute "class" "item-doc"] (docToContents doc)]

locationElement :: Location.Location -> Element.Element
locationElement (Location.MkLocation (Line.MkLine l) (Column.MkColumn c)) =
  Xml.element
    "span"
    [Xml.attribute "class" "item-location"]
    [ Xml.text
        ( Text.pack " (line "
            <> Text.pack (show l)
            <> Text.pack ", col "
            <> Text.pack (show c)
            <> Text.pack ")"
        )
    ]

kindToText :: ItemKind.ItemKind -> Text.Text
kindToText k = case k of
  ItemKind.Function -> Text.pack "function"
  ItemKind.PatternBinding -> Text.pack "pattern binding"
  ItemKind.PatternSynonym -> Text.pack "pattern"
  ItemKind.DataType -> Text.pack "data"
  ItemKind.Newtype -> Text.pack "newtype"
  ItemKind.TypeData -> Text.pack "type data"
  ItemKind.TypeSynonym -> Text.pack "type"
  ItemKind.DataConstructor -> Text.pack "constructor"
  ItemKind.GADTConstructor -> Text.pack "GADT constructor"
  ItemKind.RecordField -> Text.pack "field"
  ItemKind.Class -> Text.pack "class"
  ItemKind.ClassMethod -> Text.pack "method"
  ItemKind.ClassInstance -> Text.pack "instance"
  ItemKind.StandaloneDeriving -> Text.pack "standalone deriving"
  ItemKind.DerivedInstance -> Text.pack "deriving"
  ItemKind.OpenTypeFamily -> Text.pack "type family"
  ItemKind.ClosedTypeFamily -> Text.pack "type family"
  ItemKind.DataFamily -> Text.pack "data family"
  ItemKind.TypeFamilyInstance -> Text.pack "type instance"
  ItemKind.DataFamilyInstance -> Text.pack "data instance"
  ItemKind.ForeignImport -> Text.pack "foreign import"
  ItemKind.ForeignExport -> Text.pack "foreign export"
  ItemKind.FixitySignature -> Text.pack "fixity"
  ItemKind.InlineSignature -> Text.pack "inline"
  ItemKind.SpecialiseSignature -> Text.pack "specialise"
  ItemKind.StandaloneKindSig -> Text.pack "kind"
  ItemKind.Rule -> Text.pack "rule"
  ItemKind.Default -> Text.pack "default"
  ItemKind.Annotation -> Text.pack "annotation"

-- Doc to HTML conversion

docToContents :: Doc.Doc -> [Content.Content Element.Element]
docToContents doc = case doc of
  Doc.Empty -> []
  Doc.Append d1 d2 -> docToContents d1 <> docToContents d2
  Doc.String t -> [Xml.text t]
  Doc.Paragraph d -> [Content.Element $ Xml.element "p" [] (docToContents d)]
  Doc.Identifier i -> [Content.Element (identifierToHtml i)]
  Doc.Module m -> [Content.Element (modLinkToHtml m)]
  Doc.Emphasis d -> [Content.Element $ Xml.element "em" [] (docToContents d)]
  Doc.Monospaced d -> [Content.Element $ Xml.element "code" [] (docToContents d)]
  Doc.Bold d -> [Content.Element $ Xml.element "strong" [] (docToContents d)]
  Doc.UnorderedList items ->
    [ Content.Element $
        Xml.element "ul" [] (concatMap (\item -> [Content.Element $ Xml.element "li" [] (docToContents item)]) items)
    ]
  Doc.OrderedList items ->
    [ Content.Element $
        Xml.element
          "ol"
          []
          ( fmap
              ( \(i, d) ->
                  Content.Element $
                    Xml.element "li" [Xml.attribute "value" (show i)] (docToContents d)
              )
              items
          )
    ]
  Doc.DefList defs ->
    [ Content.Element $
        Xml.element
          "dl"
          []
          ( concatMap
              ( \(term, def) ->
                  [ Content.Element $ Xml.element "dt" [] (docToContents term),
                    Content.Element $ Xml.element "dd" [] (docToContents def)
                  ]
              )
              defs
          )
    ]
  Doc.CodeBlock d ->
    [Content.Element $ Xml.element "pre" [] [Content.Element $ Xml.element "code" [] (docToContents d)]]
  Doc.Hyperlink h -> [Content.Element (hyperlinkToHtml h)]
  Doc.Pic p -> [Content.Element (pictureToHtml p)]
  Doc.MathInline t ->
    [Content.Element $ Xml.element "span" [Xml.attribute "class" "math-inline"] [Xml.text t]]
  Doc.MathDisplay t ->
    [Content.Element $ Xml.element "div" [Xml.attribute "class" "math-display"] [Xml.text t]]
  Doc.AName t ->
    [Content.Element $ Xml.element "a" [Xml.attribute "id" (Text.unpack t)] []]
  Doc.Property t ->
    [Content.Element $ Xml.element "pre" [Xml.attribute "class" "property"] [Xml.text t]]
  Doc.Examples es -> [Content.Element (examplesToHtml es)]
  Doc.Header h -> [Content.Element (headerToHtml h)]
  Doc.Table t -> [Content.Element (tableToHtml t)]

identifierToHtml :: Identifier.Identifier -> Element.Element
identifierToHtml (Identifier.MkIdentifier ns val) =
  Xml.element "code" [Xml.attribute "class" "identifier"] [Xml.text (prefix <> val)]
  where
    prefix :: Text.Text
    prefix = case ns of
      Nothing -> Text.empty
      Just Namespace.Value -> Text.pack "v'"
      Just Namespace.Type -> Text.pack "t'"

modLinkToHtml :: ModLink.ModLink Doc.Doc -> Element.Element
modLinkToHtml (ModLink.MkModLink (ModuleName.MkModuleName modName) maybeLabel) =
  Xml.element "code" [Xml.attribute "class" "module-link"] $
    maybe [Xml.text modName] docToContents maybeLabel

hyperlinkToHtml :: Hyperlink.Hyperlink Doc.Doc -> Element.Element
hyperlinkToHtml (Hyperlink.MkHyperlink url maybeLabel) =
  Xml.element "a" [Xml.attribute "href" (Text.unpack url)] $
    maybe [Xml.text url] docToContents maybeLabel

pictureToHtml :: Picture.Picture -> Element.Element
pictureToHtml (Picture.MkPicture uri maybeTitle) =
  Xml.element
    "img"
    ( [Xml.attribute "src" (Text.unpack uri)]
        <> [Xml.attribute "alt" (Text.unpack (Maybe.fromMaybe Text.empty maybeTitle))]
        <> foldMap (\t -> [Xml.attribute "title" (Text.unpack t)]) maybeTitle
    )
    []

examplesToHtml :: [Example.Example] -> Element.Element
examplesToHtml examples =
  Xml.element
    "div"
    [Xml.attribute "class" "examples"]
    (concatMap exampleToContents examples)

exampleToContents :: Example.Example -> [Content.Content Element.Element]
exampleToContents (Example.MkExample expr results) =
  [ Content.Element $
      Xml.element
        "div"
        [Xml.attribute "class" "example"]
        ( [ Content.Element $
              Xml.element
                "div"
                [Xml.attribute "class" "example-expression"]
                [Xml.text expr]
          ]
            <> fmap
              ( \r ->
                  Content.Element $
                    Xml.element
                      "div"
                      [Xml.attribute "class" "example-result"]
                      [Xml.text r]
              )
              results
        )
  ]

headerToHtml :: Header.Header Doc.Doc -> Element.Element
headerToHtml (Header.MkHeader level title) =
  Xml.element (levelToName level) [] (docToContents title)

levelToName :: Level.Level -> String
levelToName level = case level of
  Level.One -> "h1"
  Level.Two -> "h2"
  Level.Three -> "h3"
  Level.Four -> "h4"
  Level.Five -> "h5"
  Level.Six -> "h6"

tableToHtml :: Table.Table Doc.Doc -> Element.Element
tableToHtml (Table.MkTable headerRows bodyRows) =
  Xml.element "table" [] (theadContents <> tbodyContents)
  where
    theadContents :: [Content.Content Element.Element]
    theadContents
      | null headerRows = []
      | otherwise =
          [ Content.Element $
              Xml.element "thead" [] (fmap (Content.Element . headerRowToHtml) headerRows)
          ]

    tbodyContents :: [Content.Content Element.Element]
    tbodyContents
      | null bodyRows = []
      | otherwise =
          [ Content.Element $
              Xml.element "tbody" [] (fmap (Content.Element . bodyRowToHtml) bodyRows)
          ]

    headerRowToHtml :: [TableCell.Cell Doc.Doc] -> Element.Element
    headerRowToHtml cells =
      Xml.element "tr" [] (fmap (Content.Element . headerCellToHtml) cells)

    bodyRowToHtml :: [TableCell.Cell Doc.Doc] -> Element.Element
    bodyRowToHtml cells =
      Xml.element "tr" [] (fmap (Content.Element . bodyCellToHtml) cells)

    headerCellToHtml :: TableCell.Cell Doc.Doc -> Element.Element
    headerCellToHtml (TableCell.MkCell colspan rowspan contents) =
      Xml.element "th" (cellAttrs colspan rowspan) (docToContents contents)

    bodyCellToHtml :: TableCell.Cell Doc.Doc -> Element.Element
    bodyCellToHtml (TableCell.MkCell colspan rowspan contents) =
      Xml.element "td" (cellAttrs colspan rowspan) (docToContents contents)

    cellAttrs :: Natural.Natural -> Natural.Natural -> [Attribute.Attribute]
    cellAttrs c r =
      [ Xml.attribute "colspan" (show c),
        Xml.attribute "rowspan" (show r)
      ]

-- CSS stylesheet

stylesheet :: CssStylesheet.Stylesheet
stylesheet =
  CssStylesheet.MkStylesheet
    [ rule ["*", "* ::before", "* ::after"] [("box-sizing", "border-box")],
      rule ["body"] [("font-family", "system-ui, -apple-system, sans-serif"), ("line-height", "1.6"), ("max-width", "900px"), ("margin", "0 auto"), ("padding", "2rem"), ("color", "#333")],
      rule ["h1"] [("border-bottom", "2px solid #333"), ("padding-bottom", "0.5rem"), ("margin-top", "0")],
      rule ["h2"] [("border-bottom", "1px solid #666"), ("padding-bottom", "0.3rem"), ("margin-top", "2rem")],
      rule ["h3", "h4", "h5", "h6"] [("margin-top", "1.5rem")],
      rule ["p"] [("margin", "1rem 0")],
      rule ["code"] [("background", "#f4f4f4"), ("padding", "0.2em 0.4em"), ("border-radius", "3px"), ("font-family", "Consolas, Monaco, Menlo, monospace"), ("font-size", "0.9em")],
      rule ["pre"] [("background", "#f4f4f4"), ("padding", "1rem"), ("border-radius", "5px"), ("overflow-x", "auto"), ("margin", "1rem 0")],
      rule ["pre > code"] [("background", "transparent"), ("padding", "0")],
      rule ["ul", "ol"] [("margin", "1rem 0"), ("padding-left", "2rem")],
      rule ["li"] [("margin", "0.25rem 0")],
      rule ["dl"] [("margin", "1rem 0")],
      rule ["dt"] [("font-weight", "bold"), ("margin-top", "0.5rem")],
      rule ["dd"] [("margin-left", "2rem"), ("margin-bottom", "0.5rem")],
      rule ["table"] [("border-collapse", "collapse"), ("width", "100%"), ("margin", "1rem 0")],
      rule ["th", "td"] [("border", "1px solid #ddd"), ("padding", "0.5rem"), ("text-align", "left")],
      rule ["th"] [("background", "#f4f4f4"), ("font-weight", "bold")],
      rule ["a"] [("color", "#0066cc"), ("text-decoration", "none")],
      rule ["a:hover"] [("text-decoration", "underline")],
      rule ["img"] [("max-width", "100%"), ("height", "auto")],
      rule [".module-header"] [("margin-bottom", "2rem")],
      rule [".module-doc"] [("margin", "1rem 0")],
      rule [".metadata"] [("background", "#f9f9f9"), ("border-left", "4px solid #0066cc"), ("padding", "1rem"), ("margin", "1rem 0")],
      rule [".metadata > dt"] [("display", "inline")],
      rule [".metadata > dd"] [("display", "inline"), ("margin-left", "0.5rem")],
      rule [".exports"] [("margin", "2rem 0")],
      rule [".export-group"] [("margin", "1.5rem 0")],
      rule [".export-group-title"] [("font-weight", "bold"), ("color", "#333"), ("margin-bottom", "0.5rem")],
      rule [".export-list"] [("list-style-type", "none"), ("padding-left", "0")],
      rule [".export-list > li"] [("padding", "0.25rem 0")],
      rule [".items"] [("margin", "2rem 0")],
      rule [".item"] [("margin", "1.5rem 0"), ("padding", "1rem"), ("background", "#fafafa"), ("border-radius", "5px"), ("border-left", "4px solid #ddd")],
      rule [".item-name"] [("font-family", "Consolas, Monaco, Menlo, monospace"), ("font-weight", "bold"), ("font-size", "1.1em"), ("color", "#006600")],
      rule [".item-key"] [("color", "#999"), ("font-size", "0.8em"), ("margin-left", "0.5rem")],
      rule [".item-doc"] [("margin-top", "0.5rem")],
      rule [".item-children"] [("margin-left", "1.5rem"), ("margin-top", "0.5rem"), ("border-left", "2px solid #ddd"), ("padding-left", "1rem")],
      rule [".identifier"] [("color", "#006600"), ("font-family", "Consolas, Monaco, Menlo, monospace")],
      rule [".module-link"] [("color", "#660066"), ("font-family", "Consolas, Monaco, Menlo, monospace")],
      rule [".warning"] [("background", "#fff3cd"), ("border-left", "4px solid #ffc107"), ("padding", "1rem"), ("margin", "1rem 0")],
      rule [".warning-category"] [("font-weight", "bold"), ("color", "#856404")],
      rule [".since"] [("color", "#666"), ("font-size", "0.9em")],
      rule [".examples"] [("background", "#fffef0"), ("border-left", "4px solid #e6db74"), ("padding", "1rem"), ("margin", "1rem 0")],
      rule [".example"] [("margin", "0.5rem 0")],
      rule [".example-expression"] [("font-family", "Consolas, Monaco, Menlo, monospace")],
      rule [".example-result"] [("font-family", "Consolas, Monaco, Menlo, monospace"), ("color", "#666"), ("padding-left", "1rem")],
      rule [".property"] [("background", "#f0f8ff"), ("border-left", "4px solid #4169e1"), ("padding", "1rem"), ("margin", "1rem 0")],
      rule [".math-inline"] [("font-style", "italic")],
      rule [".math-display"] [("display", "block"), ("text-align", "center"), ("margin", "1rem 0"), ("font-style", "italic")],
      rule [".extensions"] [("margin", "1rem 0")],
      rule [".extension"] [("display", "inline-block"), ("margin", "0.25rem"), ("padding", "0.25rem 0.5rem"), ("background", "#e8e8e8"), ("border-radius", "3px"), ("font-family", "Consolas, Monaco, Menlo, monospace"), ("font-size", "0.85em")],
      rule [".extension-disabled"] [("background", "#ffebeb"), ("text-decoration", "line-through")]
    ]

rule :: [String] -> [(String, String)] -> CssItem.Item
rule selectors declarations =
  CssItem.StyleRule $
    CssRule.MkRule
      (fmap (CssSelector.MkSelector . Text.pack) selectors)
      (fmap (\(p, v) -> CssDeclaration.MkDeclaration (CssName.MkName $ Text.pack p) (Text.pack v) False) declarations)

-- Tests

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.describe s "Scrod.Convert.ToHtml" $ do
    Spec.it s "converts empty module to HTML document" $ do
      let m =
            Module.MkModule
              { Module.version = Version.MkVersion (0 NonEmpty.:| [1, 0]),
                Module.language = Nothing,
                Module.extensions = Map.empty,
                Module.documentation = Doc.Empty,
                Module.since = Nothing,
                Module.name = Nothing,
                Module.warning = Nothing,
                Module.exports = Nothing,
                Module.items = []
              }
          doc = toHtml m
          encoded = Builder.toString (Xml.encode doc)
      Spec.assertNe s encoded ""

    Spec.it s "includes module name in title" $ do
      let m =
            Module.MkModule
              { Module.version = Version.MkVersion (0 NonEmpty.:| [1, 0]),
                Module.language = Nothing,
                Module.extensions = Map.empty,
                Module.documentation = Doc.Empty,
                Module.since = Nothing,
                Module.name =
                  Just $
                    Located.MkLocated
                      (Location.MkLocation (Line.MkLine 1) (Column.MkColumn 1))
                      (ModuleName.MkModuleName $ Text.pack "Data.List"),
                Module.warning = Nothing,
                Module.exports = Nothing,
                Module.items = []
              }
          encoded = Builder.toString (Xml.encode (toHtml m))
      Spec.assertEq s (List.isInfixOf "Data.List" encoded) True

    Spec.it s "renders version in metadata" $ do
      let m =
            Module.MkModule
              { Module.version = Version.MkVersion (1 NonEmpty.:| [2, 3]),
                Module.language = Nothing,
                Module.extensions = Map.empty,
                Module.documentation = Doc.Empty,
                Module.since = Nothing,
                Module.name = Nothing,
                Module.warning = Nothing,
                Module.exports = Nothing,
                Module.items = []
              }
          encoded = Builder.toString (Xml.encode (toHtml m))
      Spec.assertEq s (List.isInfixOf "1.2.3" encoded) True

    Spec.it s "renders module documentation" $ do
      let m =
            Module.MkModule
              { Module.version = Version.MkVersion (0 NonEmpty.:| [1]),
                Module.language = Nothing,
                Module.extensions = Map.empty,
                Module.documentation = Doc.String (Text.pack "Hello world"),
                Module.since = Nothing,
                Module.name = Nothing,
                Module.warning = Nothing,
                Module.exports = Nothing,
                Module.items = []
              }
          encoded = Builder.toString (Xml.encode (toHtml m))
      Spec.assertEq s (List.isInfixOf "Hello world" encoded) True

    Spec.it s "renders enabled extension" $ do
      let m =
            Module.MkModule
              { Module.version = Version.MkVersion (0 NonEmpty.:| [1]),
                Module.language = Nothing,
                Module.extensions =
                  Map.fromList
                    [ (Extension.MkExtension (Text.pack "OverloadedStrings"), True)
                    ],
                Module.documentation = Doc.Empty,
                Module.since = Nothing,
                Module.name = Nothing,
                Module.warning = Nothing,
                Module.exports = Nothing,
                Module.items = []
              }
          encoded = Builder.toString (Xml.encode (toHtml m))
      Spec.assertEq s (List.isInfixOf "OverloadedStrings" encoded) True

    Spec.it s "renders disabled extension" $ do
      let m =
            Module.MkModule
              { Module.version = Version.MkVersion (0 NonEmpty.:| [1]),
                Module.language = Nothing,
                Module.extensions =
                  Map.fromList
                    [ (Extension.MkExtension (Text.pack "NoImplicitPrelude"), False)
                    ],
                Module.documentation = Doc.Empty,
                Module.since = Nothing,
                Module.name = Nothing,
                Module.warning = Nothing,
                Module.exports = Nothing,
                Module.items = []
              }
          encoded = Builder.toString (Xml.encode (toHtml m))
      Spec.assertEq s (List.isInfixOf "extension-disabled" encoded) True

    Spec.it s "converts Doc.Paragraph to p element" $ do
      let contents = docToContents (Doc.Paragraph (Doc.String (Text.pack "test")))
          encoded = Builder.toString (foldMap (Content.encode Element.encode) contents)
      Spec.assertEq s encoded "<p>test</p>"

    Spec.it s "converts Doc.Bold to strong element" $ do
      let contents = docToContents (Doc.Bold (Doc.String (Text.pack "bold")))
          encoded = Builder.toString (foldMap (Content.encode Element.encode) contents)
      Spec.assertEq s encoded "<strong>bold</strong>"

    Spec.it s "converts Doc.Emphasis to em element" $ do
      let contents = docToContents (Doc.Emphasis (Doc.String (Text.pack "italic")))
          encoded = Builder.toString (foldMap (Content.encode Element.encode) contents)
      Spec.assertEq s encoded "<em>italic</em>"

    Spec.it s "converts Doc.CodeBlock to pre > code" $ do
      let contents = docToContents (Doc.CodeBlock (Doc.String (Text.pack "x = 1")))
          encoded = Builder.toString (foldMap (Content.encode Element.encode) contents)
      Spec.assertEq s encoded "<pre><code>x = 1</code></pre>"

    Spec.it s "converts Doc.Hyperlink to a element" $ do
      let contents = docToContents (Doc.Hyperlink (Hyperlink.MkHyperlink (Text.pack "https://example.com") Nothing))
          encoded = Builder.toString (foldMap (Content.encode Element.encode) contents)
      Spec.assertEq s encoded "<a href=\"https://example.com\">https://example.com</a>"

    Spec.it s "kindToText returns function for Function" $ do
      Spec.assertEq s (kindToText ItemKind.Function) (Text.pack "function")

    Spec.it s "kindToText returns class for Class" $ do
      Spec.assertEq s (kindToText ItemKind.Class) (Text.pack "class")
