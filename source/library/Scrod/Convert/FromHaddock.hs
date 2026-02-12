{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Convert Haddock's documentation AST (@DocH@) into Scrod's 'Doc.Doc' type.
--
-- The Haddock parser produces @DocH Void Identifier@ nodes; this module
-- maps each node kind (paragraphs, code blocks, identifiers, hyperlinks,
-- examples, tables, etc.) to the corresponding @Scrod.Core.*@ constructors.
module Scrod.Convert.FromHaddock where

import qualified Data.Text as Text
import qualified Data.Void as Void
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified Scrod.Core.Definition as Definition
import qualified Scrod.Core.Doc as Doc
import qualified Scrod.Core.Example as Example
import qualified Scrod.Core.Header as Header
import qualified Scrod.Core.Hyperlink as Hyperlink
import qualified Scrod.Core.Identifier as Identifier
import qualified Scrod.Core.Level as Level
import qualified Scrod.Core.ModLink as ModLink
import qualified Scrod.Core.ModuleName as ModuleName
import qualified Scrod.Core.Namespace as Namespace
import qualified Scrod.Core.NumberedItem as NumberedItem
import qualified Scrod.Core.Picture as Picture
import qualified Scrod.Core.Table as Table
import qualified Scrod.Core.TableCell as TableCell
import qualified Scrod.Spec as Spec

fromHaddock :: Haddock.DocH Void.Void Haddock.Identifier -> Doc.Doc
fromHaddock = convertDoc . Haddock.overIdentifier convertIdentifier

convertIdentifier :: Haddock.Namespace -> String -> Maybe Identifier.Identifier
convertIdentifier ns str =
  Just
    Identifier.MkIdentifier
      { Identifier.namespace = convertNamespace ns,
        Identifier.value = stripQualifier $ Text.pack str
      }

-- | Strips the module qualifier from an identifier. For example, @A.b@
-- becomes @b@ and @Data.List.map@ becomes @map@.
stripQualifier :: Text.Text -> Text.Text
stripQualifier t =
  let (_, name) = Text.breakOnEnd (Text.singleton '.') t
   in if Text.null name then t else name

convertNamespace :: Haddock.Namespace -> Maybe Namespace.Namespace
convertNamespace ns = case ns of
  Haddock.Value -> Just Namespace.Value
  Haddock.Type -> Just Namespace.Type
  Haddock.None -> Nothing

convertDoc :: Haddock.DocH Void.Void Identifier.Identifier -> Doc.Doc
convertDoc doc = case doc of
  Haddock.DocEmpty -> Doc.Empty
  Haddock.DocAppend a b -> Doc.Append [convertDoc a, convertDoc b]
  Haddock.DocString s -> Doc.String $ Text.pack s
  Haddock.DocParagraph d -> Doc.Paragraph $ convertDoc d
  Haddock.DocIdentifier i -> Doc.Identifier i
  Haddock.DocModule ml ->
    Doc.Module
      ModLink.MkModLink
        { ModLink.name = ModuleName.MkModuleName . Text.pack $ Haddock.modLinkName ml,
          ModLink.label = convertDoc <$> Haddock.modLinkLabel ml
        }
  Haddock.DocWarning _ -> Doc.Empty -- `DocWarning` is never found in markup.
  Haddock.DocEmphasis d -> Doc.Emphasis $ convertDoc d
  Haddock.DocMonospaced d -> Doc.Monospaced $ convertDoc d
  Haddock.DocBold d -> Doc.Bold $ convertDoc d
  Haddock.DocUnorderedList ds -> Doc.UnorderedList $ fmap convertDoc ds
  Haddock.DocOrderedList items -> Doc.OrderedList $ fmap (\(n, d) -> NumberedItem.MkNumberedItem {NumberedItem.index = n, NumberedItem.item = convertDoc d}) items
  Haddock.DocDefList defs -> Doc.DefList $ fmap (\(t, d) -> Definition.MkDefinition {Definition.term = convertDoc t, Definition.definition = convertDoc d}) defs
  Haddock.DocCodeBlock d -> Doc.CodeBlock $ convertDoc d
  Haddock.DocHyperlink h ->
    Doc.Hyperlink
      Hyperlink.MkHyperlink
        { Hyperlink.url = Text.pack $ Haddock.hyperlinkUrl h,
          Hyperlink.label = convertDoc <$> Haddock.hyperlinkLabel h
        }
  Haddock.DocPic p ->
    Doc.Pic
      Picture.MkPicture
        { Picture.uri = Text.pack $ Haddock.pictureUri p,
          Picture.title = Text.pack <$> Haddock.pictureTitle p
        }
  Haddock.DocMathInline s -> Doc.MathInline $ Text.pack s
  Haddock.DocMathDisplay s -> Doc.MathDisplay $ Text.pack s
  Haddock.DocAName s -> Doc.AName $ Text.pack s
  Haddock.DocProperty s -> Doc.Property $ Text.pack s
  Haddock.DocExamples es ->
    Doc.Examples $
      fmap
        ( \e ->
            Example.MkExample
              { Example.expression = Text.pack $ Haddock.exampleExpression e,
                Example.result = Text.pack <$> Haddock.exampleResult e
              }
        )
        es
  Haddock.DocHeader h ->
    Doc.Header
      Header.MkHeader
        { Header.level = convertLevel $ Haddock.headerLevel h,
          Header.title = convertDoc $ Haddock.headerTitle h
        }
  Haddock.DocTable t ->
    Doc.Table
      Table.MkTable
        { Table.headerRows = convertTableRow <$> Haddock.tableHeaderRows t,
          Table.bodyRows = convertTableRow <$> Haddock.tableBodyRows t
        }

convertLevel :: Int -> Level.Level
convertLevel n = case n of
  1 -> Level.One
  2 -> Level.Two
  3 -> Level.Three
  4 -> Level.Four
  5 -> Level.Five
  6 -> Level.Six
  _ -> Level.One -- Default to level one if out of range.

convertTableRow :: Haddock.TableRow (Haddock.DocH Void.Void Identifier.Identifier) -> [TableCell.Cell Doc.Doc]
convertTableRow = fmap convertTableCell . Haddock.tableRowCells

convertTableCell :: Haddock.TableCell (Haddock.DocH Void.Void Identifier.Identifier) -> TableCell.Cell Doc.Doc
convertTableCell cell =
  TableCell.MkCell
    { TableCell.colspan = fromIntegral $ Haddock.tableCellColspan cell,
      TableCell.rowspan = fromIntegral $ Haddock.tableCellRowspan cell,
      TableCell.contents = convertDoc $ Haddock.tableCellContents cell
    }

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'fromHaddock $ do
    Spec.it s "works with empty" $ do
      Spec.assertEq s (fromHaddock Haddock.DocEmpty) Doc.Empty

    Spec.it s "works with append" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocAppend (Haddock.DocString "a") (Haddock.DocString "b")
      let expected = Doc.Append [Doc.String $ Text.pack "a", Doc.String $ Text.pack "b"]
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with string" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocString "hello"
      Spec.assertEq s (fromHaddock input) (Doc.String $ Text.pack "hello")

    Spec.it s "works with paragraph" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocParagraph (Haddock.DocString "text")
      let expected = Doc.Paragraph (Doc.String $ Text.pack "text")
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with identifier" $ do
      Spec.assertEq s (fromHaddock . Haddock._doc $ Haddock.parseParas Nothing "'x'") . Doc.Paragraph . Doc.Identifier . Identifier.MkIdentifier Nothing $ Text.pack "x"

    Spec.it s "works with value identifier" $ do
      Spec.assertEq s (fromHaddock . Haddock._doc $ Haddock.parseParas Nothing "v'y'") . Doc.Paragraph . Doc.Identifier . Identifier.MkIdentifier (Just Namespace.Value) $ Text.pack "y"

    Spec.it s "works with type identifier" $ do
      Spec.assertEq s (fromHaddock . Haddock._doc $ Haddock.parseParas Nothing "t'z'") . Doc.Paragraph . Doc.Identifier . Identifier.MkIdentifier (Just Namespace.Type) $ Text.pack "z"

    Spec.it s "strips module qualifier from identifier" $ do
      Spec.assertEq s (fromHaddock . Haddock._doc $ Haddock.parseParas Nothing "'A.b'") . Doc.Paragraph . Doc.Identifier . Identifier.MkIdentifier Nothing $ Text.pack "b"

    Spec.it s "strips deep module qualifier from identifier" $ do
      Spec.assertEq s (fromHaddock . Haddock._doc $ Haddock.parseParas Nothing "'Data.List.map'") . Doc.Paragraph . Doc.Identifier . Identifier.MkIdentifier Nothing $ Text.pack "map"

    Spec.it s "works with module" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocModule Haddock.ModLink {Haddock.modLinkName = "Data.List", Haddock.modLinkLabel = Nothing}
      let expected = Doc.Module ModLink.MkModLink {ModLink.name = ModuleName.MkModuleName $ Text.pack "Data.List", ModLink.label = Nothing}
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with emphasis" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocEmphasis (Haddock.DocString "em")
      let expected = Doc.Emphasis (Doc.String $ Text.pack "em")
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with monospaced" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocMonospaced (Haddock.DocString "code")
      let expected = Doc.Monospaced (Doc.String $ Text.pack "code")
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with bold" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocBold (Haddock.DocString "strong")
      let expected = Doc.Bold (Doc.String $ Text.pack "strong")
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with unordered list" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocUnorderedList [Haddock.DocString "item1", Haddock.DocString "item2"]
      let expected = Doc.UnorderedList [Doc.String $ Text.pack "item1", Doc.String $ Text.pack "item2"]
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with ordered list" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocOrderedList [(1, Haddock.DocString "first"), (2, Haddock.DocString "second")]
      let expected =
            Doc.OrderedList
              [ NumberedItem.MkNumberedItem {NumberedItem.index = 1, NumberedItem.item = Doc.String $ Text.pack "first"},
                NumberedItem.MkNumberedItem {NumberedItem.index = 2, NumberedItem.item = Doc.String $ Text.pack "second"}
              ]
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with definition list" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocDefList [(Haddock.DocString "term", Haddock.DocString "def")]
      let expected =
            Doc.DefList
              [Definition.MkDefinition {Definition.term = Doc.String $ Text.pack "term", Definition.definition = Doc.String $ Text.pack "def"}]
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with code block" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocCodeBlock (Haddock.DocString "x = 1")
      let expected = Doc.CodeBlock (Doc.String $ Text.pack "x = 1")
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with hyperlink" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocHyperlink Haddock.Hyperlink {Haddock.hyperlinkUrl = "https://example.com", Haddock.hyperlinkLabel = Nothing}
      let expected = Doc.Hyperlink Hyperlink.MkHyperlink {Hyperlink.url = Text.pack "https://example.com", Hyperlink.label = Nothing}
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with picture" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocPic Haddock.Picture {Haddock.pictureUri = "image.png", Haddock.pictureTitle = Just "title"}
      let expected = Doc.Pic Picture.MkPicture {Picture.uri = Text.pack "image.png", Picture.title = Just $ Text.pack "title"}
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with math inline" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocMathInline "x^2"
      Spec.assertEq s (fromHaddock input) (Doc.MathInline $ Text.pack "x^2")

    Spec.it s "works with math display" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocMathDisplay "\\sum"
      Spec.assertEq s (fromHaddock input) (Doc.MathDisplay $ Text.pack "\\sum")

    Spec.it s "works with anchor name" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocAName "anchor"
      Spec.assertEq s (fromHaddock input) (Doc.AName $ Text.pack "anchor")

    Spec.it s "works with property" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocProperty "prop> x + y"
      Spec.assertEq s (fromHaddock input) (Doc.Property $ Text.pack "prop> x + y")

    Spec.it s "works with examples" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocExamples [Haddock.Example {Haddock.exampleExpression = "1 + 1", Haddock.exampleResult = ["2"]}]
      let expected = Doc.Examples [Example.MkExample {Example.expression = Text.pack "1 + 1", Example.result = [Text.pack "2"]}]
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with header" $ do
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocHeader Haddock.Header {Haddock.headerLevel = 2, Haddock.headerTitle = Haddock.DocString "Section"}
      let expected = Doc.Header Header.MkHeader {Header.level = Level.Two, Header.title = Doc.String $ Text.pack "Section"}
      Spec.assertEq s (fromHaddock input) expected

    Spec.it s "works with table" $ do
      let cell :: Haddock.TableCell (Haddock.DocH Void.Void Haddock.Identifier)
          cell = Haddock.TableCell {Haddock.tableCellColspan = 1, Haddock.tableCellRowspan = 1, Haddock.tableCellContents = Haddock.DocString "data"}
      let row :: Haddock.TableRow (Haddock.DocH Void.Void Haddock.Identifier)
          row = Haddock.TableRow [cell]
      let input :: Haddock.DocH Void.Void Haddock.Identifier
          input = Haddock.DocTable Haddock.Table {Haddock.tableHeaderRows = [], Haddock.tableBodyRows = [row]}
      let expectedCell = TableCell.MkCell {TableCell.colspan = 1, TableCell.rowspan = 1, TableCell.contents = Doc.String $ Text.pack "data"}
      let expected = Doc.Table Table.MkTable {Table.headerRows = [], Table.bodyRows = [[expectedCell]]}
      Spec.assertEq s (fromHaddock input) expected
