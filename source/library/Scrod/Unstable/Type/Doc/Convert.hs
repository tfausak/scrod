-- TODO: Move this somewhere else. Namespace is too nested. Maybe fold into higher level convert module?
module Scrod.Unstable.Type.Doc.Convert where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Data.Void as Void
import qualified Documentation.Haddock.Parser as Haddock
import qualified Documentation.Haddock.Types as Haddock
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Example as Example
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Hyperlink as Hyperlink
import qualified Scrod.Unstable.Type.Identifier as Identifier
import qualified Scrod.Unstable.Type.ModLink as ModLink
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Namespace as Namespace
import qualified Scrod.Unstable.Type.Picture as Picture
import qualified Scrod.Unstable.Type.Table as Table
import qualified Scrod.Unstable.Type.Table.Cell as Cell
import qualified Scrod.Unstable.Type.Table.Row as Row

-- | Convert Haddock's Namespace to our Namespace.
fromHaddockNamespace :: Haddock.Namespace -> Maybe Namespace.Namespace
fromHaddockNamespace ns = case ns of
  Haddock.Value -> Just Namespace.Value
  Haddock.Type -> Just Namespace.Type
  Haddock.None -> Nothing

-- | Convert a Haddock Identifier to our Identifier.
-- Used with 'Haddock.overIdentifier'.
convertIdentifier :: Haddock.Namespace -> String -> Maybe Identifier.Identifier
convertIdentifier ns str =
  Just
    Identifier.MkIdentifier
      { Identifier.namespace = fromHaddockNamespace ns,
        Identifier.value = Text.pack str
      }

-- | Convert from Haddock's parsed doc to our simplified Doc type.
fromHaddock :: Haddock.DocH Void.Void Identifier.Identifier -> Doc.Doc
fromHaddock doc = case doc of
  Haddock.DocEmpty -> Doc.Empty
  Haddock.DocAppend a b -> Doc.Append (fromHaddock a) (fromHaddock b)
  Haddock.DocString s -> Doc.String (Text.pack s)
  Haddock.DocParagraph d -> Doc.Paragraph (fromHaddock d)
  Haddock.DocIdentifier i -> Doc.Identifier i
  Haddock.DocIdentifierUnchecked v -> Void.absurd v
  Haddock.DocModule ml ->
    Doc.Module
      ModLink.MkModLink
        { ModLink.name = ModuleName.fromString (Haddock.modLinkName ml),
          ModLink.label = fmap fromHaddock (Haddock.modLinkLabel ml)
        }
  Haddock.DocWarning d -> Doc.Warning (fromHaddock d)
  Haddock.DocEmphasis d -> Doc.Emphasis (fromHaddock d)
  Haddock.DocMonospaced d -> Doc.Monospaced (fromHaddock d)
  Haddock.DocBold d -> Doc.Bold (fromHaddock d)
  Haddock.DocUnorderedList ds -> Doc.UnorderedList (fmap fromHaddock ds)
  Haddock.DocOrderedList items -> Doc.OrderedList (fmap (fmap fromHaddock) items)
  Haddock.DocDefList defs -> Doc.DefList (fmap (Bifunctor.bimap fromHaddock fromHaddock) defs)
  Haddock.DocCodeBlock d -> Doc.CodeBlock (fromHaddock d)
  Haddock.DocHyperlink h ->
    Doc.Hyperlink
      Hyperlink.MkHyperlink
        { Hyperlink.url = Text.pack (Haddock.hyperlinkUrl h),
          Hyperlink.label = fmap fromHaddock (Haddock.hyperlinkLabel h)
        }
  Haddock.DocPic p ->
    Doc.Pic
      Picture.MkPicture
        { Picture.uri = Text.pack (Haddock.pictureUri p),
          Picture.title = fmap Text.pack (Haddock.pictureTitle p)
        }
  Haddock.DocMathInline s -> Doc.MathInline (Text.pack s)
  Haddock.DocMathDisplay s -> Doc.MathDisplay (Text.pack s)
  Haddock.DocAName s -> Doc.AName (Text.pack s)
  Haddock.DocProperty s -> Doc.Property (Text.pack s)
  Haddock.DocExamples es ->
    Doc.Examples
      ( fmap
          ( \e ->
              Example.MkExample
                { Example.expression = Text.pack (Haddock.exampleExpression e),
                  Example.result = fmap Text.pack (Haddock.exampleResult e)
                }
          )
          es
      )
  Haddock.DocHeader h ->
    Doc.Header
      Header.MkHeader
        { Header.level = fromIntegral (Haddock.headerLevel h),
          Header.title = fromHaddock (Haddock.headerTitle h)
        }
  Haddock.DocTable t ->
    Doc.Table
      Table.MkTable
        { Table.headerRows = fmap convertTableRow (Haddock.tableHeaderRows t),
          Table.bodyRows = fmap convertTableRow (Haddock.tableBodyRows t)
        }
    where
      convertTableRow :: Haddock.TableRow (Haddock.DocH Void.Void Identifier.Identifier) -> Row.Row Doc.Doc
      convertTableRow row =
        Row.MkRow
          { Row.cells = fmap convertTableCell (Haddock.tableRowCells row)
          }

      convertTableCell :: Haddock.TableCell (Haddock.DocH Void.Void Identifier.Identifier) -> Cell.Cell Doc.Doc
      convertTableCell cell =
        Cell.MkCell
          { Cell.colspan = fromIntegral (Haddock.tableCellColspan cell),
            Cell.rowspan = fromIntegral (Haddock.tableCellRowspan cell),
            Cell.contents = fromHaddock (Haddock.tableCellContents cell)
          }

-- | Parse and convert documentation string to our Doc type.
parseDoc :: String -> Doc.Doc
parseDoc input =
  let metaDoc :: Haddock.MetaDoc Void.Void Haddock.Identifier
      metaDoc = Haddock.parseParas Nothing input
      haddockDoc :: Haddock.DocH Void.Void Haddock.Identifier
      haddockDoc = Haddock._doc metaDoc
      withIdentifiers :: Haddock.DocH Void.Void Identifier.Identifier
      withIdentifiers = Haddock.overIdentifier convertIdentifier haddockDoc
   in fromHaddock withIdentifiers
