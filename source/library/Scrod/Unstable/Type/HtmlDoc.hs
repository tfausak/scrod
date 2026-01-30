{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.HtmlDoc where

import qualified Data.Maybe
import qualified Data.Text as Text
import qualified Lucid
import qualified Numeric.Natural as Natural
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Example as Example
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Hyperlink as Hyperlink
import qualified Scrod.Unstable.Type.Identifier as Identifier
import qualified Scrod.Unstable.Type.Level as Level
import qualified Scrod.Unstable.Type.ModLink as ModLink
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.Namespace as Namespace
import qualified Scrod.Unstable.Type.Picture as Picture
import qualified Scrod.Unstable.Type.Table as Table
import qualified Scrod.Unstable.Type.TableCell as TableCell

-- | Convert a Doc to HTML.
toHtml :: Doc.Doc -> Lucid.Html ()
toHtml doc = case doc of
  Doc.Empty -> mempty
  Doc.Append d1 d2 -> toHtml d1 <> toHtml d2
  Doc.String t -> Lucid.toHtml t
  Doc.Paragraph d -> Lucid.p_ (toHtml d)
  Doc.Identifier i -> identifierToHtml i
  Doc.Module m -> modLinkToHtml m
  Doc.Emphasis d -> Lucid.em_ (toHtml d)
  Doc.Monospaced d -> Lucid.code_ (toHtml d)
  Doc.Bold d -> Lucid.strong_ (toHtml d)
  Doc.UnorderedList items -> unorderedListToHtml items
  Doc.OrderedList items -> orderedListToHtml items
  Doc.DefList defs -> defListToHtml defs
  Doc.CodeBlock d -> codeBlockToHtml d
  Doc.Hyperlink h -> hyperlinkToHtml h
  Doc.Pic p -> pictureToHtml p
  Doc.MathInline t -> Lucid.span_ [Lucid.class_ "math-inline"] (Lucid.toHtml t)
  Doc.MathDisplay t -> Lucid.div_ [Lucid.class_ "math-display"] (Lucid.toHtml t)
  Doc.AName t -> Lucid.a_ [Lucid.id_ t] mempty
  Doc.Property t -> Lucid.pre_ [Lucid.class_ "property"] (Lucid.toHtml t)
  Doc.Examples es -> examplesToHtml es
  Doc.Header h -> headerToHtml h
  Doc.Table t -> tableToHtml t

identifierToHtml :: Identifier.Identifier -> Lucid.Html ()
identifierToHtml (Identifier.MkIdentifier ns val) =
  Lucid.code_ [Lucid.class_ "identifier"] (Lucid.toHtml $ prefix <> val)
  where
    prefix = case ns of
      Nothing -> Text.empty
      Just Namespace.Value -> "v'"
      Just Namespace.Type -> "t'"

modLinkToHtml :: ModLink.ModLink Doc.Doc -> Lucid.Html ()
modLinkToHtml (ModLink.MkModLink (ModuleName.MkModuleName modName) maybeLabel) =
  Lucid.code_ [Lucid.class_ "module-link"] $
    maybe (Lucid.toHtml modName) toHtml maybeLabel

unorderedListToHtml :: [Doc.Doc] -> Lucid.Html ()
unorderedListToHtml items =
  Lucid.ul_ $ foldMap (Lucid.li_ . toHtml) items

orderedListToHtml :: [(Int, Doc.Doc)] -> Lucid.Html ()
orderedListToHtml items =
  case items of
    [] -> mempty
    ((start, _) : _) ->
      Lucid.ol_
        [Lucid.start_ (Text.pack $ show start) | start /= 1]
        $ foldMap (Lucid.li_ . toHtml . snd) items

defListToHtml :: [(Doc.Doc, Doc.Doc)] -> Lucid.Html ()
defListToHtml defs =
  Lucid.dl_ $
    foldMap
      (\(term, def) -> Lucid.dt_ (toHtml term) <> Lucid.dd_ (toHtml def))
      defs

codeBlockToHtml :: Doc.Doc -> Lucid.Html ()
codeBlockToHtml d =
  Lucid.pre_ $ Lucid.code_ (toHtml d)

hyperlinkToHtml :: Hyperlink.Hyperlink Doc.Doc -> Lucid.Html ()
hyperlinkToHtml (Hyperlink.MkHyperlink url maybeLabel) =
  Lucid.a_ [Lucid.href_ url] $
    maybe (Lucid.toHtml url) toHtml maybeLabel

pictureToHtml :: Picture.Picture -> Lucid.Html ()
pictureToHtml (Picture.MkPicture uri maybeTitle) =
  Lucid.img_ $
    [Lucid.src_ uri, Lucid.alt_ (Data.Maybe.fromMaybe "" maybeTitle)]
      <> foldMap (\t -> [Lucid.title_ t]) maybeTitle

examplesToHtml :: [Example.Example] -> Lucid.Html ()
examplesToHtml examples =
  Lucid.div_ [Lucid.class_ "examples"] $
    foldMap exampleToHtml examples

exampleToHtml :: Example.Example -> Lucid.Html ()
exampleToHtml (Example.MkExample expr results) =
  Lucid.div_ [Lucid.class_ "example"] $
    Lucid.div_ [Lucid.class_ "example-expression"] (Lucid.toHtml expr)
      <> foldMap
        (Lucid.div_ [Lucid.class_ "example-result"] . Lucid.toHtml)
        results

headerToHtml :: Header.Header Doc.Doc -> Lucid.Html ()
headerToHtml (Header.MkHeader level title) =
  levelToElement level (toHtml title)

levelToElement :: Level.Level -> Lucid.Html () -> Lucid.Html ()
levelToElement level = case level of
  Level.One -> Lucid.h1_
  Level.Two -> Lucid.h2_
  Level.Three -> Lucid.h3_
  Level.Four -> Lucid.h4_
  Level.Five -> Lucid.h5_
  Level.Six -> Lucid.h6_

tableToHtml :: Table.Table Doc.Doc -> Lucid.Html ()
tableToHtml (Table.MkTable headerRows bodyRows) =
  Lucid.table_ $
    (if null headerRows then mempty else Lucid.thead_ (foldMap headerRowToHtml headerRows))
      <> (if null bodyRows then mempty else Lucid.tbody_ (foldMap bodyRowToHtml bodyRows))
  where
    headerRowToHtml :: [TableCell.Cell Doc.Doc] -> Lucid.Html ()
    headerRowToHtml cells = Lucid.tr_ $ foldMap headerCellToHtml cells

    bodyRowToHtml :: [TableCell.Cell Doc.Doc] -> Lucid.Html ()
    bodyRowToHtml cells = Lucid.tr_ $ foldMap bodyCellToHtml cells

    headerCellToHtml :: TableCell.Cell Doc.Doc -> Lucid.Html ()
    headerCellToHtml (TableCell.MkCell colspan rowspan contents) =
      Lucid.th_ (cellAttrs colspan rowspan) (toHtml contents)

    bodyCellToHtml :: TableCell.Cell Doc.Doc -> Lucid.Html ()
    bodyCellToHtml (TableCell.MkCell colspan rowspan contents) =
      Lucid.td_ (cellAttrs colspan rowspan) (toHtml contents)

    cellAttrs :: Natural.Natural -> Natural.Natural -> [Lucid.Attributes]
    cellAttrs c r =
      [Lucid.colspan_ (Text.pack $ show c) | c /= 1]
        <> [Lucid.rowspan_ (Text.pack $ show r) | r /= 1]
