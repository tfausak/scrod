{-# LANGUAGE OverloadedStrings #-}

module Scrod.Html.Export where

import qualified Data.Text as Text
import qualified Lucid
import qualified Scrod.Html.Doc as HtmlDoc
import qualified Scrod.Type.Category as Category
import qualified Scrod.Type.Export as Export
import qualified Scrod.Type.ExportIdentifier as ExportIdentifier
import qualified Scrod.Type.ExportName as ExportName
import qualified Scrod.Type.ExportNameKind as ExportNameKind
import qualified Scrod.Type.Header as Header
import qualified Scrod.Type.Level as Level
import qualified Scrod.Type.Section as Section
import qualified Scrod.Type.Subordinates as Subordinates
import qualified Scrod.Type.Warning as Warning

-- | Convert an Export to HTML.
toHtml :: Export.Export -> Lucid.Html ()
toHtml export = case export of
  Export.Identifier ident -> exportIdentifierToHtml ident
  Export.Group section -> sectionToHtml section
  Export.Doc doc -> Lucid.div_ [Lucid.class_ "export-doc"] (HtmlDoc.toHtml doc)
  Export.DocNamed name -> Lucid.div_ [Lucid.class_ "export-doc-named"] (Lucid.toHtml $ "§" <> name)

exportIdentifierToHtml :: ExportIdentifier.ExportIdentifier -> Lucid.Html ()
exportIdentifierToHtml (ExportIdentifier.MkExportIdentifier name subs maybeWarning maybeDoc) =
  Lucid.div_ [Lucid.class_ "export-item"] $
    foldMap warningToHtml maybeWarning
      <> Lucid.code_ [Lucid.class_ "export-name"] (exportNameToHtml name <> subordinatesToHtml subs)
      <> foldMap (Lucid.div_ [Lucid.class_ "export-doc"] . HtmlDoc.toHtml) maybeDoc

exportNameToHtml :: ExportName.ExportName -> Lucid.Html ()
exportNameToHtml (ExportName.MkExportName maybeKind name) =
  Lucid.toHtml $ kindPrefix <> name
  where
    kindPrefix :: Text.Text
    kindPrefix = case maybeKind of
      Nothing -> ""
      Just ExportNameKind.Pattern -> "pattern "
      Just ExportNameKind.Type -> "type "
      Just ExportNameKind.Module -> "module "

subordinatesToHtml :: Maybe Subordinates.Subordinates -> Lucid.Html ()
subordinatesToHtml maybeSubs = case maybeSubs of
  Nothing -> mempty
  Just (Subordinates.MkSubordinates wildcard explicit) ->
    let wildcardText :: Text.Text
        wildcardText = if wildcard then ".." else ""
        explicitTexts :: [Text.Text]
        explicitTexts = fmap (\(ExportName.MkExportName _ n) -> n) explicit
        allTexts :: [Text.Text]
        allTexts = if wildcard then wildcardText : explicitTexts else explicitTexts
        combined :: Text.Text
        combined = Text.intercalate ", " allTexts
     in Lucid.toHtml $ "(" <> combined <> ")"

sectionToHtml :: Section.Section -> Lucid.Html ()
sectionToHtml (Section.MkSection (Header.MkHeader level title)) =
  Lucid.div_ [Lucid.class_ "export-group"] $
    levelToElement level (HtmlDoc.toHtml title)
  where
    levelToElement :: Level.Level -> Lucid.Html () -> Lucid.Html ()
    levelToElement l = case l of
      Level.One -> Lucid.h3_ [Lucid.class_ "export-group-title"]
      Level.Two -> Lucid.h4_ [Lucid.class_ "export-group-title"]
      Level.Three -> Lucid.h5_ [Lucid.class_ "export-group-title"]
      Level.Four -> Lucid.h6_ [Lucid.class_ "export-group-title"]
      Level.Five -> Lucid.h6_ [Lucid.class_ "export-group-title"]
      Level.Six -> Lucid.h6_ [Lucid.class_ "export-group-title"]

warningToHtml :: Warning.Warning -> Lucid.Html ()
warningToHtml (Warning.MkWarning (Category.MkCategory cat) val) =
  Lucid.div_ [Lucid.class_ "warning"] $
    Lucid.span_ [Lucid.class_ "warning-category"] (Lucid.toHtml cat)
      <> Lucid.toHtml (": " :: Text.Text)
      <> Lucid.toHtml val
