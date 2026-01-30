{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.HtmlExport where

import qualified Data.Text as Text
import qualified Lucid
import qualified Scrod.Unstable.Type.Category as Category
import qualified Scrod.Unstable.Type.Export as Export
import qualified Scrod.Unstable.Type.ExportIdentifier as ExportIdentifier
import qualified Scrod.Unstable.Type.ExportName as ExportName
import qualified Scrod.Unstable.Type.ExportNameKind as ExportNameKind
import qualified Scrod.Unstable.Type.Header as Header
import qualified Scrod.Unstable.Type.Html as Html
import qualified Scrod.Unstable.Type.HtmlDoc as HtmlDoc
import qualified Scrod.Unstable.Type.Level as Level
import qualified Scrod.Unstable.Type.Section as Section
import qualified Scrod.Unstable.Type.Subordinates as Subordinates
import qualified Scrod.Unstable.Type.Warning as Warning

-- | Convert an Export to HTML.
toHtml :: Export.Export -> Html.Html
toHtml export = case export of
  Export.Identifier ident -> exportIdentifierToHtml ident
  Export.Group section -> sectionToHtml section
  Export.Doc doc -> Lucid.div_ [Lucid.class_ "export-doc"] (HtmlDoc.toHtml doc)
  Export.DocNamed name -> Lucid.div_ [Lucid.class_ "export-doc-named"] (Lucid.toHtml $ "§" <> name)

exportIdentifierToHtml :: ExportIdentifier.ExportIdentifier -> Html.Html
exportIdentifierToHtml (ExportIdentifier.MkExportIdentifier name subs maybeWarning maybeDoc) =
  Lucid.div_ [Lucid.class_ "export-item"] $
    foldMap warningToHtml maybeWarning
      <> Lucid.code_ [Lucid.class_ "export-name"] (exportNameToHtml name <> subordinatesToHtml subs)
      <> foldMap (Lucid.div_ [Lucid.class_ "export-doc"] . HtmlDoc.toHtml) maybeDoc

exportNameToHtml :: ExportName.ExportName -> Html.Html
exportNameToHtml (ExportName.MkExportName maybeKind name) =
  Lucid.toHtml $ kindPrefix <> name
  where
    kindPrefix :: Text.Text
    kindPrefix = case maybeKind of
      Nothing -> ""
      Just ExportNameKind.Pattern -> "pattern "
      Just ExportNameKind.Type -> "type "
      Just ExportNameKind.Module -> "module "

subordinatesToHtml :: Maybe Subordinates.Subordinates -> Html.Html
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

sectionToHtml :: Section.Section -> Html.Html
sectionToHtml (Section.MkSection (Header.MkHeader level title)) =
  Lucid.div_ [Lucid.class_ "export-group"] $
    levelToElement level (HtmlDoc.toHtml title)
  where
    levelToElement :: Level.Level -> Html.Html -> Html.Html
    levelToElement l = case l of
      Level.One -> Lucid.h3_ [Lucid.class_ "export-group-title"]
      Level.Two -> Lucid.h4_ [Lucid.class_ "export-group-title"]
      Level.Three -> Lucid.h5_ [Lucid.class_ "export-group-title"]
      Level.Four -> Lucid.h6_ [Lucid.class_ "export-group-title"]
      Level.Five -> Lucid.h6_ [Lucid.class_ "export-group-title"]
      Level.Six -> Lucid.h6_ [Lucid.class_ "export-group-title"]

warningToHtml :: Warning.Warning -> Html.Html
warningToHtml (Warning.MkWarning (Category.MkCategory cat) val) =
  Lucid.div_ [Lucid.class_ "warning"] $
    Lucid.span_ [Lucid.class_ "warning-category"] (Lucid.toHtml cat)
      <> Lucid.toHtml (": " :: Text.Text)
      <> Lucid.toHtml val
