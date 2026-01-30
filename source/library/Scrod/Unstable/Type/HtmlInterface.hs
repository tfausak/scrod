{-# LANGUAGE OverloadedStrings #-}

module Scrod.Unstable.Type.HtmlInterface where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Lucid
import qualified Scrod.Unstable.Type.Category as Category
import qualified Scrod.Unstable.Type.Css as Css
import qualified Scrod.Unstable.Type.Doc as Doc
import qualified Scrod.Unstable.Type.Export as Export
import qualified Scrod.Unstable.Type.Extension as Extension
import qualified Scrod.Unstable.Type.Html as Html
import qualified Scrod.Unstable.Type.HtmlDoc as HtmlDoc
import qualified Scrod.Unstable.Type.HtmlExport as HtmlExport
import qualified Scrod.Unstable.Type.HtmlItem as HtmlItem
import qualified Scrod.Unstable.Type.Interface as Interface
import qualified Scrod.Unstable.Type.Item as Item
import qualified Scrod.Unstable.Type.Language as Language
import qualified Scrod.Unstable.Type.Located as Located
import qualified Scrod.Unstable.Type.ModuleName as ModuleName
import qualified Scrod.Unstable.Type.PackageName as PackageName
import qualified Scrod.Unstable.Type.Since as Since
import qualified Scrod.Unstable.Type.Version as Version
import qualified Scrod.Unstable.Type.Warning as Warning

-- | Convert an Interface to a complete HTML document.
toHtml :: Interface.Interface -> Html.Html
toHtml interface =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      Lucid.meta_ [Lucid.charset_ "utf-8"]
      Lucid.meta_ [Lucid.name_ "viewport", Lucid.content_ "width=device-width, initial-scale=1"]
      Lucid.title_ (Lucid.toHtml $ moduleTitle interface)
      Lucid.style_ Css.stylesheet
    Lucid.body_ $ do
      headerSection interface
      metadataSection interface
      exportsSection (Interface.exports interface)
      extensionsSection (Interface.extensions interface)
      itemsSection (Interface.items interface)

moduleTitle :: Interface.Interface -> Text.Text
moduleTitle interface = case Interface.name interface of
  Nothing -> "Scrod Documentation"
  Just (Located.MkLocated _ (ModuleName.MkModuleName name)) -> name

headerSection :: Interface.Interface -> Html.Html
headerSection interface =
  Lucid.header_ [Lucid.class_ "module-header"] $ do
    Lucid.h1_ (Lucid.toHtml $ moduleTitle interface)
    warningHtml
    moduleDocHtml
  where
    warningHtml :: Html.Html
    warningHtml = case Interface.warning interface of
      Nothing -> mempty
      Just (Warning.MkWarning (Category.MkCategory cat) val) ->
        Lucid.div_ [Lucid.class_ "warning"] $ do
          Lucid.span_ [Lucid.class_ "warning-category"] (Lucid.toHtml cat)
          Lucid.toHtml (": " :: Text.Text)
          Lucid.toHtml val

    moduleDocHtml :: Html.Html
    moduleDocHtml = case Interface.documentation interface of
      Doc.Empty -> mempty
      doc -> Lucid.div_ [Lucid.class_ "module-doc"] (HtmlDoc.toHtml doc)

metadataSection :: Interface.Interface -> Html.Html
metadataSection interface =
  Lucid.section_ [Lucid.class_ "metadata"] $
    Lucid.dl_ (versionItem <> languageItem <> sinceItem)
  where
    versionItem :: Html.Html
    versionItem = do
      Lucid.dt_ "Scrod version"
      Lucid.dd_ (Lucid.toHtml $ versionToText (Interface.version interface))

    languageItem :: Html.Html
    languageItem = case Interface.language interface of
      Nothing -> mempty
      Just (Language.MkLanguage lang) -> do
        Lucid.dt_ "Language"
        Lucid.dd_ (Lucid.toHtml lang)

    sinceItem :: Html.Html
    sinceItem = case Interface.since interface of
      Nothing -> mempty
      Just since -> do
        Lucid.dt_ "Since"
        Lucid.dd_ [Lucid.class_ "since"] (sinceToHtml since)

versionToText :: Version.Version -> Text.Text
versionToText (Version.MkVersion parts) =
  Text.intercalate "." . fmap (Text.pack . show) $ NonEmpty.toList parts

sinceToHtml :: Since.Since -> Html.Html
sinceToHtml (Since.MkSince maybePackage version) =
  packageHtml <> Lucid.toHtml (versionToText version)
  where
    packageHtml :: Html.Html
    packageHtml = case maybePackage of
      Nothing -> mempty
      Just (PackageName.MkPackageName pkg) -> Lucid.toHtml (pkg <> "-")

exportsSection :: Maybe [Export.Export] -> Html.Html
exportsSection maybeExports = case maybeExports of
  Nothing -> mempty
  Just [] -> mempty
  Just exports ->
    Lucid.section_ [Lucid.class_ "exports"] $ do
      Lucid.h2_ "Exports"
      Lucid.ul_ [Lucid.class_ "export-list"] (foldMap exportToListItem exports)
  where
    exportToListItem :: Export.Export -> Html.Html
    exportToListItem export = Lucid.li_ (HtmlExport.toHtml export)

extensionsSection :: Map.Map Extension.Extension Bool -> Html.Html
extensionsSection extensions
  | Map.null extensions = mempty
  | otherwise =
      Lucid.section_ [Lucid.class_ "extensions"] $ do
        Lucid.h2_ "Extensions"
        Lucid.div_ [Lucid.class_ "extension-list"] (foldMap extToHtml $ Map.toList extensions)
  where
    extToHtml :: (Extension.Extension, Bool) -> Html.Html
    extToHtml (Extension.MkExtension name, enabled) =
      let cls :: Text.Text
          cls = if enabled then "extension" else "extension extension-disabled"
       in Lucid.span_ [Lucid.class_ cls] (Lucid.toHtml name) <> " "

itemsSection :: [Located.Located Item.Item] -> Html.Html
itemsSection [] = mempty
itemsSection items =
  Lucid.section_ [Lucid.class_ "items"] $ do
    Lucid.h2_ "Declarations"
    HtmlItem.toHtmlHierarchical items
