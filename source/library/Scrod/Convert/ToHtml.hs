{-# LANGUAGE MultilineStrings #-}

-- | Render a 'Module.Module' as a self-contained HTML document.
--
-- Produces a complete @\<html\>@ document with Bootstrap 5 and KaTeX
-- loaded from CDNs. The output uses the custom XML types in
-- @Scrod.Xml.*@ and can be serialized with 'Xml.encode'.
module Scrod.Convert.ToHtml where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Scrod.Core.Category as Category
import qualified Scrod.Core.Column as Column
import qualified Scrod.Core.Definition as Definition
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
import qualified Scrod.Core.Import as Import
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
import qualified Scrod.Core.NumberedItem as NumberedItem
import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Core.Picture as Picture
import qualified Scrod.Core.Section as Section
import qualified Scrod.Core.Since as Since
import qualified Scrod.Core.Subordinates as Subordinates
import qualified Scrod.Core.Table as Table
import qualified Scrod.Core.TableCell as TableCell
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Warning as Warning
import qualified Scrod.Xml.Content as Content
import qualified Scrod.Xml.Declaration as XmlDeclaration
import qualified Scrod.Xml.Document as Xml
import qualified Scrod.Xml.Element as Element
import qualified Scrod.Xml.Misc as Misc
import qualified Scrod.Xml.Name as XmlName

t :: String -> Text.Text
t = Text.pack

element ::
  String ->
  [(String, String)] ->
  [Content.Content Element.Element] ->
  Content.Content Element.Element
element x ys = Content.Element . Xml.element x (fmap (uncurry Xml.attribute) ys)

-- | Convert a Module to a complete HTML document.
toHtml :: Module.Module -> Xml.Document
toHtml m =
  Xml.MkDocument
    { Xml.prolog =
        [ Misc.Declaration $
            XmlDeclaration.MkDeclaration
              (XmlName.MkName $ t "doctype")
              (t "html")
        ],
      Xml.root =
        Xml.element
          "html"
          []
          [ headElement m,
            bodyElement m
          ]
    }

moduleTitle :: Module.Module -> Text.Text
moduleTitle m = case Module.name m of
  Nothing -> t "Documentation"
  Just (Located.MkLocated _ (ModuleName.MkModuleName n)) -> n

headElement :: Module.Module -> Content.Content Element.Element
headElement m =
  element
    "head"
    []
    [ element "meta" [("charset", "utf-8")] [],
      element
        "meta"
        [ ("name", "viewport"),
          ("content", "width=device-width, initial-scale=1")
        ]
        [],
      element "title" [] [Xml.text (moduleTitle m)],
      element
        "link"
        [ ("rel", "stylesheet"),
          ("href", "https://esm.sh/bootstrap@5.3.8/dist/css/bootstrap.min.css"),
          ("integrity", "sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB"),
          ("crossorigin", "anonymous")
        ]
        [],
      element
        "link"
        [ ("rel", "stylesheet"),
          ("href", "https://esm.sh/katex@0.16.22/dist/katex.min.css"),
          ("integrity", "sha384-5TcZemv2l/9On385z///+d7MSYlvIEw9FuZTIdZ14vJLqWphw7e7ZPuOiCHJcFCP"),
          ("crossorigin", "anonymous")
        ]
        [],
      element
        "link"
        [ ("rel", "modulepreload"),
          ("href", "https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js"),
          ("integrity", "sha384-PV5j9Y/tL/HYr0HSxUY3afWRVHizeuTKLWTR+OwVlGHOBcN8jOZvCAS79+ULHoEU"),
          ("crossorigin", "anonymous")
        ]
        [],
      element
        "script"
        []
        [ Xml.raw . t $
            """
            const dark = matchMedia('(prefers-color-scheme: dark)');
            const setTheme = (e) =>
              document.documentElement.dataset.bsTheme = e.matches ? 'dark' : 'light';
            setTheme(dark);
            dark.addEventListener('change', setTheme);
            import('https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js')
              .then((m) => m.default(document.body, { delimiters: [
                { left: '\\\\(', right: '\\\\)', display: false },
                { left: '\\\\[', right: '\\\\]', display: true }
              ]}));
            """
        ]
    ]

bodyElement :: Module.Module -> Content.Content Element.Element
bodyElement m =
  element
    "body"
    []
    [ element
        "div"
        [("class", "container py-4 text-break")]
        ( [headerSection m]
            <> metadataContents m
            <> exportsContents (Module.exports m)
            <> importsContents (Module.imports m)
            <> extensionsContents (Module.extensions m)
            <> itemsContents (Module.items m)
            <> [footerSection m]
        )
    ]

-- Header section

headerSection :: Module.Module -> Content.Content Element.Element
headerSection m =
  element
    "header"
    ( [("class", "mb-4")]
        <> foldMap (\(Located.MkLocated loc _) -> [lineAttribute loc]) (Module.name m)
    )
    ( [element "h1" [("class", "border-bottom border-2 pb-2 mt-0")] [Xml.text (moduleTitle m)]]
        <> warningContents (Module.warning m)
        <> moduleDocContents (Module.documentation m)
    )

warningContents :: Maybe Warning.Warning -> [Content.Content Element.Element]
warningContents Nothing = []
warningContents (Just w) = [warningToHtml w]

warningToHtml :: Warning.Warning -> Content.Content Element.Element
warningToHtml (Warning.MkWarning (Category.MkCategory cat) val) =
  element
    "div"
    [("class", "alert alert-warning")]
    [ element
        "span"
        [("class", "fw-bold")]
        [Xml.text cat],
      Xml.text (t ": " <> val)
    ]

moduleDocContents :: Doc.Doc -> [Content.Content Element.Element]
moduleDocContents Doc.Empty = []
moduleDocContents doc =
  [element "div" [("class", "my-3")] (docToContents doc)]

-- Metadata section

metadataContents :: Module.Module -> [Content.Content Element.Element]
metadataContents m =
  let items = languageItem (Module.language m) <> sinceItem (Module.since m)
   in if null items
        then []
        else
          [ element
              "section"
              [("class", "card border-start border-primary border-4 mb-3")]
              [element "dl" [("class", "card-body mb-0")] items]
          ]

versionToText :: Version.Version -> Text.Text
versionToText (Version.MkVersion parts) =
  Text.intercalate (t ".") . fmap (t . show) $ NonEmpty.toList parts

languageItem :: Maybe Language.Language -> [Content.Content Element.Element]
languageItem Nothing = []
languageItem (Just (Language.MkLanguage lang)) =
  [ element "dt" [] [Xml.string "Language"],
    element "dd" [] [Xml.text lang]
  ]

sinceItem :: Maybe Since.Since -> [Content.Content Element.Element]
sinceItem Nothing = []
sinceItem (Just since) =
  [ element "dt" [] [Xml.string "Since"],
    element
      "dd"
      [("class", "text-body-secondary small")]
      [Xml.text (sinceToText since)]
  ]

sinceToText :: Since.Since -> Text.Text
sinceToText (Since.MkSince maybePackage version) =
  packageText <> versionToText version
  where
    packageText :: Text.Text
    packageText = case maybePackage of
      Nothing -> Text.empty
      Just (PackageName.MkPackageName pkg) -> pkg <> t "-"

-- Footer section

footerSection :: Module.Module -> Content.Content Element.Element
footerSection m =
  element
    "footer"
    [("class", "mt-5 pt-3 border-top text-body-secondary small")]
    [ Xml.string "Generated by ",
      element
        "a"
        [("href", "https://github.com/tfausak/scrod")]
        [Xml.string "Scrod"],
      Xml.string " version ",
      Xml.text (versionToText (Module.version m))
    ]

-- Exports section

exportsContents :: Maybe [Export.Export] -> [Content.Content Element.Element]
exportsContents Nothing = []
exportsContents (Just []) = []
exportsContents (Just exports) =
  [ element
      "section"
      [("class", "my-4")]
      ( [element "h2" [("class", "border-bottom pb-1 mt-4")] [Xml.string "Exports"]]
          <> [ element
                 "ul"
                 [("class", "list-group list-group-flush")]
                 (concatMap exportToContents exports)
             ]
      )
  ]

exportToContents :: Export.Export -> [Content.Content Element.Element]
exportToContents export = case export of
  Export.Identifier ident ->
    [element "li" [liClass] [exportIdentifierToHtml ident]]
  Export.Group section ->
    [element "li" [liClass] [sectionToHtml section]]
  Export.Doc doc ->
    [element "li" [liClass] [element "div" [("class", "mt-1")] (docToContents doc)]]
  Export.DocNamed name ->
    [element "li" [liClass] [element "div" [("class", "mt-1")] [Xml.text (t "\x00a7" <> name)]]]
  where
    liClass :: (String, String)
    liClass = ("class", "list-group-item bg-transparent py-1 px-2")

exportIdentifierToHtml :: ExportIdentifier.ExportIdentifier -> Content.Content Element.Element
exportIdentifierToHtml (ExportIdentifier.MkExportIdentifier name subs maybeWarning maybeDoc) =
  element
    "div"
    [("class", "py-1")]
    ( foldMap (\w -> [warningToHtml w]) maybeWarning
        <> [ element
               "code"
               [("class", "font-monospace")]
               ( [Xml.text (exportNameToText name)]
                   <> subordinatesToContents subs
               )
           ]
        <> foldMap
          ( \doc ->
              [element "div" [("class", "mt-1")] (docToContents doc)]
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
      Just ExportNameKind.Pattern -> t "pattern "
      Just ExportNameKind.Type -> t "type "
      Just ExportNameKind.Module -> t "module "

subordinatesToContents :: Maybe Subordinates.Subordinates -> [Content.Content Element.Element]
subordinatesToContents Nothing = []
subordinatesToContents (Just (Subordinates.MkSubordinates wildcard explicit)) =
  let wildcardText :: Text.Text
      wildcardText = t ".."
      explicitTexts :: [Text.Text]
      explicitTexts = fmap (\(ExportName.MkExportName _ n) -> n) explicit
      allTexts :: [Text.Text]
      allTexts = if wildcard then wildcardText : explicitTexts else explicitTexts
      combined :: Text.Text
      combined = Text.intercalate (t ", ") allTexts
   in [Xml.text (t "(" <> combined <> t ")")]

sectionToHtml :: Section.Section -> Content.Content Element.Element
sectionToHtml (Section.MkSection (Header.MkHeader level title)) =
  element
    "div"
    [("class", "my-3")]
    [ element
        (sectionLevelToName level)
        [("class", "fw-bold")]
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

-- Imports section

importsContents :: [Import.Import] -> [Content.Content Element.Element]
importsContents [] = []
importsContents imports =
  let uniqueCount = Set.size . Set.fromList $ fmap Import.name imports
      summary =
        "Imports ("
          <> show uniqueCount
          <> if uniqueCount == 1 then " module)" else " modules)"
   in [ element
          "details"
          [("class", "my-4")]
          ( [element "summary" [("class", "fs-4 fw-bold")] [Xml.string summary]]
              <> [ element
                     "ul"
                     [("class", "list-group list-group-flush font-monospace small")]
                     (fmap importToContent imports)
                 ]
          )
      ]

importToContent :: Import.Import -> Content.Content Element.Element
importToContent i =
  element
    "li"
    [("class", "list-group-item bg-transparent py-1 px-2")]
    ( packageContents (Import.package i)
        <> [Xml.text (ModuleName.unwrap $ Import.name i)]
        <> aliasContents (Import.alias i)
    )
  where
    packageContents :: Maybe PackageName.PackageName -> [Content.Content Element.Element]
    packageContents Nothing = []
    packageContents (Just pkg) =
      [Xml.text (t "\"" <> PackageName.unwrap pkg <> t "\" ")]

    aliasContents :: Maybe ModuleName.ModuleName -> [Content.Content Element.Element]
    aliasContents Nothing = []
    aliasContents (Just a) =
      [Xml.text (t " as " <> ModuleName.unwrap a)]

-- Extensions section

extensionsContents :: Map.Map Extension.Extension Bool -> [Content.Content Element.Element]
extensionsContents extensions
  | Map.null extensions = []
  | otherwise =
      let count = Map.size extensions
          summary =
            "Extensions ("
              <> show count
              <> if count == 1 then " extension)" else " extensions)"
       in [ element
              "details"
              [("class", "my-3")]
              ( [element "summary" [("class", "fs-4 fw-bold")] [Xml.string summary]]
                  <> [ element
                         "div"
                         [("class", "mt-2")]
                         (concatMap extToContents $ Map.toList extensions)
                     ]
              )
          ]

extToContents :: (Extension.Extension, Bool) -> [Content.Content Element.Element]
extToContents (Extension.MkExtension name, enabled) =
  let cls :: String
      cls =
        if enabled
          then "badge bg-secondary-subtle text-body font-monospace me-1 mb-1"
          else "badge bg-danger-subtle text-body text-decoration-line-through font-monospace me-1 mb-1"
   in [ element
          "a"
          [ ("class", cls),
            ("href", extensionUrl name)
          ]
          [Xml.text name]
      ]

extensionUrl :: Text.Text -> String
extensionUrl name =
  case Map.lookup name extensionUrlPaths of
    Just path -> ghcUserGuideBaseUrl <> path
    Nothing -> ghcUserGuideBaseUrl <> "exts/table.html"

ghcUserGuideBaseUrl :: String
ghcUserGuideBaseUrl = "https://downloads.haskell.org/ghc/latest/docs/users_guide/"

extensionUrlPaths :: Map.Map Text.Text String
extensionUrlPaths =
  Map.fromList
    . fmap (Bifunctor.first t)
    $ [ ("AllowAmbiguousTypes", "exts/ambiguous_types.html#extension-AllowAmbiguousTypes"),
        ("ApplicativeDo", "exts/applicative_do.html#extension-ApplicativeDo"),
        ("Arrows", "exts/arrows.html#extension-Arrows"),
        ("BangPatterns", "exts/strict.html#extension-BangPatterns"),
        ("BinaryLiterals", "exts/binary_literals.html#extension-BinaryLiterals"),
        ("BlockArguments", "exts/block_arguments.html#extension-BlockArguments"),
        ("CApiFFI", "exts/ffi.html#extension-CApiFFI"),
        ("ConstrainedClassMethods", "exts/constrained_class_methods.html#extension-ConstrainedClassMethods"),
        ("ConstraintKinds", "exts/constraint_kind.html#extension-ConstraintKinds"),
        ("Cpp", "phases.html#extension-CPP"),
        ("CUSKs", "exts/poly_kinds.html#extension-CUSKs"),
        ("DataKinds", "exts/data_kinds.html#extension-DataKinds"),
        ("DatatypeContexts", "exts/datatype_contexts.html#extension-DatatypeContexts"),
        ("DeepSubsumption", "exts/rank_polymorphism.html#extension-DeepSubsumption"),
        ("DefaultSignatures", "exts/default_signatures.html#extension-DefaultSignatures"),
        ("DeriveAnyClass", "exts/derive_any_class.html#extension-DeriveAnyClass"),
        ("DeriveDataTypeable", "exts/deriving_extra.html#extension-DeriveDataTypeable"),
        ("DeriveFoldable", "exts/deriving_extra.html#extension-DeriveFoldable"),
        ("DeriveFunctor", "exts/deriving_extra.html#extension-DeriveFunctor"),
        ("DeriveGeneric", "exts/generics.html#extension-DeriveGeneric"),
        ("DeriveLift", "exts/deriving_extra.html#extension-DeriveLift"),
        ("DeriveTraversable", "exts/deriving_extra.html#extension-DeriveTraversable"),
        ("DerivingStrategies", "exts/deriving_strategies.html#extension-DerivingStrategies"),
        ("DerivingVia", "exts/deriving_via.html#extension-DerivingVia"),
        ("DisambiguateRecordFields", "exts/disambiguate_record_fields.html#extension-DisambiguateRecordFields"),
        ("DoAndIfThenElse", "exts/doandifthenelse.html#extension-DoAndIfThenElse"),
        ("DuplicateRecordFields", "exts/duplicate_record_fields.html#extension-DuplicateRecordFields"),
        ("EmptyCase", "exts/empty_case.html#extension-EmptyCase"),
        ("EmptyDataDecls", "exts/nullary_types.html#extension-EmptyDataDecls"),
        ("EmptyDataDeriving", "exts/empty_data_deriving.html#extension-EmptyDataDeriving"),
        ("ExistentialQuantification", "exts/existential_quantification.html#extension-ExistentialQuantification"),
        ("ExplicitForAll", "exts/explicit_forall.html#extension-ExplicitForAll"),
        ("ExplicitLevelImports", "exts/template_haskell.html#extension-ExplicitLevelImports"),
        ("ExplicitNamespaces", "exts/explicit_namespaces.html#extension-ExplicitNamespaces"),
        ("ExtendedDefaultRules", "ghci.html#extension-ExtendedDefaultRules"),
        ("ExtendedLiterals", "exts/extended_literals.html#extension-ExtendedLiterals"),
        ("FieldSelectors", "exts/field_selectors.html#extension-FieldSelectors"),
        ("FlexibleContexts", "exts/flexible_contexts.html#extension-FlexibleContexts"),
        ("FlexibleInstances", "exts/instances.html#extension-FlexibleInstances"),
        ("ForeignFunctionInterface", "exts/ffi.html#extension-ForeignFunctionInterface"),
        ("FunctionalDependencies", "exts/functional_dependencies.html#extension-FunctionalDependencies"),
        ("GADTs", "exts/gadt.html#extension-GADTs"),
        ("GADTSyntax", "exts/gadt_syntax.html#extension-GADTSyntax"),
        ("GeneralisedNewtypeDeriving", "exts/newtype_deriving.html#extension-GeneralisedNewtypeDeriving"),
        ("GHC2021", "exts/control.html#extension-GHC2021"),
        ("GHC2024", "exts/control.html#extension-GHC2024"),
        ("GHCForeignImportPrim", "exts/ffi.html#extension-GHCForeignImportPrim"),
        ("Haskell2010", "exts/control.html#extension-Haskell2010"),
        ("Haskell98", "exts/control.html#extension-Haskell98"),
        ("HexFloatLiterals", "exts/hex_float_literals.html#extension-HexFloatLiterals"),
        ("ImplicitParams", "exts/implicit_parameters.html#extension-ImplicitParams"),
        ("ImplicitPrelude", "exts/rebindable_syntax.html#extension-ImplicitPrelude"),
        ("ImplicitStagePersistence", "exts/template_haskell.html#extension-ImplicitStagePersistence"),
        ("ImportQualifiedPost", "exts/import_qualified_post.html#extension-ImportQualifiedPost"),
        ("ImpredicativeTypes", "exts/impredicative_types.html#extension-ImpredicativeTypes"),
        ("IncoherentInstances", "exts/instances.html#extension-IncoherentInstances"),
        ("InstanceSigs", "exts/instances.html#extension-InstanceSigs"),
        ("InterruptibleFFI", "exts/ffi.html#extension-InterruptibleFFI"),
        ("KindSignatures", "exts/kind_signatures.html#extension-KindSignatures"),
        ("LambdaCase", "exts/lambda_case.html#extension-LambdaCase"),
        ("LexicalNegation", "exts/lexical_negation.html#extension-LexicalNegation"),
        ("LiberalTypeSynonyms", "exts/liberal_type_synonyms.html#extension-LiberalTypeSynonyms"),
        ("LinearTypes", "exts/linear_types.html#extension-LinearTypes"),
        ("ListTuplePuns", "exts/data_kinds.html#extension-ListTuplePuns"),
        ("MagicHash", "exts/magic_hash.html#extension-MagicHash"),
        ("MonadComprehensions", "exts/monad_comprehensions.html#extension-MonadComprehensions"),
        ("MonoLocalBinds", "exts/let_generalisation.html#extension-MonoLocalBinds"),
        ("MonomorphismRestriction", "exts/monomorphism.html#extension-MonomorphismRestriction"),
        ("MultilineStrings", "exts/multiline_strings.html#extension-MultilineStrings"),
        ("MultiParamTypeClasses", "exts/multi_param_type_classes.html#extension-MultiParamTypeClasses"),
        ("MultiWayIf", "exts/multiway_if.html#extension-MultiWayIf"),
        ("NamedDefaults", "exts/named_defaults.html#extension-NamedDefaults"),
        ("NamedFieldPuns", "exts/record_puns.html#extension-NamedFieldPuns"),
        ("NamedWildCards", "exts/partial_type_signatures.html#extension-NamedWildCards"),
        ("NegativeLiterals", "exts/negative_literals.html#extension-NegativeLiterals"),
        ("NondecreasingIndentation", "bugs.html#extension-NondecreasingIndentation"),
        ("NPlusKPatterns", "exts/nk_patterns.html#extension-NPlusKPatterns"),
        ("NullaryTypeClasses", "exts/multi_param_type_classes.html#extension-NullaryTypeClasses"),
        ("NumDecimals", "exts/num_decimals.html#extension-NumDecimals"),
        ("NumericUnderscores", "exts/numeric_underscores.html#extension-NumericUnderscores"),
        ("OrPatterns", "exts/or_patterns.html#extension-OrPatterns"),
        ("OverlappingInstances", "exts/instances.html#extension-OverlappingInstances"),
        ("OverloadedLabels", "exts/overloaded_labels.html#extension-OverloadedLabels"),
        ("OverloadedLists", "exts/overloaded_lists.html#extension-OverloadedLists"),
        ("OverloadedRecordDot", "exts/overloaded_record_dot.html#extension-OverloadedRecordDot"),
        ("OverloadedRecordUpdate", "exts/overloaded_record_update.html#extension-OverloadedRecordUpdate"),
        ("OverloadedStrings", "exts/overloaded_strings.html#extension-OverloadedStrings"),
        ("PackageImports", "exts/package_qualified_imports.html#extension-PackageImports"),
        ("ParallelListComp", "exts/parallel_list_comprehensions.html#extension-ParallelListComp"),
        ("PartialTypeSignatures", "exts/partial_type_signatures.html#extension-PartialTypeSignatures"),
        ("PatternGuards", "exts/pattern_guards.html#extension-PatternGuards"),
        ("PatternSynonyms", "exts/pattern_synonyms.html#extension-PatternSynonyms"),
        ("PolyKinds", "exts/poly_kinds.html#extension-PolyKinds"),
        ("PostfixOperators", "exts/rebindable_syntax.html#extension-PostfixOperators"),
        ("QualifiedDo", "exts/qualified_do.html#extension-QualifiedDo"),
        ("QuantifiedConstraints", "exts/quantified_constraints.html#extension-QuantifiedConstraints"),
        ("QuasiQuotes", "exts/template_haskell.html#extension-QuasiQuotes"),
        ("Rank2Types", "exts/rank_polymorphism.html#extension-Rank2Types"),
        ("RankNTypes", "exts/rank_polymorphism.html#extension-RankNTypes"),
        ("RebindableSyntax", "exts/rebindable_syntax.html#extension-RebindableSyntax"),
        ("RecordWildCards", "exts/record_wildcards.html#extension-RecordWildCards"),
        ("RecursiveDo", "exts/recursive_do.html#extension-RecursiveDo"),
        ("RequiredTypeArguments", "exts/required_type_arguments.html#extension-RequiredTypeArguments"),
        ("RoleAnnotations", "exts/roles.html#extension-RoleAnnotations"),
        ("Safe", "exts/safe_haskell.html#extension-Safe"),
        ("ScopedTypeVariables", "exts/scoped_type_variables.html#extension-ScopedTypeVariables"),
        ("StandaloneDeriving", "exts/standalone_deriving.html#extension-StandaloneDeriving"),
        ("StandaloneKindSignatures", "exts/poly_kinds.html#extension-StandaloneKindSignatures"),
        ("StarIsType", "exts/poly_kinds.html#extension-StarIsType"),
        ("StaticPointers", "exts/static_pointers.html#extension-StaticPointers"),
        ("Strict", "exts/strict.html#extension-Strict"),
        ("StrictData", "exts/strict.html#extension-StrictData"),
        ("TemplateHaskell", "exts/template_haskell.html#extension-TemplateHaskell"),
        ("TemplateHaskellQuotes", "exts/template_haskell.html#extension-TemplateHaskellQuotes"),
        ("TraditionalRecordSyntax", "exts/traditional_record_syntax.html#extension-TraditionalRecordSyntax"),
        ("TransformListComp", "exts/generalised_list_comprehensions.html#extension-TransformListComp"),
        ("Trustworthy", "exts/safe_haskell.html#extension-Trustworthy"),
        ("TupleSections", "exts/tuple_sections.html#extension-TupleSections"),
        ("TypeAbstractions", "exts/type_abstractions.html#extension-TypeAbstractions"),
        ("TypeApplications", "exts/type_applications.html#extension-TypeApplications"),
        ("TypeData", "exts/type_data.html#extension-TypeData"),
        ("TypeFamilies", "exts/type_families.html#extension-TypeFamilies"),
        ("TypeFamilyDependencies", "exts/type_families.html#extension-TypeFamilyDependencies"),
        ("TypeInType", "exts/poly_kinds.html#extension-TypeInType"),
        ("TypeOperators", "exts/type_operators.html#extension-TypeOperators"),
        ("TypeSynonymInstances", "exts/instances.html#extension-TypeSynonymInstances"),
        ("UnboxedSums", "exts/primitives.html#extension-UnboxedSums"),
        ("UnboxedTuples", "exts/primitives.html#extension-UnboxedTuples"),
        ("UndecidableInstances", "exts/instances.html#extension-UndecidableInstances"),
        ("UndecidableSuperClasses", "exts/undecidable_super_classes.html#extension-UndecidableSuperClasses"),
        ("UnicodeSyntax", "exts/unicode_syntax.html#extension-UnicodeSyntax"),
        ("UnliftedDatatypes", "exts/primitives.html#extension-UnliftedDatatypes"),
        ("UnliftedFFITypes", "exts/ffi.html#extension-UnliftedFFITypes"),
        ("UnliftedNewtypes", "exts/primitives.html#extension-UnliftedNewtypes"),
        ("Unsafe", "exts/safe_haskell.html#extension-Unsafe"),
        ("ViewPatterns", "exts/view_patterns.html#extension-ViewPatterns")
      ]

-- Items section

itemsContents :: [Located.Located Item.Item] -> [Content.Content Element.Element]
itemsContents [] = []
itemsContents items =
  [ element
      "section"
      [("class", "my-4")]
      ( [element "h2" [("class", "border-bottom pb-1 mt-4")] [Xml.string "Declarations"]]
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
       in [itemToHtml li]
            <> if null children
              then []
              else
                [ element
                    "div"
                    [("class", "ms-4 mt-2 border-start border-2 ps-3")]
                    (concatMap renderItemWithChildren children)
                ]

itemToHtml :: Located.Located Item.Item -> Content.Content Element.Element
itemToHtml (Located.MkLocated loc (Item.MkItem key itemKind _parentKey maybeName doc maybeSince maybeSig)) =
  element
    "div"
    [ ("class", "card mb-3 border-start border-4"),
      ("style", kindBorderStyle itemKind),
      ("id", "item-" <> show (ItemKey.unwrap key)),
      lineAttribute loc
    ]
    ( [ element
          "div"
          [("class", "card-header bg-transparent d-flex align-items-center py-2")]
          ( nameContents
              <> sigBeforeKind
              <> [kindContent]
              <> sigAfterKind
              <> sinceContents
              <> [locationElement loc]
          )
      ]
        <> docContents'
    )
  where
    nameContents :: [Content.Content Element.Element]
    nameContents = case maybeName of
      Nothing -> []
      Just (ItemName.MkItemName n) ->
        [element "span" [("class", "font-monospace fw-bold text-success")] [Xml.text n]]

    isTypeVarSignature :: Bool
    isTypeVarSignature = case itemKind of
      ItemKind.DataType -> True
      ItemKind.Newtype -> True
      ItemKind.TypeData -> True
      ItemKind.TypeSynonym -> True
      ItemKind.Class -> True
      _ -> False

    kindContent :: Content.Content Element.Element
    kindContent =
      element
        "span"
        [("class", "badge " <> kindBadgeClass itemKind <> " ms-2")]
        [Xml.text (kindToText itemKind)]

    sigBeforeKind :: [Content.Content Element.Element]
    sigBeforeKind =
      if isTypeVarSignature then signatureContents else []

    sigAfterKind :: [Content.Content Element.Element]
    sigAfterKind =
      if isTypeVarSignature then [] else signatureContents

    signatureContents :: [Content.Content Element.Element]
    signatureContents = case maybeSig of
      Nothing -> []
      Just sig ->
        let prefix = if isTypeVarSignature then t "\x00a0" else t " :: "
         in [ element
                "span"
                [("class", "font-monospace text-body-secondary")]
                [Xml.text (prefix <> sig)]
            ]

    sinceContents :: [Content.Content Element.Element]
    sinceContents = case maybeSince of
      Nothing -> []
      Just s ->
        [ element
            "span"
            [("class", "text-body-secondary small ms-2")]
            [Xml.text (t "since " <> sinceToText s)]
        ]

    docContents' :: [Content.Content Element.Element]
    docContents' = case doc of
      Doc.Empty -> []
      _ -> [element "div" [("class", "card-body")] (docToContents doc)]

lineAttribute :: Location.Location -> (String, String)
lineAttribute loc =
  ("data-line", show (Line.unwrap (Location.line loc)))

columnAttribute :: Location.Location -> (String, String)
columnAttribute loc =
  ("data-col", show (Column.unwrap (Location.column loc)))

locationElement :: Location.Location -> Content.Content Element.Element
locationElement loc =
  let lineNum = Line.unwrap (Location.line loc)
   in element
        "button"
        [ ("type", "button"),
          ("class", "item-location ms-auto text-body-tertiary small bg-transparent border-0 p-0"),
          ("aria-label", "Go to line " <> show lineNum),
          lineAttribute loc,
          columnAttribute loc
        ]
        [ Xml.text
            ( t "line "
                <> t (show lineNum)
            )
        ]

kindToText :: ItemKind.ItemKind -> Text.Text
kindToText k = t $ case k of
  ItemKind.Function -> "function"
  ItemKind.PatternBinding -> "pattern binding"
  ItemKind.PatternSynonym -> "pattern"
  ItemKind.DataType -> "data"
  ItemKind.Newtype -> "newtype"
  ItemKind.TypeData -> "type data"
  ItemKind.TypeSynonym -> "type"
  ItemKind.DataConstructor -> "constructor"
  ItemKind.GADTConstructor -> "GADT constructor"
  ItemKind.RecordField -> "field"
  ItemKind.Class -> "class"
  ItemKind.ClassMethod -> "method"
  ItemKind.ClassInstance -> "instance"
  ItemKind.StandaloneDeriving -> "standalone deriving"
  ItemKind.DerivedInstance -> "deriving"
  ItemKind.OpenTypeFamily -> "type family"
  ItemKind.ClosedTypeFamily -> "type family"
  ItemKind.DataFamily -> "data family"
  ItemKind.TypeFamilyInstance -> "type instance"
  ItemKind.DataFamilyInstance -> "data instance"
  ItemKind.ForeignImport -> "foreign import"
  ItemKind.ForeignExport -> "foreign export"
  ItemKind.FixitySignature -> "fixity"
  ItemKind.InlineSignature -> "inline"
  ItemKind.SpecialiseSignature -> "specialise"
  ItemKind.StandaloneKindSig -> "kind"
  ItemKind.Rule -> "rule"
  ItemKind.Default -> "default"
  ItemKind.Annotation -> "annotation"
  ItemKind.Splice -> "splice"
  ItemKind.Warning -> "warning"
  ItemKind.MinimalPragma -> "minimal"
  ItemKind.CompletePragma -> "complete"
  ItemKind.DefaultMethodSignature -> "default"
  ItemKind.RoleAnnotation -> "role"

data KindColor
  = KindSuccess
  | KindInfo
  | KindSecondary
  | KindPrimary
  | KindWarning

kindColor :: ItemKind.ItemKind -> KindColor
kindColor k = case k of
  ItemKind.Function -> KindSuccess
  ItemKind.PatternBinding -> KindSuccess
  ItemKind.PatternSynonym -> KindSuccess
  ItemKind.DataType -> KindInfo
  ItemKind.Newtype -> KindInfo
  ItemKind.TypeData -> KindInfo
  ItemKind.TypeSynonym -> KindInfo
  ItemKind.DataConstructor -> KindSecondary
  ItemKind.GADTConstructor -> KindSecondary
  ItemKind.RecordField -> KindSecondary
  ItemKind.Class -> KindPrimary
  ItemKind.ClassMethod -> KindPrimary
  ItemKind.ClassInstance -> KindPrimary
  ItemKind.StandaloneDeriving -> KindPrimary
  ItemKind.DerivedInstance -> KindPrimary
  ItemKind.OpenTypeFamily -> KindInfo
  ItemKind.ClosedTypeFamily -> KindInfo
  ItemKind.DataFamily -> KindInfo
  ItemKind.TypeFamilyInstance -> KindInfo
  ItemKind.DataFamilyInstance -> KindInfo
  ItemKind.ForeignImport -> KindWarning
  ItemKind.ForeignExport -> KindWarning
  ItemKind.FixitySignature -> KindSecondary
  ItemKind.InlineSignature -> KindSecondary
  ItemKind.SpecialiseSignature -> KindSecondary
  ItemKind.StandaloneKindSig -> KindInfo
  ItemKind.Rule -> KindSecondary
  ItemKind.Default -> KindSecondary
  ItemKind.Annotation -> KindSecondary
  ItemKind.Splice -> KindSecondary
  ItemKind.Warning -> KindWarning
  ItemKind.MinimalPragma -> KindSecondary
  ItemKind.CompletePragma -> KindSecondary
  ItemKind.DefaultMethodSignature -> KindPrimary
  ItemKind.RoleAnnotation -> KindSecondary

kindBadgeClass :: ItemKind.ItemKind -> String
kindBadgeClass k = case kindColor k of
  KindSuccess -> "bg-success-subtle text-success-emphasis"
  KindInfo -> "bg-info-subtle text-info-emphasis"
  KindSecondary -> "bg-secondary-subtle text-body"
  KindPrimary -> "bg-primary-subtle text-primary-emphasis"
  KindWarning -> "bg-warning-subtle text-warning-emphasis"

kindBorderStyle :: ItemKind.ItemKind -> String
kindBorderStyle k = case kindColor k of
  KindSuccess -> "border-left-color: var(--bs-success)"
  KindInfo -> "border-left-color: var(--bs-info)"
  KindSecondary -> "border-left-color: var(--bs-secondary)"
  KindPrimary -> "border-left-color: var(--bs-primary)"
  KindWarning -> "border-left-color: var(--bs-warning)"

-- Doc to HTML conversion

docToContents :: Doc.Doc -> [Content.Content Element.Element]
docToContents doc = case doc of
  Doc.Empty -> []
  Doc.Append ds -> concatMap docToContents ds
  Doc.String x -> [Xml.text x]
  Doc.Paragraph d -> [element "p" [] (docToContents d)]
  Doc.Identifier i -> [identifierToHtml i]
  Doc.Module m -> [modLinkToHtml m]
  Doc.Emphasis d -> [element "em" [] (docToContents d)]
  Doc.Monospaced d -> [element "code" [] (docToContents d)]
  Doc.Bold d -> [element "strong" [] (docToContents d)]
  Doc.UnorderedList items ->
    [element "ul" [] (concatMap (\item -> [element "li" [] (docToContents item)]) items)]
  Doc.OrderedList items ->
    [ element
        "ol"
        []
        ( fmap
            ( \ni ->
                element "li" [("value", show $ NumberedItem.index ni)] (docToContents $ NumberedItem.item ni)
            )
            items
        )
    ]
  Doc.DefList defs ->
    [ element
        "dl"
        []
        ( concatMap
            ( \d ->
                [ element "dt" [] (docToContents $ Definition.term d),
                  element "dd" [] (docToContents $ Definition.definition d)
                ]
            )
            defs
        )
    ]
  Doc.CodeBlock d ->
    [element "pre" [("class", "bg-body-secondary rounded p-3 my-3")] [element "code" [] (docToContents d)]]
  Doc.Hyperlink h -> [hyperlinkToHtml h]
  Doc.Pic p -> [pictureToHtml p]
  Doc.MathInline x ->
    [Xml.text $ t "\\(" <> x <> t "\\)"]
  Doc.MathDisplay x ->
    [Xml.text $ t "\\[" <> x <> t "\\]"]
  Doc.AName x ->
    [element "a" [("id", Text.unpack x)] []]
  Doc.Property x ->
    [ element
        "div"
        [("class", "border-start border-4 border-primary bg-primary-subtle rounded-end p-3 my-3")]
        [ element "div" [("class", "fw-bold mb-1")] [Xml.string "Property:"],
          element "pre" [("class", "mb-0 bg-transparent font-monospace")] [Xml.text x]
        ]
    ]
  Doc.Examples es -> [examplesToHtml es]
  Doc.Header h -> [headerToHtml h]
  Doc.Table x -> [tableToHtml x]

identifierToHtml :: Identifier.Identifier -> Content.Content Element.Element
identifierToHtml (Identifier.MkIdentifier ns val) =
  element
    "span"
    []
    ( [element "code" [("class", "font-monospace text-success")] [Xml.text val]]
        <> namespaceBadge ns
    )
  where
    namespaceBadge :: Maybe Namespace.Namespace -> [Content.Content Element.Element]
    namespaceBadge Nothing = []
    namespaceBadge (Just n) =
      [ element
          "span"
          [("class", "badge bg-secondary-subtle text-body ms-1")]
          [Xml.text (namespaceToText n)]
      ]

    namespaceToText :: Namespace.Namespace -> Text.Text
    namespaceToText Namespace.Value = t "value"
    namespaceToText Namespace.Type = t "type"

modLinkToHtml :: ModLink.ModLink Doc.Doc -> Content.Content Element.Element
modLinkToHtml (ModLink.MkModLink (ModuleName.MkModuleName modName) maybeLabel) =
  element "code" [("class", "font-monospace text-info")] $
    maybe [Xml.text modName] docToContents maybeLabel

hyperlinkToHtml :: Hyperlink.Hyperlink Doc.Doc -> Content.Content Element.Element
hyperlinkToHtml (Hyperlink.MkHyperlink url maybeLabel) =
  element "a" [("href", Text.unpack url)] $
    maybe [Xml.text url] docToContents maybeLabel

pictureToHtml :: Picture.Picture -> Content.Content Element.Element
pictureToHtml (Picture.MkPicture uri maybeTitle) =
  element
    "img"
    ( [("src", Text.unpack uri)]
        <> [("alt", Text.unpack (Maybe.fromMaybe Text.empty maybeTitle))]
        <> foldMap (\x -> [("title", Text.unpack x)]) maybeTitle
    )
    []

examplesToHtml :: NonEmpty.NonEmpty Example.Example -> Content.Content Element.Element
examplesToHtml examples =
  element
    "div"
    [("class", "border-start border-4 border-warning bg-warning-subtle rounded-end p-3 my-3")]
    ( [ element
          "div"
          [("class", "fw-bold mb-1")]
          [Xml.string (case examples of _ NonEmpty.:| [] -> "Example:"; _ -> "Examples:")]
      ]
        <> concatMap exampleToContents (NonEmpty.toList examples)
    )

exampleToContents :: Example.Example -> [Content.Content Element.Element]
exampleToContents (Example.MkExample expr results) =
  [ element
      "div"
      [("class", "my-1")]
      ( [ element
            "div"
            [("class", "font-monospace")]
            [ element
                "span"
                [("class", "text-warning-emphasis user-select-none")]
                [Xml.string ">>> "],
              Xml.text expr
            ]
        ]
          <> fmap
            ( \r ->
                element
                  "div"
                  [("class", "font-monospace text-body-secondary ps-3")]
                  [Xml.text r]
            )
            results
      )
  ]

headerToHtml :: Header.Header Doc.Doc -> Content.Content Element.Element
headerToHtml (Header.MkHeader level title) =
  element (levelToName level) [] (docToContents title)

levelToName :: Level.Level -> String
levelToName level = case level of
  Level.One -> "h1"
  Level.Two -> "h2"
  Level.Three -> "h3"
  Level.Four -> "h4"
  Level.Five -> "h5"
  Level.Six -> "h6"

tableToHtml :: Table.Table Doc.Doc -> Content.Content Element.Element
tableToHtml (Table.MkTable headerRows bodyRows) =
  element "table" [("class", "table table-bordered table-sm table-striped my-3")] (theadContents <> tbodyContents)
  where
    theadContents :: [Content.Content Element.Element]
    theadContents
      | null headerRows = []
      | otherwise =
          [element "thead" [] (fmap headerRowToHtml headerRows)]

    tbodyContents :: [Content.Content Element.Element]
    tbodyContents
      | null bodyRows = []
      | otherwise =
          [element "tbody" [] (fmap bodyRowToHtml bodyRows)]

    headerRowToHtml :: [TableCell.Cell Doc.Doc] -> Content.Content Element.Element
    headerRowToHtml cells =
      element "tr" [] (fmap headerCellToHtml cells)

    bodyRowToHtml :: [TableCell.Cell Doc.Doc] -> Content.Content Element.Element
    bodyRowToHtml cells =
      element "tr" [] (fmap bodyCellToHtml cells)

    headerCellToHtml :: TableCell.Cell Doc.Doc -> Content.Content Element.Element
    headerCellToHtml (TableCell.MkCell colspan rowspan contents) =
      element "th" (cellAttrs colspan rowspan) (docToContents contents)

    bodyCellToHtml :: TableCell.Cell Doc.Doc -> Content.Content Element.Element
    bodyCellToHtml (TableCell.MkCell colspan rowspan contents) =
      element "td" (cellAttrs colspan rowspan) (docToContents contents)

    cellAttrs :: Natural.Natural -> Natural.Natural -> [(String, String)]
    cellAttrs c r =
      [ ("colspan", show c),
        ("rowspan", show r)
      ]
