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
import qualified Scrod.Core.Table as Table
import qualified Scrod.Core.TableCell as TableCell
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Warning as Warning
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
        Xml.element
          "link"
          [ Xml.attribute "rel" "stylesheet",
            Xml.attribute "href" "https://esm.sh/bootstrap@5.3.8/dist/css/bootstrap.min.css",
            Xml.attribute "integrity" "sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB",
            Xml.attribute "crossorigin" "anonymous"
          ]
          [],
      Content.Element $
        Xml.element
          "link"
          [ Xml.attribute "rel" "stylesheet",
            Xml.attribute "href" "https://esm.sh/katex@0.16.22/dist/katex.min.css",
            Xml.attribute "integrity" "sha384-5TcZemv2l/9On385z///+d7MSYlvIEw9FuZTIdZ14vJLqWphw7e7ZPuOiCHJcFCP",
            Xml.attribute "crossorigin" "anonymous"
          ]
          [],
      Content.Element $
        Xml.element
          "link"
          [ Xml.attribute "rel" "modulepreload",
            Xml.attribute "href" "https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js",
            Xml.attribute "integrity" "sha384-PV5j9Y/tL/HYr0HSxUY3afWRVHizeuTKLWTR+OwVlGHOBcN8jOZvCAS79+ULHoEU",
            Xml.attribute "crossorigin" "anonymous"
          ]
          [],
      Content.Element $
        Xml.element
          "script"
          []
          [ Xml.raw . Text.pack $
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

bodyElement :: Module.Module -> Element.Element
bodyElement m =
  Xml.element
    "body"
    []
    [ Content.Element $
        Xml.element
          "div"
          [Xml.attribute "class" "container py-4 text-break"]
          ( [Content.Element (headerSection m)]
              <> metadataContents m
              <> importsContents (Module.imports m)
              <> extensionsContents (Module.extensions m)
              <> declarationsContents (Module.exports m) (Module.items m)
              <> [Content.Element (footerSection m)]
          )
    ]

-- Header section

headerSection :: Module.Module -> Element.Element
headerSection m =
  Xml.element
    "header"
    ( [Xml.attribute "class" "mb-4"]
        <> foldMap (\(Located.MkLocated loc _) -> [lineAttribute loc]) (Module.name m)
    )
    ( [Content.Element $ Xml.element "h1" [Xml.attribute "class" "border-bottom border-2 pb-2 mt-0"] [Xml.text (moduleTitle m)]]
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
    [Xml.attribute "class" "alert alert-warning"]
    [ Content.Element $
        Xml.element
          "span"
          [Xml.attribute "class" "fw-bold"]
          [Xml.text cat],
      Xml.text (Text.pack ": " <> val)
    ]

moduleDocContents :: Doc.Doc -> [Content.Content Element.Element]
moduleDocContents Doc.Empty = []
moduleDocContents doc =
  [Content.Element $ Xml.element "div" [Xml.attribute "class" "my-3"] (docToContents doc)]

-- Metadata section

metadataContents :: Module.Module -> [Content.Content Element.Element]
metadataContents m =
  let items = languageItem (Module.language m) <> sinceItem (Module.since m)
   in if null items
        then []
        else
          [ Content.Element $
              Xml.element
                "section"
                [Xml.attribute "class" "card border-start border-primary border-4 mb-3"]
                [Content.Element $ Xml.element "dl" [Xml.attribute "class" "card-body mb-0"] items]
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
        [Xml.attribute "class" "text-body-secondary small"]
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

-- Footer section

footerSection :: Module.Module -> Element.Element
footerSection m =
  Xml.element
    "footer"
    [Xml.attribute "class" "mt-5 pt-3 border-top text-body-secondary small"]
    [ Xml.string "Generated by ",
      Content.Element $
        Xml.element
          "a"
          [Xml.attribute "href" "https://github.com/tfausak/scrod"]
          [Xml.string "Scrod"],
      Xml.string " version ",
      Xml.text (versionToText (Module.version m))
    ]

-- Declarations section

sectionToHtml :: Section.Section -> Element.Element
sectionToHtml (Section.MkSection (Header.MkHeader level title)) =
  Xml.element
    "div"
    [Xml.attribute "class" "my-3"]
    [ Content.Element $
        Xml.element
          (sectionLevelToName level)
          [Xml.attribute "class" "fw-bold"]
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
   in [ Content.Element $
          Xml.element
            "details"
            [Xml.attribute "class" "my-4"]
            ( [ Content.Element $
                  Xml.element "summary" [Xml.attribute "class" "fs-4 fw-bold"] [Xml.string summary]
              ]
                <> [ Content.Element $
                       Xml.element
                         "ul"
                         [Xml.attribute "class" "list-group list-group-flush font-monospace small"]
                         (fmap importToContent imports)
                   ]
            )
      ]

importToContent :: Import.Import -> Content.Content Element.Element
importToContent i =
  Content.Element $
    Xml.element
      "li"
      [Xml.attribute "class" "list-group-item bg-transparent py-1 px-2"]
      ( packageContents (Import.package i)
          <> [Xml.text (ModuleName.unwrap $ Import.name i)]
          <> aliasContents (Import.alias i)
      )
  where
    packageContents :: Maybe PackageName.PackageName -> [Content.Content Element.Element]
    packageContents Nothing = []
    packageContents (Just pkg) =
      [Xml.text (Text.pack "\"" <> PackageName.unwrap pkg <> Text.pack "\" ")]

    aliasContents :: Maybe ModuleName.ModuleName -> [Content.Content Element.Element]
    aliasContents Nothing = []
    aliasContents (Just a) =
      [Xml.text (Text.pack " as " <> ModuleName.unwrap a)]

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
       in [ Content.Element $
              Xml.element
                "details"
                [Xml.attribute "class" "my-3"]
                ( [ Content.Element $
                      Xml.element "summary" [Xml.attribute "class" "fs-4 fw-bold"] [Xml.string summary]
                  ]
                    <> [ Content.Element $
                           Xml.element
                             "div"
                             [Xml.attribute "class" "mt-2"]
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
   in [ Content.Element $
          Xml.element
            "a"
            [ Xml.attribute "class" cls,
              Xml.attribute "href" (extensionUrl name)
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
    . fmap (Bifunctor.first Text.pack)
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

declarationsContents :: Maybe [Export.Export] -> [Located.Located Item.Item] -> [Content.Content Element.Element]
declarationsContents _ [] = []
declarationsContents exports items =
  case exports of
    Nothing -> defaultDeclarations
    Just [] -> defaultDeclarations
    Just es -> exportDrivenDeclarations es
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
                      [Xml.attribute "class" "ms-4 mt-2 border-start border-2 ps-3"]
                      (concatMap renderItemWithChildren children)
                ]

    -- \| Map from item name to top-level item for export matching.
    nameMap :: Map.Map Text.Text (Located.Located Item.Item)
    nameMap =
      Map.fromList
        [ (ItemName.unwrap n, li)
        | li <- topLevelItems,
          Just n <- [Item.name (Located.value li)]
        ]

    defaultDeclarations :: [Content.Content Element.Element]
    defaultDeclarations =
      [ Content.Element $
          Xml.element
            "section"
            [Xml.attribute "class" "my-4"]
            ( [Content.Element $ Xml.element "h2" [Xml.attribute "class" "border-bottom pb-1 mt-4"] [Xml.string "Declarations"]]
                <> concatMap renderItemWithChildren topLevelItems
            )
      ]

    exportDrivenDeclarations :: [Export.Export] -> [Content.Content Element.Element]
    exportDrivenDeclarations es =
      let (exportedContents, usedNames) = renderExports es Set.empty
          unexported =
            [ li
            | li <- topLevelItems,
              case Item.name (Located.value li) of
                Nothing -> True
                Just n -> not (Set.member (ItemName.unwrap n) usedNames)
            ]
          unexportedContents
            | null unexported = []
            | otherwise =
                [ Content.Element $
                    Xml.element
                      "h3"
                      [Xml.attribute "class" "border-bottom pb-1 mt-5 text-body-secondary"]
                      [Xml.string "Unexported"]
                ]
                  <> concatMap renderItemWithChildren unexported
       in [ Content.Element $
              Xml.element
                "section"
                [Xml.attribute "class" "my-4"]
                ( [Content.Element $ Xml.element "h2" [Xml.attribute "class" "border-bottom pb-1 mt-4"] [Xml.string "Declarations"]]
                    <> exportedContents
                    <> unexportedContents
                )
          ]

    renderExports :: [Export.Export] -> Set.Set Text.Text -> ([Content.Content Element.Element], Set.Set Text.Text)
    renderExports [] used = ([], used)
    renderExports (e : es) used = case e of
      Export.Identifier ident ->
        let name = ExportName.name (ExportIdentifier.name ident)
         in case Map.lookup name nameMap of
              Just li
                | not (Set.member name used) ->
                    let here = renderItemWithChildren li
                        (rest, used') = renderExports es (Set.insert name used)
                     in (here <> rest, used')
              _ -> renderExports es used
      Export.Group section ->
        let here = [Content.Element (sectionToHtml section)]
            (rest, used') = renderExports es used
         in (here <> rest, used')
      Export.Doc doc ->
        let here = [Content.Element $ Xml.element "div" [Xml.attribute "class" "my-3"] (docToContents doc)]
            (rest, used') = renderExports es used
         in (here <> rest, used')
      Export.DocNamed name ->
        let here = [Content.Element $ Xml.element "div" [Xml.attribute "class" "my-3"] [Xml.text (Text.pack "\x00a7" <> name)]]
            (rest, used') = renderExports es used
         in (here <> rest, used')

itemToHtml :: Located.Located Item.Item -> Element.Element
itemToHtml (Located.MkLocated loc (Item.MkItem key itemKind _parentKey maybeName doc maybeSince maybeSig)) =
  Xml.element
    "div"
    [ Xml.attribute "class" "card mb-3 border-start border-4",
      Xml.attribute "style" (kindBorderStyle itemKind),
      Xml.attribute "id" ("item-" <> show (ItemKey.unwrap key)),
      lineAttribute loc
    ]
    ( [ Content.Element $
          Xml.element
            "div"
            [Xml.attribute "class" "card-header bg-transparent d-flex align-items-center py-2"]
            ( nameContents
                <> sigBeforeKind
                <> [Content.Element kindElement]
                <> sigAfterKind
                <> sinceContents
                <> [Content.Element (locationElement loc)]
            )
      ]
        <> docContents'
    )
  where
    nameContents :: [Content.Content Element.Element]
    nameContents = case maybeName of
      Nothing -> []
      Just (ItemName.MkItemName n) ->
        [Content.Element $ Xml.element "span" [Xml.attribute "class" "font-monospace fw-bold text-success"] [Xml.text n]]

    isTypeVarSignature :: Bool
    isTypeVarSignature = case itemKind of
      ItemKind.DataType -> True
      ItemKind.Newtype -> True
      ItemKind.TypeData -> True
      ItemKind.TypeSynonym -> True
      ItemKind.Class -> True
      _ -> False

    kindElement :: Element.Element
    kindElement =
      Xml.element
        "span"
        [Xml.attribute "class" ("badge " <> kindBadgeClass itemKind <> " ms-2")]
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
        let prefix = if isTypeVarSignature then Text.pack "\x00a0" else Text.pack " :: "
         in [ Content.Element $
                Xml.element
                  "span"
                  [Xml.attribute "class" "font-monospace text-body-secondary"]
                  [Xml.text (prefix <> sig)]
            ]

    sinceContents :: [Content.Content Element.Element]
    sinceContents = case maybeSince of
      Nothing -> []
      Just s ->
        [ Content.Element $
            Xml.element
              "span"
              [Xml.attribute "class" "text-body-secondary small ms-2"]
              [Xml.text (Text.pack "since " <> sinceToText s)]
        ]

    docContents' :: [Content.Content Element.Element]
    docContents' = case doc of
      Doc.Empty -> []
      _ -> [Content.Element $ Xml.element "div" [Xml.attribute "class" "card-body"] (docToContents doc)]

lineAttribute :: Location.Location -> Attribute.Attribute
lineAttribute loc =
  Xml.attribute "data-line" (show (Line.unwrap (Location.line loc)))

columnAttribute :: Location.Location -> Attribute.Attribute
columnAttribute loc =
  Xml.attribute "data-col" (show (Column.unwrap (Location.column loc)))

locationElement :: Location.Location -> Element.Element
locationElement loc =
  let lineNum = Line.unwrap (Location.line loc)
   in Xml.element
        "button"
        [ Xml.attribute "type" "button",
          Xml.attribute "class" "item-location ms-auto text-body-tertiary small bg-transparent border-0 p-0",
          Xml.attribute "aria-label" ("Go to line " <> show lineNum),
          lineAttribute loc,
          columnAttribute loc
        ]
        [ Xml.text
            ( Text.pack "line "
                <> Text.pack (show lineNum)
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
  ItemKind.Splice -> Text.pack "splice"
  ItemKind.Warning -> Text.pack "warning"
  ItemKind.MinimalPragma -> Text.pack "minimal"
  ItemKind.CompletePragma -> Text.pack "complete"
  ItemKind.DefaultMethodSignature -> Text.pack "default"
  ItemKind.RoleAnnotation -> Text.pack "role"

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
              ( \ni ->
                  Content.Element $
                    Xml.element "li" [Xml.attribute "value" (show $ NumberedItem.index ni)] (docToContents $ NumberedItem.item ni)
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
              ( \d ->
                  [ Content.Element $ Xml.element "dt" [] (docToContents $ Definition.term d),
                    Content.Element $ Xml.element "dd" [] (docToContents $ Definition.definition d)
                  ]
              )
              defs
          )
    ]
  Doc.CodeBlock d ->
    [Content.Element $ Xml.element "pre" [Xml.attribute "class" "bg-body-secondary rounded p-3 my-3"] [Content.Element $ Xml.element "code" [] (docToContents d)]]
  Doc.Hyperlink h -> [Content.Element (hyperlinkToHtml h)]
  Doc.Pic p -> [Content.Element (pictureToHtml p)]
  Doc.MathInline t ->
    [Xml.text $ Text.pack "\\(" <> t <> Text.pack "\\)"]
  Doc.MathDisplay t ->
    [Xml.text $ Text.pack "\\[" <> t <> Text.pack "\\]"]
  Doc.AName t ->
    [Content.Element $ Xml.element "a" [Xml.attribute "id" (Text.unpack t)] []]
  Doc.Property t ->
    [ Content.Element $
        Xml.element
          "div"
          [Xml.attribute "class" "border-start border-4 border-primary bg-primary-subtle rounded-end p-3 my-3"]
          [ Content.Element $ Xml.element "div" [Xml.attribute "class" "fw-bold mb-1"] [Xml.string "Property:"],
            Content.Element $ Xml.element "pre" [Xml.attribute "class" "mb-0 bg-transparent font-monospace"] [Xml.text t]
          ]
    ]
  Doc.Examples es -> [Content.Element (examplesToHtml es)]
  Doc.Header h -> [Content.Element (headerToHtml h)]
  Doc.Table t -> [Content.Element (tableToHtml t)]

identifierToHtml :: Identifier.Identifier -> Element.Element
identifierToHtml (Identifier.MkIdentifier ns val) =
  Xml.element
    "span"
    []
    ( [Content.Element $ Xml.element "code" [Xml.attribute "class" "font-monospace text-success"] [Xml.text val]]
        <> namespaceBadge ns
    )
  where
    namespaceBadge :: Maybe Namespace.Namespace -> [Content.Content Element.Element]
    namespaceBadge Nothing = []
    namespaceBadge (Just n) =
      [ Content.Element $
          Xml.element
            "span"
            [Xml.attribute "class" "badge bg-secondary-subtle text-body ms-1"]
            [Xml.text (namespaceToText n)]
      ]

    namespaceToText :: Namespace.Namespace -> Text.Text
    namespaceToText Namespace.Value = Text.pack "value"
    namespaceToText Namespace.Type = Text.pack "type"

modLinkToHtml :: ModLink.ModLink Doc.Doc -> Element.Element
modLinkToHtml (ModLink.MkModLink (ModuleName.MkModuleName modName) maybeLabel) =
  Xml.element "code" [Xml.attribute "class" "font-monospace text-info"] $
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

examplesToHtml :: NonEmpty.NonEmpty Example.Example -> Element.Element
examplesToHtml examples =
  Xml.element
    "div"
    [Xml.attribute "class" "border-start border-4 border-warning bg-warning-subtle rounded-end p-3 my-3"]
    ( [ Content.Element $
          Xml.element
            "div"
            [Xml.attribute "class" "fw-bold mb-1"]
            [Xml.string (case examples of _ NonEmpty.:| [] -> "Example:"; _ -> "Examples:")]
      ]
        <> concatMap exampleToContents (NonEmpty.toList examples)
    )

exampleToContents :: Example.Example -> [Content.Content Element.Element]
exampleToContents (Example.MkExample expr results) =
  [ Content.Element $
      Xml.element
        "div"
        [Xml.attribute "class" "my-1"]
        ( [ Content.Element $
              Xml.element
                "div"
                [Xml.attribute "class" "font-monospace"]
                [ Content.Element $
                    Xml.element
                      "span"
                      [Xml.attribute "class" "text-warning-emphasis user-select-none"]
                      [Xml.string ">>> "],
                  Xml.text expr
                ]
          ]
            <> fmap
              ( \r ->
                  Content.Element $
                    Xml.element
                      "div"
                      [Xml.attribute "class" "font-monospace text-body-secondary ps-3"]
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
  Xml.element "table" [Xml.attribute "class" "table table-bordered table-sm table-striped my-3"] (theadContents <> tbodyContents)
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
