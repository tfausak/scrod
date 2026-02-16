{-# LANGUAGE MultilineStrings #-}

-- | Render a 'Module.Module' as a self-contained HTML document.
--
-- Produces a complete @\<html\>@ document with Bootstrap 5 and KaTeX
-- loaded from CDNs. The output uses the custom XML types in
-- @Scrod.Xml.*@ and can be serialized with 'Xml.encode'.
module Scrod.Convert.ToHtml (toHtml) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
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
toHtml x =
  Xml.MkDocument
    { Xml.prolog =
        [ Misc.Declaration . XmlDeclaration.MkDeclaration (XmlName.MkName $ t "doctype") $ t "html"
        ],
      Xml.root =
        Xml.element
          "html"
          []
          [ headElement x,
            bodyElement x
          ]
    }

headElement :: Module.Module -> Content.Content Element.Element
headElement x =
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
      element "title" [] [Xml.text $ moduleTitle x],
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
            const setTheme = (x) => document.documentElement.dataset.bsTheme = x.matches ? 'dark' : 'light';
            setTheme(dark);
            dark.addEventListener('change', setTheme);
            import('https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js').then((x) => x.default(document.body));
            """
        ]
    ]

moduleTitle :: Module.Module -> Text.Text
moduleTitle =
  maybe (t "Documentation") (ModuleName.unwrap . Located.value)
    . Module.name

bodyElement :: Module.Module -> Content.Content Element.Element
bodyElement x =
  element
    "body"
    []
    [ element
        "div"
        [("class", "container py-5")]
        ( [element "h1" [("class", "text-break")] [Xml.text $ moduleTitle x]]
            <> foldMap (pure . warningAlert) (Module.warning x)
            <> foldMap (pure . sinceAlert) (Module.since x)
            <> extensionsContents (Module.language x) (Module.extensions x)
            <> importsContents (Module.imports x)
            <> [element "hr" [] []]
            <> docContents (Module.documentation x)
            <> exportsContents (Module.exports x)
            <> itemsContents (Module.items x)
            <> [element "hr" [] []]
            <> [footerSection x]
        )
    ]

warningAlert :: Warning.Warning -> Content.Content Element.Element
warningAlert x =
  element
    "div"
    [ ("class", "alert alert-warning"),
      ("role", "alert")
    ]
    [ element "strong" [] [Xml.string "Warning"],
      Xml.string " (",
      element "span" [("class", "text-break")] [Xml.text . Category.unwrap $ Warning.category x],
      Xml.string "): ",
      element "span" [("class", "text-break")] [Xml.text $ Warning.value x]
    ]

sinceAlert :: Since.Since -> Content.Content Element.Element
sinceAlert x =
  element
    "div"
    [ ("class", "alert alert-info"),
      ("role", "alert")
    ]
    [ element "strong" [] [Xml.string "Since"],
      Xml.string ": ",
      element "span" [("class", "text-break")] [Xml.text $ sinceToText x]
    ]

versionToText :: Version.Version -> Text.Text
versionToText = t . List.intercalate "." . NonEmpty.toList . fmap show . Version.unwrap

sinceToText :: Since.Since -> Text.Text
sinceToText x =
  foldMap ((<> t "-") . PackageName.unwrap) (Since.package x)
    <> versionToText (Since.version x)

-- Footer section

footerSection :: Module.Module -> Content.Content Element.Element
footerSection m =
  element
    "footer"
    [("class", "text-secondary")]
    [ Xml.string "Generated by ",
      element
        "a"
        [("href", "https://github.com/tfausak/scrod")]
        [Xml.string "Scrod"],
      Xml.string " version ",
      element "spam" [("class", "text-break")] [Xml.text . versionToText $ Module.version m],
      Xml.string "."
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
    [element "li" [liClass] [sectionContent section]]
  Export.Doc doc ->
    [element "li" [liClass] [element "div" [("class", "mt-1")] (docContents doc)]]
  Export.DocNamed name ->
    [element "li" [liClass] [element "div" [("class", "mt-1")] [Xml.text (t "\x00a7" <> name)]]]
  where
    liClass :: (String, String)
    liClass = ("class", "list-group-item bg-transparent py-1 px-2")

exportIdentifierToHtml :: ExportIdentifier.ExportIdentifier -> Content.Content Element.Element
exportIdentifierToHtml ei =
  element
    "div"
    [("class", "py-1")]
    ( foldMap (\w -> [warningAlert w]) (ExportIdentifier.warning ei)
        <> [ element
               "code"
               [("class", "font-monospace")]
               ( [Xml.text (exportNameToText (ExportIdentifier.name ei))]
                   <> subordinatesToContents (ExportIdentifier.subordinates ei)
               )
           ]
        <> foldMap
          ( \doc ->
              [element "div" [("class", "mt-1")] (docContents doc)]
          )
          (ExportIdentifier.doc ei)
    )

exportNameToText :: ExportName.ExportName -> Text.Text
exportNameToText en =
  kindPrefix <> ExportName.name en
  where
    kindPrefix :: Text.Text
    kindPrefix = case ExportName.kind en of
      Nothing -> Text.empty
      Just ExportNameKind.Pattern -> t "pattern "
      Just ExportNameKind.Type -> t "type "
      Just ExportNameKind.Module -> t "module "

subordinatesToContents :: Maybe Subordinates.Subordinates -> [Content.Content Element.Element]
subordinatesToContents Nothing = []
subordinatesToContents (Just subs) =
  let wildcardText :: Text.Text
      wildcardText = t ".."
      explicitTexts :: [Text.Text]
      explicitTexts = fmap ExportName.name (Subordinates.explicit subs)
      allTexts :: [Text.Text]
      allTexts = if Subordinates.wildcard subs then wildcardText : explicitTexts else explicitTexts
      combined :: Text.Text
      combined = Text.intercalate (t ", ") allTexts
   in [Xml.text (t "(" <> combined <> t ")")]

sectionContent :: Section.Section -> Content.Content Element.Element
sectionContent x =
  element (levelToName . Header.level $ Section.header x) []
    . docContents
    . Header.title
    $ Section.header x

-- Imports section

importsContents :: [Import.Import] -> [Content.Content Element.Element]
importsContents imports =
  [ element "h2" [] [Xml.string "Imports"],
    case length imports of
      0 -> Xml.string "None."
      count ->
        element
          "details"
          []
          [ element
              "summary"
              []
              [ Xml.string "Show/hide ",
                Xml.string $ pluralize count "import",
                Xml.string "."
              ],
            element "ul" [] . fmap importContent $ List.sortOn Import.name imports
          ]
  ]

importContent :: Import.Import -> Content.Content Element.Element
importContent x =
  element
    "li"
    []
    [ element "span" [("class", "text-break")] [Xml.text . ModuleName.unwrap $ Import.name x],
      case Import.package x of
        Nothing -> Xml.string ""
        Just p ->
          element
            "span"
            [("class", "text-body-secondary")]
            [ Xml.string " (",
              element "span" [("class", "text-break")] [Xml.text $ PackageName.unwrap p],
              Xml.string ")"
            ]
    ]

-- Extensions section

extensionsContents ::
  Maybe Language.Language ->
  Map.Map Extension.Extension Bool ->
  [Content.Content Element.Element]
extensionsContents language extensions =
  [ element "h2" [] [Xml.string "Extensions"],
    case length language + length extensions of
      0 -> Xml.string "None."
      count ->
        element
          "details"
          []
          [ element
              "summary"
              []
              [ Xml.string "Show/hide ",
                Xml.string $ pluralize count "extension",
                Xml.string "."
              ],
            element "ul" []
              . fmap (uncurry extensionContent)
              $ foldMap (\x -> [(Language.unwrap x, True)]) language
                <> Map.toList (Map.mapKeys Extension.unwrap extensions)
          ]
  ]

extensionContent :: Text.Text -> Bool -> Content.Content Element.Element
extensionContent x y =
  element
    "li"
    []
    [ element
        "a"
        [ ("class", "link-underline link-underline-opacity-25 text-break"),
          ("href", extensionUrl x)
        ]
        [ Xml.string $ if y then "" else "No",
          Xml.text x
        ]
    ]

pluralize :: Int -> String -> String
pluralize count singular = pluralizeWith count singular (singular <> "s")

pluralizeWith :: Int -> String -> String -> String
pluralizeWith count singular plural =
  show count <> " " <> if count == 1 then singular else plural

extensionUrl :: Text.Text -> String
extensionUrl name =
  ghcUserGuideBaseUrl <> Map.findWithDefault "exts/table.html" name extensionUrlPaths

ghcUserGuideBaseUrl :: String
ghcUserGuideBaseUrl = "https://downloads.haskell.org/ghc/latest/docs/users_guide/"

extensionUrlPaths :: Map.Map Text.Text String
extensionUrlPaths =
  Map.mapKeys t . Map.fromList $
    [ ("AllowAmbiguousTypes", "exts/ambiguous_types.html#extension-AllowAmbiguousTypes"),
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
-- itemsContents [] = []
-- itemsContents items =
--   [ element
--       "section"
--       [("class", "my-4")]
--       ( [element "h2" [("class", "border-bottom pb-1 mt-4")] [Xml.string "Declarations"]]
--           <> concatMap renderItemWithChildren topLevelItems
--       )
--   ]
--   where
--     childrenMap :: Map.Map Natural.Natural [Located.Located Item.Item]
--     childrenMap = foldr addChild Map.empty items

--     addChild :: Located.Located Item.Item -> Map.Map Natural.Natural [Located.Located Item.Item] -> Map.Map Natural.Natural [Located.Located Item.Item]
--     addChild li acc = case Item.parentKey (Located.value li) of
--       Nothing -> acc
--       Just pk -> Map.insertWith (<>) (ItemKey.unwrap pk) [li] acc

--     topLevelItems :: [Located.Located Item.Item]
--     topLevelItems = filter (isTopLevel . Located.value) items

--     isTopLevel :: Item.Item -> Bool
--     isTopLevel item = case Item.parentKey item of
--       Nothing -> True
--       Just _ -> False

--     renderItemWithChildren :: Located.Located Item.Item -> [Content.Content Element.Element]
--     renderItemWithChildren li =
--       let k = ItemKey.unwrap (Item.key (Located.value li))
--           children = Map.findWithDefault [] k childrenMap
--        in [itemToHtml li]
--             <> if null children
--               then []
--               else
--                 [ element
--                     "div"
--                     [("class", "ms-4 mt-2 border-start border-2 ps-3")]
--                     (concatMap renderItemWithChildren children)
--                 ]
itemsContents items =
  element "h2" [] [Xml.string "Declarations"]
    -- TODO: Group children under parent, but keep new styling of cards.
    : fmap itemContent items

itemContent :: Located.Located Item.Item -> Content.Content Element.Element
itemContent item =
  element
    "div"
    [ ("class", "card my-3"),
      ("id", "item-" <> (show . ItemKey.unwrap . Item.key $ Located.value item))
    ]
    [ element
        "div"
        [("class", "align-items-center card-header d-flex")]
        [ element
            "div"
            []
            [ element "code" [("class", "text-break")] [Xml.text . foldMap ItemName.unwrap . Item.name $ Located.value item]
            ],
          element
            "div"
            [("class", "mx-1")]
            [ element "span" [("class", "badge text-bg-secondary")] [Xml.string . kindToString . Item.kind $ Located.value item]
            ],
          -- TODO: Signature goes before kind for some things, like type variables.
          case Item.signature $ Located.value item of
            Nothing -> Xml.string ""
            Just signature ->
              element
                "div"
                [("class", "mx-1")]
                [ element
                    "code"
                    [("class", "text-break text-secondary")]
                    -- TODO: Signature prefix is different depending on the kind.
                    [ Xml.string ":: ",
                      Xml.text signature
                    ]
                ],
          element
            "div"
            [("class", "ms-auto")]
            [ element
                "button"
                [ ("class", "btn btn-outline-secondary btn-sm"),
                  ("data-col", show . Column.unwrap . Location.column $ Located.location item),
                  ("data-line", show . Line.unwrap . Location.line $ Located.location item),
                  ("type", "button")
                ]
                [ Xml.string . show . Line.unwrap . Location.line $ Located.location item,
                  Xml.string ":",
                  Xml.string . show . Column.unwrap . Location.column $ Located.location item
                ]
            ]
        ],
      element
        "div"
        [("class", "card-body")]
        $ foldMap (pure . sinceAlert) (Item.since $ Located.value item)
          <> docContents (Item.documentation $ Located.value item)
    ]

-- itemToHtml :: Located.Located Item.Item -> Content.Content Element.Element
-- itemToHtml item =
--   element
--     "div"
--     [ ("class", "card mb-3 border-start border-4"),
--       ("style", kindBorderStyle itemKind),
--       ("id", "item-" <> (show . ItemKey.unwrap . Item.key $ Located.value item)),
--       lineAttribute $ Located.location item
--     ]
--     ( [ element
--           "div"
--           [("class", "card-header bg-transparent d-flex align-items-center py-2")]
--           ( nameContents
--               <> sigBeforeKind
--               <> [kindContent]
--               <> sigAfterKind
--               <> sinceContents
--               <> [locationElement $ Located.location item]
--           )
--       ]
--         <> docContents'
--     )
--   where
--     itemKind = Item.kind $ Located.value item

--     nameContents :: [Content.Content Element.Element]
--     nameContents = case Item.name $ Located.value item of
--       Nothing -> []
--       Just n ->
--         [element "span" [("class", "font-monospace fw-bold text-success")] [Xml.text (ItemName.unwrap n)]]

--     isTypeVarSignature :: Bool
--     isTypeVarSignature = case itemKind of
--       ItemKind.DataType -> True
--       ItemKind.Newtype -> True
--       ItemKind.TypeData -> True
--       ItemKind.TypeSynonym -> True
--       ItemKind.Class -> True
--       _ -> False

--     kindContent :: Content.Content Element.Element
--     kindContent =
--       element
--         "span"
--         [("class", "badge " <> kindBadgeClass itemKind <> " ms-2")]
--         [Xml.string (kindToString itemKind)]

--     sigBeforeKind :: [Content.Content Element.Element]
--     sigBeforeKind =
--       if isTypeVarSignature then signatureContents else []

--     sigAfterKind :: [Content.Content Element.Element]
--     sigAfterKind =
--       if isTypeVarSignature then [] else signatureContents

--     signatureContents :: [Content.Content Element.Element]
--     signatureContents = case Item.signature $ Located.value item of
--       Nothing -> []
--       Just sig ->
--         let prefix = if isTypeVarSignature then t "\x00a0" else t " :: "
--          in [ element
--                 "span"
--                 [("class", "font-monospace text-body-secondary")]
--                 [Xml.text (prefix <> sig)]
--             ]

--     sinceContents :: [Content.Content Element.Element]
--     sinceContents = case Item.since $ Located.value item of
--       Nothing -> []
--       Just s ->
--         [ element
--             "span"
--             [("class", "text-body-secondary small ms-2")]
--             [Xml.text (t "since " <> sinceToText s)]
--         ]

--     docContents' :: [Content.Content Element.Element]
--     docContents' = case Item.documentation $ Located.value item of
--       Doc.Empty -> []
--       doc -> [element "div" [("class", "card-body")] (docContents doc)]

kindToString :: ItemKind.ItemKind -> String
kindToString x = case x of
  ItemKind.Annotation -> "annotation"
  ItemKind.Class -> "class"
  ItemKind.ClassInstance -> "instance"
  ItemKind.ClassMethod -> "method"
  ItemKind.ClosedTypeFamily -> "type family"
  ItemKind.CompletePragma -> "complete"
  ItemKind.DataConstructor -> "constructor"
  ItemKind.DataFamily -> "data family"
  ItemKind.DataFamilyInstance -> "data instance"
  ItemKind.DataType -> "data"
  ItemKind.Default -> "default"
  ItemKind.DefaultMethodSignature -> "default"
  ItemKind.DerivedInstance -> "instance"
  ItemKind.FixitySignature -> "fixity"
  ItemKind.ForeignExport -> "foreign export"
  ItemKind.ForeignImport -> "foreign import"
  ItemKind.Function -> "function"
  ItemKind.GADTConstructor -> "constructor"
  ItemKind.InlineSignature -> "inline"
  ItemKind.MinimalPragma -> "minimal"
  ItemKind.Newtype -> "newtype"
  ItemKind.OpenTypeFamily -> "type family"
  ItemKind.PatternBinding -> "pattern"
  ItemKind.PatternSynonym -> "pattern"
  ItemKind.RecordField -> "field"
  ItemKind.RoleAnnotation -> "role"
  ItemKind.Rule -> "rule"
  ItemKind.SpecialiseSignature -> "specialise"
  ItemKind.Splice -> "splice"
  ItemKind.StandaloneDeriving -> "instance"
  ItemKind.StandaloneKindSig -> "kind"
  ItemKind.TypeData -> "type data"
  ItemKind.TypeFamilyInstance -> "type instance"
  ItemKind.TypeSynonym -> "type"
  ItemKind.Warning -> "warning"

-- Doc to HTML conversion

docContents :: Doc.Doc -> [Content.Content Element.Element]
docContents doc = case doc of
  Doc.Empty -> [Xml.string ""]
  Doc.Append xs -> foldMap docContents xs
  Doc.String x -> [Xml.text x]
  Doc.Paragraph x -> [element "p" [] $ docContents x]
  Doc.Identifier x -> [identifierContent x]
  Doc.Module x -> [modLinkContent x]
  Doc.Emphasis x -> [element "em" [] $ docContents x]
  Doc.Monospaced x -> [element "code" [] $ docContents x]
  Doc.Bold x -> [element "strong" [] $ docContents x]
  Doc.UnorderedList xs -> [element "ul" [] $ fmap (element "li" [] . docContents) xs]
  Doc.OrderedList xs -> [element "ol" [] $ fmap orderedListItemContent xs]
  Doc.DefList xs -> [element "dl" [] $ foldMap definitionContents xs]
  Doc.CodeBlock x -> [codeBlockContent x]
  Doc.Hyperlink x -> [hyperlinkContent x]
  Doc.Pic x -> [pictureContent x]
  Doc.MathInline x -> [Xml.string "\\(", Xml.text x, Xml.string "\\)"]
  Doc.MathDisplay x -> [Xml.string "\\[", Xml.text x, Xml.string "\\]"]
  Doc.AName x -> [element "a" [("id", Text.unpack x)] [Xml.string ""]]
  Doc.Property x -> [propertyContent x]
  Doc.Examples xs -> exampleContent <$> NonEmpty.toList xs
  Doc.Header x -> [headerContent x]
  Doc.Table x -> [tableContent x]

definitionContents :: Definition.Definition Doc.Doc -> [Content.Content Element.Element]
definitionContents x =
  [ element "dt" [] . docContents $ Definition.term x,
    element "dd" [] . docContents $ Definition.definition x
  ]

codeBlockContent :: Doc.Doc -> Content.Content Element.Element
codeBlockContent x =
  element
    "div"
    [("class", "card my-3")]
    [ element
        "div"
        [("class", "card-body")]
        [ element
            "pre"
            [("class", "mb-0")]
            [ element "code" [] $ docContents x
            ]
        ]
    ]

orderedListItemContent :: NumberedItem.NumberedItem Doc.Doc -> Content.Content Element.Element
orderedListItemContent x = element "li" [("value", show $ NumberedItem.index x)] . docContents $ NumberedItem.item x

propertyContent :: Text.Text -> Content.Content Element.Element
propertyContent x =
  element
    "div"
    [("class", "card my-3")]
    [ element
        "div"
        [("class", "card-header")]
        [ element "strong" [] [Xml.string "Property"]
        ],
      element
        "div"
        [("class", "card-body")]
        [ element
            "pre"
            [("class", "mb-0")]
            [ element "code" [] [Xml.text $ Text.stripEnd x]
            ]
        ]
    ]

identifierContent :: Identifier.Identifier -> Content.Content Element.Element
identifierContent x =
  element
    "span"
    []
    [ element "code" [] [Xml.text $ Identifier.value x],
      case Identifier.namespace x of
        Nothing -> Xml.string ""
        Just ns ->
          element
            "span"
            [("class", "text-body-secondary")]
            [ Xml.string " (",
              Xml.string $ case ns of
                Namespace.Value -> "value"
                Namespace.Type -> "type",
              Xml.string ")"
            ]
    ]

modLinkContent :: ModLink.ModLink Doc.Doc -> Content.Content Element.Element
modLinkContent x =
  element "code" []
    . maybe [Xml.text . ModuleName.unwrap $ ModLink.name x] docContents
    $ ModLink.label x

hyperlinkContent :: Hyperlink.Hyperlink Doc.Doc -> Content.Content Element.Element
hyperlinkContent x =
  element "a" [("href", Text.unpack $ Hyperlink.url x)]
    . maybe [Xml.text $ Hyperlink.url x] docContents
    $ Hyperlink.label x

pictureContent :: Picture.Picture -> Content.Content Element.Element
pictureContent x =
  element
    "img"
    ( ("src", Text.unpack $ Picture.uri x)
        : foldMap (\y -> [("title", Text.unpack y)]) (Picture.title x)
    )
    []

exampleContent :: Example.Example -> Content.Content Element.Element
exampleContent x =
  element
    "div"
    [("class", "card my-3")]
    [ element
        "div"
        [("class", "card-header")]
        [ element "strong" [] [Xml.string "Example"]
        ],
      element
        "div"
        [("class", "card-body")]
        [ element
            "pre"
            [("class", "mb-0")]
            [ element
                "code"
                []
                [ Xml.string ">>> ",
                  element "strong" [] [Xml.text $ Example.expression x],
                  Xml.text . foldMap (t "\n" <>) $ Example.result x
                ]
            ]
        ]
    ]

headerContent :: Header.Header Doc.Doc -> Content.Content Element.Element
headerContent x =
  element (levelToName $ Header.level x) [] . docContents $ Header.title x

levelToName :: Level.Level -> String
levelToName x = case x of
  Level.One -> "h1"
  Level.Two -> "h2"
  Level.Three -> "h3"
  Level.Four -> "h4"
  Level.Five -> "h5"
  Level.Six -> "h6"

tableContent :: Table.Table Doc.Doc -> Content.Content Element.Element
tableContent tbl =
  let tr n = element "tr" [] . fmap (td n)
      td n c =
        element
          n
          [ ("colspan", show $ TableCell.colspan c),
            ("rowspan", show $ TableCell.rowspan c)
          ]
          . docContents
          $ TableCell.contents c
   in element
        "table"
        [("class", "my-3 table table-bordered table-striped")]
        [ element "thead" [] . fmap (tr "th") $ Table.headerRows tbl,
          element "tbody" [] . fmap (tr "td") $ Table.bodyRows tbl
        ]
