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
import qualified Scrod.Core.PackageName as PackageName
import qualified Scrod.Core.Picture as Picture
import qualified Scrod.Core.Section as Section
import qualified Scrod.Core.Since as Since
import qualified Scrod.Core.Subordinates as Subordinates
import qualified Scrod.Core.Table as Table
import qualified Scrod.Core.TableCell as TableCell
import qualified Scrod.Core.Version as Version
import qualified Scrod.Core.Warning as Warning
import qualified Scrod.Css.AtRule as CssAtRule
import qualified Scrod.Css.Block as CssBlock
import qualified Scrod.Css.BlockContent as CssBlockContent
import qualified Scrod.Css.Declaration as CssDeclaration
import qualified Scrod.Css.Item as CssItem
import qualified Scrod.Css.Name as CssName
import qualified Scrod.Css.Rule as CssRule
import qualified Scrod.Css.Selector as CssSelector
import qualified Scrod.Css.Stylesheet as CssStylesheet
import qualified Scrod.Extra.Builder as Builder
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
        Xml.element "style" [] [Xml.raw . Builder.toText $ CssStylesheet.encode stylesheet]
    ]

bodyElement :: Module.Module -> Element.Element
bodyElement m =
  Xml.element
    "body"
    []
    ( [Content.Element (headerSection m)]
        <> metadataContents m
        <> exportsContents (Module.exports m)
        <> importsContents (Module.imports m)
        <> extensionsContents (Module.extensions m)
        <> itemsContents (Module.items m)
        <> [Content.Element (footerSection m)]
    )

-- Header section

headerSection :: Module.Module -> Element.Element
headerSection m =
  Xml.element
    "header"
    ( [Xml.attribute "class" "module-header"]
        <> foldMap (\(Located.MkLocated loc _) -> [lineAttribute loc]) (Module.name m)
    )
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

metadataContents :: Module.Module -> [Content.Content Element.Element]
metadataContents m =
  let items = languageItem (Module.language m) <> sinceItem (Module.since m)
   in if null items
        then []
        else
          [ Content.Element $
              Xml.element
                "section"
                [Xml.attribute "class" "metadata"]
                [Content.Element $ Xml.element "dl" [] items]
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

-- Footer section

footerSection :: Module.Module -> Element.Element
footerSection m =
  Xml.element
    "footer"
    [Xml.attribute "class" "generated-by"]
    [ Xml.string "Generated by ",
      Content.Element $
        Xml.element
          "a"
          [Xml.attribute "href" "https://github.com/tfausak/scrod"]
          [Xml.string "Scrod"],
      Xml.string " version ",
      Xml.text (versionToText (Module.version m))
    ]

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
            [Xml.attribute "class" "imports"]
            ( [ Content.Element $
                  Xml.element "summary" [] [Xml.string summary]
              ]
                <> [ Content.Element $
                       Xml.element
                         "ul"
                         [Xml.attribute "class" "import-list"]
                         (fmap importToContent imports)
                   ]
            )
      ]

importToContent :: Import.Import -> Content.Content Element.Element
importToContent i =
  Content.Element $
    Xml.element
      "li"
      []
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
                [Xml.attribute "class" "extensions"]
                ( [ Content.Element $
                      Xml.element "summary" [] [Xml.string summary]
                  ]
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
          Xml.element
            "a"
            [ Xml.attribute "class" cls,
              Xml.attribute "href" (extensionUrl name)
            ]
            [Xml.text name],
        Xml.string " "
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
      Xml.attribute "id" ("item-" <> show (ItemKey.unwrap key)),
      lineAttribute loc
    ]
    ( nameContents
        <> if isTypeVarSignature
          then signatureContents <> [Content.Element kindElement]
          else [Content.Element kindElement] <> signatureContents
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

    isTypeVarSignature :: Bool
    isTypeVarSignature = case itemKind of
      ItemKind.DataType -> True
      ItemKind.Newtype -> True
      ItemKind.TypeData -> True
      ItemKind.Class -> True
      _ -> False

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
        let prefix = if isTypeVarSignature then Text.pack " " else Text.pack " :: "
         in [ Content.Element $
                Xml.element
                  "span"
                  [Xml.attribute "class" "item-signature"]
                  [Xml.text (prefix <> sig)]
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

lineAttribute :: Location.Location -> Attribute.Attribute
lineAttribute loc =
  Xml.attribute "data-line" (show (Line.unwrap (Location.line loc)))

columnAttribute :: Location.Location -> Attribute.Attribute
columnAttribute loc =
  Xml.attribute "data-col" (show (Column.unwrap (Location.column loc)))

locationElement :: Location.Location -> Element.Element
locationElement loc =
  Xml.element
    "span"
    [ Xml.attribute "class" "item-location",
      lineAttribute loc,
      columnAttribute loc
    ]
    [ Xml.text
        ( Text.pack " (line "
            <> Text.pack (show (Line.unwrap (Location.line loc)))
            <> Text.pack ", col "
            <> Text.pack (show (Column.unwrap (Location.column loc)))
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
  ItemKind.Splice -> Text.pack "splice"

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
      rule
        ["body"]
        [ ("--scrod-text", "#333"),
          ("--scrod-text-secondary", "#666"),
          ("--scrod-text-muted", "#999"),
          ("--scrod-bg", "white"),
          ("--scrod-bg-subtle", "#f4f4f4"),
          ("--scrod-metadata-bg", "#f9f9f9"),
          ("--scrod-item-bg", "#fafafa"),
          ("--scrod-border", "#ddd"),
          ("--scrod-accent", "#0066cc"),
          ("--scrod-code-color", "#006600"),
          ("--scrod-module-color", "#660066"),
          ("--scrod-warning-bg", "#fff3cd"),
          ("--scrod-warning-border", "#ffc107"),
          ("--scrod-warning-text", "#856404"),
          ("--scrod-examples-bg", "#fffef0"),
          ("--scrod-examples-border", "#e6db74"),
          ("--scrod-property-bg", "#f0f8ff"),
          ("--scrod-property-border", "#4169e1"),
          ("--scrod-extension-bg", "#e8e8e8"),
          ("--scrod-extension-disabled-bg", "#ffebeb"),
          ("font-family", "system-ui, -apple-system, sans-serif"),
          ("line-height", "1.6"),
          ("max-width", "900px"),
          ("margin", "0 auto"),
          ("padding", "2rem"),
          ("color", "var(--scrod-text)"),
          ("background-color", "var(--scrod-bg)"),
          ("color-scheme", "light dark")
        ],
      rule ["h1"] [("border-bottom", "2px solid var(--scrod-text)"), ("padding-bottom", "0.5rem"), ("margin-top", "0")],
      rule ["h2"] [("border-bottom", "1px solid var(--scrod-text-secondary)"), ("padding-bottom", "0.3rem"), ("margin-top", "2rem")],
      rule ["h3", "h4", "h5", "h6"] [("margin-top", "1.5rem")],
      rule ["p"] [("margin", "1rem 0")],
      rule ["code"] [("background", "var(--scrod-bg-subtle)"), ("padding", "0.2em 0.4em"), ("border-radius", "3px"), ("font-family", "Consolas, Monaco, Menlo, monospace"), ("font-size", "0.9em")],
      rule ["pre"] [("background", "var(--scrod-bg-subtle)"), ("padding", "1rem"), ("border-radius", "5px"), ("overflow-x", "auto"), ("margin", "1rem 0")],
      rule ["pre > code"] [("background", "transparent"), ("padding", "0")],
      rule ["ul", "ol"] [("margin", "1rem 0"), ("padding-left", "2rem")],
      rule ["li"] [("margin", "0.25rem 0")],
      rule ["dl"] [("margin", "1rem 0")],
      rule ["dt"] [("font-weight", "bold"), ("margin-top", "0.5rem")],
      rule ["dd"] [("margin-left", "2rem"), ("margin-bottom", "0.5rem")],
      rule ["table"] [("border-collapse", "collapse"), ("width", "100%"), ("margin", "1rem 0")],
      rule ["th", "td"] [("border", "1px solid var(--scrod-border)"), ("padding", "0.5rem"), ("text-align", "left")],
      rule ["th"] [("background", "var(--scrod-bg-subtle)"), ("font-weight", "bold")],
      rule ["a"] [("color", "var(--scrod-accent)"), ("text-decoration", "none")],
      rule ["a:hover"] [("text-decoration", "underline")],
      rule ["img"] [("max-width", "100%"), ("height", "auto")],
      rule [".module-header"] [("margin-bottom", "2rem")],
      rule [".module-doc"] [("margin", "1rem 0")],
      rule [".metadata"] [("background", "var(--scrod-metadata-bg)"), ("border-left", "4px solid var(--scrod-accent)"), ("padding", "1rem"), ("margin", "1rem 0")],
      rule [".metadata > dt"] [("display", "inline")],
      rule [".metadata > dd"] [("display", "inline"), ("margin-left", "0.5rem")],
      rule [".exports"] [("margin", "2rem 0")],
      rule [".export-group"] [("margin", "1.5rem 0")],
      rule [".export-group-title"] [("font-weight", "bold"), ("color", "var(--scrod-text)"), ("margin-bottom", "0.5rem")],
      rule [".export-list"] [("list-style-type", "none"), ("padding-left", "0")],
      rule [".export-list > li"] [("padding", "0.25rem 0")],
      rule [".imports"] [("margin", "2rem 0")],
      rule [".imports > summary"] [("font-size", "1.5em"), ("font-weight", "bold"), ("cursor", "pointer"), ("border-bottom", "1px solid var(--scrod-text-secondary)"), ("padding-bottom", "0.3rem")],
      rule [".import-list"] [("list-style-type", "none"), ("padding-left", "0")],
      rule [".import-list > li"] [("padding", "0.25rem 0"), ("font-family", "Consolas, Monaco, Menlo, monospace"), ("font-size", "0.9em")],
      rule [".items"] [("margin", "2rem 0")],
      rule [".item"] [("margin", "1.5rem 0"), ("padding", "1rem"), ("background", "var(--scrod-item-bg)"), ("border-radius", "5px"), ("border-left", "4px solid var(--scrod-border)")],
      rule [".item-name"] [("font-family", "Consolas, Monaco, Menlo, monospace"), ("font-weight", "bold"), ("font-size", "1.1em"), ("color", "var(--scrod-code-color)")],
      rule [".item-key"] [("color", "var(--scrod-text-muted)"), ("font-size", "0.8em"), ("margin-left", "0.5rem")],
      rule [".item-doc"] [("margin-top", "0.5rem")],
      rule [".item-children"] [("margin-left", "1.5rem"), ("margin-top", "0.5rem"), ("border-left", "2px solid var(--scrod-border)"), ("padding-left", "1rem")],
      rule [".identifier"] [("color", "var(--scrod-code-color)"), ("font-family", "Consolas, Monaco, Menlo, monospace")],
      rule [".module-link"] [("color", "var(--scrod-module-color)"), ("font-family", "Consolas, Monaco, Menlo, monospace")],
      rule [".warning"] [("background", "var(--scrod-warning-bg)"), ("border-left", "4px solid var(--scrod-warning-border)"), ("padding", "1rem"), ("margin", "1rem 0")],
      rule [".warning-category"] [("font-weight", "bold"), ("color", "var(--scrod-warning-text)")],
      rule [".since"] [("color", "var(--scrod-text-secondary)"), ("font-size", "0.9em")],
      rule [".examples"] [("background", "var(--scrod-examples-bg)"), ("border-left", "4px solid var(--scrod-examples-border)"), ("padding", "1rem"), ("margin", "1rem 0")],
      rule [".example"] [("margin", "0.5rem 0")],
      rule [".example-expression"] [("font-family", "Consolas, Monaco, Menlo, monospace")],
      rule [".example-result"] [("font-family", "Consolas, Monaco, Menlo, monospace"), ("color", "var(--scrod-text-secondary)"), ("padding-left", "1rem")],
      rule [".property"] [("background", "var(--scrod-property-bg)"), ("border-left", "4px solid var(--scrod-property-border)"), ("padding", "1rem"), ("margin", "1rem 0")],
      rule [".math-inline"] [("font-style", "italic")],
      rule [".math-display"] [("display", "block"), ("text-align", "center"), ("margin", "1rem 0"), ("font-style", "italic")],
      rule [".extensions"] [("margin", "1rem 0")],
      rule [".extensions > summary"] [("font-size", "1.5em"), ("font-weight", "bold"), ("cursor", "pointer"), ("border-bottom", "1px solid var(--scrod-text-secondary)"), ("padding-bottom", "0.3rem")],
      rule [".extension"] [("display", "inline-block"), ("margin", "0.25rem"), ("padding", "0.25rem 0.5rem"), ("background", "var(--scrod-extension-bg)"), ("border-radius", "3px"), ("font-family", "Consolas, Monaco, Menlo, monospace"), ("font-size", "0.85em")],
      rule [".extension-disabled"] [("background", "var(--scrod-extension-disabled-bg)"), ("text-decoration", "line-through")],
      rule [".generated-by"] [("margin-top", "3rem"), ("padding-top", "1rem"), ("border-top", "1px solid var(--scrod-border)"), ("color", "var(--scrod-text-muted)"), ("font-size", "0.85em")],
      mediaRule
        "(prefers-color-scheme: dark)"
        [ rule
            ["body"]
            [ ("--scrod-text", "#ddd"),
              ("--scrod-text-secondary", "#aaa"),
              ("--scrod-text-muted", "#888"),
              ("--scrod-bg", "#121212"),
              ("--scrod-bg-subtle", "#2a2a2a"),
              ("--scrod-metadata-bg", "#1e1e1e"),
              ("--scrod-item-bg", "#1a1a1a"),
              ("--scrod-border", "#444"),
              ("--scrod-accent", "#4da6ff"),
              ("--scrod-code-color", "#66cc66"),
              ("--scrod-module-color", "#cc66cc"),
              ("--scrod-warning-bg", "#3d3000"),
              ("--scrod-warning-border", "#ffc107"),
              ("--scrod-warning-text", "#ffd75e"),
              ("--scrod-examples-bg", "#2a2800"),
              ("--scrod-examples-border", "#e6db74"),
              ("--scrod-property-bg", "#1a2233"),
              ("--scrod-property-border", "#6b8ef0"),
              ("--scrod-extension-bg", "#3a3a3a"),
              ("--scrod-extension-disabled-bg", "#3d1a1a")
            ]
        ]
    ]

rule :: [String] -> [(String, String)] -> CssItem.Item
rule selectors declarations =
  CssItem.StyleRule $
    CssRule.MkRule
      (fmap (CssSelector.MkSelector . Text.pack) selectors)
      (fmap (\(p, v) -> CssDeclaration.MkDeclaration (CssName.MkName $ Text.pack p) (Text.pack v) False) declarations)

mediaRule :: String -> [CssItem.Item] -> CssItem.Item
mediaRule query items =
  CssItem.ItemAtRule $
    CssAtRule.MkAtRule
      (CssName.MkName $ Text.pack "media")
      (Text.pack query)
      (Just $ CssBlock.MkBlock (fmap CssBlockContent.ContentItem items))
