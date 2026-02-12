// Renders the extensions section of a module page.
// TypeScript translation of `extensionsContents` from
// source/library/Scrod/Convert/ToHtml.hs (lines 429-619).

import { escapeHtml } from "./doc.js";

const ghcUserGuideBaseUrl =
  "https://downloads.haskell.org/ghc/latest/docs/users_guide/";

const extensionUrlPaths: Record<string, string> = {
  AllowAmbiguousTypes:
    "exts/ambiguous_types.html#extension-AllowAmbiguousTypes",
  ApplicativeDo: "exts/applicative_do.html#extension-ApplicativeDo",
  Arrows: "exts/arrows.html#extension-Arrows",
  BangPatterns: "exts/strict.html#extension-BangPatterns",
  BinaryLiterals: "exts/binary_literals.html#extension-BinaryLiterals",
  BlockArguments: "exts/block_arguments.html#extension-BlockArguments",
  CApiFFI: "exts/ffi.html#extension-CApiFFI",
  ConstrainedClassMethods:
    "exts/constrained_class_methods.html#extension-ConstrainedClassMethods",
  ConstraintKinds: "exts/constraint_kind.html#extension-ConstraintKinds",
  Cpp: "phases.html#extension-CPP",
  CUSKs: "exts/poly_kinds.html#extension-CUSKs",
  DataKinds: "exts/data_kinds.html#extension-DataKinds",
  DatatypeContexts:
    "exts/datatype_contexts.html#extension-DatatypeContexts",
  DeepSubsumption:
    "exts/rank_polymorphism.html#extension-DeepSubsumption",
  DefaultSignatures:
    "exts/default_signatures.html#extension-DefaultSignatures",
  DeriveAnyClass: "exts/derive_any_class.html#extension-DeriveAnyClass",
  DeriveDataTypeable:
    "exts/deriving_extra.html#extension-DeriveDataTypeable",
  DeriveFoldable: "exts/deriving_extra.html#extension-DeriveFoldable",
  DeriveFunctor: "exts/deriving_extra.html#extension-DeriveFunctor",
  DeriveGeneric: "exts/generics.html#extension-DeriveGeneric",
  DeriveLift: "exts/deriving_extra.html#extension-DeriveLift",
  DeriveTraversable:
    "exts/deriving_extra.html#extension-DeriveTraversable",
  DerivingStrategies:
    "exts/deriving_strategies.html#extension-DerivingStrategies",
  DerivingVia: "exts/deriving_via.html#extension-DerivingVia",
  DisambiguateRecordFields:
    "exts/disambiguate_record_fields.html#extension-DisambiguateRecordFields",
  DoAndIfThenElse:
    "exts/doandifthenelse.html#extension-DoAndIfThenElse",
  DuplicateRecordFields:
    "exts/duplicate_record_fields.html#extension-DuplicateRecordFields",
  EmptyCase: "exts/empty_case.html#extension-EmptyCase",
  EmptyDataDecls: "exts/nullary_types.html#extension-EmptyDataDecls",
  EmptyDataDeriving:
    "exts/empty_data_deriving.html#extension-EmptyDataDeriving",
  ExistentialQuantification:
    "exts/existential_quantification.html#extension-ExistentialQuantification",
  ExplicitForAll: "exts/explicit_forall.html#extension-ExplicitForAll",
  ExplicitLevelImports:
    "exts/template_haskell.html#extension-ExplicitLevelImports",
  ExplicitNamespaces:
    "exts/explicit_namespaces.html#extension-ExplicitNamespaces",
  ExtendedDefaultRules:
    "ghci.html#extension-ExtendedDefaultRules",
  ExtendedLiterals:
    "exts/extended_literals.html#extension-ExtendedLiterals",
  FieldSelectors: "exts/field_selectors.html#extension-FieldSelectors",
  FlexibleContexts:
    "exts/flexible_contexts.html#extension-FlexibleContexts",
  FlexibleInstances: "exts/instances.html#extension-FlexibleInstances",
  ForeignFunctionInterface:
    "exts/ffi.html#extension-ForeignFunctionInterface",
  FunctionalDependencies:
    "exts/functional_dependencies.html#extension-FunctionalDependencies",
  GADTs: "exts/gadt.html#extension-GADTs",
  GADTSyntax: "exts/gadt_syntax.html#extension-GADTSyntax",
  GeneralisedNewtypeDeriving:
    "exts/newtype_deriving.html#extension-GeneralisedNewtypeDeriving",
  GHC2021: "exts/control.html#extension-GHC2021",
  GHC2024: "exts/control.html#extension-GHC2024",
  GHCForeignImportPrim:
    "exts/ffi.html#extension-GHCForeignImportPrim",
  Haskell2010: "exts/control.html#extension-Haskell2010",
  Haskell98: "exts/control.html#extension-Haskell98",
  HexFloatLiterals:
    "exts/hex_float_literals.html#extension-HexFloatLiterals",
  ImplicitParams:
    "exts/implicit_parameters.html#extension-ImplicitParams",
  ImplicitPrelude:
    "exts/rebindable_syntax.html#extension-ImplicitPrelude",
  ImplicitStagePersistence:
    "exts/template_haskell.html#extension-ImplicitStagePersistence",
  ImportQualifiedPost:
    "exts/import_qualified_post.html#extension-ImportQualifiedPost",
  ImpredicativeTypes:
    "exts/impredicative_types.html#extension-ImpredicativeTypes",
  IncoherentInstances:
    "exts/instances.html#extension-IncoherentInstances",
  InstanceSigs: "exts/instances.html#extension-InstanceSigs",
  InterruptibleFFI: "exts/ffi.html#extension-InterruptibleFFI",
  KindSignatures:
    "exts/kind_signatures.html#extension-KindSignatures",
  LambdaCase: "exts/lambda_case.html#extension-LambdaCase",
  LexicalNegation:
    "exts/lexical_negation.html#extension-LexicalNegation",
  LiberalTypeSynonyms:
    "exts/liberal_type_synonyms.html#extension-LiberalTypeSynonyms",
  LinearTypes: "exts/linear_types.html#extension-LinearTypes",
  ListTuplePuns: "exts/data_kinds.html#extension-ListTuplePuns",
  MagicHash: "exts/magic_hash.html#extension-MagicHash",
  MonadComprehensions:
    "exts/monad_comprehensions.html#extension-MonadComprehensions",
  MonoLocalBinds:
    "exts/let_generalisation.html#extension-MonoLocalBinds",
  MonomorphismRestriction:
    "exts/monomorphism.html#extension-MonomorphismRestriction",
  MultilineStrings:
    "exts/multiline_strings.html#extension-MultilineStrings",
  MultiParamTypeClasses:
    "exts/multi_param_type_classes.html#extension-MultiParamTypeClasses",
  MultiWayIf: "exts/multiway_if.html#extension-MultiWayIf",
  NamedDefaults: "exts/named_defaults.html#extension-NamedDefaults",
  NamedFieldPuns: "exts/record_puns.html#extension-NamedFieldPuns",
  NamedWildCards:
    "exts/partial_type_signatures.html#extension-NamedWildCards",
  NegativeLiterals:
    "exts/negative_literals.html#extension-NegativeLiterals",
  NondecreasingIndentation:
    "bugs.html#extension-NondecreasingIndentation",
  NPlusKPatterns: "exts/nk_patterns.html#extension-NPlusKPatterns",
  NullaryTypeClasses:
    "exts/multi_param_type_classes.html#extension-NullaryTypeClasses",
  NumDecimals: "exts/num_decimals.html#extension-NumDecimals",
  NumericUnderscores:
    "exts/numeric_underscores.html#extension-NumericUnderscores",
  OrPatterns: "exts/or_patterns.html#extension-OrPatterns",
  OverlappingInstances:
    "exts/instances.html#extension-OverlappingInstances",
  OverloadedLabels:
    "exts/overloaded_labels.html#extension-OverloadedLabels",
  OverloadedLists:
    "exts/overloaded_lists.html#extension-OverloadedLists",
  OverloadedRecordDot:
    "exts/overloaded_record_dot.html#extension-OverloadedRecordDot",
  OverloadedRecordUpdate:
    "exts/overloaded_record_update.html#extension-OverloadedRecordUpdate",
  OverloadedStrings:
    "exts/overloaded_strings.html#extension-OverloadedStrings",
  PackageImports:
    "exts/package_qualified_imports.html#extension-PackageImports",
  ParallelListComp:
    "exts/parallel_list_comprehensions.html#extension-ParallelListComp",
  PartialTypeSignatures:
    "exts/partial_type_signatures.html#extension-PartialTypeSignatures",
  PatternGuards: "exts/pattern_guards.html#extension-PatternGuards",
  PatternSynonyms:
    "exts/pattern_synonyms.html#extension-PatternSynonyms",
  PolyKinds: "exts/poly_kinds.html#extension-PolyKinds",
  PostfixOperators:
    "exts/rebindable_syntax.html#extension-PostfixOperators",
  QualifiedDo: "exts/qualified_do.html#extension-QualifiedDo",
  QuantifiedConstraints:
    "exts/quantified_constraints.html#extension-QuantifiedConstraints",
  QuasiQuotes: "exts/template_haskell.html#extension-QuasiQuotes",
  Rank2Types: "exts/rank_polymorphism.html#extension-Rank2Types",
  RankNTypes: "exts/rank_polymorphism.html#extension-RankNTypes",
  RebindableSyntax:
    "exts/rebindable_syntax.html#extension-RebindableSyntax",
  RecordWildCards:
    "exts/record_wildcards.html#extension-RecordWildCards",
  RecursiveDo: "exts/recursive_do.html#extension-RecursiveDo",
  RequiredTypeArguments:
    "exts/required_type_arguments.html#extension-RequiredTypeArguments",
  RoleAnnotations: "exts/roles.html#extension-RoleAnnotations",
  Safe: "exts/safe_haskell.html#extension-Safe",
  ScopedTypeVariables:
    "exts/scoped_type_variables.html#extension-ScopedTypeVariables",
  StandaloneDeriving:
    "exts/standalone_deriving.html#extension-StandaloneDeriving",
  StandaloneKindSignatures:
    "exts/poly_kinds.html#extension-StandaloneKindSignatures",
  StarIsType: "exts/poly_kinds.html#extension-StarIsType",
  StaticPointers:
    "exts/static_pointers.html#extension-StaticPointers",
  Strict: "exts/strict.html#extension-Strict",
  StrictData: "exts/strict.html#extension-StrictData",
  TemplateHaskell:
    "exts/template_haskell.html#extension-TemplateHaskell",
  TemplateHaskellQuotes:
    "exts/template_haskell.html#extension-TemplateHaskellQuotes",
  TraditionalRecordSyntax:
    "exts/traditional_record_syntax.html#extension-TraditionalRecordSyntax",
  TransformListComp:
    "exts/generalised_list_comprehensions.html#extension-TransformListComp",
  Trustworthy: "exts/safe_haskell.html#extension-Trustworthy",
  TupleSections: "exts/tuple_sections.html#extension-TupleSections",
  TypeAbstractions:
    "exts/type_abstractions.html#extension-TypeAbstractions",
  TypeApplications:
    "exts/type_applications.html#extension-TypeApplications",
  TypeData: "exts/type_data.html#extension-TypeData",
  TypeFamilies: "exts/type_families.html#extension-TypeFamilies",
  TypeFamilyDependencies:
    "exts/type_families.html#extension-TypeFamilyDependencies",
  TypeInType: "exts/poly_kinds.html#extension-TypeInType",
  TypeOperators: "exts/type_operators.html#extension-TypeOperators",
  TypeSynonymInstances:
    "exts/instances.html#extension-TypeSynonymInstances",
  UnboxedSums: "exts/primitives.html#extension-UnboxedSums",
  UnboxedTuples: "exts/primitives.html#extension-UnboxedTuples",
  UndecidableInstances:
    "exts/instances.html#extension-UndecidableInstances",
  UndecidableSuperClasses:
    "exts/undecidable_super_classes.html#extension-UndecidableSuperClasses",
  UnicodeSyntax: "exts/unicode_syntax.html#extension-UnicodeSyntax",
  UnliftedDatatypes:
    "exts/primitives.html#extension-UnliftedDatatypes",
  UnliftedFFITypes: "exts/ffi.html#extension-UnliftedFFITypes",
  UnliftedNewtypes: "exts/primitives.html#extension-UnliftedNewtypes",
  Unsafe: "exts/safe_haskell.html#extension-Unsafe",
  ViewPatterns: "exts/view_patterns.html#extension-ViewPatterns",
};

function extensionUrl(name: string): string {
  const path = extensionUrlPaths[name];
  if (path != null) {
    return ghcUserGuideBaseUrl + path;
  }
  return ghcUserGuideBaseUrl + "exts/table.html";
}

/**
 * Render the extensions section as a collapsible details element with badges.
 * Returns an empty string if the extensions record is empty.
 */
export function renderExtensions(
  extensions: Record<string, boolean>
): string {
  const entries = Object.entries(extensions);
  if (entries.length === 0) {
    return "";
  }

  const count = entries.length;
  const noun = count === 1 ? "extension" : "extensions";
  const summary = `Extensions (${count} ${noun})`;

  const badges = entries
    .map(([name, enabled]) => {
      const cls = enabled
        ? "badge bg-secondary-subtle text-body font-monospace me-1 mb-1"
        : "badge bg-danger-subtle text-body text-decoration-line-through font-monospace me-1 mb-1";
      const href = extensionUrl(name);
      return `<a class="${cls}" href="${escapeHtml(href)}">${escapeHtml(name)}</a>`;
    })
    .join("");

  return (
    `<details class="my-3">` +
    `<summary class="fs-4 fw-bold">${escapeHtml(summary)}</summary>` +
    `<div class="mt-2">${badges}</div>` +
    `</details>`
  );
}
