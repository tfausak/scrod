// src/sections/doc.ts
function escapeHtml(text) {
  return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;").replace(/'/g, "&#39;");
}
function renderDoc(doc) {
  switch (doc.type) {
    case "Empty":
      return "";
    case "Append":
      return renderDoc(doc.value[0]) + renderDoc(doc.value[1]);
    case "String":
      return escapeHtml(doc.value);
    case "Paragraph":
      return `<p>${renderDoc(doc.value)}</p>`;
    case "Identifier": {
      const id = doc.value;
      let prefix = "";
      if (id.namespace != null) {
        prefix = id.namespace.type === "Value" ? "v'" : "t'";
      }
      return `<code class="font-monospace text-success">${escapeHtml(prefix + id.value)}</code>`;
    }
    case "Module": {
      const mod = doc.value;
      const inner = mod.label != null ? renderDoc(mod.label) : escapeHtml(mod.name);
      return `<code class="font-monospace text-info">${inner}</code>`;
    }
    case "Emphasis":
      return `<em>${renderDoc(doc.value)}</em>`;
    case "Monospaced":
      return `<code>${renderDoc(doc.value)}</code>`;
    case "Bold":
      return `<strong>${renderDoc(doc.value)}</strong>`;
    case "UnorderedList": {
      const items = doc.value.map((item) => `<li>${renderDoc(item)}</li>`).join("");
      return `<ul>${items}</ul>`;
    }
    case "OrderedList": {
      const items = doc.value.map(([n, d]) => `<li value="${n}">${renderDoc(d)}</li>`).join("");
      return `<ol>${items}</ol>`;
    }
    case "DefList": {
      const entries = doc.value.map(
        ([term, def]) => `<dt>${renderDoc(term)}</dt><dd>${renderDoc(def)}</dd>`
      ).join("");
      return `<dl>${entries}</dl>`;
    }
    case "CodeBlock":
      return `<pre class="bg-body-secondary rounded p-3 my-3"><code>${renderDoc(doc.value)}</code></pre>`;
    case "Hyperlink": {
      const h = doc.value;
      const inner = h.label != null ? renderDoc(h.label) : escapeHtml(h.url);
      return `<a href="${escapeHtml(h.url)}">${inner}</a>`;
    }
    case "Pic": {
      const p = doc.value;
      const alt = p.title != null ? escapeHtml(p.title) : "";
      const titleAttr = p.title != null ? ` title="${escapeHtml(p.title)}"` : "";
      return `<img src="${escapeHtml(p.uri)}" alt="${alt}"${titleAttr}>`;
    }
    case "MathInline":
      return `\\(${escapeHtml(doc.value)}\\)`;
    case "MathDisplay":
      return `\\[${escapeHtml(doc.value)}\\]`;
    case "AName":
      return `<a id="${escapeHtml(doc.value)}"></a>`;
    case "Property":
      return `<pre class="border-start border-4 border-primary bg-primary-subtle rounded-end p-3 my-3 font-monospace">${escapeHtml(doc.value)}</pre>`;
    case "Examples": {
      const examples = doc.value.map((ex) => {
        const resultLines = ex.result.map(
          (r) => `<div class="font-monospace text-body-secondary ps-3">${escapeHtml(r)}</div>`
        ).join("");
        return `<div class="my-1"><div class="font-monospace"><span class="text-warning-emphasis user-select-none">&gt;&gt;&gt; </span>${escapeHtml(ex.expression)}</div>` + resultLines + `</div>`;
      }).join("");
      return `<div class="border-start border-4 border-warning bg-warning-subtle rounded-end p-3 my-3">${examples}</div>`;
    }
    case "Header": {
      const h = doc.value;
      const level = Math.max(1, Math.min(6, h.level));
      return `<h${level}>${renderDoc(h.title)}</h${level}>`;
    }
    case "Table": {
      const t = doc.value;
      let thead = "";
      if (t.headerRows.length > 0) {
        const rows = t.headerRows.map((row) => {
          const cells = row.map(
            (cell) => `<th colspan="${cell.colspan}" rowspan="${cell.rowspan}">${renderDoc(cell.contents)}</th>`
          ).join("");
          return `<tr>${cells}</tr>`;
        }).join("");
        thead = `<thead>${rows}</thead>`;
      }
      let tbody = "";
      if (t.bodyRows.length > 0) {
        const rows = t.bodyRows.map((row) => {
          const cells = row.map(
            (cell) => `<td colspan="${cell.colspan}" rowspan="${cell.rowspan}">${renderDoc(cell.contents)}</td>`
          ).join("");
          return `<tr>${cells}</tr>`;
        }).join("");
        tbody = `<tbody>${rows}</tbody>`;
      }
      return `<table class="table table-bordered table-sm table-striped my-3">${thead}${tbody}</table>`;
    }
  }
}

// src/sections/header.ts
function renderHeader(module) {
  const title = module.name != null ? module.name.value : "Documentation";
  const dataLine = module.name != null ? ` data-line="${module.name.location.line}"` : "";
  const warning = module.warning != null ? `<div class="alert alert-warning"><span class="fw-bold">${escapeHtml(module.warning.category)}</span>: ${escapeHtml(module.warning.value)}</div>` : "";
  const doc = module.documentation.type !== "Empty" ? `<div class="my-3">${renderDoc(module.documentation)}</div>` : "";
  return `<header class="mb-4"${dataLine}><h1 class="border-bottom border-2 pb-2 mt-0">${escapeHtml(title)}</h1>` + warning + doc + `</header>`;
}

// src/sections/metadata.ts
function renderMetadata(module) {
  let items = "";
  if (module.language != null) {
    items += `<dt>Language</dt><dd>${escapeHtml(module.language)}</dd>`;
  }
  if (module.since != null) {
    const packagePrefix = module.since.package != null ? escapeHtml(module.since.package) + "-" : "";
    const version = module.since.version.join(".");
    items += `<dt>Since</dt><dd class="text-body-secondary small">${packagePrefix}${escapeHtml(version)}</dd>`;
  }
  if (items === "") {
    return "";
  }
  return `<section class="card border-start border-primary border-4 mb-3"><dl class="card-body mb-0">${items}</dl></section>`;
}

// src/sections/exports.ts
function renderExports(exports) {
  if (exports == null || exports.length === 0) {
    return "";
  }
  const items = exports.map(renderExport).join("");
  return `<section class="my-4"><h2 class="border-bottom pb-1 mt-4">Exports</h2><ul class="list-group list-group-flush">${items}</ul></section>`;
}
function renderExport(exp) {
  const liClass = "list-group-item bg-transparent py-1 px-2";
  switch (exp.type) {
    case "Identifier":
      return `<li class="${liClass}">${renderExportIdentifier(exp.value)}</li>`;
    case "Group":
      return `<li class="${liClass}">${renderSection(exp.value)}</li>`;
    case "Doc":
      return `<li class="${liClass}"><div class="mt-1">${renderDoc(exp.value)}</div></li>`;
    case "DocNamed":
      return `<li class="${liClass}"><div class="mt-1">${escapeHtml("\xA7" + exp.value)}</div></li>`;
  }
}
function renderExportIdentifier(ident) {
  const warning = ident.warning != null ? `<div class="alert alert-warning"><span class="fw-bold">${escapeHtml(ident.warning.category)}</span>: ${escapeHtml(ident.warning.value)}</div>` : "";
  const nameText = exportNameToText(ident.name);
  const subs = renderSubordinates(ident.subordinates);
  const doc = ident.doc != null ? `<div class="mt-1">${renderDoc(ident.doc)}</div>` : "";
  return `<div class="py-1">` + warning + `<code class="font-monospace">${escapeHtml(nameText)}${subs}</code>` + doc + `</div>`;
}
function exportNameToText(name) {
  let prefix = "";
  if (name.kind != null) {
    switch (name.kind.type) {
      case "Pattern":
        prefix = "pattern ";
        break;
      case "Type":
        prefix = "type ";
        break;
      case "Module":
        prefix = "module ";
        break;
    }
  }
  return prefix + name.name;
}
function renderSubordinates(subs) {
  if (subs == null) {
    return "";
  }
  const wildcardText = "..";
  const explicitTexts = subs.explicit.map((en) => en.name);
  const allTexts = subs.wildcard ? [wildcardText, ...explicitTexts] : explicitTexts;
  const combined = allTexts.join(", ");
  return escapeHtml("(" + combined + ")");
}
function renderSection(section) {
  const tag = sectionLevelToName(section.level);
  return `<div class="my-3"><${tag} class="fw-bold">${renderDoc(section.title)}</${tag}></div>`;
}
function sectionLevelToName(level) {
  switch (level) {
    case 1:
      return "h3";
    case 2:
      return "h4";
    case 3:
      return "h5";
    default:
      return "h6";
  }
}

// src/sections/imports.ts
function renderImports(imports) {
  if (imports.length === 0) {
    return "";
  }
  const uniqueNames = new Set(imports.map((i) => i.name));
  const uniqueCount = uniqueNames.size;
  const noun = uniqueCount === 1 ? "module" : "modules";
  const summary = `Imports (${uniqueCount} ${noun})`;
  const items = imports.map(renderImport).join("");
  return `<details class="my-4"><summary class="fs-4 fw-bold">${escapeHtml(summary)}</summary><ul class="list-group list-group-flush font-monospace small">${items}</ul></details>`;
}
function renderImport(imp) {
  const pkg = imp.package != null ? escapeHtml(`"${imp.package}" `) : "";
  const name = escapeHtml(imp.name);
  const alias = imp.alias != null ? escapeHtml(` as ${imp.alias}`) : "";
  return `<li class="list-group-item bg-transparent py-1 px-2">${pkg}${name}${alias}</li>`;
}

// src/sections/extensions.ts
var ghcUserGuideBaseUrl = "https://downloads.haskell.org/ghc/latest/docs/users_guide/";
var extensionUrlPaths = {
  AllowAmbiguousTypes: "exts/ambiguous_types.html#extension-AllowAmbiguousTypes",
  ApplicativeDo: "exts/applicative_do.html#extension-ApplicativeDo",
  Arrows: "exts/arrows.html#extension-Arrows",
  BangPatterns: "exts/strict.html#extension-BangPatterns",
  BinaryLiterals: "exts/binary_literals.html#extension-BinaryLiterals",
  BlockArguments: "exts/block_arguments.html#extension-BlockArguments",
  CApiFFI: "exts/ffi.html#extension-CApiFFI",
  ConstrainedClassMethods: "exts/constrained_class_methods.html#extension-ConstrainedClassMethods",
  ConstraintKinds: "exts/constraint_kind.html#extension-ConstraintKinds",
  Cpp: "phases.html#extension-CPP",
  CUSKs: "exts/poly_kinds.html#extension-CUSKs",
  DataKinds: "exts/data_kinds.html#extension-DataKinds",
  DatatypeContexts: "exts/datatype_contexts.html#extension-DatatypeContexts",
  DeepSubsumption: "exts/rank_polymorphism.html#extension-DeepSubsumption",
  DefaultSignatures: "exts/default_signatures.html#extension-DefaultSignatures",
  DeriveAnyClass: "exts/derive_any_class.html#extension-DeriveAnyClass",
  DeriveDataTypeable: "exts/deriving_extra.html#extension-DeriveDataTypeable",
  DeriveFoldable: "exts/deriving_extra.html#extension-DeriveFoldable",
  DeriveFunctor: "exts/deriving_extra.html#extension-DeriveFunctor",
  DeriveGeneric: "exts/generics.html#extension-DeriveGeneric",
  DeriveLift: "exts/deriving_extra.html#extension-DeriveLift",
  DeriveTraversable: "exts/deriving_extra.html#extension-DeriveTraversable",
  DerivingStrategies: "exts/deriving_strategies.html#extension-DerivingStrategies",
  DerivingVia: "exts/deriving_via.html#extension-DerivingVia",
  DisambiguateRecordFields: "exts/disambiguate_record_fields.html#extension-DisambiguateRecordFields",
  DoAndIfThenElse: "exts/doandifthenelse.html#extension-DoAndIfThenElse",
  DuplicateRecordFields: "exts/duplicate_record_fields.html#extension-DuplicateRecordFields",
  EmptyCase: "exts/empty_case.html#extension-EmptyCase",
  EmptyDataDecls: "exts/nullary_types.html#extension-EmptyDataDecls",
  EmptyDataDeriving: "exts/empty_data_deriving.html#extension-EmptyDataDeriving",
  ExistentialQuantification: "exts/existential_quantification.html#extension-ExistentialQuantification",
  ExplicitForAll: "exts/explicit_forall.html#extension-ExplicitForAll",
  ExplicitLevelImports: "exts/template_haskell.html#extension-ExplicitLevelImports",
  ExplicitNamespaces: "exts/explicit_namespaces.html#extension-ExplicitNamespaces",
  ExtendedDefaultRules: "ghci.html#extension-ExtendedDefaultRules",
  ExtendedLiterals: "exts/extended_literals.html#extension-ExtendedLiterals",
  FieldSelectors: "exts/field_selectors.html#extension-FieldSelectors",
  FlexibleContexts: "exts/flexible_contexts.html#extension-FlexibleContexts",
  FlexibleInstances: "exts/instances.html#extension-FlexibleInstances",
  ForeignFunctionInterface: "exts/ffi.html#extension-ForeignFunctionInterface",
  FunctionalDependencies: "exts/functional_dependencies.html#extension-FunctionalDependencies",
  GADTs: "exts/gadt.html#extension-GADTs",
  GADTSyntax: "exts/gadt_syntax.html#extension-GADTSyntax",
  GeneralisedNewtypeDeriving: "exts/newtype_deriving.html#extension-GeneralisedNewtypeDeriving",
  GHC2021: "exts/control.html#extension-GHC2021",
  GHC2024: "exts/control.html#extension-GHC2024",
  GHCForeignImportPrim: "exts/ffi.html#extension-GHCForeignImportPrim",
  Haskell2010: "exts/control.html#extension-Haskell2010",
  Haskell98: "exts/control.html#extension-Haskell98",
  HexFloatLiterals: "exts/hex_float_literals.html#extension-HexFloatLiterals",
  ImplicitParams: "exts/implicit_parameters.html#extension-ImplicitParams",
  ImplicitPrelude: "exts/rebindable_syntax.html#extension-ImplicitPrelude",
  ImplicitStagePersistence: "exts/template_haskell.html#extension-ImplicitStagePersistence",
  ImportQualifiedPost: "exts/import_qualified_post.html#extension-ImportQualifiedPost",
  ImpredicativeTypes: "exts/impredicative_types.html#extension-ImpredicativeTypes",
  IncoherentInstances: "exts/instances.html#extension-IncoherentInstances",
  InstanceSigs: "exts/instances.html#extension-InstanceSigs",
  InterruptibleFFI: "exts/ffi.html#extension-InterruptibleFFI",
  KindSignatures: "exts/kind_signatures.html#extension-KindSignatures",
  LambdaCase: "exts/lambda_case.html#extension-LambdaCase",
  LexicalNegation: "exts/lexical_negation.html#extension-LexicalNegation",
  LiberalTypeSynonyms: "exts/liberal_type_synonyms.html#extension-LiberalTypeSynonyms",
  LinearTypes: "exts/linear_types.html#extension-LinearTypes",
  ListTuplePuns: "exts/data_kinds.html#extension-ListTuplePuns",
  MagicHash: "exts/magic_hash.html#extension-MagicHash",
  MonadComprehensions: "exts/monad_comprehensions.html#extension-MonadComprehensions",
  MonoLocalBinds: "exts/let_generalisation.html#extension-MonoLocalBinds",
  MonomorphismRestriction: "exts/monomorphism.html#extension-MonomorphismRestriction",
  MultilineStrings: "exts/multiline_strings.html#extension-MultilineStrings",
  MultiParamTypeClasses: "exts/multi_param_type_classes.html#extension-MultiParamTypeClasses",
  MultiWayIf: "exts/multiway_if.html#extension-MultiWayIf",
  NamedDefaults: "exts/named_defaults.html#extension-NamedDefaults",
  NamedFieldPuns: "exts/record_puns.html#extension-NamedFieldPuns",
  NamedWildCards: "exts/partial_type_signatures.html#extension-NamedWildCards",
  NegativeLiterals: "exts/negative_literals.html#extension-NegativeLiterals",
  NondecreasingIndentation: "bugs.html#extension-NondecreasingIndentation",
  NPlusKPatterns: "exts/nk_patterns.html#extension-NPlusKPatterns",
  NullaryTypeClasses: "exts/multi_param_type_classes.html#extension-NullaryTypeClasses",
  NumDecimals: "exts/num_decimals.html#extension-NumDecimals",
  NumericUnderscores: "exts/numeric_underscores.html#extension-NumericUnderscores",
  OrPatterns: "exts/or_patterns.html#extension-OrPatterns",
  OverlappingInstances: "exts/instances.html#extension-OverlappingInstances",
  OverloadedLabels: "exts/overloaded_labels.html#extension-OverloadedLabels",
  OverloadedLists: "exts/overloaded_lists.html#extension-OverloadedLists",
  OverloadedRecordDot: "exts/overloaded_record_dot.html#extension-OverloadedRecordDot",
  OverloadedRecordUpdate: "exts/overloaded_record_update.html#extension-OverloadedRecordUpdate",
  OverloadedStrings: "exts/overloaded_strings.html#extension-OverloadedStrings",
  PackageImports: "exts/package_qualified_imports.html#extension-PackageImports",
  ParallelListComp: "exts/parallel_list_comprehensions.html#extension-ParallelListComp",
  PartialTypeSignatures: "exts/partial_type_signatures.html#extension-PartialTypeSignatures",
  PatternGuards: "exts/pattern_guards.html#extension-PatternGuards",
  PatternSynonyms: "exts/pattern_synonyms.html#extension-PatternSynonyms",
  PolyKinds: "exts/poly_kinds.html#extension-PolyKinds",
  PostfixOperators: "exts/rebindable_syntax.html#extension-PostfixOperators",
  QualifiedDo: "exts/qualified_do.html#extension-QualifiedDo",
  QuantifiedConstraints: "exts/quantified_constraints.html#extension-QuantifiedConstraints",
  QuasiQuotes: "exts/template_haskell.html#extension-QuasiQuotes",
  Rank2Types: "exts/rank_polymorphism.html#extension-Rank2Types",
  RankNTypes: "exts/rank_polymorphism.html#extension-RankNTypes",
  RebindableSyntax: "exts/rebindable_syntax.html#extension-RebindableSyntax",
  RecordWildCards: "exts/record_wildcards.html#extension-RecordWildCards",
  RecursiveDo: "exts/recursive_do.html#extension-RecursiveDo",
  RequiredTypeArguments: "exts/required_type_arguments.html#extension-RequiredTypeArguments",
  RoleAnnotations: "exts/roles.html#extension-RoleAnnotations",
  Safe: "exts/safe_haskell.html#extension-Safe",
  ScopedTypeVariables: "exts/scoped_type_variables.html#extension-ScopedTypeVariables",
  StandaloneDeriving: "exts/standalone_deriving.html#extension-StandaloneDeriving",
  StandaloneKindSignatures: "exts/poly_kinds.html#extension-StandaloneKindSignatures",
  StarIsType: "exts/poly_kinds.html#extension-StarIsType",
  StaticPointers: "exts/static_pointers.html#extension-StaticPointers",
  Strict: "exts/strict.html#extension-Strict",
  StrictData: "exts/strict.html#extension-StrictData",
  TemplateHaskell: "exts/template_haskell.html#extension-TemplateHaskell",
  TemplateHaskellQuotes: "exts/template_haskell.html#extension-TemplateHaskellQuotes",
  TraditionalRecordSyntax: "exts/traditional_record_syntax.html#extension-TraditionalRecordSyntax",
  TransformListComp: "exts/generalised_list_comprehensions.html#extension-TransformListComp",
  Trustworthy: "exts/safe_haskell.html#extension-Trustworthy",
  TupleSections: "exts/tuple_sections.html#extension-TupleSections",
  TypeAbstractions: "exts/type_abstractions.html#extension-TypeAbstractions",
  TypeApplications: "exts/type_applications.html#extension-TypeApplications",
  TypeData: "exts/type_data.html#extension-TypeData",
  TypeFamilies: "exts/type_families.html#extension-TypeFamilies",
  TypeFamilyDependencies: "exts/type_families.html#extension-TypeFamilyDependencies",
  TypeInType: "exts/poly_kinds.html#extension-TypeInType",
  TypeOperators: "exts/type_operators.html#extension-TypeOperators",
  TypeSynonymInstances: "exts/instances.html#extension-TypeSynonymInstances",
  UnboxedSums: "exts/primitives.html#extension-UnboxedSums",
  UnboxedTuples: "exts/primitives.html#extension-UnboxedTuples",
  UndecidableInstances: "exts/instances.html#extension-UndecidableInstances",
  UndecidableSuperClasses: "exts/undecidable_super_classes.html#extension-UndecidableSuperClasses",
  UnicodeSyntax: "exts/unicode_syntax.html#extension-UnicodeSyntax",
  UnliftedDatatypes: "exts/primitives.html#extension-UnliftedDatatypes",
  UnliftedFFITypes: "exts/ffi.html#extension-UnliftedFFITypes",
  UnliftedNewtypes: "exts/primitives.html#extension-UnliftedNewtypes",
  Unsafe: "exts/safe_haskell.html#extension-Unsafe",
  ViewPatterns: "exts/view_patterns.html#extension-ViewPatterns"
};
function extensionUrl(name) {
  const path = extensionUrlPaths[name];
  if (path != null) {
    return ghcUserGuideBaseUrl + path;
  }
  return ghcUserGuideBaseUrl + "exts/table.html";
}
function renderExtensions(extensions) {
  const entries = Object.entries(extensions);
  if (entries.length === 0) {
    return "";
  }
  const count = entries.length;
  const noun = count === 1 ? "extension" : "extensions";
  const summary = `Extensions (${count} ${noun})`;
  const badges = entries.map(([name, enabled]) => {
    const cls = enabled ? "badge bg-secondary-subtle text-body font-monospace me-1 mb-1" : "badge bg-danger-subtle text-body text-decoration-line-through font-monospace me-1 mb-1";
    const href = extensionUrl(name);
    return `<a class="${cls}" href="${escapeHtml(href)}">${escapeHtml(name)}</a>`;
  }).join("");
  return `<details class="my-3"><summary class="fs-4 fw-bold">${escapeHtml(summary)}</summary><div class="mt-2">${badges}</div></details>`;
}

// src/sections/items.ts
function kindToText(kind) {
  switch (kind.type) {
    case "Function":
      return "function";
    case "PatternBinding":
      return "pattern binding";
    case "PatternSynonym":
      return "pattern";
    case "DataType":
      return "data";
    case "Newtype":
      return "newtype";
    case "TypeData":
      return "type data";
    case "TypeSynonym":
      return "type";
    case "DataConstructor":
      return "constructor";
    case "GADTConstructor":
      return "GADT constructor";
    case "RecordField":
      return "field";
    case "Class":
      return "class";
    case "ClassMethod":
      return "method";
    case "ClassInstance":
      return "instance";
    case "StandaloneDeriving":
      return "standalone deriving";
    case "DerivedInstance":
      return "deriving";
    case "OpenTypeFamily":
      return "type family";
    case "ClosedTypeFamily":
      return "type family";
    case "DataFamily":
      return "data family";
    case "TypeFamilyInstance":
      return "type instance";
    case "DataFamilyInstance":
      return "data instance";
    case "ForeignImport":
      return "foreign import";
    case "ForeignExport":
      return "foreign export";
    case "FixitySignature":
      return "fixity";
    case "InlineSignature":
      return "inline";
    case "SpecialiseSignature":
      return "specialise";
    case "StandaloneKindSig":
      return "kind";
    case "Rule":
      return "rule";
    case "Default":
      return "default";
    case "Annotation":
      return "annotation";
    case "Splice":
      return "splice";
  }
}
function kindColor(kind) {
  switch (kind.type) {
    case "Function":
    case "PatternBinding":
    case "PatternSynonym":
      return "success";
    case "DataType":
    case "Newtype":
    case "TypeData":
    case "TypeSynonym":
    case "OpenTypeFamily":
    case "ClosedTypeFamily":
    case "DataFamily":
    case "TypeFamilyInstance":
    case "DataFamilyInstance":
    case "StandaloneKindSig":
      return "info";
    case "DataConstructor":
    case "GADTConstructor":
    case "RecordField":
    case "FixitySignature":
    case "InlineSignature":
    case "SpecialiseSignature":
    case "Rule":
    case "Default":
    case "Annotation":
    case "Splice":
      return "secondary";
    case "Class":
    case "ClassMethod":
    case "ClassInstance":
    case "StandaloneDeriving":
    case "DerivedInstance":
      return "primary";
    case "ForeignImport":
    case "ForeignExport":
      return "warning";
  }
}
function kindBadgeClass(kind) {
  switch (kindColor(kind)) {
    case "success":
      return "bg-success-subtle text-success-emphasis";
    case "info":
      return "bg-info-subtle text-info-emphasis";
    case "secondary":
      return "bg-secondary-subtle text-body";
    case "primary":
      return "bg-primary-subtle text-primary-emphasis";
    case "warning":
      return "bg-warning-subtle text-warning-emphasis";
  }
}
function kindBorderStyle(kind) {
  switch (kindColor(kind)) {
    case "success":
      return "border-left-color: var(--bs-success)";
    case "info":
      return "border-left-color: var(--bs-info)";
    case "secondary":
      return "border-left-color: var(--bs-secondary)";
    case "primary":
      return "border-left-color: var(--bs-primary)";
    case "warning":
      return "border-left-color: var(--bs-warning)";
  }
}
function isTypeVarSignature(kind) {
  switch (kind.type) {
    case "DataType":
    case "Newtype":
    case "TypeData":
    case "TypeSynonym":
    case "Class":
      return true;
    default:
      return false;
  }
}
function renderItem(located) {
  const loc = located.location;
  const item = located.value;
  const nameHtml = item.name != null ? `<span class="font-monospace fw-bold text-success">${escapeHtml(item.name)}</span>` : "";
  const badgeCls = `badge ${kindBadgeClass(item.kind)} ms-2`;
  const kindElement = `<span class="${badgeCls}">${escapeHtml(kindToText(item.kind))}</span>`;
  let sigBeforeKind = "";
  let sigAfterKind = "";
  if (item.signature != null) {
    const typeVar = isTypeVarSignature(item.kind);
    const prefix = typeVar ? " " : " :: ";
    const sigHtml = `<span class="font-monospace text-body-secondary">${escapeHtml(prefix + item.signature)}</span>`;
    if (typeVar) {
      sigBeforeKind = sigHtml;
    } else {
      sigAfterKind = sigHtml;
    }
  }
  const lineNum = loc.line;
  const locationButton = `<button type="button" class="item-location ms-auto text-body-tertiary small bg-transparent border-0 p-0" aria-label="Go to line ${lineNum}" data-line="${lineNum}" data-col="${loc.column}">line ${lineNum}</button>`;
  const cardHeader = `<div class="card-header bg-transparent d-flex align-items-center py-2">` + nameHtml + sigBeforeKind + kindElement + sigAfterKind + locationButton + `</div>`;
  const cardBody = item.documentation.type !== "Empty" ? `<div class="card-body">${renderDoc(item.documentation)}</div>` : "";
  return `<div class="card mb-3 border-start border-4" style="${kindBorderStyle(item.kind)}" id="item-${item.key}" data-line="${lineNum}">` + cardHeader + cardBody + `</div>`;
}
function renderItems(items) {
  if (items.length === 0) {
    return "";
  }
  const childrenMap = /* @__PURE__ */ new Map();
  for (const li of items) {
    const pk = li.value.parentKey;
    if (pk != null) {
      const existing = childrenMap.get(pk);
      if (existing != null) {
        existing.push(li);
      } else {
        childrenMap.set(pk, [li]);
      }
    }
  }
  const topLevelItems = items.filter((li) => li.value.parentKey == null);
  function renderItemWithChildren(li) {
    const k = li.value.key;
    const children = childrenMap.get(k) ?? [];
    let result = renderItem(li);
    if (children.length > 0) {
      const childrenHtml = children.map(renderItemWithChildren).join("");
      result += `<div class="ms-4 mt-2 border-start border-2 ps-3">${childrenHtml}</div>`;
    }
    return result;
  }
  const itemsHtml = topLevelItems.map(renderItemWithChildren).join("");
  return `<section class="my-4"><h2 class="border-bottom pb-1 mt-4">Declarations</h2>` + itemsHtml + `</section>`;
}

// src/sections/footer.ts
function renderFooter(module) {
  const version = module.version.join(".");
  return `<footer class="mt-5 pt-3 border-top text-body-secondary small">Generated by <a href="https://github.com/tfausak/scrod">Scrod</a> version ${escapeHtml(version)}</footer>`;
}

// src/render.ts
function renderModule(module) {
  const title = module.name != null ? module.name.value : "Documentation";
  const head = `<head><meta charset="utf-8" /><meta name="viewport" content="width=device-width, initial-scale=1" /><title>${escapeHtml2(title)}</title><link rel="stylesheet" href="https://esm.sh/bootstrap@5.3.8/dist/css/bootstrap.min.css" integrity="sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB" crossorigin="anonymous" /><link rel="stylesheet" href="https://esm.sh/katex@0.16.22/dist/katex.min.css" integrity="sha384-5TcZemv2l/9On385z///+d7MSYlvIEw9FuZTIdZ14vJLqWphw7e7ZPuOiCHJcFCP" crossorigin="anonymous" /><link rel="modulepreload" href="https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js" integrity="sha384-PV5j9Y/tL/HYr0HSxUY3afWRVHizeuTKLWTR+OwVlGHOBcN8jOZvCAS79+ULHoEU" crossorigin="anonymous" /><script>const dark = matchMedia('(prefers-color-scheme: dark)');
const setTheme = (e) =>
  document.documentElement.dataset.bsTheme = e.matches ? 'dark' : 'light';
setTheme(dark);
dark.addEventListener('change', setTheme);
import('https://esm.sh/katex@0.16.22/dist/contrib/auto-render.min.js')
  .then((m) => m.default(document.body, { delimiters: [
    { left: '\\\\(', right: '\\\\)', display: false },
    { left: '\\\\[', right: '\\\\]', display: true }
  ]}));<\/script></head>`;
  const body = `<body><div class="container py-4 text-break">` + renderHeader(module) + renderMetadata(module) + renderExports(module.exports) + renderImports(module.imports) + renderExtensions(module.extensions) + renderItems(module.items) + renderFooter(module) + `</div></body>`;
  return `<!doctype html>
<html>${head}${body}</html>`;
}
function escapeHtml2(text) {
  return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}
export {
  renderModule
};
