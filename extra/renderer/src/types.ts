// TypeScript types matching the Scrod JSON Schema (extra/renderer/schema.json).
// These are hand-written to match the exact JSON shapes produced by Scrod's
// ToJson serializer.

// ---------------------------------------------------------------------------
// Location & Located
// ---------------------------------------------------------------------------

export interface Location {
  line: number;
  column: number;
}

export interface Located<T> {
  location: Location;
  value: T;
}

// ---------------------------------------------------------------------------
// Doc (22-variant discriminated union)
// ---------------------------------------------------------------------------

export type Doc =
  | { type: "Empty" }
  | { type: "Append"; value: Doc[] }
  | { type: "String"; value: string }
  | { type: "Paragraph"; value: Doc }
  | { type: "Identifier"; value: Identifier }
  | { type: "Module"; value: ModLink }
  | { type: "Emphasis"; value: Doc }
  | { type: "Monospaced"; value: Doc }
  | { type: "Bold"; value: Doc }
  | { type: "UnorderedList"; value: Doc[] }
  | { type: "OrderedList"; value: { index: number; item: Doc }[] }
  | { type: "DefList"; value: { term: Doc; definition: Doc }[] }
  | { type: "CodeBlock"; value: Doc }
  | { type: "Hyperlink"; value: Hyperlink }
  | { type: "Pic"; value: Picture }
  | { type: "MathInline"; value: string }
  | { type: "MathDisplay"; value: string }
  | { type: "AName"; value: string }
  | { type: "Property"; value: string }
  | { type: "Examples"; value: Example[] }
  | { type: "Header"; value: Header }
  | { type: "Table"; value: Table };

// ---------------------------------------------------------------------------
// Namespace
// ---------------------------------------------------------------------------

export type Namespace = { type: "Value" } | { type: "Type" };

// ---------------------------------------------------------------------------
// Identifier (used inside Doc.Identifier)
// ---------------------------------------------------------------------------

export interface Identifier {
  namespace?: Namespace;
  value: string;
}

// ---------------------------------------------------------------------------
// ModLink (used inside Doc.Module)
// ---------------------------------------------------------------------------

export interface ModLink {
  name: string;
  label?: Doc;
}

// ---------------------------------------------------------------------------
// Hyperlink (used inside Doc.Hyperlink)
// ---------------------------------------------------------------------------

export interface Hyperlink {
  url: string;
  label?: Doc;
}

// ---------------------------------------------------------------------------
// Picture (used inside Doc.Pic)
// ---------------------------------------------------------------------------

export interface Picture {
  uri: string;
  title?: string;
}

// ---------------------------------------------------------------------------
// Example (used inside Doc.Examples)
// ---------------------------------------------------------------------------

export interface Example {
  expression: string;
  result: string[];
}

// ---------------------------------------------------------------------------
// Header (used inside Doc.Header)
// ---------------------------------------------------------------------------

export interface Header {
  level: number;
  title: Doc;
}

// ---------------------------------------------------------------------------
// TableCell & Table (used inside Doc.Table)
// ---------------------------------------------------------------------------

export interface TableCell {
  colspan: number;
  rowspan: number;
  contents: Doc;
}

export interface Table {
  headerRows: TableCell[][];
  bodyRows: TableCell[][];
}

// ---------------------------------------------------------------------------
// Warning
// ---------------------------------------------------------------------------

export interface Warning {
  category: string;
  value: string;
}

// ---------------------------------------------------------------------------
// Since
// ---------------------------------------------------------------------------

export interface Since {
  package?: string;
  version: number[];
}

// ---------------------------------------------------------------------------
// ExportNameKind
// ---------------------------------------------------------------------------

export type ExportNameKind =
  | { type: "Pattern" }
  | { type: "Type" }
  | { type: "Module" };

// ---------------------------------------------------------------------------
// ExportName
// ---------------------------------------------------------------------------

export interface ExportName {
  kind?: ExportNameKind;
  name: string;
}

// ---------------------------------------------------------------------------
// Subordinates
// ---------------------------------------------------------------------------

export interface Subordinates {
  wildcard: boolean;
  explicit: ExportName[];
}

// ---------------------------------------------------------------------------
// ExportIdentifier
// ---------------------------------------------------------------------------

export interface ExportIdentifier {
  name: ExportName;
  subordinates?: Subordinates;
  warning?: Warning;
  doc?: Doc;
}

// ---------------------------------------------------------------------------
// Section (export group heading)
// ---------------------------------------------------------------------------

export interface Section {
  level: number;
  title: Doc;
}

// ---------------------------------------------------------------------------
// Export (4-variant discriminated union)
// ---------------------------------------------------------------------------

export type Export =
  | { type: "Identifier"; value: ExportIdentifier }
  | { type: "Group"; value: Section }
  | { type: "Doc"; value: Doc }
  | { type: "DocNamed"; value: string };

// ---------------------------------------------------------------------------
// Import
// ---------------------------------------------------------------------------

export interface Import {
  name: string;
  package?: string;
  alias?: string;
}

// ---------------------------------------------------------------------------
// ItemKind (30-variant discriminated union)
// ---------------------------------------------------------------------------

export type ItemKind =
  | { type: "Function" }
  | { type: "PatternBinding" }
  | { type: "PatternSynonym" }
  | { type: "DataType" }
  | { type: "Newtype" }
  | { type: "TypeData" }
  | { type: "TypeSynonym" }
  | { type: "DataConstructor" }
  | { type: "GADTConstructor" }
  | { type: "RecordField" }
  | { type: "Class" }
  | { type: "ClassMethod" }
  | { type: "ClassInstance" }
  | { type: "StandaloneDeriving" }
  | { type: "DerivedInstance" }
  | { type: "OpenTypeFamily" }
  | { type: "ClosedTypeFamily" }
  | { type: "DataFamily" }
  | { type: "TypeFamilyInstance" }
  | { type: "DataFamilyInstance" }
  | { type: "ForeignImport" }
  | { type: "ForeignExport" }
  | { type: "FixitySignature" }
  | { type: "InlineSignature" }
  | { type: "SpecialiseSignature" }
  | { type: "StandaloneKindSig" }
  | { type: "Rule" }
  | { type: "Default" }
  | { type: "Annotation" }
  | { type: "Splice" };

// ---------------------------------------------------------------------------
// Item
// ---------------------------------------------------------------------------

export interface Item {
  key: number;
  kind: ItemKind;
  parentKey?: number;
  name?: string;
  documentation: Doc;
  signature?: string;
}

// ---------------------------------------------------------------------------
// Module (root type)
// ---------------------------------------------------------------------------

export interface Module {
  version: number[];
  name?: Located<string>;
  language?: string;
  documentation: Doc;
  warning?: Warning;
  since?: Since;
  extensions: Record<string, boolean>;
  exports?: Export[];
  imports: Import[];
  items: Located<Item>[];
  signature: boolean;
}
