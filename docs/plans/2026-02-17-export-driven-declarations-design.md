# Export-Driven Declarations in HTML Output

Fixes #225.

## Problem

The HTML output has a standalone "Exports" section and a "Declarations" section
that are completely disconnected. The export list should drive the declarations
section: determining which items appear, in what order, and grouping unexported
items separately.

## Approach

Render-time only (Approach A). Build a name-to-item lookup in `ToHtml.hs` and
walk the export list to emit items in export order. No changes to core data
types or the FromGhc pipeline. JSON output stays unchanged.

## Behavior

When a module has **no explicit export list** (`exports = Nothing`): keep
current behavior — all items in source order, no grouping.

When a module has an **explicit export list** (`exports = Just [...]`): the
export list drives the Declarations section. The standalone "Exports" section
is removed.

## Export Disposition Categories

Each top-level item falls into one of these categories:

### Category A: Name-matched

Items whose name can appear in an export list. Shown at their export-list
position when present, in the "Unexported" section when absent.

`Function`, `PatternBinding`, `PatternSynonym`, `DataType`, `Newtype`,
`TypeData`, `TypeSynonym`, `Class`, `OpenTypeFamily`, `ClosedTypeFamily`,
`DataFamily`, `ForeignImport`, `ForeignExport`, `StandaloneKindSig`

### Category B: Always visible

Items implicitly exported by GHC regardless of the export list. Never appear
in the "Unexported" section. Rendered in source order after export-driven items.

`ClassInstance`, `StandaloneDeriving`, `DerivedInstance` (when top-level),
`Rule`, `Default`, `Annotation`, `Splice`

### Category C: Follow-parent

Items with a `parentKey`. Rendered alongside their parent when the parent is
rendered. Traditional subordinates can be filtered by the export's subordinate
restrictions.

`DataConstructor`, `GADTConstructor`, `RecordField`, `ClassMethod`,
`DefaultMethodSignature`, `MinimalPragma`, `Argument`,
`TypeFamilyInstance` (in closed families), `DerivedInstance` (when parented),
`InlineSignature`, `SpecialiseSignature`, `FixitySignature`, `Warning` (pragma),
`RoleAnnotation`

### Category D: Parent-follows-children

Top-level, nameless items that own children. Shown in the exported section if
any children are exported; otherwise in the "Unexported" section. Silently
dropped if all children were consumed elsewhere.

`CompletePragma`

### Category E: Always shown

`DocumentationChunk` — no export semantics.

### Overlap rule

Some kinds (e.g., `ClassInstance`) can be either top-level or parented. If an
item has a `parentKey`, it is Category C. If top-level, it uses its kind's
default category.

## Subordinate Filtering

When a parent is exported with subordinate restrictions, children are filtered:

| Export syntax          | Subordinates value               | Rule                         |
|------------------------|----------------------------------|------------------------------|
| `T`                    | `Nothing`                        | No subordinates shown        |
| `T(..)`               | `Just (wildcard=True, [])`       | All children shown           |
| `T(C1, ..)`           | `Just (wildcard=True, [C1])`     | All children shown           |
| `T(C1, C2)`           | `Just (wildcard=False, [C1,C2])` | Only named subordinates      |

This filtering applies only to **traditional subordinates**: `DataConstructor`,
`GADTConstructor`, `RecordField`, `ClassMethod`, `DefaultMethodSignature`.

**Non-subordinate children** (pragmas, arguments, derived instances) are always
shown when their parent is rendered, regardless of the subordinate list.

## Rendering Algorithm

### Step 1: Build lookup structures

- `nameMap :: Map Text (Located Item)` — item names to top-level items
- `childrenMap :: Map Natural [Located Item]` — parent keys to children
- `usedKeys :: Set Natural` — accumulated set of rendered item keys

### Step 2: Walk the export list

Process each `Export` entry in order:

- `Export.Identifier` — look up name in `nameMap`. If found and unused, render
  the item with filtered children. Add all rendered keys to `usedKeys`.
- `Export.Group` — render as section heading.
- `Export.Doc` — render inline documentation.
- `Export.DocNamed` — render named doc chunk.

### Step 3: Always-visible items

Render top-level Category B items not in `usedKeys`, in source order. Add
their keys and children's keys to `usedKeys`.

### Step 4: COMPLETE pragmas

For each top-level `CompletePragma` not in `usedKeys`: if all children are in
`usedKeys`, drop silently. Otherwise, it falls through to step 5.

### Step 5: Unexported section

Collect remaining top-level items not in `usedKeys`. If any exist, render
under an **"Unexported"** heading in source order, with their remaining
unconsumed children.

## Files to Modify

- `source/library/Scrod/Convert/ToHtml.hs` — main rendering logic
- `source/library/Scrod/TestSuite/Integration.hs` — add tests for
  export-driven ordering, subordinate filtering, always-visible items, and
  unexported grouping
