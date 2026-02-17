# Function Argument Display

Issue: #235

## Problem

When a function has per-argument Haddock docs (`-- ^`), the signature is rendered
via GHC's `ppr` which includes those doc comments inline as text. This looks
syntactically incorrect in the current `<code>` rendering. For example:

```haskell
f :: a -- ^ i
  -> a -- ^ o
```

The signature displays with embedded comments and awkward newlines rather than a
clean `:: a -> a`.

## Design

Model each positional function/constructor argument as a child Item with a new
`Argument` ItemKind. Strip inline doc comments from the parent signature so it
renders cleanly. Argument items render as nested cards (the same pattern used by
RecordField items).

### Core types

Add one variant to `ItemKind`:

```haskell
| Argument  -- ^ Positional argument of a function or constructor
```

No changes to `Item`. Argument items use existing fields:

- `signature` = the individual argument's type text (e.g. `"Int"`, `"a"`)
- `documentation` = the per-argument Haddock doc (or `Empty` if undocumented)
- `parentKey` = the owning function or constructor
- `name` = `Nothing` (positional args have no names)
- `kind` = `Argument`

### GHC extraction: functions

In `Scrod.Convert.FromGhc.Names`:

- New function `extractSigArguments` walks the `HsFunTy` / `HsDocTy` chain in a
  type signature. For each arrow-separated argument: if it's wrapped in
  `HsDocTy`, extract the doc and the inner type; otherwise `(type, Nothing)`.
  Returns `[(Text, Maybe LHsDoc)]`.
- Modify `extractSigSignature` to strip `HsDocTy` nodes before `ppr`-ing, so the
  parent signature is clean (e.g. `"a -> a"`).

In `Scrod.Convert.FromGhc`:

- After creating the function Item, call `extractSigArguments` and emit a child
  Argument item for each positional argument (all args, not only documented ones).

### GHC extraction: constructors

In `Scrod.Convert.FromGhc.Constructors`:

- For `PrefixCon fields`: iterate over `HsConDeclField` list, extract `cdf_doc`
  and `cdf_type`, emit Argument child items.
- For `InfixCon l r`: same, two Argument items.
- For `PrefixConGADT`: same pattern.
- Record constructors are unchanged (they already have named RecordField children).
- Continue stripping `cdf_doc` from the parent signature.

### HTML rendering

No special-casing in `ToHtml`. Argument child items render as nested cards via
the existing `renderItemWithChildren` recursion.

Changes:

- Add `ItemKind.Argument -> "argument"` to `kindToString`.
- Badge color: covered by existing wildcard `_ -> "text-bg-secondary"`.

### JSON output

No changes needed. Argument items appear in the `items` array with
`"kind": {"type": "Argument"}`, `"parentKey"` pointing to their function or
constructor, `"signature"` with the type text, and `"documentation"` with the arg
doc.

### Testing

Integration tests:

- Functions with per-argument docs: verify parent signature is clean, verify child
  Argument items exist with correct signature and documentation.
- Functions with no arg docs: verify Argument child items have `Empty` docs.
- Update existing constructor arg doc tests to verify Argument child items instead
  of expecting docs to be silently stripped.
- Edge cases: constraints + forall + args, mixed documented/undocumented args.

## Alternatives considered

### New `arguments` field on Item

Add `arguments :: [(Text, Doc)]` to `Item`. Simpler (no ItemKey allocation) but
breaks the "everything is an Item" model, requires JSON schema changes, and is
less extensible.

### Structured Signature type

Replace `signature :: Maybe Text` with a structured `Signature` type containing
argument breakdown. Larger refactor, couples args to signatures, and doesn't give
arguments first-class item status.
