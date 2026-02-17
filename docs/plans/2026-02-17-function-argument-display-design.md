# Function Argument Display Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Display per-argument Haddock docs as child items instead of embedding them in the type signature text.

**Architecture:** Add `Argument` to `ItemKind`, walk the GHC type AST to extract per-argument types and docs, emit child Argument items, strip doc comments from parent signatures. Existing child-item rendering handles display.

**Tech Stack:** Haskell, GHC 9.14 API (`Language.Haskell.Syntax`, `GHC.Hs.Doc`), Tasty/HUnit tests.

---

### Task 1: Add `Argument` to `ItemKind`

**Files:**
- Modify: `source/library/Scrod/Core/ItemKind.hs:82` (before `deriving`)

**Step 1: Add the variant**

In `source/library/Scrod/Core/ItemKind.hs`, add before the `deriving` line (line 84):

```haskell
  | -- | Positional argument of a function or constructor
    Argument
```

This goes after `DocumentationChunk` (line 83) and before `deriving` (line 84).

**Step 2: Add to `kindToString` in ToHtml**

In `source/library/Scrod/Convert/ToHtml.hs`, add a new case in `kindToString` (after line 714, the `RecordField` case):

```haskell
  ItemKind.Argument -> "argument"
```

**Step 3: Build to verify**

Run: `cabal build --flags=pedantic 2>&1 | tail -5`

The build must succeed with no warnings. The `-Weverything` flag ensures the new `ItemKind` variant is covered in all pattern matches. If any `-Wincomplete-patterns` warnings appear, fix them.

**Step 4: Commit**

```bash
git add source/library/Scrod/Core/ItemKind.hs source/library/Scrod/Convert/ToHtml.hs
git commit -m "Add Argument variant to ItemKind (#235)"
```

---

### Task 2: Add argument extraction for function type signatures

**Files:**
- Modify: `source/library/Scrod/Convert/FromGhc/Names.hs`

This task adds two new functions and modifies one existing function in `Names.hs`.

**Step 1: Add new imports**

At the top of `source/library/Scrod/Convert/FromGhc/Names.hs`, add:

```haskell
import qualified GHC.Hs.Doc as HsDoc
import qualified GHC.Parser.Annotation as Annotation
```

(Keep existing imports; `Annotation` might already be needed indirectly via `Internal`.)

**Step 2: Write `stripHsDocTy` helper**

Add a recursive function that strips `HsDocTy` wrappers from an `LHsType`:

```haskell
-- | Recursively strip HsDocTy wrappers from a type.
-- This removes inline Haddock comments so the type pretty-prints cleanly.
stripHsDocTy :: Syntax.LHsType Ghc.GhcPs -> Syntax.LHsType Ghc.GhcPs
stripHsDocTy lTy = case SrcLoc.unLoc lTy of
  Syntax.HsDocTy _ innerTy _ -> stripHsDocTy innerTy
  Syntax.HsFunTy x mult lhs rhs ->
    let lTy' = SrcLoc.L (Annotation.getLocA lTy) (Syntax.HsFunTy x mult (stripHsDocTy lhs) (stripHsDocTy rhs))
     in lTy'
  Syntax.HsForAllTy x tele body ->
    SrcLoc.L (Annotation.getLocA lTy) (Syntax.HsForAllTy x tele (stripHsDocTy body))
  Syntax.HsQualTy x ctxt body ->
    SrcLoc.L (Annotation.getLocA lTy) (Syntax.HsQualTy x ctxt (stripHsDocTy body))
  Syntax.HsParTy x inner ->
    SrcLoc.L (Annotation.getLocA lTy) (Syntax.HsParTy x (stripHsDocTy inner))
  _ -> lTy
```

**Step 3: Write `extractSigArguments`**

Add a function that walks the `HsFunTy` chain and extracts `(type_text, doc, since)` triples:

```haskell
-- | Extract positional arguments from a type signature.
-- Walks the HsFunTy chain, extracting each argument's type text
-- and optional documentation from HsDocTy wrappers.
-- The return type of the function (the final non-arrow part) is NOT included.
extractSigArguments ::
  Syntax.Sig Ghc.GhcPs ->
  [(Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))]
extractSigArguments sig = case sig of
  Syntax.TypeSig _ _ wcType ->
    extractTypeArguments . Syntax.sig_body . SrcLoc.unLoc . Syntax.hswc_body $ wcType
  Syntax.PatSynSig _ _ sigType ->
    extractTypeArguments . Syntax.sig_body . SrcLoc.unLoc $ sigType
  Syntax.ClassOpSig _ _ _ sigType ->
    extractTypeArguments . Syntax.sig_body . SrcLoc.unLoc $ sigType
  _ -> []

-- | Walk an LHsType, skipping forall/context, collecting arrow-separated arguments.
extractTypeArguments ::
  Syntax.LHsType Ghc.GhcPs ->
  [(Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))]
extractTypeArguments lTy = case SrcLoc.unLoc lTy of
  Syntax.HsForAllTy _ _ body -> extractTypeArguments body
  Syntax.HsQualTy _ _ body -> extractTypeArguments body
  Syntax.HsFunTy _ _ lhs rhs ->
    extractOneArgument lhs : extractTypeArguments rhs
  Syntax.HsDocTy _ innerTy doc ->
    -- A doc on the outermost type (the return type) — no arguments
    -- Actually this shouldn't happen at the top for a function with args,
    -- but handle gracefully by returning empty.
    []
  _ -> []

-- | Extract type text and optional doc from a single argument type.
extractOneArgument ::
  Syntax.LHsType Ghc.GhcPs ->
  (Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))
extractOneArgument lTy = case SrcLoc.unLoc lTy of
  Syntax.HsDocTy _ innerTy doc ->
    (Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ innerTy, Just doc)
  _ ->
    (Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $ lTy, Nothing)
```

**Step 4: Modify `extractSigSignature` to strip docs**

Change `extractSigSignature` to strip `HsDocTy` before pretty-printing. Replace the current implementation:

```haskell
extractSigSignature :: Syntax.Sig Ghc.GhcPs -> Maybe Text.Text
extractSigSignature sig = case sig of
  Syntax.TypeSig _ _ ty ->
    Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      stripHsSigWcType ty
  Syntax.PatSynSig _ _ ty ->
    Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      stripHsSigType ty
  Syntax.ClassOpSig _ _ _ ty ->
    Just . Text.pack . Outputable.showSDocUnsafe . Outputable.ppr $
      stripHsSigType ty
  _ -> Nothing

-- | Strip HsDocTy from a wildcard-wrapped sig type.
stripHsSigWcType ::
  Syntax.LHsSigWcType Ghc.GhcPs -> Syntax.LHsSigWcType Ghc.GhcPs
stripHsSigWcType wc@(Syntax.HsWC {Syntax.hswc_body = lSigTy}) =
  wc {Syntax.hswc_body = stripHsSigType lSigTy}
stripHsSigWcType wc = wc

-- | Strip HsDocTy from a sig type.
stripHsSigType ::
  Syntax.LHsSigType Ghc.GhcPs -> Syntax.LHsSigType Ghc.GhcPs
stripHsSigType lSigTy = case SrcLoc.unLoc lSigTy of
  Syntax.HsSig x bndrs body ->
    SrcLoc.L (Annotation.getLocA lSigTy) (Syntax.HsSig x bndrs (stripHsDocTy body))
  _ -> lSigTy
```

**Step 5: Build to verify**

Run: `cabal build --flags=pedantic 2>&1 | tail -10`

Expected: SUCCESS. The modified `extractSigSignature` should compile and the new functions should have no warnings.

**Step 6: Commit**

```bash
git add source/library/Scrod/Convert/FromGhc/Names.hs
git commit -m "Add argument extraction and doc stripping for type signatures (#235)"
```

---

### Task 3: Emit Argument child items for function signatures

**Files:**
- Modify: `source/library/Scrod/Convert/FromGhc.hs:336-378`

**Step 1: Modify `convertSigDeclM` for `TypeSig`**

Change the `TypeSig` case (lines 343-346) to also extract arguments and emit child items:

```haskell
  Syntax.TypeSig _ names _ ->
    let sigText = Names.extractSigSignature sig
        args = Names.extractSigArguments sig
     in fmap concat . flip traverse names $ \lName -> do
          parentResult <- Internal.mkItemWithKeyM
            (Annotation.getLocA lName)
            Nothing
            (Just $ Internal.extractIdPName lName)
            doc
            docSince
            sigText
            ItemKind.Function
          case parentResult of
            Nothing -> pure []
            Just (parentItem, parentKey) -> do
              argItems <- convertArguments (Just parentKey) (Annotation.getLocA lName) args
              pure $ [parentItem] <> argItems
```

Do the same for `PatSynSig` (lines 347-349):

```haskell
  Syntax.PatSynSig _ names _ ->
    let sigText = Names.extractSigSignature sig
        args = Names.extractSigArguments sig
     in fmap concat . flip traverse names $ \lName -> do
          parentResult <- Internal.mkItemWithKeyM
            (Annotation.getLocA lName)
            Nothing
            (Just $ Internal.extractIdPName lName)
            doc
            docSince
            sigText
            ItemKind.PatternSynonym
          case parentResult of
            Nothing -> pure []
            Just (parentItem, parentKey) -> do
              argItems <- convertArguments (Just parentKey) (Annotation.getLocA lName) args
              pure $ [parentItem] <> argItems
```

**Step 2: Add `convertArguments` helper**

Add this function to `FromGhc.hs`:

```haskell
-- | Convert extracted argument data into child Argument items.
convertArguments ::
  Maybe ItemKey.ItemKey ->
  SrcLoc.SrcSpan ->
  [(Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs))] ->
  Internal.ConvertM [Located.Located Item.Item]
convertArguments parentKey srcSpan args =
  Maybe.catMaybes <$> traverse (convertOneArgument parentKey srcSpan) args

-- | Convert a single argument to an Argument item.
convertOneArgument ::
  Maybe ItemKey.ItemKey ->
  SrcLoc.SrcSpan ->
  (Text.Text, Maybe (HsDoc.LHsDoc Ghc.GhcPs)) ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertOneArgument parentKey srcSpan (sigText, mDoc) =
  let (argDoc, argSince) = maybe (Doc.Empty, Nothing) GhcDoc.convertLHsDoc mDoc
   in Internal.mkItemM srcSpan parentKey Nothing argDoc argSince (Just sigText) ItemKind.Argument
```

Also add the new import at the top of `FromGhc.hs`:

```haskell
import qualified GHC.Hs.Doc as HsDoc
```

(Note: `HsDoc` may already be imported — check and add only if missing.)

**Step 3: Remove now-unused `convertSigNameM`**

The old `convertSigNameM` function (lines 369-378) is no longer called by the `TypeSig`/`PatSynSig` cases. Check if any other code calls it. If not, remove it to avoid dead-code warnings.

Actually, the `ClassOpSig` case at line 367 uses it indirectly via the catch-all. Check: the `ClassOpSig` case in `convertSigDeclM` may also need updating if class methods can have argument docs. For now, keep `convertSigNameM` if `ClassOpSig` still uses it (it's handled via the catch-all `_ ->` at line 367).

Wait — looking at lines 343-349 again, `ClassOpSig` is NOT handled by those cases; it falls through to the catch-all at line 367 which calls `convertDeclWithDocM`. So `convertSigNameM` is only used by the `TypeSig` and `PatSynSig` cases. If we change both, `convertSigNameM` becomes unused — remove it.

**Step 4: Build to verify**

Run: `cabal build --flags=pedantic 2>&1 | tail -10`

Expected: SUCCESS.

**Step 5: Commit**

```bash
git add source/library/Scrod/Convert/FromGhc.hs
git commit -m "Emit Argument child items for function type signatures (#235)"
```

---

### Task 4: Handle class method signatures

**Files:**
- Modify: `source/library/Scrod/Convert/FromGhc.hs`

**Step 1: Look at how ClassOpSig is handled**

`ClassOpSig` is handled in `convertClassSigsWithDocsM`. Find this function and apply the same pattern: extract arguments, create parent item with key, emit argument children.

The approach is the same as Task 3 but for `ClassOpSig` inside class bodies. Find all call sites that create items for `ClassOpSig` or `ClassMethod` items and ensure they also emit argument children.

**Step 2: Build and test**

Run: `cabal build --flags=pedantic 2>&1 | tail -10`

**Step 3: Commit**

```bash
git add source/library/Scrod/Convert/FromGhc.hs
git commit -m "Emit Argument child items for class method signatures (#235)"
```

---

### Task 5: Emit Argument child items for prefix constructors

**Files:**
- Modify: `source/library/Scrod/Convert/FromGhc/Constructors.hs:201-218`

**Step 1: Modify `extractFieldsFromH98DetailsM`**

Change the `PrefixCon` and `InfixCon` cases to emit Argument items:

```haskell
extractFieldsFromH98DetailsM parentKey details = case details of
  Syntax.PrefixCon fields -> convertPrefixArgsM parentKey fields
  Syntax.InfixCon l r -> convertPrefixArgsM parentKey [l, r]
  Syntax.RecCon lFields -> convertConDeclFieldsM parentKey (SrcLoc.unLoc lFields)
```

**Step 2: Modify `extractFieldsFromGADTDetailsM`**

```haskell
extractFieldsFromGADTDetailsM parentKey details = case details of
  Syntax.PrefixConGADT _ fields -> convertPrefixArgsM parentKey fields
  Syntax.RecConGADT _ lFields -> convertConDeclFieldsM parentKey (SrcLoc.unLoc lFields)
```

**Step 3: Add `convertPrefixArgsM`**

```haskell
-- | Convert prefix constructor arguments to Argument items.
convertPrefixArgsM ::
  Maybe ItemKey.ItemKey ->
  [Syntax.HsConDeclField Ghc.GhcPs] ->
  Internal.ConvertM [Located.Located Item.Item]
convertPrefixArgsM parentKey = Maybe.catMaybes <$> traverse (convertPrefixArgM parentKey)

-- | Convert a single prefix constructor argument to an Argument item.
convertPrefixArgM ::
  Maybe ItemKey.ItemKey ->
  Syntax.HsConDeclField Ghc.GhcPs ->
  Internal.ConvertM (Maybe (Located.Located Item.Item))
convertPrefixArgM parentKey field =
  let (argDoc, argSince) = maybe (Doc.Empty, Nothing) GhcDoc.convertLHsDoc $ Syntax.cdf_doc field
      sig =
        Just . Text.pack . Outputable.showSDocUnsafe $
          unpackednessDoc (Syntax.cdf_unpack field)
            Outputable.<> strictnessDoc (Syntax.cdf_bang field)
            Outputable.<> Outputable.ppr (Syntax.cdf_type field)
   in Internal.mkItemM
        (Annotation.getLocA (Syntax.cdf_type field))
        parentKey
        Nothing
        argDoc
        argSince
        sig
        ItemKind.Argument
```

Note: The `Annotation.getLocA` call on `cdf_type` gives us the source location of the argument type. We might need to use a different span — check what `Annotation.getLocA` returns for `LHsType`. If it's `SrcSpanAnn`, this should work. If not, use `SrcLoc.getLoc` instead.

**Step 4: Build to verify**

Run: `cabal build --flags=pedantic 2>&1 | tail -10`

Expected: SUCCESS.

**Step 5: Commit**

```bash
git add source/library/Scrod/Convert/FromGhc/Constructors.hs
git commit -m "Emit Argument child items for prefix constructors (#235)"
```

---

### Task 6: Write integration tests for function arguments

**Files:**
- Modify: `source/library/Scrod/TestSuite/Integration.hs`

**Step 1: Add test for function with arg docs**

Add after the existing function tests section:

```haskell
    Spec.it s "function with arg docs" $ do
      check
        s
        """
        f :: a -- ^ i
          -> a -- ^ o
        """
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/name", "\"f\""),
          ("/items/0/value/signature", "\"a -> a\""),
          ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/parentKey", show (items0Key)),
          ("/items/1/value/signature", "\"a\""),
          ("/items/1/value/documentation/type", "\"Paragraph\""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/signature", "\"a\""),
          ("/items/2/value/documentation/type", "\"Paragraph\"")
        ]
```

Note: The exact item indices and key values depend on how `ConvertM` assigns keys. Run the test first to discover the actual values, then update assertions to match.

**Step 2: Add test for function without arg docs**

```haskell
    Spec.it s "function without arg docs" $ do
      check
        s
        "f :: Int -> Bool -> String"
        [ ("/items/0/value/kind/type", "\"Function\""),
          ("/items/0/value/signature", "\"Int -> Bool -> String\""),
          ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/signature", "\"Int\""),
          ("/items/1/value/documentation/type", "\"Empty\""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/signature", "\"Bool\""),
          ("/items/2/value/documentation/type", "\"Empty\"")
        ]
```

**Step 3: Add test for function with forall + constraints + arg docs**

```haskell
    Spec.it s "function with forall and constraints and arg docs" $ do
      check
        s
        """
        {-# language ExplicitForAll #-}
        f :: forall a. Show a => a -- ^ input
          -> String -- ^ output
        """
        [ ("/items/0/value/signature", "\"forall a. Show a => a -> String\""),
          ("/items/1/value/kind/type", "\"Argument\""),
          ("/items/1/value/signature", "\"a\""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/signature", "\"String\"")
        ]
```

**Step 4: Run tests**

Run: `cabal test --test-options='--hide-successes -p "/function with arg/"'`

Expected: All new tests PASS. If indices or key values are wrong, adjust assertions.

**Step 5: Commit**

```bash
git add source/library/Scrod/TestSuite/Integration.hs
git commit -m "Add integration tests for function argument items (#235)"
```

---

### Task 7: Update existing constructor arg doc tests

**Files:**
- Modify: `source/library/Scrod/TestSuite/Integration.hs:1244-1268`

**Step 1: Update "data constructor with arg doc" test**

The existing test at line 1244 currently only checks the parent signature. Update it to also verify the Argument child items:

```haskell
    Spec.it s "data constructor with arg doc" $ do
      check
        s
        """
        data T2
          = C2
            Int -- ^ arg doc
            Bool
        """
        [ ("/items/1/value/name", "\"C2\""),
          ("/items/1/value/signature", "\"Int -> Bool -> T2\""),
          -- Argument child items:
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/signature", "\"Int\""),
          ("/items/2/value/documentation/type", "\"Paragraph\""),
          ("/items/3/value/kind/type", "\"Argument\""),
          ("/items/3/value/signature", "\"Bool\""),
          ("/items/3/value/documentation/type", "\"Empty\"")
        ]
```

**Step 2: Update "data constructor GADT with arg doc" test**

```haskell
    Spec.it s "data constructor GADT with arg doc" $ do
      check
        s
        """
        data T3 where
          C3 ::
            Int -- ^ arg doc
            -> T3
        """
        [ ("/items/1/value/name", "\"C3\""),
          ("/items/1/value/signature", "\"Int -> T3\""),
          ("/items/2/value/kind/type", "\"Argument\""),
          ("/items/2/value/signature", "\"Int\""),
          ("/items/2/value/documentation/type", "\"Paragraph\"")
        ]
```

Note: Item indices may need adjustment depending on how keys are allocated. Run tests and adjust.

**Step 3: Run tests**

Run: `cabal test --test-options='--hide-successes -p "/data constructor/"'`

Expected: All constructor tests PASS.

**Step 4: Run full test suite**

Run: `cabal test --test-options='--hide-successes'`

Expected: ALL tests PASS. No regressions.

**Step 5: Commit**

```bash
git add source/library/Scrod/TestSuite/Integration.hs
git commit -m "Update constructor arg doc tests to verify Argument items (#235)"
```

---

### Task 8: Final verification

**Step 1: Run pedantic build**

Run: `cabal build --flags=pedantic 2>&1 | tail -10`

Expected: SUCCESS with no warnings.

**Step 2: Run full test suite**

Run: `cabal test --test-options='--hide-successes'`

Expected: ALL tests PASS.

**Step 3: Manual smoke test with the example from the issue**

Run:
```bash
echo 'f :: a -- ^ i
  -> a -- ^ o' | cabal run scrod
```

Verify:
- The function item's `signature` field is `"a -> a"` (no doc comments)
- Two Argument child items exist with `signature` of `"a"` and docs `"i"` / `"o"`

**Step 4: Commit if any fixes were needed**

```bash
git add -A
git commit -m "Fix issues found during final verification (#235)"
```
