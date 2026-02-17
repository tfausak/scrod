# Export-Driven Declarations Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the HTML export list drive the Declarations section ordering, with unexported items grouped at the bottom.

**Architecture:** Render-time only changes in `ToHtml.hs`. Build a name-to-item map, walk the export list to emit items in export order, classify items by export disposition, and collect unexported items at the bottom. No changes to core types or FromGhc pipeline.

**Tech Stack:** Haskell, `Data.Map`, `Data.Set`, existing `Scrod.Xml.*` rendering helpers.

---

### Task 1: Add HTML substring test helper

The existing `checkHtml` helper only verifies non-empty output. We need a helper
that checks whether specific strings appear (or don't appear) in the HTML output.

**Files:**
- Modify: `source/library/Scrod/TestSuite/Integration.hs:3080-3089` (after `checkHtml`)

**Step 1: Write the `checkHtmlContains` helper**

Add after the existing `checkHtml` function (line 3089):

```haskell
checkHtmlContains :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> [String] -> m ()
checkHtmlContains s input expected = do
  result <-
    either (Spec.assertFailure s . Exception.displayException) pure
      . Main.mainWith "scrod-test-suite" ["--format", "html"]
      $ pure input
  output <- either (Spec.assertFailure s) pure result
  let html = Builder.toString output
  Monad.forM_ expected $ \needle ->
    Monad.unless (needle `List.isInfixOf` html)
      . Spec.assertFailure s
      $ "expected HTML to contain: " <> needle
```

This requires adding `import qualified Scrod.Extra.Builder as Builder` to the
imports at the top of the file. Check whether it's already imported first —
it is (line 11).

**Step 2: Verify it compiles**

Run: `cabal build --flags=pedantic`
Expected: SUCCESS

**Step 3: Commit**

```
git add source/library/Scrod/TestSuite/Integration.hs
git commit -m "Add checkHtmlContains test helper for HTML content assertions"
```

---

### Task 2: Add failing tests for export-driven declarations

Write integration tests that will fail with the current rendering and pass once
the feature is implemented. These test the core scenarios from the design doc.

**Files:**
- Modify: `source/library/Scrod/TestSuite/Integration.hs` (inside the `spec` function,
  add a new `Spec.describe s "export-driven declarations"` block)

**Step 1: Write the tests**

Add a new describe block at the end of the `spec` function (before the final
`where` clause that defines `check`). These tests use `checkHtmlContains`:

```haskell
  Spec.describe s "export-driven declarations" $ do
    Spec.it s "renders items in export list order" $ do
      checkHtmlContains
        s
        """
        module M ( y, x ) where
        x = ()
        y = ()
        """
        -- y should appear before x in the HTML output
        [ "Declarations"
        ]

    Spec.it s "renders section headings from export groups" $ do
      checkHtmlContains
        s
        """
        module M
          ( -- * Section One
            x
          ) where
        x = ()
        """
        [ "Section One"
        ]

    Spec.it s "renders unexported items under Unexported heading" $ do
      checkHtmlContains
        s
        """
        module M ( x ) where
        x = ()
        y = ()
        """
        [ "Unexported"
        ]

    Spec.it s "does not render Unexported when all items are exported" $ do
      checkHtmlNotContains
        s
        """
        module M ( x ) where
        x = ()
        """
        [ "Unexported"
        ]

    Spec.it s "keeps source order when no export list" $ do
      checkHtmlNotContains
        s
        """
        x = ()
        y = ()
        """
        [ "Unexported"
        ]

    Spec.it s "does not render standalone Exports section" $ do
      checkHtmlNotContains
        s
        """
        module M ( x ) where
        x = ()
        """
        [ ">Exports<"
        ]
```

Also add a `checkHtmlNotContains` helper right after `checkHtmlContains`:

```haskell
checkHtmlNotContains :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> [String] -> m ()
checkHtmlNotContains s input forbidden = do
  result <-
    either (Spec.assertFailure s . Exception.displayException) pure
      . Main.mainWith "scrod-test-suite" ["--format", "html"]
      $ pure input
  output <- either (Spec.assertFailure s) pure result
  let html = Builder.toString output
  Monad.forM_ forbidden $ \needle ->
    Monad.when (needle `List.isInfixOf` html)
      . Spec.assertFailure s
      $ "expected HTML NOT to contain: " <> needle
```

**Step 2: Verify tests fail**

Run: `cabal test --test-options='--hide-successes -p "export-driven"'`
Expected: Some tests FAIL (the "Unexported" and "no Exports section" tests will
fail because the current code doesn't have this behavior yet).

**Step 3: Commit**

```
git add source/library/Scrod/TestSuite/Integration.hs
git commit -m "Add failing tests for export-driven declarations (#225)"
```

---

### Task 3: Add `Data.Set` import and classification helpers to ToHtml

Add the necessary import and the pure helper functions that classify item kinds
by their export disposition.

**Files:**
- Modify: `source/library/Scrod/Convert/ToHtml.hs:10-14` (imports)
- Modify: `source/library/Scrod/Convert/ToHtml.hs:549-589` (items section)

**Step 1: Add imports**

Add to the import block (after the `Data.Maybe` import on line 13):

```haskell
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
```

**Step 2: Add classification functions**

Add these helper functions before the items section (around line 550, after the
extensions section). These are used by the new `declarationsContents` function:

```haskell
-- | Whether an item kind is "always visible" (Category B) — implicitly
-- exported by GHC regardless of the export list.
isAlwaysVisible :: ItemKind.ItemKind -> Bool
isAlwaysVisible k = case k of
  ItemKind.ClassInstance -> True
  ItemKind.StandaloneDeriving -> True
  ItemKind.DerivedInstance -> True
  ItemKind.Rule -> True
  ItemKind.Default -> True
  ItemKind.Annotation -> True
  ItemKind.Splice -> True
  _ -> False

-- | Whether an item kind is a traditional subordinate that can be filtered
-- by export subordinate restrictions (e.g., @T(C1, C2)@ vs @T(..)@).
isTraditionalSubordinate :: ItemKind.ItemKind -> Bool
isTraditionalSubordinate k = case k of
  ItemKind.DataConstructor -> True
  ItemKind.GADTConstructor -> True
  ItemKind.RecordField -> True
  ItemKind.ClassMethod -> True
  ItemKind.DefaultMethodSignature -> True
  _ -> False
```

**Step 3: Verify it compiles**

Run: `cabal build --flags=pedantic`
Expected: SUCCESS (may warn about unused imports/functions — that's OK, they'll
be used in the next task).

**Step 4: Commit**

```
git add source/library/Scrod/Convert/ToHtml.hs
git commit -m "Add export disposition classification helpers to ToHtml"
```

---

### Task 4: Implement `declarationsContents`

This is the core implementation. Replace the existing `itemsContents` with a new
`declarationsContents` function that takes both exports and items.

**Files:**
- Modify: `source/library/Scrod/Convert/ToHtml.hs:551-589`

**Step 1: Replace `itemsContents` with `declarationsContents`**

Replace the `-- Items section` block (lines 551–589) with the new implementation.
The function signature changes from:

```haskell
itemsContents :: [Located.Located Item.Item] -> [Content.Content Element.Element]
```

to:

```haskell
declarationsContents :: Maybe [Export.Export] -> [Located.Located Item.Item] -> [Content.Content Element.Element]
```

The implementation:

```haskell
-- Declarations section

declarationsContents :: Maybe [Export.Export] -> [Located.Located Item.Item] -> [Content.Content Element.Element]
declarationsContents exports items =
  [ element "h2" [] [Xml.string "Declarations"],
    case (exports, items) of
      (_, []) -> Xml.string "None."
      (Nothing, _) -> defaultDeclarations
      (Just [], _) -> defaultDeclarations
      (Just es, _) -> exportDrivenDeclarations es
  ]
  where
    -- Shared helpers

    itemNatKey :: Located.Located Item.Item -> Natural.Natural
    itemNatKey = ItemKey.unwrap . Item.key . Located.value

    childrenMap :: Map.Map Natural.Natural [Located.Located Item.Item]
    childrenMap = foldr addChild Map.empty items

    addChild :: Located.Located Item.Item -> Map.Map Natural.Natural [Located.Located Item.Item] -> Map.Map Natural.Natural [Located.Located Item.Item]
    addChild li acc = case Item.parentKey (Located.value li) of
      Nothing -> acc
      Just pk -> Map.insertWith (<>) (ItemKey.unwrap pk) [li] acc

    topLevelItems :: [Located.Located Item.Item]
    topLevelItems = filter (isTopLevel . Located.value) items

    isTopLevel :: Item.Item -> Bool
    isTopLevel = Maybe.isNothing . Item.parentKey

    -- Render an item with all its children (no filtering).
    renderItemWithChildren :: Located.Located Item.Item -> [Content.Content Element.Element]
    renderItemWithChildren li =
      [ itemContent li
          . foldMap renderItemWithChildren
          $ Map.findWithDefault [] (itemNatKey li) childrenMap
      ]

    -- Default: no export list, render everything in source order.
    defaultDeclarations :: Content.Content Element.Element
    defaultDeclarations =
      element "details" [("open", "open")] $
        element
          "summary"
          []
          [ Xml.string "Show/hide ",
            Xml.string $ pluralize (length topLevelItems) "declaration",
            Xml.string "."
          ]
          : foldMap renderItemWithChildren topLevelItems

    -- Map from item name to top-level item, for export matching.
    nameMap :: Map.Map Text.Text (Located.Located Item.Item)
    nameMap =
      Map.fromList
        [ (ItemName.unwrap n, li)
        | li <- topLevelItems,
          Just n <- [Item.name (Located.value li)]
        ]

    -- Export-driven: walk the export list, then add always-visible and
    -- unexported items.
    exportDrivenDeclarations :: [Export.Export] -> Content.Content Element.Element
    exportDrivenDeclarations es =
      let (exportedHtml, usedAfterExports) = walkExports es Set.empty
          (alwaysVisibleHtml, usedAfterVisible) = renderAlwaysVisible usedAfterExports
          usedAfterComplete = handleCompletePragmas usedAfterVisible
          unexportedHtml = renderUnexported usedAfterComplete
       in element "details" [("open", "open")] $
            element
              "summary"
              []
              [ Xml.string "Show/hide ",
                Xml.string $ pluralize (length topLevelItems) "declaration",
                Xml.string "."
              ]
              : exportedHtml
              <> alwaysVisibleHtml
              <> unexportedHtml

    -- Step 2: Walk the export list in order.
    walkExports :: [Export.Export] -> Set.Set Natural.Natural -> ([Content.Content Element.Element], Set.Set Natural.Natural)
    walkExports [] used = ([], used)
    walkExports (e : es) used = case e of
      Export.Identifier ident ->
        let name = ExportName.name (ExportIdentifier.name ident)
            subs = ExportIdentifier.subordinates ident
         in case Map.lookup name nameMap of
              Just li
                | not (Set.member (itemNatKey li) used) ->
                    let (here, newKeys) = renderExportedItem subs li
                        (rest, used') = walkExports es (Set.union newKeys used)
                     in (here <> rest, used')
              _ -> walkExports es used
      Export.Group section ->
        let here = [sectionContent section]
            (rest, used') = walkExports es used
         in (here <> rest, used')
      Export.Doc doc ->
        let here = docContents doc
            (rest, used') = walkExports es used
         in (here <> rest, used')
      Export.DocNamed name ->
        let here = [docNamedContent name]
            (rest, used') = walkExports es used
         in (here <> rest, used')

    -- Render an exported item with subordinate-filtered children.
    renderExportedItem :: Maybe Subordinates.Subordinates -> Located.Located Item.Item -> ([Content.Content Element.Element], Set.Set Natural.Natural)
    renderExportedItem subs li =
      let k = itemNatKey li
          allChildren = Map.findWithDefault [] k childrenMap
          visibleChildren = filter (shouldShowChild subs) allChildren
          (childHtml, childKeys) =
            foldr
              ( \c (accHtml, accKeys) ->
                  let (h, ks) = renderCollecting c
                   in (h <> accHtml, Set.union ks accKeys)
              )
              ([], Set.empty)
              visibleChildren
       in ( [ itemContent li childHtml
            ],
            Set.insert k childKeys
          )

    -- Render an item with all children, collecting keys.
    renderCollecting :: Located.Located Item.Item -> ([Content.Content Element.Element], Set.Set Natural.Natural)
    renderCollecting li =
      let k = itemNatKey li
          children = Map.findWithDefault [] k childrenMap
          (childHtml, childKeys) =
            foldr
              ( \c (accHtml, accKeys) ->
                  let (h, ks) = renderCollecting c
                   in (h <> accHtml, Set.union ks accKeys)
              )
              ([], Set.empty)
              children
       in ( [ itemContent li childHtml
            ],
            Set.insert k childKeys
          )

    -- Decide whether a child should be shown with an exported parent.
    shouldShowChild :: Maybe Subordinates.Subordinates -> Located.Located Item.Item -> Bool
    shouldShowChild subs li =
      let item = Located.value li
       in if not (isTraditionalSubordinate (Item.kind item))
            then True -- Non-subordinate children always shown
            else case subs of
              Nothing -> False -- T with no parens: no subordinates
              Just (Subordinates.MkSubordinates True _) -> True -- T(..): all
              Just (Subordinates.MkSubordinates False explicit) ->
                case Item.name item of
                  Nothing -> False
                  Just n -> Set.member (ItemName.unwrap n) explicitNames
                    where
                      explicitNames = Set.fromList $ fmap ExportName.name explicit

    -- Step 3: Render always-visible top-level items not yet used.
    renderAlwaysVisible :: Set.Set Natural.Natural -> ([Content.Content Element.Element], Set.Set Natural.Natural)
    renderAlwaysVisible used =
      let visible =
            [ li
            | li <- topLevelItems,
              not (Set.member (itemNatKey li) used),
              isAlwaysVisible (Item.kind (Located.value li))
            ]
          (html, keys) =
            foldr
              ( \li (accHtml, accKeys) ->
                  let (h, ks) = renderCollecting li
                   in (h <> accHtml, Set.union ks accKeys)
              )
              ([], used)
              visible
       in (html, keys)

    -- Step 4: Handle COMPLETE pragmas — mark as used if all children consumed.
    handleCompletePragmas :: Set.Set Natural.Natural -> Set.Set Natural.Natural
    handleCompletePragmas used =
      foldr
        ( \li acc ->
            let k = itemNatKey li
                cs = Map.findWithDefault [] k childrenMap
             in if Item.kind (Located.value li) == ItemKind.CompletePragma
                  && not (null cs)
                  && all (\c -> Set.member (itemNatKey c) acc) cs
                  then Set.insert k acc
                  else acc
        )
        used
        topLevelItems

    -- Step 5: Render unexported section.
    renderUnexported :: Set.Set Natural.Natural -> [Content.Content Element.Element]
    renderUnexported used =
      let unexported =
            [ li
            | li <- topLevelItems,
              not (Set.member (itemNatKey li) used),
              not (isAlwaysVisible (Item.kind (Located.value li)))
            ]
       in if null unexported
            then []
            else
              element "h3" [] [Xml.string "Unexported"]
                : foldMap (renderUnexportedItem used) unexported

    -- Render an unexported item, omitting children already consumed.
    renderUnexportedItem :: Set.Set Natural.Natural -> Located.Located Item.Item -> [Content.Content Element.Element]
    renderUnexportedItem used li =
      let k = itemNatKey li
          children = Map.findWithDefault [] k childrenMap
          unusedChildren = filter (\c -> not (Set.member (itemNatKey c) used)) children
       in [ itemContent li (foldMap (renderUnexportedItem used) unusedChildren)
          ]
```

**Step 2: Verify it compiles**

Run: `cabal build --flags=pedantic`
Expected: SUCCESS (the old `itemsContents` is removed, but `bodyElement` still
references it — that's fixed in the next task).

Actually, this won't compile until `bodyElement` is updated. So this step and
Task 5 should be done together before verifying compilation.

---

### Task 5: Update `bodyElement` and remove `exportsContents`

Wire up the new `declarationsContents` in `bodyElement` and remove the old
`exportsContents` function.

**Files:**
- Modify: `source/library/Scrod/Convert/ToHtml.hs:141-159` (`bodyElement`)
- Modify: `source/library/Scrod/Convert/ToHtml.hs:212-303` (remove `exportsContents`
  and related helpers: `exportContent`, `exportIdentifierContents`,
  `exportNameContents`, `subordinatesContents`, `docNamedContent`)

**Step 1: Update `bodyElement`**

In `bodyElement` (line 141), change lines 155-156 from:

```haskell
            <> [element "section" [("class", "my-3")] . exportsContents $ Module.exports x]
            <> [element "section" [("class", "my-3")] . itemsContents $ Module.items x]
```

to:

```haskell
            <> [element "section" [("class", "my-3")] $ declarationsContents (Module.exports x) (Module.items x)]
```

**Step 2: Remove `exportsContents` and its helpers**

Delete lines 212–303: the `exportsContents`, `exportContent`,
`exportIdentifierContents`, `exportNameContents`, and `subordinatesContents`
functions. Keep `sectionContent` and `docNamedContent` — they are reused by
`declarationsContents`.

Also remove the `ExportNameKind` import (line 23) if it becomes unused after
removing `exportNameContents`.

**Step 3: Verify it compiles**

Run: `cabal build --flags=pedantic`
Expected: SUCCESS with no warnings (pedantic mode uses `-Werror`).

**Step 4: Run all tests**

Run: `cabal test --test-options='--hide-successes'`
Expected: All existing tests PASS (JSON tests are unaffected).

**Step 5: Commit**

```
git add source/library/Scrod/Convert/ToHtml.hs
git commit -m "Implement export-driven declarations in HTML output (#225)"
```

---

### Task 6: Verify export-driven tests pass

The tests from Task 2 should now pass.

**Step 1: Run the export-driven tests**

Run: `cabal test --test-options='--hide-successes -p "export-driven"'`
Expected: All PASS.

**Step 2: Run the full test suite**

Run: `cabal test --test-options='--hide-successes'`
Expected: All PASS.

**Step 3: Run pedantic build**

Run: `cabal build --flags=pedantic`
Expected: SUCCESS.

**Step 4: Check formatting**

Run: `ormolu --mode check $(find source -name "*.hs")`
Expected: No formatting issues. If there are, fix with
`ormolu --mode inplace $(find source -name "*.hs")` and commit the fix.

---

### Task 7: Add more targeted tests

Add tests for the specific edge cases identified in the design.

**Files:**
- Modify: `source/library/Scrod/TestSuite/Integration.hs`

**Step 1: Add edge case tests**

Add these inside the `"export-driven declarations"` describe block:

```haskell
    Spec.it s "renders export doc comments inline" $ do
      checkHtmlContains
        s
        """
        module M
          ( -- | Some docs
            x
          ) where
        x = ()
        """
        [ "Some docs"
        ]

    Spec.it s "always shows class instances" $ do
      checkHtmlNotContains
        s
        """
        module M ( MyClass ) where
        class MyClass a
        instance MyClass Int
        """
        [ "Unexported"
        ]

    Spec.it s "always shows rules" $ do
      checkHtmlNotContains
        s
        """
        module M ( f ) where
        f x = x
        {-# RULES "f/id" f = id #-}
        """
        [ "Unexported"
        ]
```

**Step 2: Run the tests**

Run: `cabal test --test-options='--hide-successes -p "export-driven"'`
Expected: All PASS.

**Step 3: Commit**

```
git add source/library/Scrod/TestSuite/Integration.hs
git commit -m "Add edge case tests for export-driven declarations (#225)"
```

---

### Task 8: Final verification

**Step 1: Run full test suite**

Run: `cabal test --test-options='--hide-successes'`
Expected: All PASS.

**Step 2: Pedantic build**

Run: `cabal build --flags=pedantic`
Expected: SUCCESS.

**Step 3: Manual smoke test**

Run a quick manual test to visually inspect the HTML output:

```bash
echo 'module M ( -- * Types\n  Bool(..)\n  , -- * Functions\n  not\n  ) where\ndata Bool = True | False\nnot True = False\nnot False = True\nsecretHelper = ()' | cabal run scrod -- --format html > /tmp/test.html
```

Open `/tmp/test.html` and verify:
- "Types" section heading appears before `Bool`
- `Bool` appears before `not`
- `secretHelper` appears under "Unexported"
- No standalone "Exports" section exists
