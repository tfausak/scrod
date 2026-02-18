# Cabal Extensions Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow Scrod to accept language extensions and language settings from external sources (CLI flags and `.cabal` file content), and have the VSCode extension automatically discover and pass these from workspace `.cabal` files.

**Architecture:** Add `--ghc-option` and `--cabal` CLI flags that thread extra GHC options through to `Parse.parse`. Extend `Scrod.Cabal` to parse full `.cabal` files (recursing into sections). Update the VSCode extension to find, read, and pass `.cabal` file content to the WASM engine.

**Tech Stack:** Haskell (GHC API, Cabal-syntax `Distribution.Fields`), TypeScript (VSCode extension API)

---

### Task 1: Extend `Scrod.Cabal` with full cabal file parsing

**Files:**
- Modify: `source/library/Scrod/Cabal.hs`

**Step 1: Write failing tests for `parseCabalFile`**

Add to the `spec` function at the end of `Scrod.Cabal` (after the existing `extractHeader` tests, around line 143):

```haskell
  Spec.named s 'parseCabalFile $ do
    Spec.it s "returns empty for empty input" $ do
      Spec.assertEq s (parseCabalFile "") ([], Nothing)

    Spec.it s "returns empty for invalid cabal content" $ do
      Spec.assertEq s (parseCabalFile "!!!") ([], Nothing)

    Spec.it s "discovers top-level default-extensions" $ do
      Spec.assertEq
        s
        (parseCabalFile "default-extensions: OverloadedStrings")
        (["OverloadedStrings"], Nothing)

    Spec.it s "discovers default-language" $ do
      Spec.assertEq
        s
        (parseCabalFile "default-language: GHC2021")
        ([], Just "GHC2021")

    Spec.it s "discovers extensions inside a section" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            "library\n  default-extensions: GADTs"
        )
        (["GADTs"], Nothing)

    Spec.it s "discovers extensions in multiple sections" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            "common warnings\n  default-extensions: OverloadedStrings\nlibrary\n  default-extensions: GADTs"
        )
        (["OverloadedStrings", "GADTs"], Nothing)

    Spec.it s "picks the last default-language" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            "common base\n  default-language: Haskell2010\nlibrary\n  default-language: GHC2021"
        )
        ([], Just "GHC2021")

    Spec.it s "discovers both extensions and language" $ do
      Spec.assertEq
        s
        ( parseCabalFile
            "library\n  default-language: GHC2021\n  default-extensions: OverloadedStrings"
        )
        (["OverloadedStrings"], Just "GHC2021")
```

**Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='--hide-successes -p "/parseCabalFile/"'`
Expected: compilation error (`parseCabalFile` not defined)

**Step 3: Implement `parseCabalFile`**

Add these functions to `Scrod.Cabal` (before the `spec` function). Also add `import qualified Data.List as List` if not present, and add `TemplateHaskellQuotes` is already enabled.

```haskell
-- | Parse a full @.cabal@ file and extract @default-extensions@ and
-- @default-language@ from all stanzas (top-level fields and sections).
parseCabalFile :: String -> ([String], Maybe String)
parseCabalFile content =
  case Fields.readFields (Char8.pack content) of
    Left _ -> ([], Nothing)
    Right fields ->
      let allFlat = flattenFields fields
          exts = concatMap (fieldValues "default-extensions") allFlat
          langs = concatMap (fieldValues "default-language") allFlat
       in (exts, lastMaybe langs)

flattenFields :: [Fields.Field pos] -> [Fields.Field pos]
flattenFields = concatMap go
  where
    go field@(Fields.Field _ _) = [field]
    go (Fields.Section _ _ nested) = concatMap go nested

fieldValues :: String -> Fields.Field pos -> [String]
fieldValues target (Fields.Field (Fields.Name _ name) fieldLines)
  | Char8.map Char.toLower name == Char8.pack target =
      concatMap getWords fieldLines
fieldValues _ _ = []

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (List.last xs)
```

Also add `parseCabalFile` to the export list. The module currently has no explicit export list so it exports everything — confirm this and if it does, no change needed.

**Step 4: Run tests to verify they pass**

Run: `cabal test --test-options='--hide-successes -p "/parseCabalFile/"'`
Expected: all parseCabalFile tests PASS

**Step 5: Commit**

```bash
git add source/library/Scrod/Cabal.hs
git commit -m "Add parseCabalFile for extracting extensions from .cabal files"
```

---

### Task 2: Add `--ghc-option` and `--cabal` CLI flags

**Files:**
- Modify: `source/library/Scrod/Executable/Flag.hs`
- Modify: `source/library/Scrod/Executable/Config.hs`

**Step 1: Write failing tests for `Flag`**

In `Flag.hs`, add tests to the `spec` function (after the `version` describe block, around line 93):

```haskell
    Spec.describe s "ghc-option" $ do
      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--ghc-option=-XOverloadedStrings"]) $ Just [GhcOption "-XOverloadedStrings"]

      Spec.it s "fails with no argument" $ do
        Spec.assertEq s (fromArguments ["--ghc-option"]) Nothing

      Spec.it s "works with multiple" $ do
        Spec.assertEq s (fromArguments ["--ghc-option=-XCPP", "--ghc-option=-XGADTs"]) $ Just [GhcOption "-XCPP", GhcOption "-XGADTs"]

    Spec.describe s "cabal" $ do
      Spec.it s "works with an argument" $ do
        Spec.assertEq s (fromArguments ["--cabal=content"]) $ Just [Cabal "content"]

      Spec.it s "fails with no argument" $ do
        Spec.assertEq s (fromArguments ["--cabal"]) Nothing
```

**Step 2: Run tests to verify they fail**

Run: `cabal test --test-options='--hide-successes -p "/fromArguments/"'`
Expected: compilation error (`GhcOption` and `Cabal` not defined)

**Step 3: Implement Flag changes**

In `Flag.hs`:

Add `Cabal String` and `GhcOption String` to the `Flag` data type (maintain alphabetical order):

```haskell
data Flag
  = Cabal String
  | Format String
  | GhcOption String
  | Help (Maybe String)
  | Literate (Maybe String)
  | Schema (Maybe String)
  | Signature (Maybe String)
  | Version (Maybe String)
  deriving (Eq, Ord, Show)
```

Add to `optDescrs` (maintain alphabetical order):

```haskell
optDescrs :: [GetOpt.OptDescr Flag]
optDescrs =
  [ GetOpt.Option [] ["cabal"] (GetOpt.ReqArg Cabal "CONTENT") "Sets the cabal file content for discovering extensions.",
    GetOpt.Option [] ["format"] (GetOpt.ReqArg Format "FORMAT") "Sets the output format (json or html).",
    GetOpt.Option [] ["ghc-option"] (GetOpt.ReqArg GhcOption "OPTION") "Sets a GHC option (e.g. -XOverloadedStrings).",
    GetOpt.Option ['h'] ["help"] (GetOpt.OptArg Help "BOOL") "Shows the help.",
    GetOpt.Option [] ["literate"] (GetOpt.OptArg Literate "BOOL") "Treats the input as Literate Haskell.",
    GetOpt.Option [] ["schema"] (GetOpt.OptArg Schema "BOOL") "Shows the JSON output schema.",
    GetOpt.Option [] ["signature"] (GetOpt.OptArg Signature "BOOL") "Treats the input as a Backpack signature."
  ]
```

**Step 4: Write failing tests for `Config`**

In `Config.hs`, add tests to the `spec` function (after existing test blocks):

```haskell
    Spec.describe s "ghcOptions" $ do
      Spec.it s "defaults to empty" $ do
        Spec.assertEq s (ghcOptions <$> fromFlags []) $ Just []

      Spec.it s "collects one option" $ do
        Spec.assertEq s (ghcOptions <$> fromFlags [Flag.GhcOption "-XCPP"]) $ Just ["-XCPP"]

      Spec.it s "collects multiple options in order" $ do
        Spec.assertEq s (ghcOptions <$> fromFlags [Flag.GhcOption "-XCPP", Flag.GhcOption "-XGADTs"]) $ Just ["-XCPP", "-XGADTs"]

    Spec.describe s "cabal" $ do
      Spec.it s "defaults to nothing" $ do
        Spec.assertEq s (cabal <$> fromFlags []) $ Just Nothing

      Spec.it s "works with content" $ do
        Spec.assertEq s (cabal <$> fromFlags [Flag.Cabal "library\n  default-extensions: GADTs"]) $ Just (Just "library\n  default-extensions: GADTs")
```

**Step 5: Run tests to verify they fail**

Run: `cabal test --test-options='--hide-successes -p "/fromFlags/"'`
Expected: compilation error (`ghcOptions` and `cabal` not fields of Config)

**Step 6: Implement Config changes**

In `Config.hs`:

Update the `Config` data type:

```haskell
data Config = MkConfig
  { cabal :: Maybe String,
    format :: Format.Format,
    ghcOptions :: [String],
    help :: Bool,
    literate :: Bool,
    schema :: Bool,
    signature :: Bool,
    version :: Bool
  }
  deriving (Eq, Ord, Show)
```

Update `initial`:

```haskell
initial :: Config
initial =
  MkConfig
    { cabal = Nothing,
      format = Format.Json,
      ghcOptions = [],
      help = False,
      literate = False,
      schema = False,
      signature = False,
      version = False
    }
```

Add cases to `applyFlag`:

```haskell
applyFlag config flag = case flag of
  Flag.Cabal string -> pure config {cabal = Just string}
  Flag.Format string -> do
    fmt <- Format.fromString string
    pure config {format = fmt}
  Flag.GhcOption string -> pure config {ghcOptions = ghcOptions config <> [string]}
  -- ... rest unchanged ...
```

**Step 7: Run tests to verify they pass**

Run: `cabal test --test-options='--hide-successes -p "/fromArguments/|/fromFlags/"'`
Expected: all flag and config tests PASS

**Step 8: Commit**

```bash
git add source/library/Scrod/Executable/Flag.hs source/library/Scrod/Executable/Config.hs
git commit -m "Add --ghc-option and --cabal CLI flags"
```

---

### Task 3: Thread extra options through `Parse.parse`

**Files:**
- Modify: `source/library/Scrod/Ghc/Parse.hs`
- Modify: `source/library/Scrod/Executable/Main.hs`

**Step 1: Modify `Parse.parse` signature to accept extra options**

Change `parse` in `Parse.hs` from:

```haskell
parse ::
  Bool ->
  String ->
  Either ...
parse isSignature string = do
  let originalStringBuffer = StringBuffer.stringToStringBuffer string
  let cabalOptions = cabalExtensionOptions string
  languageAndExtensions <- Bifunctor.first Exception.displayException $ discoverExtensions cabalOptions originalStringBuffer
```

To:

```haskell
parse ::
  Bool ->
  [SrcLoc.Located String] ->
  String ->
  Either ...
parse isSignature extraOptions string = do
  let originalStringBuffer = StringBuffer.stringToStringBuffer string
  let cabalOptions = cabalExtensionOptions string
  languageAndExtensions <- Bifunctor.first Exception.displayException $ discoverExtensions (extraOptions <> cabalOptions) originalStringBuffer
```

**Step 2: Update existing `Parse.spec` tests**

Update all test calls from `parse False ""` to `parse False [] ""`, etc. Every call to `parse` in the spec needs the new `[]` argument. There are approximately 9 test cases that call `parse`.

For example:
```haskell
-- Before:
Spec.assertEq s (fst <$> parse False "") $ Right (Nothing, [])
-- After:
Spec.assertEq s (fst <$> parse False [] "") $ Right (Nothing, [])
```

**Step 3: Add a new test for extra options**

Add to `Parse.spec`:

```haskell
    Spec.it s "succeeds with extra options" $ do
      Spec.assertEq s (fst <$> parse False [SrcLoc.L SrcLoc.noSrcSpan "-XOverloadedStrings"] "") $ Right (Nothing, [Session.On Extension.OverloadedStrings])
```

This requires adding `import qualified GHC.Types.SrcLoc as SrcLoc` if not already present (it is — line 28).

**Step 4: Update `Main.hs` to thread options**

In `Main.hs`, add imports:

```haskell
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified Scrod.Cabal as Cabal
```

Change line 66 from:

```haskell
  result <- Either.throw . Bifunctor.first userError $ Parse.parse isSignature source
```

To:

```haskell
  let cabalFileOptions = maybe [] Cabal.parseCabalFileOptions (Config.cabal config)
  let ghcOpts = fmap (SrcLoc.L SrcLoc.noSrcSpan) (Config.ghcOptions config)
  let extraOptions = cabalFileOptions <> ghcOpts
  result <- Either.throw . Bifunctor.first userError $ Parse.parse isSignature extraOptions source
```

**Step 5: Add `parseCabalFileOptions` to `Scrod.Cabal`**

Add to `Scrod.Cabal`:

```haskell
import qualified GHC.Types.SrcLoc as SrcLoc

-- | Parse a @.cabal@ file and return GHC option strings for discovered
-- extensions and language.
parseCabalFileOptions :: String -> [SrcLoc.Located String]
parseCabalFileOptions content =
  let (exts, lang) = parseCabalFile content
      langOpts = maybe [] (\l -> [SrcLoc.L SrcLoc.noSrcSpan $ "-X" <> l]) lang
      extOpts = fmap (\e -> SrcLoc.L SrcLoc.noSrcSpan $ "-X" <> e) exts
   in langOpts <> extOpts
```

**Step 6: Build and run all tests**

Run: `cabal build`
Then: `cabal test --test-options='--hide-successes'`
Expected: all tests PASS

**Step 7: Commit**

```bash
git add source/library/Scrod/Ghc/Parse.hs source/library/Scrod/Executable/Main.hs source/library/Scrod/Cabal.hs
git commit -m "Thread extra GHC options from CLI flags through Parse.parse"
```

---

### Task 4: Add integration tests

**Files:**
- Modify: `source/library/Scrod/TestSuite/Integration.hs`

**Step 1: Add integration tests for `--ghc-option`**

Find the extensions test block (around line 51). Add after the existing "works with cabal script header with shebang" test (around line 129):

```haskell
    Spec.it s "works with --ghc-option extension" $ do
      checkWith
        s
        ["--ghc-option", "-XOverloadedStrings"]
        ""
        [("/extensions/OverloadedStrings", "true")]

    Spec.it s "works with --ghc-option language" $ do
      checkWith
        s
        ["--ghc-option", "-XHaskell2010"]
        ""
        [("/language", "\"Haskell2010\"")]

    Spec.it s "source pragmas override --ghc-option" $ do
      checkWith
        s
        ["--ghc-option", "-XOverloadedStrings"]
        "{-# language NoOverloadedStrings #-}"
        [("/extensions/OverloadedStrings", "false")]
```

**Step 2: Add integration tests for `--cabal`**

```haskell
    Spec.it s "works with --cabal extensions" $ do
      checkWith
        s
        ["--cabal", "library\n  default-extensions: OverloadedStrings"]
        ""
        [("/extensions/OverloadedStrings", "true")]

    Spec.it s "works with --cabal language" $ do
      checkWith
        s
        ["--cabal", "library\n  default-language: GHC2021"]
        ""
        [("/language", "\"GHC2021\"")]
```

**Step 3: Run integration tests**

Run: `cabal test --test-options='--hide-successes -p "/integration/"'`
Expected: all integration tests PASS (including new ones)

**Step 4: Commit**

```bash
git add source/library/Scrod/TestSuite/Integration.hs
git commit -m "Add integration tests for --ghc-option and --cabal flags"
```

---

### Task 5: Update VSCode extension — WASM engine

**Files:**
- Modify: `extra/vscode/src/wasmEngine.ts`

**Step 1: Extend `Process` type to accept cabal content**

Change the type signature and return function:

```typescript
type Process = (
  source: string,
  literate: boolean,
  signature: boolean,
  cabalContent: string | null
) => Promise<string>;
```

Update the return statement (lines 55-60):

```typescript
  return async (source, literate, signature, cabalContent) => {
    const args = ["--format", "html"];
    if (literate) args.push("--literate");
    if (signature) args.push("--signature");
    if (cabalContent) args.push("--cabal", cabalContent);
    return scrod(args, source);
  };
```

**Step 2: Commit**

```bash
git add extra/vscode/src/wasmEngine.ts
git commit -m "Extend WASM engine to accept cabal content"
```

---

### Task 6: Update VSCode extension — cabal file discovery

**Files:**
- Modify: `extra/vscode/src/extension.ts`

**Step 1: Add cabal content cache and discovery**

Add after the existing module-level variables (after line 11):

```typescript
let cabalContent: string | null = null;
```

Add a function to discover and read the `.cabal` file:

```typescript
async function discoverCabalContent(): Promise<string | null> {
  const files = await vscode.workspace.findFiles("**/*.cabal", "**/dist-newstyle/**", 1);
  if (files.length === 0) return null;
  const bytes = await vscode.workspace.fs.readFile(files[0]);
  return Buffer.from(bytes).toString("utf-8");
}
```

**Step 2: Initialize cabal content and set up file watcher**

In the `activate` function, after loading the WASM engine (after line 14), add:

```typescript
  discoverCabalContent().then((content) => {
    cabalContent = content;
  });

  const cabalWatcher = vscode.workspace.createFileSystemWatcher("**/*.cabal");
  cabalWatcher.onDidChange(() => {
    discoverCabalContent().then((content) => {
      cabalContent = content;
    });
  });
  cabalWatcher.onDidCreate(() => {
    discoverCabalContent().then((content) => {
      cabalContent = content;
    });
  });
  cabalWatcher.onDidDelete(() => {
    cabalContent = null;
  });
  context.subscriptions.push(cabalWatcher);
```

**Step 3: Pass cabal content to WASM engine**

Update the `update` function (around line 107) to pass cabal content:

```typescript
// Before:
    html = await process(document.getText(), literate, isSignature);
// After:
    html = await process(document.getText(), literate, isSignature, cabalContent);
```

**Step 4: Update the engine type in the module-level variable**

Update line 6:

```typescript
// Before:
let engine: Promise<(source: string, literate: boolean, signature: boolean) => Promise<string>>;
// After:
let engine: Promise<(source: string, literate: boolean, signature: boolean, cabalContent: string | null) => Promise<string>>;
```

**Step 5: Commit**

```bash
git add extra/vscode/src/extension.ts
git commit -m "Discover .cabal files and pass content to WASM engine"
```

---

### Task 7: Build and verify everything

**Step 1: Build Haskell**

Run: `cabal build`
Expected: clean build

**Step 2: Run all tests**

Run: `cabal test --test-options='--hide-successes'`
Expected: all tests PASS

**Step 3: Check TypeScript compiles**

Run: `cd extra/vscode && npx tsc --noEmit`
Expected: no errors

**Step 4: Final commit if any formatting fixes needed**

Run: `ormolu --mode check $(find source -name "*.hs")` and fix if needed.

**Step 5: Push branch and create PR**

```bash
git push -u origin HEAD
gh pr create --draft --title "Get language extensions from Cabal file" --body "Fixes #248.

## Summary
- Add \`--ghc-option\` and \`--cabal\` CLI flags to Scrod
- Extend \`Scrod.Cabal\` to parse full \`.cabal\` files for extensions/language
- Thread extra GHC options through the parse pipeline
- Update VSCode extension to discover \`.cabal\` files and pass content to WASM engine

## Test plan
- Unit tests for \`parseCabalFile\` with various cabal file structures
- Unit tests for new CLI flags
- Integration tests for \`--ghc-option\` and \`--cabal\` affecting parse output
- TypeScript type-checks for VSCode extension changes
"
```
