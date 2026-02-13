# Contributing

## Build Commands

```bash
cabal build                          # build library + CLI
cabal build --flags=pedantic         # build with -Werror (used in CI)
cabal test --test-options='--hide-successes' # run test suite
cabal run scrod                      # run CLI (reads stdin, outputs JSON by default)
cabal run scrod -- --format html     # output HTML instead of JSON
```

### Running a single test or test group

Tests use Tasty, so you can filter with `-p`:

```bash
cabal test --test-options='--hide-successes -p "integration"'       # run integration tests only
cabal test --test-options='--hide-successes -p "/mkDecimal/"'       # run a specific test group
```

The pattern matches against the test path hierarchy (group names joined by `.`).

### WASM build (requires Nix devshell)

```bash
nix develop --command extra/wasm/build.hs
```

### VSCode extension build

You must run the WASM build first.

```bash
nix develop --command extra/vscode/build.sh # produces a .vsix file
```

### Linting and formatting (CI checks all of these)

```bash
hlint source/                                        # lint Haskell
ormolu --mode check $(find source -name "*.hs")      # check formatting
ormolu --mode inplace $(find source -name "*.hs")    # fix formatting
cabal-gild --input scrod.cabal --mode check          # check .cabal formatting
cabal-gild --input scrod.cabal --mode format         # fix .cabal formatting
cabal check                                          # validate .cabal file
```

## Code Conventions

- **Haskell2010** with extensions enabled per-file via pragmas
- **Qualified imports everywhere** (e.g., `import qualified Data.Text as Text`)
- **Formatting:** Ormolu (enforced in CI)
- **Warnings:** `-Weverything` with specific exclusions; `-Werror` under `--flags=pedantic`
- **HLint config:** `.hlint.yaml` — enables dollar/future/generalise groups; ignores "Use infix", "Use list comprehension", "Use tuple-section"
- **No external dependencies** for JSON/HTML generation — uses custom in-tree implementations (`Scrod.Json.*`, `Scrod.Xml.*`)

## Git Conventions

- **Never amend commits or force push.** Always create new commits.
