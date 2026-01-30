# CLAUDE.md

This file provides guidance for AI assistants working with the Scrod codebase.

## Project Overview

**Scrod** is a Haskell documentation extraction library that parses Haskell source code using GHC's parser and Haddock's documentation processor, converting it into a simplified AST representation. It extracts module metadata, exports, documentation, language pragmas, extensions, and source locations.

- **Repository**: https://github.com/tfausak/scrod
- **Maintainer**: Taylor Fausak
- **License**: 0BSD (Zero Clause License)
- **Language**: Haskell 2010
- **Compiler**: GHC 9.10.3

## Directory Structure

```
scrod/
├── source/
│   ├── library/                    # Core library code
│   │   └── Scrod/
│   │       ├── Scrod.hs           # Public re-export module
│   │       └── Unstable/
│   │           ├── Main.hs        # CLI entry point
│   │           ├── Parse.hs       # GHC parsing logic
│   │           ├── Convert.hs     # AST conversion
│   │           ├── Spec.hs        # Test specifications
│   │           ├── Type/          # Data type definitions (25+ modules)
│   │           ├── Extra/         # GHC-related utilities
│   │           └── Exception/     # Exception types
│   ├── executable/
│   │   └── Main.hs                # CLI executable wrapper
│   └── test-suite/
│       └── Main.hs                # Test runner
├── scrod.cabal                     # Package definition
├── cabal.project                   # Cabal project file
├── shell.nix                       # Nix development environment
├── nixpkgs.nix                     # Nix package pinning
├── .hlint.yaml                     # HLint configuration
├── weeder.toml                     # Unused code detection config
└── .github/workflows/ci.yaml       # CI pipeline
```

## Development Environment Setup

This project uses Nix for reproducible builds. Enter the development shell:

```bash
nix-shell
```

The shell provides: `cabal-install`, `ghc`, `haskell-language-server`, `hlint`, `ormolu`, `cabal-gild`, `weeder`, `nixfmt-rfc-style`

## Scripts

**IMPORTANT**: Use the scripts in `scripts/` instead of running equivalent commands directly.

| Script | Purpose |
|--------|---------|
| `scripts/hlint.sh` | Run HLint on source directory |
| `scripts/ormolu.sh` | Format all Haskell files in place |
| `scripts/cabal-test.sh` | Run tests (hides passing tests) |
| `scripts/cabal-gild.sh` | Format scrod.cabal in place |
| `scripts/weeder.sh` | Check for unused code |
| `scripts/cabal-repl.sh` | Start REPL with warnings as warnings |
| `scripts/cabal-check.sh` | Run cabal check on the package |

## Common Commands

### Building

```bash
cabal build                    # Build the project
cabal build --flags=pedantic   # Build with -Werror (CI mode)
```

### Testing

```bash
scripts/cabal-test.sh          # Run test suite (preferred)
```

### Code Quality

```bash
scripts/hlint.sh               # Run linter (preferred)
scripts/ormolu.sh              # Format code in place (preferred)
scripts/cabal-gild.sh          # Format .cabal file (preferred)
scripts/weeder.sh              # Check for unused code (preferred)
scripts/cabal-check.sh         # Validate package metadata
```

### Running the Tool

```bash
echo "module Foo where" | cabal run -- scrod
```

## Git Workflow

### Avoiding Force Push

**IMPORTANT**: AI agents should **NEVER** use force push (`git push --force` or `git push -f`).

Instead, follow this approach:
- Make regular commits with clear, descriptive messages
- Push commits normally using `git push` or `git push -u origin <branch-name>`
- If you need to make changes after a commit, create a new commit rather than amending

**Why avoid force push:**
- Preserves complete commit history for debugging and review
- Prevents accidentally overwriting others' work
- Safer for collaborative environments
- Allows easier rollback if issues arise
- Maintains git history integrity

**Exception**: Only use force push if explicitly requested by the user and you understand the risks.

## Code Style and Conventions

### Import Style

All imports must be qualified with explicit module aliases:

```haskell
import qualified Data.Map as Map
import qualified Scrod.Unstable.Type.Interface as Interface
```

### Data Types

- Use `newtype` wrappers for semantic types
- Constructor naming: `MkTypeName` prefix
- Always derive `Eq`, `Ord`, `Show`

```haskell
newtype Version = MkVersion { value :: NonEmpty.NonEmpty Natural.Natural }
  deriving (Eq, Ord, Show)

data Interface = MkInterface
  { version :: Version.Version,
    language :: Maybe Language.Language
  }
  deriving (Eq, Ord, Show)
```

### Module Naming

- Public API: `Scrod` (re-exports stable interface)
- Unstable API: `Scrod.Unstable.*` prefix indicates API may change
- Types: `Scrod.Unstable.Type.<TypeName>`
- Utilities: `Scrod.Unstable.Extra.<Name>`

### Conversion Functions

Use these naming patterns for type conversions:
- `fromGhc` - Convert from GHC types
- `fromHaddock` - Convert from Haddock types
- `fromString` - Convert from String
- `fromBase` - Convert from base package types

### HLint Rules

The project ignores these suggestions (see `.hlint.yaml`):
- `Use <$>` - Prefer explicit `fmap`
- `Use tuple-section` - Avoid tuple sections

### Compiler Warnings

The project uses `-Weverything` with selective suppressions. Key enabled warnings include all standard warnings except:
- `-Wno-missing-export-lists`
- `-Wno-missing-safe-haskell-mode`
- `-Wno-prepositive-qualified-module`

## Key Modules

| Module | Purpose |
|--------|---------|
| `Scrod.Unstable.Main` | Entry point; `extract :: String -> Either String Interface` |
| `Scrod.Unstable.Parse` | Uses GHC to parse Haskell source |
| `Scrod.Unstable.Convert` | Converts GHC AST to Scrod types |
| `Scrod.Unstable.Type.Interface` | Top-level module representation |
| `Scrod.Unstable.Type.Doc` | Simplified Haddock documentation AST |
| `Scrod.Unstable.Type.Export` | Export list types |
| `Scrod.Unstable.Spec` | Comprehensive test suite (1800+ tests) |

## Testing Approach

Tests use Tasty framework with Heck DSL:

```haskell
describe "feature" $ do
  it "should do something" $ do
    assertSatisfies "description" actual predicate
```

Tests are organized hierarchically by feature in `Scrod.Unstable.Spec`.

## CI Pipeline

GitHub Actions runs on push/PR to `main`:

1. **Gild**: Validates `.cabal` file formatting
2. **Build**: Builds with pedantic flags, generates docs, runs tests
3. **Cabal**: Runs `cabal check`
4. **HLint**: Linting check
5. **Ormolu**: Code formatting validation

All checks must pass before merging.

## Architecture Notes

### Pipeline Flow

```
Source Code (String)
    ↓ Parse.parse
GHC AST + Pragmas
    ↓ Convert.convert
Scrod Interface
```

### Type Safety

- Uses `Void` for phantom types where needed
- Explicit `Either` for error handling (no exceptions in pure code)
- `Maybe` for optional values
- `Located` wrapper for source location tracking

### GHC Integration

The project directly uses GHC library modules (`GHC.Parser`, `GHC.Driver.DynFlags`, etc.) for parsing. This creates tight coupling with the GHC version.

## Version Scheme

Uses date-based versioning following Package Versioning Policy (PVP):
- Format: `0.YYYY.M.D` (e.g., `0.2026.1.26`)

## Adding New Features

1. Create type definitions in `Scrod.Unstable.Type.*`
2. Add conversion logic in `Scrod.Unstable.Convert`
3. Write tests in `Scrod.Unstable.Spec`
4. Expose in library if needed (update `scrod.cabal`)
5. Run full CI checks locally before committing
