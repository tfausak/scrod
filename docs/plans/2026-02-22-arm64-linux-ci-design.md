# ARM64 Linux CI Builds — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add ARM64 Linux builds to CI so release artifacts include both x86_64 and aarch64 Linux binaries.

**Architecture:** Extend the build matrix with a new `runner`/`suffix` scheme. ARM64 entry builds only the main binary (no bench/doc/test). Release script learns two new platform suffixes.

**Tech Stack:** GitHub Actions YAML, Bash

---

### Task 1: Create branch

**Step 1: Create and switch to a new branch**

Run: `git checkout -b tfausak/arm64-linux-ci`

**Step 2: Commit the design doc**

Run:
```bash
git add docs/plans/2026-02-22-arm64-linux-ci-design.md
git commit -m "Add ARM64 Linux CI design doc"
```

---

### Task 2: Restructure build matrix in ci.yml

**Files:**
- Modify: `.github/workflows/ci.yml:11-63`

**Step 1: Replace matrix and runs-on**

Replace the current matrix block (lines 12-23) and update all references. The new matrix uses `runner` (for `runs-on`) and `suffix` (for artifact naming). Drop the `os` field entirely.

New matrix:
```yaml
  build:
    name: Build ${{ matrix.suffix }}
    runs-on: ${{ matrix.runner }}
    needs: meta
    strategy:
      matrix:
        include:
          - runner: macos-latest
            suffix: macOS
          - runner: ubuntu-latest
            suffix: Linux-x86_64
            bench: true
            doc: true
            test: true
          - runner: ubuntu-24.04-arm
            suffix: Linux-aarch64
          - runner: windows-latest
            suffix: Windows
```

**Step 2: Update cache key to include suffix (prevent cross-arch cache sharing)**

Line 42 currently:
```yaml
          key: ${{ runner.os }}-${{ hashFiles('cabal.project.freeze') }}
```
Change to:
```yaml
          key: ${{ matrix.suffix }}-${{ hashFiles('cabal.project.freeze') }}
```

Line 44 currently:
```yaml
          restore-keys: ${{ runner.os }}-
```
Change to:
```yaml
          restore-keys: ${{ matrix.suffix }}-
```

**Step 3: Update artifact upload name**

Line 60 currently:
```yaml
          name: ${{ needs.meta.outputs.name }}-${{ github.sha }}-${{ runner.os }}
```
Change to:
```yaml
          name: ${{ needs.meta.outputs.name }}-${{ github.sha }}-${{ matrix.suffix }}
```

**Step 4: Verify** — Read ci.yml lines 11-63 to confirm all `matrix.os` and `runner.os` references (except the two `runner.os == 'Windows'` checks on lines 55 and 62, which must stay) are replaced.

**Step 5: Commit**

```bash
git add .github/workflows/ci.yml
git commit -m "Restructure build matrix for ARM64 Linux support"
```

---

### Task 3: Update downstream jobs in ci.yml

**Files:**
- Modify: `.github/workflows/ci.yml:105-134` (test and bench jobs)
- Modify: `.github/workflows/ci.yml:210-212` (github-pages job)

These jobs run on `ubuntu-latest` (x86_64) and download the Linux artifact by name. Since `runner.os` still resolves to `Linux` on these runners, but the artifact is now named `Linux-x86_64`, we must hardcode the suffix.

**Step 1: Update test job artifact reference**

Line 115 currently:
```yaml
          name: ${{ needs.meta.outputs.name }}-${{ github.sha }}-${{ runner.os }}
```
Change to:
```yaml
          name: ${{ needs.meta.outputs.name }}-${{ github.sha }}-Linux-x86_64
```

**Step 2: Update bench job artifact reference**

Line 127 currently:
```yaml
          name: ${{ needs.meta.outputs.name }}-${{ github.sha }}-${{ runner.os }}
```
Change to:
```yaml
          name: ${{ needs.meta.outputs.name }}-${{ github.sha }}-Linux-x86_64
```

**Step 3: Update github-pages job artifact reference**

Line 212 currently:
```yaml
          name: ${{ needs.meta.outputs.name }}-${{ github.sha }}-Linux
```
Change to:
```yaml
          name: ${{ needs.meta.outputs.name }}-${{ github.sha }}-Linux-x86_64
```

**Step 4: Commit**

```bash
git add .github/workflows/ci.yml
git commit -m "Point downstream jobs at Linux-x86_64 artifact"
```

---

### Task 4: Update release.sh

**Files:**
- Modify: `.github/workflows/release.sh`

**Step 1: Update hardcoded Linux artifact references (lines 9-10)**

Currently:
```bash
tar --extract --file "${artifact_prefix}-Linux/artifact.tar" --strip-components=1 --wildcards '*.tar.gz'
tar --extract --file "${artifact_prefix}-Linux/artifact.tar" --strip-components=1 artifact/schema.json
```
Change to:
```bash
tar --extract --file "${artifact_prefix}-Linux-x86_64/artifact.tar" --strip-components=1 --wildcards '*.tar.gz'
tar --extract --file "${artifact_prefix}-Linux-x86_64/artifact.tar" --strip-components=1 artifact/schema.json
```

**Step 2: Update case statement (lines 24-26)**

The existing `##*-` parsing extracts the last hyphen segment. For `Linux-x86_64` that gives `x86_64`; for `Linux-aarch64` that gives `aarch64`.

Remove:
```bash
    Linux) platform=linux; file="${name}" ;;
```

Add in its place:
```bash
    x86_64) platform=linux-x86_64; file="${name}" ;;
    aarch64) platform=linux-aarch64; file="${name}" ;;
```

**Step 3: Commit**

```bash
git add .github/workflows/release.sh
git commit -m "Handle x86_64 and aarch64 Linux artifacts in release script"
```

---

### Task 5: Review all changes

**Step 1:** Read final `.github/workflows/ci.yml` end-to-end and verify:
- No remaining `matrix.os` references
- No remaining `runner.os` references in artifact names or cache keys
- The two `runner.os == 'Windows'` checks on the exe extension are preserved
- ARM64 entry has no bench/doc/test flags

**Step 2:** Read final `.github/workflows/release.sh` and verify:
- Hardcoded `Linux-x86_64` in sdist/schema extraction
- Case handles `x86_64`, `aarch64`, `macOS`, `Windows`, `wasm`, `vscode`, `bench`
- No remaining `Linux)` case
