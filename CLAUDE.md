# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`fints2ledger` is a Haskell CLI tool that downloads bank transactions from FinTS banking APIs (used by German/European banks) and converts them into [hledger](http://hledger.org/) journal files. A Python script (`data/pyfints.py`) handles the actual FinTS communication; Haskell handles everything else.

## Build & Test Commands

```bash
cabal build                                        # build
cabal test                                         # run all tests
cabal test --test-options="--golden-reset"         # update snapshot (golden) tests after intentional changes
cabal run fints2ledger -- --demo                   # run with demo transactions
cabal run fints2ledger -- --config                 # launch config TUI
cabal install                                      # install to bin
```

The project also supports Nix: `nix build .#` (uses devenv/direnv for the dev shell).

## Architecture

The entry point is `app/Main.hs` → `src/Lib.hs` (`runFints2Ledger`). The app uses a **Reader monad** (`App` type in `src/App.hs`) for dependency injection — all side-effectful operations (file I/O, prompting, sleeping) are injected via the `Env` record so tests can mock them.

**Data flow:**
1. Parse `CliConfig` (CLI args) + `EnvConfig` (env vars) + `YamlConfig` (~/.config/fints2ledger/config.yml) → merged `AppConfig`
2. Fetch transactions from FinTS (via Python subprocess), CSV file, or demo data
3. For each transaction: check dedup (MD5), run auto-fill rules (`src/Matching/`), prompt user for remaining fields, append rendered ledger entry to journal

**Key modules:**
- `src/Lib.hs` — orchestration
- `src/App.hs` — `App` monad and `Env` dependency injection type
- `src/Transactions.hs` — transaction types, FinTS/CSV/demo loading
- `src/Prompt.hs` — core logic: transaction → ledger entry (dedup, matching, prompting, rendering)
- `src/Config/` — `CliConfig`, `EnvConfig`, `YamlConfig`, `AppConfig` (merged), `Files`, `StartupChecks`
- `src/Matching/` — regex and amount-comparison matching for auto-fill rules
- `src/Dates/Parser.hs` — natural language date parsing ("90 days ago", "last monday")
- `src/UI/` — `brick`-based TUI for editing config
- `src/Ledger.hs` — hledger journal reading and account name extraction (for tab completion)

## Dependency Injection Pattern

Functions with return type `App a` have access to all injected `Env` dependencies. Use `ask (.fieldName)` to retrieve a dependency, then `liftIO` to run it inside `App`:

```haskell
someFunction = do
  sleep <- ask (.sleep)
  liftIO sleep
```

Only use IO operations from `Env` (not direct `IO`) so tests can substitute mocks.

## String Types

Multiple string types are in use due to library requirements:
- `String` — default Haskell string
- Strict `Text` — used by most libraries; convert with `pack`/`unpack`
- Lazy `Text` — required by one template library
- `ByteString` — used by the JSON (aeson) library

## Testing

Tests live in `test/` and use `sydtest` with QuickCheck for property tests and golden files in `test/files/` for snapshot tests. Each test module mirrors a `src/` module (e.g. `PromptSpec.hs` tests `Prompt.hs`). The `Env` record makes it straightforward to inject mock dependencies in tests.
