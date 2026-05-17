# BaySREn Architecture

## Project Overview

- BaySREn currently exists as an R package with active exploratory code for
   systematic-review automation.
- The repository now has a documented target architecture for official method
   benchmarking, but much of the method implementation is still in transition
   from interim experiments.
- Implemented reality is expected to lag `PLAN.md`; `BACKLOG.md` tracks the
   work needed to close that gap.

## Current Repository Shape

### Package Code

- `R/` contains package functions and reusable helpers.
- Existing package areas include record search, record management, analysis,
   reporting, ranking, LLM classification, parallel LLM transport, refinement
   helpers, and assisted-screening helpers.
- New method-benchmarking helpers should use the `R/method_*.R` naming
   convention.

### Planned Official Benchmarks

- `method_pipelines/` has been scaffolded as the intended tracked root for
   official method benchmarks.
- Each method track has its own `_targets.R` and `diary.md`.
- The files are currently scaffolds; method implementation still needs to move
   from interim experiments into shared helpers.

### Caches

- Top-level `cache/` is the official reusable cache root.
- The current commit contains the cache policy, but no official cache files.
- Interim overlay cache behavior is defined in `AGENTS.md` and implemented by
   the method cache helper work.

## Implemented Functionality

### Existing Package Functionality

- Citation and record handling functions exist in the current package code.
- Embedding ranking and assisted-screening experiments exist in current R
   helpers and interim target scripts.
- LLM transport and caching helpers exist in current parallel LLM code, with
   additional shared cache/solver contracts planned for method benchmarks.

## Dependency And Tooling Shape

### Runtime

- The project is an R package.

### Targets

- Official method pipelines use `targets`.
- Target stores are explicit runtime state and are never committed.
- Method pipeline stores live as `_targets/` inside each method track folder
   and are ignored by git before running mutating target operations.

### Testing

- Tests live under `tests/testthat/`.

## Active Technical Decisions

- Official method names are:
   - `abstract_concentration`.
   - `criteria_refiner`.
   - `candidate_abstract_selection`.
   - `llm_classifier`.
   - `integrated_workflow`.
- Shared method helpers use the `R/method_*.R` naming convention.

## Current Milestone Status

- `M1` repository operating backbone is in progress.
- `M2` official benchmark scaffold is in progress.
- `M3` shared cache, model-spec, and LLM-solver contracts are partially
   implemented.
- `M4` method implementation migration is pending.
- `M5` official benchmark runs are pending.
