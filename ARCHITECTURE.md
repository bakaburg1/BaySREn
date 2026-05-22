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
- `cache/embeddings/` stores reusable dataset-level embedding artifacts in a
   role-first layout:
   `cache/embeddings/<text_role>/<dataset>/<cache_model_slug>__<document|query>_embeddings.rds`.
   Each file stores one row per unique normalized text hash. Older sharded or
   model-first cache trees are migration inputs only, not the current layout.
- `cache/llm/` stores reusable LLM artifacts by task content, model, and
   criteria hash. Synthetic seed-abstract replies live under
   `cache/llm/seed_abstracts/<dataset>/` so other method tracks can reuse them.
- Final stabilized abstract-concentration outputs are written to
   `method_pipelines/abstract_concentration/outputs/`.
- Interim overlay cache behavior is defined in `AGENTS.md` and implemented by
   the method cache helper work.

## Implemented Functionality

### Existing Package Functionality

- Citation and record handling functions exist in the current package code.
- Embedding ranking and assisted-screening experiments exist in current R
   helpers and interim target scripts.
- LLM transport and caching helpers exist in current parallel LLM code, with
   additional shared cache/solver contracts planned for method benchmarks.
- Abstract-concentration helper logic is implemented in
   `R/method_abstract_concentration.R`, covering track input preparation,
   scoring, metric calculation, mixed-model fitting, marginal summaries, and
   final output writing. Shared packaged-dataset discovery/loading helpers are
   named `get_datasets()` and `get_dataset()`.

### Abstract Concentration

- `R/method_embedding_cache.R` provides the reusable cache-first embedding
   layer keyed by normalized text content and a supported-model registry with
   explicit cache slugs. Supported labels are `cohere/embed-v4.0`,
   `google/gemini-embedding-001`, `google/gemini-embedding-2-preview`,
   `perplexity/pplx-embed-v1-0.6b`, and `perplexity/pplx-embed-v1-4b`.
   The public interface uses shared `embedding_mode = document/query` values,
   mapped to native Cohere `input_type`, native Gemini `taskType`, and
   OpenRouter for Perplexity. Text roles are `abstracts`, `seed_abstracts`,
   `criteria_items`, and `criteria_blocks`. Cache files are written
   atomically, read with full mode/role validation, and regenerated with
   row-scoped `force`.
- `R/method_abstract_concentration.R` owns track-specific input preparation,
   seed-bank generation, scoring orchestration, metric calculation,
   mixed-model fitting, marginal summaries, and final output writing.
- `method_pipelines/abstract_concentration/_targets.R` is the official target
   graph for the abstract-concentration benchmark.
- The graph discovers all packaged datasets with matching criteria artifacts,
   defines the method grid in the target script, branches across embedding
   profiles with `crew`, keeps dataset and method work sequential within each
   branch, and writes final outputs to
   `method_pipelines/abstract_concentration/outputs/`.
- The current embedding profiles compare Cohere query/document vs
   document/document, Gemini 001 and Gemini 2-preview query/document vs
   document/document through the native Gemini API, and Perplexity `0.6b` vs
   `4b` through OpenRouter.
- The graph compares synthetic positive seeds, contrastive variants, criteria
   item/block variants, positive-seed-negative-criteria variants, and the
   weighted centroid ensemble.

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
- `M4` method implementation migration has started with abstract
   concentration.
- `M5` official benchmark runs are pending user confirmation after the
   abstract-concentration target graph is validated.
