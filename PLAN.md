# BaySREn Method Benchmarking Plan

## Project Identity

- BaySREn is an R package and benchmarking workbench for systematic-review
   automation methods.
- The repository is being restructured as a method-testing repository, not as
   a polished user-directed package.
- The core objective is to benchmark how well different method components
   reduce abstract-review workload while preserving early discovery of
   positives.

## Target Outcome

- The repo should expose reproducible official method benchmarks under
   `method_pipelines/`.
- Official and interim code should share reusable helpers in `R/`.
- Official benchmark diaries should preserve results and failed pathways,
   including lessons from uncommitted interim experiments, without recording
   scratch paths that will not exist in git.
- Official pipelines should share model caches when cache specifications and
   hashes match.

## Scope Boundaries

### In Scope

- Five official benchmarking tracks covering abstract concentration, criteria
   refinement, candidate abstract selection, LLM classification, and the
   integrated workflow.
- Reusable method helpers in `R/method_*.R`.
- Official cache contracts for LLM and embedding artifacts.
- Focused tests for helper contracts and pipeline behavior.

### Out Of Scope

- Treating `experiments/` as committed source.
- Committing interim target stores, scratch logs, or model caches under
   `experiments/`.
- Shipping the repo as a final user-facing package interface before the
   method-benchmarking contracts stabilize.
- Replacing all existing package APIs during the first restructure pass.

## Target Architecture

### Cache Design

- Cache identity is stable and includes provider, model, system prompt,
   schema or output type, solver parameters, input hash, and cache schema
   version.
- Pipeline cache reuse is allowed only when the cache hash/specification
   matches.

### Targets Design

- `_targets.R` files stay orchestration-only.
- Reusable logic lives in `R/`.
- Multiple approaches to the same benchmark question should use one general
   target and switch behavior through parameters or grid rows.

## Method Tracks

### Abstract Concentration

- Benchmark positive abstract early concentration for ML training via
   embeddings.
- Compare embedding models and seed-scoring variants.
- Track top-k positive density, rank-to-positive metrics, and practical
   warmup implications.

### Criteria Refiner

- Use an LLM refiner agent to update screening criteria from warmup evidence.
- Repeat abstract concentration after criteria updates.
- Record criteria update decisions in structured benchmark outputs.

### Candidate Abstract Selection

- Benchmark how to choose the next abstracts for review after warmup.
- Include `ranking_only`, `bart_only`, `hybrid`, and
   `ranking_with_fallback` policies.
- Allow BART and other fast predictive methods internally while keeping the
   method track named by the benchmark question.

### LLM Classifier

- Evaluate LLM classifier agent prompts, schemas, parsing, and metrics against
   benchmark labels.
- Use the shared solver and cache policy.

### Integrated Workflow

- Combine abstract concentration, criteria refinement, candidate abstract
   selection, and LLM classification.
- Compute end-to-end workload, recall, precision, and early-yield metrics.

## Roadmap

### Milestone M1: Repository Operating Backbone

- Merge project-init coordination files with BaySREn rules.
- Establish ignore and commit boundaries.
- Initialize project memory.

### Milestone M2: Official Benchmark Scaffold

- Create official method folders.
- Add diaries and orchestration-only targets skeletons.
- Establish the official cache root contract without placeholder files.

### Milestone M3: Shared Helper Contracts

- Add method cache resolver contracts.
- Add BaySREn-native model-spec and LLM-solver interfaces.
- Consolidate method helper ownership in `R/`.

### Milestone M4: Method Implementation Migration

- Promote reusable logic from interim experiments into `R/method_*.R`.
- Keep failed interim pathways documented in method diaries.
- Add method-specific tests.

### Milestone M5: Official Benchmark Runs

- Run official pipelines with explicit, non-committed target stores.
- Track official cache artifacts and method diary results.
- Summarize integrated workflow performance.

## Open Questions

- Which existing packaged datasets are official benchmark-ready after the
   final provenance review?
- Which official cache files should be committed after the first complete
   official benchmark run?
- Which solver features should remain in the shared interface and which
   should be simplified after the first cache-contract scaffold lands?
