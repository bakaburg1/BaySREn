# BaySREn Method Benchmarking Backlog

## Legend

- `[ ]` pending.
- `[-]` in progress.
- `[x]` done.
- `[!]` blocked.
- `[a]` aborted/deprecated.

## Current Next Unblocked Task

- `M1-03` is the remaining repository-operating-backbone task.

## Milestones

### M1: Repository Operating Backbone

Goal: establish the repo coordination files, memory layer, and commit/ignore
policy for official method benchmarking.

- [x] `M1-01` Merge the project-init operating backbone into `AGENTS.md`.
   Depends on: none. Arch: `3.2`, `5`.
- [x] `M1-02` Create `PLAN.md`, `ARCHITECTURE.md`, and `BACKLOG.md`.
   Depends on: `M1-01`. Arch: `3.2`.
- [-] `M1-03` Initialize `.agents/memory/` with durable repository decisions.
   Depends on: `M1-02`. Arch: `3.2`.
- [x] `M1-04` Update ignore rules for official cache, interim experiments,
   `.cursor/`, `.playwright-mcp/`, and target stores. Depends on: `M1-01`.
   Arch: `2.3`, `2.4`, `5`.

### M2: Official Benchmark Scaffold

Goal: create the tracked method-pipeline structure and documentation shells.

- [x] `M2-01` Create `method_pipelines/` folders for all five method tracks.
   Depends on: `M1-01`. Arch: `2.2`, `5`.
- [x] `M2-02` Add one `diary.md` and one orchestration-only `_targets.R` per
   method track. Depends on: `M2-01`. Arch: `2.2`.
- [a] `M2-03` Add official cache directory placeholders under `cache/`.
   Deprecated: empty cache placeholders were removed because official cache
   files should be tracked only when produced by official benchmarks. Depends
   on: `M1-04`. Arch: `2.4`.

### M3: Shared Helper Contracts

Goal: add reusable R helper contracts that future method pipelines can use
without embedding implementation logic inside `_targets.R`.

- [x] `M3-01` Add `R/method_cache.R` and tests for official/interim cache
   resolution. Depends on: `M1-04`, `M2-03`. Arch: `2.4`, `5.3`.
- [x] `M3-02` Add `R/method_model_spec.R` for BaySREn-native model-spec
   handling. Depends on: `M3-01`. Arch: `3.1`.
- [x] `M3-03` Add `R/method_llm_solver.R` for BaySREn-native solver/cache
   orchestration. Depends on: `M3-01`, `M3-02`. Arch: `3.1`.
- [ ] `M3-04` Add skeleton helper files for each method track with documented
   function contracts. Depends on: `M2-02`. Arch: `2.1`, `5.6`.

### M4: Method Implementation Migration

Goal: promote durable logic from interim experiments into documented,
tested method helpers.

- [ ] `M4-01` Migrate abstract concentration helpers and tests. Depends on:
   `M3-04`. Arch: `2.1`.
- [ ] `M4-02` Migrate criteria refiner helpers and tests. Depends on:
   `M3-03`, `M3-04`. Arch: `2.1`.
- [ ] `M4-03` Migrate candidate abstract selection helpers and tests. Depends
   on: `M3-04`. Arch: `2.1`.
- [ ] `M4-04` Migrate LLM classifier helpers and tests. Depends on:
   `M3-03`, `M3-04`. Arch: `2.1`.
- [ ] `M4-05` Add integrated workflow helpers and tests. Depends on:
   `M4-01`, `M4-02`, `M4-03`, `M4-04`. Arch: `2.1`.

### M5: Official Benchmark Runs

Goal: run official method benchmarks and record durable results in tracked
diaries.

- [ ] `M5-01` Run the abstract concentration official benchmark. Depends on:
   `M4-01`. Arch: `2.2`, `2.4`.
- [ ] `M5-02` Run the criteria refiner official benchmark. Depends on:
   `M4-02`, `M5-01`. Arch: `2.2`, `2.4`.
- [ ] `M5-03` Run the candidate abstract selection official benchmark.
   Depends on: `M4-03`, `M5-01`. Arch: `2.2`, `2.4`.
- [ ] `M5-04` Run the LLM classifier official benchmark. Depends on:
   `M4-04`. Arch: `2.2`, `2.4`.
- [ ] `M5-05` Run the integrated workflow official benchmark. Depends on:
   `M5-01`, `M5-02`, `M5-03`, `M5-04`. Arch: `2.2`, `2.4`.

## Decision Gates

- [ ] `D1` Decide which currently packaged benchmark datasets are official
   after the final provenance pass. Depends on: `M3-04`.
- [ ] `D2` Decide which official cache artifacts should be committed after
   the first successful official benchmark run. Depends on: `M5-01`.
- [ ] `D3` Decide which solver features should remain in the shared
   interface or be simplified after the BaySREn cache contract is tested.
   Depends on:
   `M3-01`.

## Completion Traceability

1. Add commit hashes, PR references, or memory IDs here only after those
   durable artifacts exist.
