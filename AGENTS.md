## Project Description

BaySREn is an R package and method-benchmarking repository for testing
systematic-review automation methods. The repository is currently organized as
a method-testing workbench rather than a final user-directed package: official
benchmarks live in tracked method pipelines, while exploratory and failed
iterations live in ignored interim experiment space.

## Agent Operative System

This repository is governed by the canonical coordination files:
`AGENTS.md`, `PLAN.md`, `ARCHITECTURE.md`, `BACKLOG.md`, and
`.agents/memory/`. Use them as the repo operating system for coordination,
planning, execution, and durable memory.

### Canonical Coordination Files

- `AGENTS.md` is the repo-wide operating manual. It defines stable,
   horizontal working rules for agents and contributors. It must not be used
   for live task status, implementation backlog detail, or method-specific
   target design.
- `PLAN.md` describes the intended end state, roadmap, and major project
   contracts. Update it only when the target state changes.
- `ARCHITECTURE.md` describes implemented reality only. Update it when landed
   code, data layout, cache layout, or tooling actually changes.
- `BACKLOG.md` is the canonical execution tracker. Work to do is listed
   there, with stable task IDs and explicit status markers.
- `.agents/memory/` stores durable decisions, constraints, risks, questions,
   learning points, solved issues, errors, and resolutions that should survive
   across sessions. It must not become a diary or duplicate the plan or
   backlog.

The gap between `PLAN.md` and `ARCHITECTURE.md` is expected. `BACKLOG.md`
should make that gap actionable and show progress as it closes.

### Required Read Order

- Read `AGENTS.md`.
- Read `PLAN.md`.
- Read `ARCHITECTURE.md`.
- Read `BACKLOG.md`.
- Read `.agents/memory/MEMORY.json` if present.
- Read only the child memory files needed for the task.
- Read the relevant source files and roxygen documentation before using,
   modifying, or discussing a function.

### Execution Loop

- Pick the next unblocked backlog item unless the user redirects the work.
- Assess the live repo state before changing nontrivial code.
- Test assumptions in the console with focused checks or small repros.
- Present a detailed plan before any nontrivial code change and wait for user
   approval.
- Implement the approved change.
- Add or update focused tests when code behavior changes.
- Run relevant tests after the edit.
- Update `BACKLOG.md` when task status or newly discovered work changes.
- Update `ARCHITECTURE.md` if implemented reality changed.
- Update `.agents/memory/` with `$project-memory` rules if durable learning
    was produced.
- Update `PLAN.md` only if the target state or roadmap changed.

Exception: the planning and wait-for-approval process is not needed when the
user asks only to add documentation.

### When In Doubt, Ask First

Do not blindly interpret and enact changes if:

- Requirements or desiderata are not clear.
- There are two or more drastically different approaches to solve an issue.
- Evidence found during inspection would require bold or significant choices.
- Something else appears wrong or unexpectedly broken while working.

Always ask to confirm the course of action when in doubt.

## Backlog Discipline

- Use stable task IDs.
- Use the default status markers:
  - `[ ]` pending.
  - `[-]` in progress.
  - `[x]` done.
  - `[!]` blocked.
  - `[a]` aborted/deprecated.
- Keep tasks atomic and finishable in one focused implementation effort.
- Record explicit dependencies when they matter for sequencing or parallel
   work.
- Keep decision gates visible as explicit backlog items.
- Reference architecture sections and memory IDs where useful.
- Ask user permission before adding newly discovered work to the backlog
   unless the user explicitly asks you to update it.

## Memory Usage

Use `$project-memory` for initialization and maintenance of `.agents/memory/`.

- Read `.agents/memory/MEMORY.json` first.
- Use the root `branches` list and `tag_index` to decide which child memory
   to open.
- Traverse selectively; do not scan the full memory tree by default.
- Use the traversal helper described in the project-memory skill when a tag
   or keyword lookup is needed.
- When tests fail, runtime behavior regresses, or a repeated issue appears,
   inspect relevant memory before assuming the problem is new.
- Store only durable learnings: decisions, constraints, preferences, risks,
   questions, test outcomes, errors, and resolutions.
- Do not store routine progress notes, raw logs, or copies of `PLAN.md`,
   `ARCHITECTURE.md`, or `BACKLOG.md`.
- Keep traceability by linking backlog task IDs and source files in memory
   records when useful.

## Structured Communication

- When answering the user in chat, present headings, plans, questions, and
   lists with ordered numbering or lettering so replies can be referenced
   precisely.
- Use nested numbering or lettered sub-points in chat when hierarchy is needed.
- Apply the chat numbering rule to plans of action, questions, options,
   step-by-step instructions, and review findings.
- Do not force ordered numbering into generated or edited Markdown files.
   Markdown files should use the structure that best fits the artifact, such
   as prose, headings, unordered lists, ordered lists, tables, or task lists.

## Skills And Guidelines

- Use the `$r-guidelines` skill
   whenever writing or reviewing R code, tests, targets pipelines, helpers,
   package structure, or style.
- Use `$project-memory` when initializing or maintaining `.agents/memory/`.
- Use the `$commit` skill
   whenever the user asks to stage, draft, organize, or apply commits.
- Use the verify-review and PR-description guidance when the user asks for
   review application or pull-request text.
- Do not read unrelated guidelines when they are not relevant to the current
   task.

## Git Rules

- Preserve user changes; never revert or discard work you did not create
   unless the user explicitly asks for that operation.
- Always ask for explicit user confirmation before any write operation to the
   git repository, including commit, push, merge, tag, branch rewrite, or other
   history-changing operation.
- Do not stage files until the user approves a staging boundary.
- On commit requests, follow the `$commit` skill
   workflow: inspect the repository state, inspect the relevant diffs, check
   recent commit style, identify a relevant backlog or milestone ID when one
   is clearly applicable, propose the exact staging boundary and commit
   message, and wait for explicit confirmation before staging or committing.
- Use conventional-style commit messages with a short imperative title, a
   scoped type when appropriate, body bullets for the committed items, and a
   `Why:` line explaining the rationale.
- Mention milestone or backlog IDs in commit messages only when a relevant ID
   is clearly identifiable from the repo context. Do not invent IDs and do not
   mention unrelated milestones, uncommitted work, excluded artifacts, or future
   commits in the commit message.

## Method Benchmarking Structure

### Official And Interim Split

- `method_pipelines/` contains official, tracked method benchmarks.
- `experiments/` contains interim experiments, failed pathways, scratch
   target stores, exploratory scripts, and scratch logs. It is ignored and is
   not committed.
- Each official method pipeline has one method folder with:
   - `_targets.R`.
   - `diary.md`.
- Each `diary.md` is the single tracked prose file for the method track. It
   contains the track introduction, target/cache instructions, official
   benchmark results, interim findings, failed pathways that should not be
   retried blindly, interpretation, and next benchmark decisions.
- Do not add per-track `README.md` files; put track prose in `diary.md`.
- Diaries must not record paths to interim experiment stores or scratch
   outputs because those paths are not part of the git tree.

### Official Method Tracks

- `abstract_concentration`: positive abstract early concentration for ML
   training via embeddings.
- `criteria_refiner`: criteria update via an LLM refiner agent followed by
   repeated abstract concentration.
- `candidate_abstract_selection`: post-warmup candidate abstract selection
   using BART or other fast predictive methods, named by the benchmark
   question rather than posterior mechanics.
- `llm_classifier`: LLM classifier agent evaluation.
- `integrated_workflow`: all method tracks combined end to end.

### Cache Policy

- Top-level `cache/` is the official reusable cache root and is tracked when
   official cache files are produced by official benchmarks.
- `cache/llm/` stores official LLM cache families.
- `cache/embeddings/` stores official embedding cache families.
- `experiments/cache/` is the ignored interim overlay cache.
- Official pipelines read and write `cache/`.
- Interim experiments read through `cache/` first and then
   `experiments/cache/`.
- Interim experiments write cache misses only to `experiments/cache/`.
- Cache reuse must be keyed by stable cache hashes/specifications. A target
   pipeline may reuse a model cache only when the cache hash/spec matches.

### Targets Pipelines

- Each `method_pipelines/*/_targets.R` contains only:
   - package loading.
   - `tar_option_set()`.
   - method-local static configuration.
   - target declarations.
   - simple target-local code when it is run once and is easier to read
      inline.
- `_targets.R` files must not contain:
   - reusable helper definitions.
   - complex LLM/cache code.
   - duplicated method implementations.
   - target-store paths intended for git.
- If multiple approaches test the same problem, use one general target and
   switch the called method function through a parameter or grid row.
- Do not commit any `_targets/` store.
- Before any `targets` command, check the active store explicitly with
   `targets::tar_config_get("store")`.

### R Helper Boundaries

- Consolidate tracked and untracked experiment helpers in `R/`.
- Do not write reusable helpers inside `_targets.R` pipelines.
- Use explicit inline code in a target only when the operation is simple,
   target-local, and run once.
- Use helpers for repeated behavior or large/complex code, such as the LLM
   solver.
- Use dot-prefixed functions for internal helpers that are not called
   directly by target pipelines.
- Avoid atomic helpers for trivial one-off operations of a couple of lines.
- Every function, including helpers, must have roxygen documentation.
- Internal helpers must have roxygen documentation and `@keywords internal`.
- Every minimal logical code block should have a preceding comment unless it
   is totally self-evident.
- Documentation comments should explain why the block exists more than what
    it does, unless the operation is not self-explicit.
- Never hand-edit `.Rd` files or `NAMESPACE`; regenerate them with
    `devtools::document()`.
- Do not introduce or update `renv.lock` unless the user explicitly requests
    it.

## Project Structure

- `R/`: package functions and reusable method-benchmarking helpers.
- `method_pipelines/`: official, tracked benchmarking target pipelines.
- `cache/`: official, tracked reusable model caches produced by official
   pipelines.
- `experiments/`: ignored interim experiments and scratch state.
- `man/`: roxygen-generated documentation; do not edit by hand.
- `NAMESPACE`: roxygen-generated namespace; do not edit by hand.
- `README.Rmd`: source for `README.md`.
- `data-raw/`: scripts and inputs used to build packaged datasets.
- `data/`: packaged datasets.
- `tests/testthat/`: unit tests.

## Testing Workflow

- Use console repros or focused checks to test assumptions before nontrivial
   edits.
- Run tests after implementation, not before, unless explicitly requested.
- Run test files with `Rscript -e 'devtools::test(filter = "...")'`.
- Use `devtools::load_all(quiet = TRUE)` only for small console repros and
   package-load checks, not as a substitute for tests.
- After changing ignore rules, verify:
   - `git check-ignore -v experiments experiments/cache .cursor .playwright-mcp`.
   - `git check-ignore cache` returns no match.
- Inspect `git status --short --untracked-files=all` before proposing any
   staging list.

## Dependency Management

- `renv` is not used in this repository unless explicitly requested.
- Only update or introduce `renv.lock` when stable versions are reached and
   the user asks for it.
- Add packages with `usethis::use_package(..., min_version = TRUE)` when
   dependency metadata must change.

## Learning Points

Update this list when durable project-specific or general workflow lessons are
learned. File each memory into the correct group.

```yaml
current_project:
  - name: prompt line wrapping
    description: Do not wrap prompt lines in system or user prompts; keep line breaks intentional.
    scope: prompt formatting
  - name: parallel chat prompt types
    description: parallel_chat_promises expects prompts as character or list; coerce glue output with as.character() or build as plain character.
    scope: prompt formatting
  - name: verify local ellmer version
    description: Check the installed ellmer version before assuming features from upstream release notes; this repo was still loading ellmer 0.3.2 while investigating 0.4.x behavior.
    scope: llm transport
  - name: llm solver prompt normalization
    description: llm_solver should normalize bare character vectors to a list before handing prompts to ellmer; local ellmer 0.4 helper internals reject raw character vectors in some code paths.
    scope: llm transport
  - name: warmup stratified sample cap
    description: When building warmup refiner samples with per-bucket quotas, always enforce the global max_total cap after bucket selection so stratification cannot silently exceed the requested prompt budget.
    scope: assisted screening warmup
  - name: seed ranking distance direction
    description: Multi-seed embedding aggregation must respect that rank_by_embeddings defaults to cosine_distance, so lower embedding_score is better and aggregate rankings must sort ascending, not descending.
    scope: assisted screening ranking
  - name: dynamic ranking needs review order
    description: Any ranking strategy based on the last reviewed positives or negatives must persist an explicit review_order in branch state; batch membership alone is not enough to reconstruct the anchor history.
    scope: exploratory screening ranking
  - name: vella data-raw source path
    description: The maintained Vella source files live under data-raw/Vella, not data-raw/SIIAM/Vella; rebuilding the packaged dataset must use the current path and apply the adjudication workbook there.
    scope: vella dataset maintenance
general:
  - name: cli alert level conventions
    description: Use cli_alert for action logs, cli_alert_info for supplemental details, cli_warn for runtime logical issues that would have used warning(), and cli_alert_warning for non-code cautions when results need careful interpretation (e.g., low-quality input, incomplete data).
    scope: logging/messages
  - name: cli alert capture
    description: cli_alert* emits messages that expect_message() can capture in tests; expect_warning() will not.
    scope: testing
  - name: git write confirmation
    description: Always ask for explicit user confirmation before performing any write operation to the git repository, such as commit, push, or other actions that modify git history.
    scope: git operations
  - name: roxygen generation only
    description: Never write .Rd or NAMESPACE manually; always run devtools::document() to update documentation and exports.
    scope: documentation
  - name: dependency additions via usethis
    description: Always add packages with usethis::use_package using min_version = TRUE when updating dependencies.
    scope: dependencies
  - name: dplyr select tidy eval
    description: Avoid using .data pronouns inside select(); use explicit column strings with any_of/all_of instead.
    scope: style
  - name: dplyr select dynamic names
    description: When renaming/ selecting with dynamic column names in select(), build expressions with rlang::list2() and splice with !!! so := stays inside dynamic dots.
    scope: style
  - name: code block commentary
    description: Add a short, accurate comment above each logical block when modifying or adding code, so intent is explicit. A logical block is a very small set of instructions (even one line) that enact an action and it's not clearly self-explanatory. Put docu comments always above, never on the same line as the code.
    scope: style
  - name: cli alert bullet handling
    description: cli_alert* functions do not support named bullet vectors; emit one cli_alert* call per bullet message instead of passing a named vector.
    scope: logging/messages
  - name: devtools test filter usage
    description: Run tests with Rscript -e 'devtools::test(filter = "...")' (no testthat::test_file); use devtools::load_all() only for small console repros, not test runs.
    scope: testing
  - name: propose next target
    description: Before enhancing a function/target, propose which one to edit and wait for confirmation before proceeding.
    scope: workflow
  - name: read function sources
    description: When using or discussing a function, always read and internalize its source code and roxygen documentation.
    scope: workflow
  - name: test assumptions
    description: Always test assumptions in the console before proposing or implementing changes.
    scope: workflow
  - name: rscript dollar expansion
    description: When running `Rscript -e` in the shell, escape `$` or use single quotes otherwise shell expansion will transform e.g. `Rscript -e "a <- list(); a$b <- 3"` into `a <- list(); a <- 3`.
    scope: shell
```

## Tested Assumptions

Record every time an assumption was tested and found to be wrong, to avoid
repeating the same errors or beliefs.

```yaml
current_project:
  - assumption: "Per-bucket sampling quotas in the warmup refiner sampler would automatically respect the global max_total cap."
    test: "Ran `select_warmup_refiner_samples()` on a six-record reviewed set with `max_total = 5` and default quotas."
    outcome: "The helper returned 6 records because the initial bucket-wise bind exceeded the requested cap before any top-up logic ran."
    correction: "After bucket selection, sort deterministically and truncate to `max_total` before any top-up step."
  - assumption: "The aggregated seed ranking should order larger embedding scores first."
    test: "Ran isolated non-target ranking checks on Gastaldi and Vella and compared current aggregate_seed_rankings output with manually corrected distance-ordered aggregation."
    outcome: "The current helper inverted the ranking because rank_by_embeddings defaults to cosine_distance, where smaller scores are better."
    correction: "Sort aggregated embedding_score ascending, then use mean rank and id as tie-breakers."
  - assumption: "The ellmer 0.4 parallel error-handling behavior from the upstream news page was already available in the local project environment."
    test: "Ran `Rscript -e 'packageVersion(\"ellmer\")'` and inspected `parallel_chat_structured()` locally."
    outcome: "The installed package version was `0.3.2`, so the local API and failure semantics lagged the investigated 0.4.x docs."
    correction: "Verify the installed ellmer version first and build adapters against the local API unless the dependency is intentionally upgraded."
general:
  - assumption: "rlang::enexpr(vec) would preserve the caller expression for error messages."
    test: "Tried `vec <- iris$Species; vec_expr <- rlang::enexpr(vec); cli::cli_abort(\"Unknown or uninitialised column: `{rlang::as_label(vec_expr)}`.\")`."
    outcome: "Error printed `<fct>` instead of the caller expression."
    correction: "Use `rlang::enquo(vec)` and `rlang::as_label(vec_quo)` to capture the caller expression."
  - assumption: "`rlang::is_string()` implicitly checks for names."
    test: "Tried `rlang::is_string(c(\"A\" = \"red\"))`."
    outcome: "Result was `TRUE`. `is_string()` only checks character type and length 1."
    correction: "Always pair `is_string(x)` with `!rlang::is_named(x)` if you need to distinguish between a simple string and a named mapping."
  - assumption: "`drop = FALSE` is always needed for safety in base R subsetting."
    test: "Checked data frame row subsetting `df[rows, ]` with 1 vs 2+ columns."
    outcome: "Only 1-column data frames drop to a vector. 2+ column data frames remain data frames even without `drop = FALSE`."
    correction: "`drop = FALSE` remains good defensive practice, but is strictly redundant if the object has at least 2 columns."
  - assumption: "`.data$col` interpolates inside `glue::glue()` calls in dplyr verbs."
    test: "Ran a small `summarise()` with `glue::glue(\"{.data$model}\")` and observed the output."
    outcome: "Interpolation produced `<environment>` placeholders instead of column values."
    correction: "Use `glue::glue_data(dplyr::pick(...), ...)` or `paste0()` inside verbs."
  - assumption: "`dbarts::bart2()` would train safely with single-class labels."
    test: "Ran `dbarts::bart2()` on synthetic data where `y` contained only zeros."
    outcome: "Model initialization failed with `sigma estimate must be greater than 0`."
    correction: "Guard assisted-loop model fitting and only train BART when reviewed labels contain both classes."
  - assumption: "`labels %in% TRUE` would preserve matrix shape for row-wise vote counts."
    test: "Computed `rowSums(labels %in% TRUE)` inside `combine_llm_labels()` tests."
    outcome: "`%in%` flattened the matrix, causing `rowSums()` to fail (`x must be an array`)."
    correction: "Use `rowSums(labels, na.rm = TRUE)` directly on the logical matrix."
```
