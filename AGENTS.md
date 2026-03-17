## General instructions

For any requested change or task which is not trivial, you should always assess the situation, test your assumptions in the console (e.g., small repros or focused checks), make tests, and then present a detailed plan of action before ANY change to the code. Run relevant unit tests after the edit (not before), unless explicitly requested otherwise.
You'll enact your plan of action after the plan has been approved by the user.

**Exception:** The planning and wait for approval process is not needed when asked to add documentation.

**⚠️ IMPORTANT: When In Doubt, Ask First**

Do not blindly interpret and enact changes if:

- The requirements or desiderata are not clear.
- There are two or more drastically different approaches to solve an issue.
- Some evidence you found would lead you to take bold or significant choices.
- You noticed something else that seems to be wrong or not working as expected while trying to perform a requested task.

**Always ask to confirm the course of action before proceeding.** When in doubt, ask.

### Structured communication and referencing

When presenting headings, plans, questions, or lists, always use ordered numbering or lettering (e.g., 1, 2, 3; a, b, c; I, II, III) and nest them hierarchically when necessary. This allows easy reference when the user replies or comments on specific points.

Examples:

- Use numbered lists for main points: 1. First point, 2. Second point, 3. Third point
- Use lettered sub-points when needed: a. Sub-point, b. Another sub-point
- Use nested numbering for hierarchical structures: 1. Main item, 1.1. Sub-item, 1.2. Another sub-item
- Apply this to all structured content: plans of action, questions, lists of options, step-by-step instructions, etc.

### Agent skills and guidelines

Read and follow these guidelines when appropriate:

- Writing R code, to read whenever R code is written: <https://raw.githubusercontent.com/bakaburg1/my-ai-skills/main/R/R-rules.md>
- Writing R unit tests, to read whenever R unit tests are written: <https://raw.githubusercontent.com/bakaburg1/my-ai-skills/main/R/unit-testing.md>
- Writing and performing git commits: <https://raw.githubusercontent.com/bakaburg1/llm-commands/refs/heads/main/commit.md?token=GHSAT0AAAAAADQTRX5XEH6LVZ6U66CKTCAS2K2JA7A>
- Assess and apply code review recommendations: <https://raw.githubusercontent.com/bakaburg1/llm-commands/refs/heads/main/verify.md?token=GHSAT0AAAAAADQTRX5X6GHRJ2U3Z6462RKS2K2JDZA>
- Write git Pull Request descriptions: <https://raw.githubusercontent.com/bakaburg1/llm-commands/refs/heads/main/pr.md?token=GHSAT0AAAAAADQTRX5XREGUYOQKWLGPGTWI2K2JEPQ>

Do not read these guidelines if not relevant to the task at hand.

### Track learning points

Update progressively this document when you learn something about how to better perform your tasks related to this project. This could be coding best practices, implementation details, overall design decisions, etc. and expecially, corrections and remarks from the user.

Update the list below with the new learning points, filing each memory into the correct group:

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

### Tested assumptions

Record every time an assumption was tested and found to be wrong, to avoid
repeating the same errors or beliefs. If you are unsure which group applies,
ask the user before recording the memory.

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

### Dependency Management (`renv`)

`renv` is not used in this repository unless explicitly requested. Only update
or introduce `renv.lock` when stable versions are reached and the user asks for
it.

## Project structure

This repository contains the BaySREn R package for automating parts of
systematic reviews (citation collection, screening, and query generation).

Key locations:

- `R/`: package functions.
- `man/`: roxygen-generated documentation (do not edit by hand).
- `NAMESPACE`: generated by roxygen.
- `README.Rmd`: source for `README.md` (regenerate after edits).
- `data-raw/`: scripts and inputs used to build any packaged datasets.
- `data/`: packaged datasets (if present).
- `tests/testthat/`: unit tests.
- `experiments/`: prototypes and research scripts.

## Targets / experiments

When running experiment target pipelines, update the targets config in your R
session before calling `tar_make()`, for example:

```r
targets::tar_config_set(
  script = here::here("experiments/targets_scripts/_target_llm_screening.R"),
  store = here::here("experiments/targets_scripts/_target_llm_screening"),
  use_crew = TRUE
)
```
