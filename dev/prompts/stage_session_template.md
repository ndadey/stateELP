# Stage session — <STAGE>            (copy, fill <>, paste into Claude Code)

Read `CLAUDE.md`, `sim_contract.md` (v0.2.0), and this stage's instruction
file — the numbered file in `internal_docs/steps/` ending `_<STAGE>.md`
(numbers give run order) — before writing anything. The instruction
file is binding: its Sample, slots, figure list, and guardrails define this
stage; do not reinterpret them from memory of other stages.

**Scope for this session:** <ONE LINE — e.g. "first pass: build the stage to
the instruction file's Done-when (shell)" or "build $exit_curve +
plot_exit_curve() only" or "build Model 2 per instruction file §Later
passes">

Rules:
- Build only the scope. Do not build ahead into the instruction file's
  "Later passes", and do not touch other stages' prefixes. You may **call**
  exported functions from `R/utils.R` and earlier stages; you may not edit
  them — if the scope genuinely needs a shared helper changed, stop and say
  which and why.
- Every `compute_*()` built gets `stopifnot()` validation, roxygen, and the
  testthat triple (happy path, structure, small-n suppression: `NA`, not
  dropped).
- Render with `render_step("<STAGE>")` to docx and html; both must be clean.
- Run-order numbers exist on prompt and instruction files only: `.qmd`
  names, `R/` prefixes, and `render_step("<STAGE>")` ids carry no numbers.
- `devtools::check()`: 0 errors, 0 warnings.
- Update CLAUDE.md §3 (Current package state) for this stage.
- End with: what was built, what remains in "Later passes", and any
  `[PROPOSED — CONFIRM]` calls made. Stop.
