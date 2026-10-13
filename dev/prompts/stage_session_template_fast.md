# Stage session, FAST MODE — <STAGE>   (copy, fill <>, paste into Claude Code)

Deadline variant of `stage_session_template.md`. Identical except the
verification loop: no `devtools::check()`, Word only. The canonical template is
unchanged; the full archived check runs once, after the last stage lands.

Read `CLAUDE.md`, `sim_contract.md` (v0.4.0), and this stage's instruction
file — the numbered file in `internal_docs/steps/` whose name contains
`_<STAGE>_` (e.g. `6_check2_flows.md` for `check2`) — before writing anything.
The instruction file is binding: its Sample, slots, figure list, and guardrails
define this stage; do not reinterpret them from memory of other stages.

**Scope for this session:** <ONE LINE — e.g. "first pass: build the stage to
the instruction file's Done-when (shell)">

Rules:
- Build only the scope. Do not build ahead into the instruction file's
  "Later passes", and do not touch other stages' prefixes. You may **call**
  exported functions from `R/utils.R` and earlier stages; you may not edit
  them — if the scope genuinely needs a shared helper changed, stop and say
  which and why.
- Every `compute_*()` built gets `stopifnot()` validation, roxygen, and the
  testthat triple (happy path, structure, small-n suppression: `NA`, not
  dropped).
- Run-order numbers exist on prompt and instruction files only: `.qmd`
  names, `R/` prefixes, and `render_step("<STAGE>")` ids carry no numbers.
- **Verification loop — this overrides the `devtools::check()` requirement
  in `CLAUDE.md` §11 and the canonical template, for this session only.**
  Do **not** run `devtools::check()`. Instead:
  1. `devtools::document()` then `devtools::load_all()`
  2. while iterating: `devtools::test(filter = "<STAGE>")`
  3. once, before you finish: the full `devtools::test()` — 0 failures, and
     no test that passed before this session may fail
  4. render Word only: `render_step("<STAGE>", formats = "docx")` — must be
     clean. Skip HTML.
- Update CLAUDE.md §3 (Current package state) for this stage, and record in
  its Notes that `devtools::check()` and the HTML render were not run.
- End with: what was built, what remains in "Later passes", whether the full
  `devtools::test()` passed, and any `[PROPOSED — CONFIRM]` calls made. Stop.
