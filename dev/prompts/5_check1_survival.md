# Stage session — Check 1: Cohort survival analysis     (paste into Claude Code)

Read `CLAUDE.md`, `sim_contract.md` (v0.4.0), and this stage's instruction
file — `internal_docs/steps/5_check1_survival.md` — before writing anything.
The instruction file is binding: its Sample, slots, figure list, and
guardrails define this stage; do not reinterpret them from memory of other
stages.

**Scope for this session:** first pass — the instruction file's Done-when
(shell), plus two decisions made 2026-09-20: **D1** (which cohort) and **D2**
(the Data fields used box), below. Nothing from "Later passes".

Rules:
- Build only the scope. Do not build ahead into the instruction file's
  "Later passes", and do not touch other stages' prefixes. You may **call**
  exported functions from `R/utils.R` and earlier stages; you may not edit
  them — if the scope genuinely needs a shared helper changed, stop and say
  which and why.
- Every `compute_*()` built gets `stopifnot()` validation, roxygen, and the
  testthat triple (happy path, structure, small-n suppression: `NA`, not
  dropped).
- Render with `render_step("check1")` to docx and html; both must be clean.
- Run-order numbers exist on prompt and instruction files only: `.qmd`
  names, `R/` prefixes, and `render_step("check1")` ids carry no numbers.
- `devtools::check()`: 0 errors, 0 warnings.
- Update CLAUDE.md §3 (Current package state) for this stage.
- End with: what was built, what remains in "Later passes", and any
  `[PROPOSED — CONFIRM]` calls made. Stop.

## Commit 1, before any code: record the decisions

D1 and D2 are decided — do not reopen them. Record them, and the four places
this session departs from CLAUDE.md as written, as their own commit (§14), so
every departure is verifiable in the repo:

- `internal_docs/steps/5_check1_survival.md` — **Sample** gets D1's
  selection rule; **Output slots** defines `$cohorts` per D1; **Static text**
  gets D2's placement; **Done-when** adds "the Sample names the selected
  cohort; the Data fields used box renders directly after it".
- Same file, **Data needs** — one correction only: `ENTRY_GRADE` and
  `COHORT_LABEL` are derived by `build_cohort()` from `GRADE` / `YEAR` /
  `EL_STATUS` and never read from the delivery (`sim_contract.md` §3.5 marks
  both D-tier). Leave the rest of the line, including `INITIAL_ELP`, as is.
- CLAUDE.md §6, Check 1 block — one sentence each for D1 and D2, including
  that `compute_cohorts()` is exported so Check 2 takes the same cohort.
- CLAUDE.md, the four departures, one line each:
  1. **§15 run order** — Check 1 is built before Prep 1–3 by decision
     (2026-09-20). It depends only on the foundation (§15: Checks 1–2 consume
     its shared flags). Do not build or stub the Prep stages.
  2. **§6 render order** — qualify the opening "Every `.qmd` renders, in
     order" sentence: Check 1 inserts its Data fields used box directly after
     the Sample callout.
  3. **§12 #4 computed inline values** — resolved for Check 1's Sample line,
     which carries the selected cohort, its N, and its window (D1).
  4. **§14 shared derivations** — cohort *membership* stays in `R/utils.R`
     (`build_cohort()`, called unchanged). The *selection* lives in the
     `check1_` prefix because it is Check 1's sample decision; Check 2
     follows it by calling the export, which §14 allows.

## D1 — One cohort: the most complete

Check 1 analyzes a single entry cohort.

- **Candidates** are the usable cohorts `build_cohort(roster, spec,
  start_grade)` returns with its default `entry_years` — call it, never
  re-derive (panel range + `max_years_to_exit`; `LEFT_TRUNCATED` excluded).
- **Most complete = longest observed follow-up**: `FOLLOWUP_YEARS` = the
  panel's last year − entry year + 1. Follow-up shrinks with every later
  entry year, so in practice this is **the earliest usable cohort**. It
  equals the number of life-table rows, so it is the cohort with the most of
  its trajectory observed rather than censored at the window's edge.
- **Only a cohort with `N_ENTRY` ≥ `min_cell` can be selected.** If no
  candidate qualifies — for example, a panel too short for any cohort to
  reach `max_years_to_exit` + 1 years — every analysis slot is `NULL` and
  the document renders a note saying why; `$meta` always renders, and
  `$cohorts` renders whenever there are candidates. Never a crash.
- `compute_cohorts(data, spec, start_grade = "K")` → `$cohorts`, one row per
  candidate: `COHORT_LABEL`, `COHORT_START_YEAR`, `N_ENTRY`,
  `FOLLOWUP_YEARS`, `SELECTED`. The table answers only which cohort and why.
  It has no outcome columns: outcomes are the life table's job, for the
  selected cohort. Suppress with `apply_suppression(n_col = "N_ENTRY",
  cols = character(0), blank_n = TRUE)` — the default `cols` would blank
  `COHORT_START_YEAR` and `FOLLOWUP_YEARS`. A suppressed candidate stays as
  a row, and selection reads its `SUPPRESSED` flag, so it is never chosen.
- **Membership:** the students are `build_cohort()`'s default output
  filtered to the selected `COHORT_LABEL`. The selected cohort drives every
  slot.
- **Export `compute_cohorts()`.** Check 2's Sample is "the same cohort as
  Check 1", so Check 2 calls it instead of selecting again.
- `$meta$sample` and the Sample callout name the selected cohort, its N, and
  its window, built from `$cohorts` — never a literal. On the sim it should
  read like: *"The K-2015 entry cohort: 2,373 students who entered EL status
  in kindergarten in 2015, followed through 2025 (11 years). The most
  complete of the five usable K cohorts — the longest follow-up."*
- `$cohorts` renders beside the **Checkpoint** as its evidence: is the
  chosen cohort large and stable relative to the others?

## D2 — "Data fields used" box

Directly after the Sample callout, before Looking at: a boxed callout titled
**Data fields used**. The Sample callout stays first.

- **Generated, never hand-written.** One internal declaration in
  `R/check1_*.R`, a row per field: `FIELD`, `TABLE`, `KIND` (`required` /
  `optional` / `derived`), `SLOTS` (the results slots that consume it),
  `USED_FOR` (one plain-language line), `FALLBACK` (optional fields: what
  happens when absent). The compute functions validate their inputs from
  this same declaration, so the box and the code cannot disagree. Declare
  only the fields this pass's slots consume; each later pass adds its own
  rows.
- **`$meta$data_fields`**: the declaration rows whose `SLOTS` were computed
  in this run (a slot that degrades to `NULL` drops its rows, so the box
  grows as later passes land), plus `YOUR_COLUMN` resolved through
  `spec$data$columns` (the canonical name when unmapped) and `PRESENT` for
  optional fields.
- **Render:** delivered fields as a flextable — Field · Your column · Used
  for — with Your column shown only when the crosswalk renames a listed
  field. Below it, one line naming the derived fields and stating that they
  are recomputed from `EL_STATUS` transitions, so a delivered column of the
  same name is not used. An absent optional field shows its fallback.
- **Shell content, for reference:** required `STUDENT_ID`, `YEAR`, `GRADE`,
  `EL_STATUS`; optional `LEFT_TRUNCATED` (fallback: entrants in the panel's
  first year are treated as left-truncated — `build_cohort()`'s rule);
  derived `COHORT_LABEL`, `YEAR_IN_STATUS`, exit year, censor type, and the
  life table's A–F.
- Build the flextable in `R/` (a `check1_` helper), not in the `.qmd`. This
  syntax renders in Word as a bordered box with a shaded title bar and the
  flextable inside (verified: Quarto 1.8.24, flextable 0.9.4 — confirm on
  your render). `appearance="simple"` gives only a left rule, not a box.

  ```
  ::: {.callout-note appearance="default" icon=false}
  ## Data fields used
  <chunk printing the flextable>
  <derived-fields line>
  :::
  ```

- Check 1 only, this session. Extending the box to every stage, with its
  helper moved to `R/utils.R`, is `[PROPOSED — CONFIRM]` for the summary.

## Session notes

- **flextable.** Check 1 is the first stage to render a table: add
  `flextable` to `Imports` and `renv::snapshot()`. No other new packages.
- **Sim facts to check against** (from the shipped data): usable K cohorts
  K-2015…K-2019, `N_ENTRY` 2,360–2,411, `FOLLOWUP_YEARS` 11 down to 7;
  selection K-2015, N = 2,373. No K-cohort student disappears in the sim, so
  C is 0 until the last row — a generator property; do not "fix" it.

## Tests — `test-check1.R`

Un-skip and implement the four shell stubs (slide-52 life table, last row,
no mean time in status, cohorts from the panel), and add:

- **Selection follows the panel, never literals:** drop the panel's first
  year → selection moves to the next entry year; drop the `LEFT_TRUNCATED`
  column → the first-year cohort is excluded and selection moves the same
  way. Express both relative to the panel — no years in the test.
- Selection is the qualifying candidate with maximum `FOLLOWUP_YEARS`; a
  candidate below `min_cell` is never selected; no qualifying candidate →
  `NULL` slots and the note, no error.
- `compute_cohorts()` triple: happy path, structure, small-n suppression.
- **Data fields:** removing any `required` field makes `run_check1()` error
  naming it — remove it from the *built* bundle, since `as_el_data()`
  rejects such a roster before `run_check1()` sees it; `$meta$data_fields`
  equals the declaration filtered to computed slots; an absent optional
  field is reported with its fallback; `YOUR_COLUMN` resolves through the NJ
  spec's crosswalk (`STUDENT_ID` → `Scrambled_SID`).

Every other stub stays `skip()`ped. In the summary's `[PROPOSED — CONFIRM]`
list, include at minimum: the box for every stage; and a cohort override for
a later pass — without one, a selected cohort that fails the Checkpoint
cannot be swapped, and any override must reach Check 2 too.
