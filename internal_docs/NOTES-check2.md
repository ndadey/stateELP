# NOTES -- Check 2 (on-time vs. prolonged exiters)

First pass, scope: the instruction file's Done-when (shell). Governing file:
`internal_docs/steps/6_check2_flows.md`.

## Built

- `inst/templates/check2_flows.qmd` -- Sample callout, authored Looking at /
  Looking for / Interpreting text, the crosstab table, the slide-67 reminder and
  slide-81 caution, "Still to come", the Type-gated Additional check, and the
  Decide box. Renders to docx and html on the SIM spec.
- `R/check2_compute.R` -- `compute_exit_flags()` (`$flags`) and
  `compute_crosstab()` (`$crosstab`, the Total block).
- `R/check2_run.R` -- `run_check2()`; `R/check2_render.R` -- `ft_check2_crosstab()`.
- The real element: the crosstab's **Total students** and **% of all EL
  students** rows across All / Exit_Ever / Exit_Max / Exit_OnTime / No_Exit /
  Disappeared.

## Not built (slots are `NULL`, tests `skip()`ped)

`$crosstab` row blocks (language, district size band, sex / SES / SPED, service
type, initial ELP band), `$academic_summary`, `$trajectories`,
`$blocked_groups` (Types 2-6).

## How Check 2 relates to what already exists

- Cohort: `run_check2()` takes the selected row of Check 1's exported
  `compute_cohorts()`. It does not select again, so the two documents cannot
  disagree about the sample.
- Exit dating, nested indicators, on-time / prolonged split: `R/utils.R`'s
  `build_student_spans()` and `add_exit_flags()`, called unchanged.
- Nothing from `check1_*` is used except the exported `compute_cohorts()`.

## `[PROPOSED - CONFIRM]`

1. **A zero count is shown as zero, not suppressed.** `apply_suppression()`
   blanks `N < min_cell`, which includes 0. The shipped sim has no
   disappearances in its K cohort, so without this the Disappeared column would
   render as suppressed when it is simply empty. `compute_crosstab()` restores
   zero cells after calling `apply_suppression()`; the shared function is not
   edited.
2. **A suppressed cell blanks the count as well as the percent**
   (`blank_n = TRUE`), the precedent set by `compute_cohorts()` and
   `compute_exit_curve()`. No complementary suppression is done: a suppressed
   column can in principle be recovered by subtraction from All and the other
   columns. Matters once row blocks land.
3. **No "Prolonged" column.** The instruction file's six columns are the
   deck's; Prolonged (late exiters + No_Exit) is defined in the text but has no
   column of its own. Prolonged = Exit_Ever - Exit_OnTime + No_Exit, and
   `$flags$PROLONGED` carries it per student. Adding the column is small
   (`PROLONGED` into `.CHECK2_FLAG_COLUMNS`, plus a header label in
   `check2_render.R`); confirm before doing it.
4. **"Mobility"** is named in the Process Overview's Looking for but is not in
   the deck's row list, and the roster has no school-mobility field
   (`MIGRANT_STATUS`, `IMMIGRANT_STATUS` and `NEWCOMER_FLAG` are nearby, none is
   the same thing). The row-block passes need a decision on what stands in for it.
5. **`$flags` is the student-level indicator frame** (no demographics). The
   instruction file lists the slot without saying what it holds.
6. **The Sample line carries `[DERIVED - VERIFY]`** on the on-time timeline
   whenever `policy$timelines$source` is `"provisional"` -- as it is for SIM,
   whose 5-year on-time value is a choice, not a source.
7. **The Additional-check text separates "does not apply" (Type 1) from
   "applies, not built yet" (Types 2-6)**, through `$meta$type_gate_applies`.

## Something a later pass will hit

`add_exit_flags()` reads a by-initial-band on-time rule from the roster's
**delivered** `INITIAL_ELP` column (`R/utils.R`, `.on_time_years()`), whereas
Check 1 derives the initial band from the annual score and treats a delivered
`INITIAL_ELP` as a validation input only. A state whose `on_time_rule` is keyed
by initial band would therefore error in Check 2 if the delivery lacks the
column, and would silently use the delivered band if it has one -- possibly
different from the band Check 1 draws. SIM's `by_initial_band` is `NULL`, so
nothing here exercises it. It is a `R/utils.R` matter, so this session did not
touch it.

## Sim limits

- The K cohort has no disappeared students, so the Disappeared column is
  all zeros on the sim. The non-trivial cases (a small nonzero column that
  suppresses; a real Disappeared column) are exercised by
  `make_check2_roster()`, whose exit outcomes are stated, not simulated.
- Per CLAUDE.md 4, no group difference on sim data is a finding.
