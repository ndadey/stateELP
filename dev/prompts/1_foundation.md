# Session 1 — foundation (no stages)

Read `CLAUDE.md` in full, then `sim_contract.md` (v0.2.0) in full, then
`internal_docs/source/process_overview.md` in full, before writing anything.
This session builds the shared infrastructure every stage session depends on
— and **no `.qmd` files, no stage code**. Stages are built afterward, one
session each. If a task below conflicts with CLAUDE.md, stop and say so.

Repo state at start: `CLAUDE.md`, `sim_contract.md`,
`data-raw/el_long_sim.R`, `internal_docs/source/` (Process Overview, Report
Views, deck extractions, original `NJ_elp_2023.R`),
`internal_docs/steps/` (eight stage instruction files, numbered `2_`–`9_` in run order), `dev/prompts/`.

## Order of work

1. **Skeleton.** `DESCRIPTION` (Package: stateELP; Imports individual
   packages only, never tidyverse; R 4.3.3), generated `NAMESPACE`,
   `R/globals.R`, `.Rbuildignore`/`.gitignore` in the ReportR pattern
   (`.Rbuildignore` additionally covers `^\.claude$` — the repo may carry a
   committed `.claude/settings.json` with permission deny rules; leave that
   file untouched), `renv::init()`. Commit.

2. **Spec layer.** `R/spec.R`: `el_spec()` constructor + validator per
   CLAUDE.md §5 — `elp` block (`components` with
   `kind`/`measures`/`from`/`native_name`, `headline`, `level_scale`,
   `bands`, `grade_clusters`, `cuts = NULL`, `alternate = NULL`); `policy`
   block with the fixed field list (`exit` with `terms` + `types` +
   pathways; `timelines` with `exit_dating` and `initial_elp_source`;
   `growth`; `accountability_indicator`; `suppression`;
   `academic_benchmark`; `district_size_bands`; `program`;
   `error_tolerance`); `academic`; `data$columns`; `verification`;
   `source_documents`. Validator: policy fields under `policy` and nowhere
   else; every policy value carries a `source`; every exit-rule term
   references an existing component and one of its measures (validate,
   never evaluate); `types` consistent with the shape of `terms`/`pathways`
   (Type 6 with a single overall term is an error); bands well-formed. Three
   specs under `inst/specs/`: `00_SIM_EL_Spec.R` (Type 1,
   `error_tolerance = "unknown"`, `[DERIVED — VERIFY]` values,
   `verification$status = "synthetic"`); `NJ_elp_2023.R` rewritten to the new
   shape per the CLAUDE.md spec diff table, values unverified;
   `00_ELPA21_EL_Spec.R` (Type 6: `headline = "determination"`, a level-only
   component, a domain-level exit rule with `exempt_if_absent`). Separate
   demographics/lookup file. `test-spec.R` passes. Commit.

3. **Data.** `Rscript data-raw/el_long_sim.R`. Every guardrail must print
   PASS; on any FAIL, stop and report — never relax a threshold. Fill
   `sim_contract.md`'s `[FILL IN after regeneration]` placeholders from the
   run output; confirm version 0.2.0. Write `R/data.R` against the
   contract's §3.5/§4.2/§5.1 tables, with the column-agreement test. Commit.

4. **Bundle and adapters.** `R/el_data.R`: `as_el_data(el_long_sim, roster = el_roster_sim, academic = ...)` (key
   uniqueness; academic `GRADE` required and equal to the roster's;
   base-table `ELP_FORM` equal to roster `ELP_FORM`; the year × grade coverage
   table), the **`elp_headline()` accessor** — base table filtered to
   `spec$elp$headline` on the annual form, joined to roster keys, banded per
   spec, returning `STUDENT_ID`, `YEAR`, `ACHIEVEMENT_LEVEL`, `SCALE_SCORE`,
   `ELP_BAND`; the only route any stage takes to the headline — and
   `as_academic_long()`. Fixtures: hand-reshaped SGP-long
   (`CONTENT_AREA = "READING"` → `overall` via crosswalk, `GRADE "0"` →
   `"K"`, labeled levels, `VALID_CASE` honored, warn on integer-only
   levels); small stateSummative-shaped academic fixture; the
   ELPA21-headline fixture (a base + roster pair, a few rows) as a shared test helper.
   `test-el_data.R`: identical bundles across accepted forms;
   join-direction row-count guard; `COMPONENT` ≠ `SUBJECT`. Commit.

5. **Shared derivations.** In `R/utils.R`: suppression (`NA`, never dropped
   rows; `min_cell` from `policy$suppression` only), banding from
   `spec$elp$bands`, weighting helpers, and the status-transition
   derivations both cohort checks consume — `YEAR_IN_STATUS`, `censored`,
   exit dating (**first former year**, absorbing), cohort keys (excluding
   `LEFT_TRUNCATED`), the nested exit flags, on-time/prolonged. Include the
   sim-vs-package exit-year `+1` test and the last-current-dating
   counter-test here, driven by the A–J tibble from deck slides 43–51 (50%
   at year 7 / grade 6; 40% within 6 years). Commit.

6. **Dev loop.** `dev/render_step.R` + exported `render_step("<stage>")`:
   renders one stage `.qmd` (once stages exist) against the sim + SIM spec
   to docx and html in an output directory (copy in, render, RDS beside,
   remove copy) and opens the result. Smoke-test it against a trivial
   placeholder `.qmd` that is then deleted. Commit.

7. **Finish.** `devtools::check()`: 0 errors, 0 warnings. Update CLAUDE.md
   §3. List any `[PROPOSED — CONFIRM]` calls made. Stop — no stage work.

## Hard constraints

- No component name as a string literal outside spec files.
- No mean/average time-in-status anywhere.
- If `data-raw/el_long_sim.R` fails or is missing, stop and say so. Never
  re-simulate.
