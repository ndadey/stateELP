# CLAUDE.md — `stateELP`

R package. Part of the **stateR** family (`stateSpecs`, `stateSummative`,
`stateAccountability`, `stateScoring`, `stateELP`). Read this file, then the
stage's instruction file, before writing anything. If a rule here would be
violated by the change you are about to make, stop and say so.

---

## 1. Purpose

`stateELP` is the analytic engine for the **"Process Overview: Evaluating and
Setting English Learner Exit Thresholds"** (v1.8, `internal_docs/source/
process_overview.md`) — the north-star question is still *"It's 2026 — do you
know how your exit criteria are doing?"*, and the process now runs all the way
to an answer: three Prep Steps (context) → four Data Checks (analysis) → one
Decision stage. The organizing principle survives from the Report Views
rebuild and is the reversal of the failed `ELReportR` design: **the
deliverable is the set of `.qmd` files, one per stage, and every function in
`R/` exists to make one of them render well.** No analytic modules, no
audience report templates, no ingest pipeline. Every stage renders end-to-end
against the shipped simulated data from day one; analyses are then developed
close-in, one `compute_*`/`plot_*` pair at a time.

**Source precedence.** The Process Overview governs structure: the stage list,
each stage's **Sample**, its Looking at / Looking for / Interpreting / Decide
blocks, its Checkpoints, and the Type-gated Additional checks. The deck
("Faulkner-Bond EL Collab Spring 2026 — Data Views (REVISED)") remains the
template authority for the figures and tables it specifies (district charts,
life table, crosstab, slide-73 box plot). The Report Views doc supplies
supplementary walkthrough prose where the Process Overview is briefer. Where
they conflict: Process Overview > deck templates > doc prose. All three live
as extractions in `internal_docs/source/`. Walkthrough text is **authored**
from these sources; auto-generated narrative is not in the shell
`[PROPOSED — CONFIRM]`.

The document is written for two audiences — the analyst running this package
and the policy reader who greenlights the process. Audience-differentiated
rendering is a later addition; the shell renders one document per stage.

## 2. Stages, files, results

Nine stages, eight `.qmd` files (Prep 1 and 3 are spec/inventory-driven; the
Decision stage synthesizes prior results). Prep 3, skipped in the old plan, is
now in scope: it carries the error-tolerance question that Data Check 4 and
the Decision stage consume.

| Stage | Process Overview section | `.qmd` (`inst/templates/`) | `R/` prefix | Test file | Results object |
|---|---|---|---|---|---|
| Prep 1 | Policy documentation | `prep1_policies.qmd` | `prep1_` | `test-prep1.R` | `prep1_results` |
| Prep 2 | Population analysis | `prep2_population.qmd` | `prep2_` | `test-prep2.R` | `prep2_results` |
| Prep 3 | Program implementation | `prep3_program.qmd` | `prep3_` | `test-prep3.R` | `prep3_results` |
| Check 1 | Cohort survival analysis | `check1_survival.qmd` | `check1_` | `test-check1.R` | `check1_results` |
| Check 2 | On-time vs. prolonged exiters | `check2_flows.qmd` | `check2_` | `test-check2.R` | `check2_results` |
| Check 3 | Academic patterns: box plots + logistic | `check3_academics.qmd` | `check3_` | `test-check3.R` | `check3_results` |
| Check 4 | Decision consistency + dependence | `check4_consistency.qmd` | `check4_` | `test-check4.R` | `check4_results` |
| Decision | Decision & next steps | `decision_synthesis.qmd` | `decision_` | `test-decision.R` | `decision_results` |

**Every stage `.qmd` opens with its Sample statement** (Process Overview v1.5:
the analytic sample is called out on its own line, never buried in prose), and
every results object carries `$meta$sample` with the same text. The sample
shifts across stages — full statewide population (Prep 2), a single entry
cohort (Checks 1–2), cross-sectional all-ELs-plus-comparison-groups
(Checks 3–4) — and misstating it is the canonical way to misread a stage.

Shared infrastructure: `R/spec.R` (constructor + validator), `R/data.R`,
`R/el_data.R` (`as_el_data()`, `as_academic_long()`, accessors, coverage
table), `R/utils.R` (suppression, banding, weighting), `R/globals.R`. Flat
`R/`, no subdirectories; one session edits one prefix; no session edits
`NAMESPACE` or `man/`. Build notes in `internal_docs/NOTES-<stage>.md`, never
non-`.R` files in `R/`.

## 3. Current package state

Maintain this table. It starts as the shell inventory.

| Piece | Status | Notes |
|---|---|---|
| Package skeleton, `DESCRIPTION`, `renv` | **built** | `renv::init()`, explicit snapshot; `NAMESPACE`/`man` generated |
| `data-raw/el_long_sim.R` + `sim_contract.md` | **written, RUN** | contract **v0.4.0**; every `[FILL IN]` filled from the 2026-09-16 build; values verified identical to the 0.2.0 build after renaming (§2.1) |
| `data/` (three `.rda`) | **built** | `el_long_sim` (base) / `el_roster_sim` / `academic_long_sim`, 6.84 MB, `sim_contract_version` 0.4.0; `elp_components_sim` retired; regenerate, don't hand-edit |
| `R/spec.R` + three specs | **built** | SIM (Type 1) / TEMPLATE / ELPA21 (Type 6); ALL-CAPS `data$columns` crosswalks + `value_maps`. No `demographics_spec`: display labels moved out of the spec tree to `inst/lookups/Display_Labels.R` and no spec points at them |
| `R/el_data.R` (bundle, adapter, coverage) | **built** | `as_el_data()` (separate objects, merged single file), `as_el_data_sgp()`, `as_academic_long()`, `elp_headline()`, coverage table |
| `R/utils.R` (shared derivations) | **built** | suppression, banding, weighting, `YEAR_IN_STATUS`, `LT_EL`, spans / exit dating, exit flags, cohorts — all ALL CAPS |
| `R/render_step.R` + `dev/render_step.R` | **built** | smoke-tested to docx and html on the v0.4.0 bundle; see the Dropbox note below |
| `internal_docs/steps/` (8 files) + `dev/prompts/` | **written** | numbered `1_`–`9_` in run order; binding per-stage contracts; refine as stages evolve |
| `R/check1_*.R` + `check1_survival.qmd` | **complete** | Every slot the instruction file names is filled: `compute_cohorts()` (D1 selection, exported for Check 2), `compute_life_table()` (slide-52 A–F), `compute_exit_curve()` (overall and by initial ELP band), `compute_survival_est()` + `compute_years_to_50()` (with a Greenwood interval on the estimate, cloglog scale; F never banded), `compute_at_max_timeline()`, **`compute_model1()`** (the ELP-only person-period hazard model — Model 1's fitted half, the baseline Model 2 is nested in) and `compute_model2()` (the same through one shared `.check1_fit_hazard()`, lagged achievement added, quasi-separation flagged), `compute_grade_weights()` (experimental, labeled, used nowhere), the Type 3 gate, `plot_exit_curve()` (optional banded estimate on the whole-cohort curve), `ft_check1_*()`, `run_check1()`. No `skip()`ped tests remain in `test-check1.R`. Decisions beyond the instruction file are in `internal_docs/NOTES-check1.md` `[PROPOSED — CONFIRM]`. Not built, and recorded as such in the document: per-grade selection strength, later start grades |
| `R/prep2_*.R` + `prep2_population.qmd` | **full port built** | ported from the standalone draft (`stateData/MT/prep2_population_V2.qmd`), not just the shell's Done-when: every figure and table in the instruction file's list, plus entry dating, time in status and the Type-gated crosswalk; `compute_*()` / `plot_*()` / `ft_prep2_*()` / `run_prep2()`; renders docx + html. Slots and calls beyond the instruction file are in `internal_docs/NOTES-prep2.md` `[PROPOSED — CONFIRM]` |
| `R/check2_*.R` + `check2_flows.qmd` | **complete** | Every slot the instruction file names is filled: `compute_exit_flags()` (the nested indicators on Check 1's cohort via the exported `compute_cohorts()`, plus the three-way `EXIT_GROUP`), `compute_cohort_profile()` (characteristics at entry; district size and initial ELP band derived, never delivered), `compute_crosstab()` (the slide-63/64 table: Total block plus five characteristic blocks, `BLOCK_TYPE` distinguishing a partition from independent indicators and `N_BASE` carrying every denominator), `compute_academic_summary()` (proficiency at the same `YEAR_IN_STATUS`), `compute_trajectories()` (`$levels` + `$composition`, the slide-81 mechanism as data), `compute_blocked_groups()` (Types 2–6, via Prep 2's `compute_criteria_crosswalk()`), `plot_academic_summary()`, `plot_trajectories()`, `ft_check2_*()`, `run_check2()`; renders docx + html. No `skip()`ped tests remain in `test-check2.R`. **First code in the package to read `inst/lookups/Display_Labels.R`** (render-side only; slots stay in the delivery's own codes). Decisions beyond the instruction file are in `internal_docs/NOTES-check2.md` `[PROPOSED — CONFIRM]`. Not built, and recorded as such in the document: later start grades, starting grade as crosstab rows, the logistic regression for correlated characteristics |
| `R/check3_*.R` + `check3_academics.qmd` | **complete** | instruction file's Done-when: `compute_panel()` (subject / grade / year all DERIVED from the spec, never chosen), `compute_box_data()` (slide-73 box statistics, not student rows), `plot_box_panel()`, `run_check3()`; `$coverage` filled from the existing `el_coverage()`. Later passes all built: `compute_summary_table()` (slide-71, from the box data so figure and table cannot disagree), `compute_logistic()` (P(proficient) on the headline scale score, fitted on current ELs, 50% crossing reported, `CROSSING_IN_RANGE` flagging an extrapolation), `compute_landing()` (first band reaching half + the crossing, with `EXIT_BAND` read from the rule and `LANDING_AT_OR_ABOVE_EXIT` flagged), both fairness checks, `plot_landing()`, `ft_check3_*()`. Every declared grade x subject is its own panel; slots are long and keyed, never pooled. Every named slot is built, the Type-gated `compute_contested_models()` included: it derives per-student criterion outcomes in `check3_` because Prep 2 computes but does not expose them, with a test asserting the derivation reproduces Prep 2's own `blocked_groups` exactly. Delta-method intervals on the logistic crossing. No `skip()`ped tests remain. Renders docx + html. The band-axis hazard -- the top bands hold students the exit rule is about to remove -- renders as a warning callout; see `internal_docs/NOTES-check3.md` `[PROPOSED — CONFIRM]` |
| Eight `.qmd` shells | **4 of 8 built** | `check1_survival.qmd`, `prep2_population.qmd`, `check2_flows.qmd`, `check3_academics.qmd`; one stage session each for the rest (§15) |
| Fixtures: A–J, slide-52, J-vs-consistency | **A–J + slide-52 built** | A–J + last-current counter-test in `test-utils.R`; slide-52 worked numbers and the cohort rosters in `helper-check1.R`; ELPA21 base + roster pair and SGP / summative fixtures in `helper-el_data.R`; J-vs-consistency lands with Check 4 |
| Stage analyses (all others) | not built | later passes per instruction file |
| Git remote / publication | **done 2026-10-09** | public at `github.com/ndadey/stateELP`, 34 commits — see below |

**Publication state: published 2026-10-09** to
**https://github.com/ndadey/stateELP**, public, default branch `main`,
`origin` tracking. Verified at the time: local HEAD equals `origin/main`, and
`internal_docs/source/` appears in **none** of the 34 published commits
(checked commit by commit, not just at the tip).

How it was done, because the shape matters if it is ever repeated:

1. **Public**, not private (§9 says so).
2. **The name was taken.** `github.com/ndadey/stateELP` was the origin of
   `Git/ELPAnalysis` — the ELReportR-era design this package replaced, 12
   commits, unrelated history. It was **renamed to `ELReportR-archive`**
   (still private, its history intact), and `ELPAnalysis`'s own `origin` was
   **repointed at the archive before the new repo was created**: GitHub's
   rename redirect dies the moment a new repo takes the old name, so that
   directory would otherwise have silently pointed at this package.
3. **`internal_docs/source/` was rewritten out of history**, not deleted —
   a public repo hands over its history. It lived in exactly one commit
   (`3230733`, the skeleton), which carried much else, so
   `git filter-branch --index-filter` dropped it without emptying any commit.
   It is now in `.gitignore`.

**Two corrections to the earlier plan, learned by doing it:**

- **`--index-filter` does NOT leave the working tree alone.** `filter-branch`
  syncs the working tree to the rewritten HEAD when it finishes, which
  **deleted all four `internal_docs/source/` files from disk.** They were
  restored from the `.git` backup and verified byte-identical. **Back up
  `.git` before any `filter-branch` here, and expect to restore the filtered
  paths afterwards.**
- **`filter-branch` refuses to run on a dirty working tree** ("Cannot rewrite
  branches: You have unstaged changes"). Untracked files are fine; modified
  tracked files are not. Commit or set them aside first.

**Why `internal_docs/source/` is excluded.** It holds a verbatim extraction
of Molly Faulkner-Bond's NCIEA deck, a document marked "Draft v1 … being
revised per Molly's feedback", internal planning prose, and an NJ spec whose
values §5 records as unverified. None of that is ours to publish.
**Consequence to accept deliberately:** §1 and §14 name
`internal_docs/source/` as governing documents, and a fresh clone of the
public repo will not contain them. A session working from a clone has
CLAUDE.md, `sim_contract.md`, the stage instruction files and the NOTES — but
not the deck or Process Overview extractions. Ask for them rather than
inferring their content.

**Current, as of 2026-10-09 (HEAD `452c6e6`): the whole suite is 1,168
passing, 25 `skip()`ped, 0 failing, 0 warnings**, and `devtools::check()` on
a staging export is 0 errors / 0 warnings / 2 known NOTEs (the 6.5 MB `data/`
directory; "unable to verify current time", no network on this machine). The
25 skips are the four unbuilt stages and nothing else: `test-prep1.R` 7,
`test-prep3.R` 4, `test-check4.R` 7, `test-decision.R` 7. Every built stage
has zero.

Run `tools::checkDocFiles(dir = ".")` before `check()` — it takes seconds and
catches an undocumented argument, which is the only warning this package has
produced twice.

The paragraphs below are the running history, newest last. They record how
each figure was reached and what was verified at the time; read them for
provenance, not for the current number.

**Test count at the end of Check 1's first pass (2026-09-21):** 375
expectations passing, 54 `skip()`ped (the seven unbuilt stages, plus Check 1's
own later passes), 0 failures. `devtools::check()` on a clean export of HEAD:
**0 errors, 0 warnings**, 1 NOTE — the 6.5 MB `data/` directory (expected;
`sim_contract.md` §9). The `as_el_data()` example takes ~9 s on the full sim
(reported, not a NOTE). `flextable` joined `Imports` with Check 1, the first
stage to render a table; `renv::snapshot()` records it.

**Test count after Prep 2 (2026-09-21):** 698 expectations passing across the
suite (Prep 2's 280 skip nothing), 48 `skip()`ped, 0 failures.
`devtools::check()` on a staging copy of the working tree, with `document()`
run in the copy only: **0 errors, 0 warnings**, the same `data/` NOTE.
`ggplot2` joined `Imports` with Prep 2, the first stage to draw a figure;
`renv::snapshot()` records it.

**Test count after Check 1's curves pass and the component-naming rewording
(2026-09-21):** 706 expectations passing, 47 `skip()`ped (Check 1's remaining
five: survival column, Model 2 ×2, weights, Type 3), 0 failures. `devtools::check()` on a staging copy of the
working tree, `document()` run in the copy: **0 errors, 0 warnings**, the same
`data/` NOTE.

**Test count after Check 1 was completed (2026-09-21):** `test-check1.R` 186
expectations passing across 44 tests, **0 skipped** — the stage has no
placeholder left; the whole suite 842 passing, 38 `skip()`ped, 0 failing.
Rendered to **Word only**, and **`devtools::check()` was not run** for this
pass, per the loop below. **Both were run afterwards**, on HEAD at
`2ba4e0c` (Check 1 complete + Check 2's first pass): `devtools::check()` on a
clean export — 0 errors, 0 warnings, 2 NOTEs (the 6.5 MB `data/`; "unable to
verify current time", no network on this machine) — and the Check 1 HTML
render, clean. The debt that paragraph recorded is discharged.

**Verification for the rest of the 2026-09-21 session (directed):**
`devtools::check()` is **not run**, and stage documents are rendered to
**Word only — no HTML**. The loop after each change is
`devtools::document()` → `devtools::load_all()` →
`devtools::test(filter = "<stage>")`, with one full `devtools::test()` before
the session ends, and `render_step("<stage>", formats = "docx")`. This
overrides §11's "`devtools::check()` clean" condition and the stage
template's for this session only. **Work landed under this loop is
unchecked and has no HTML render** — say so in its commit, and run both
before the shell is called done. Everything committed earlier in the session
(through the component-naming commits) was verified with a clean
`devtools::check()` and rendered in both formats.

**Lifted, later the same day.** `check()` and the HTML render were run on
`2ba4e0c`, clearing the debt above, and the full loop — `document()` →
`load_all()` → `test()` → `render_step()` in both formats →
`devtools::check()` on a clean export — is back in force from the Model 1
work onward.

**Test count after Model 1 was corrected (2026-09-21):** `test-check1.R` 261
expectations across 53 tests, 0 skipped; the whole suite **989 passing, 35
`skip()`ped, 0 failing, 0 warnings**. Rendered to docx and html; `check()` on
a staging copy of the working tree. The jump from 968 is Check 1's new
guardrails: the same-year-covariate ban, the life-table identity, the
clustered-interval check.

**Test count after Check 2's first pass (2026-09-21):** `test-check2.R` 65
expectations passing, 3 `skip()`ped (row blocks, academic summary,
trajectories); the whole suite 767 passing, 44 `skip()`ped, 0 failing when run
in the working tree. `devtools::check()` on a staging copy of the working tree,
`document()` run in the tree: **0 errors, 0 warnings**, the `data/` NOTE and
"unable to verify current time" (no network on this machine). No new `Imports`.

**Test count after Check 2 was completed (2026-09-21):** `test-check2.R` 137
expectations passing across 29 tests, **0 skipped** — the stage has no
placeholder left; the whole suite 963 passing, 36 `skip()`ped (the five unbuilt
stages), 0 failing. Rendered to **docx and html**. `devtools::check()` on a
staging copy of HEAD plus only this stage's files: **0 errors, 0 warnings**, the
same two NOTEs. No new `Imports`.

**Test count after Check 3's shell (2026-09-21):** the whole suite 1,079
passing, 28 `skip()`ped, 0 failing. One box-plot panel as the stage's real
element; the later-pass slots `NULL` with `skip()`ped tests.

**Test count after Check 3 was completed (2026-09-22):** `test-check3.R` 167
expectations, **0 skipped**; the whole suite **1,168 passing, 25 skipped, 0
failing, 0 warnings**. Renders docx + html. The jump from 1,079 is Check 3's
later passes: the summary table, the logistic and its landing statistics,
both fairness checks, the per-grade loop over all 18 declared grade × subject
panels, delta-method intervals on the crossing, and the Type-gated
contested-zone models with the Prep 2 agreement test behind them.

**Concurrent-session note (2026-09-21).** Check 1's Model 1 pass and Check 2's
completion were built in two sessions running at the same time, which is worth
recording because two *generated* files are shared. `NAMESPACE` is regenerated
wholesale by `document()`, so each session's run picked up the other's
uncommitted exports; Check 2's commit carries a `NAMESPACE` generated from HEAD
plus its own files only, because committing the working copy would have exported
`compute_model1` and friends before the code defining them was committed. The
same applies to §3 of this file: Check 2 edited its own row in the working tree
and did not stage the file, so that row lands with the Check 1 session's commit.
A full-suite run during the overlap also failed twice in `test-check1.R` purely
because the other session was mid-edit; it passed on re-run. **Regenerate
`NAMESPACE` and re-read §3 before committing when another session is live.**

**Column naming.** ALL CAPS for every column of every data frame, delivered,
derived and summary alike — the rule and its rationale live in §4. Confirmed
2026-09-20.

**Three environment facts a later session will otherwise rediscover the hard
way** (first recorded 2026-09-15, still true):

1. **R 4.3.3 is not installed on this machine** (4.2.1 / 4.4.2 / 4.5.3 are).
   The shipped data is built on **4.4.2** (`sim_contract.md` §2.1 records the
   cross-version verification). Only R 4.4.2 has a working dev toolchain,
   through the renv project library; on a fresh clone run
   `renv::hydrate(c("devtools", "roxygen2"))` after `renv::restore()`. The
   generator's `tidyr` is not in the project library, so it runs as
   `Rscript --vanilla data-raw/el_long_sim.R` against the user library.
2. **`devtools::check()` cannot run from the repo root while any file in
   `internal_docs/working/` is open in another application.** `R CMD build`
   copies the whole package directory *before* applying `.Rbuildignore`, so a
   single locked file anywhere in the tree aborts the copy. Check a clean
   export of the tracked files instead —
   `git archive --format=tar HEAD | tar -x -C <staging>`, drop `.Rprofile`,
   `renv.lock` and `renv/` from the staging copy, then
   `devtools::check("<staging>")`.
3. **This repository is inside a Dropbox tree, and Quarto cannot clean up
   after itself there** (the sync client holds `<name>_files`, os error 32).
   `render_step()` therefore renders in `tempdir()` and moves the finished
   documents into `out_dir` — a deviation from §9's "render there"
   `[PROPOSED — CONFIRM]`; no `_files/` ever reaches the source tree.

## 4. Data contract — three long tables

**The authority on the shipped data is `sim_contract.md` (repo root), v0.4.0.**
Its §3.5 / §4.2 / §5.1 column tables are the codebook; `R/data.R` mirrors them
and a test asserts agreement. Bump the contract version and this citation
together. What follows is the working summary; read the contract for detail.

ELP instruments don't share a score structure (WIDA: 4 domains + 3 composites
+ overall, each scale + level; ELPA21: 4 domain scales with levels plus a
categorical determination that is a profile rule over domains). The package
handles this the way ReportR handled subjects: **the thing that varies is a
row key, not a set of columns** — the same atomic-layer-plus-resolver split as
stateSpecs' `tests` + `map`. **Column names conform to ReportR's
`student_results_long` conventions** (the NOTES file in the ReportR repo is
the authority): ALL CAPS, `STUDENT_ID × YEAR` keys, canonical names adopted
exactly where they exist, EL-specific names coined in the same idiom. Values
are unchanged; the contract's v0.4.0 note records the three documented
divergences (character `GRADE` carrying `"K"`; numeric/integer
`ACHIEVEMENT_LEVEL` with labels in the spec; lowercase `COMPONENT`/`SUBJECT`
values as spec keys) and the alias-collision decisions (`school_code` dropped;
`FRL_DETAIL`; the derived binary `EL` beside the three-level `EL_STATUS`).

**Every column of every data frame is ALL CAPS**, following the SGP
`LONG_DATA` convention the family shares — the three delivered tables, every
derived column (`YEAR_IN_STATUS`, `CENSORED`, `EXIT_EVER`, `ELP_BAND`, …) and
every summary column a `compute_*()` returns (`N`, `PCT`, `SUPPRESSED`,
`N_NEVER_WEIGHTED`, the life table's `A`–`F`). Only *values* keep their own
case: `current`, `annual`, the `COMPONENT` and `SUBJECT` spec keys, band
labels. Confirmed 2026-09-20; where this file's prose still shows a lowercase
column name, the ALL-CAPS name is the real one.

| Table | Grain | Sim object | Core columns |
|---|---|---|---|
| **Base** (all analysis) | student × year × component | `el_long_sim` | `STUDENT_ID`, `YEAR`, `COMPONENT`, `ELP_FORM`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL` — every ELP measurement is a row here; the **headline** resolves from it via `elp_headline()` and `spec$elp$headline` |
| Roster (enrollment frame) | student × year | `el_roster_sim` (52 cols) | `STUDENT_ID`, `YEAR`, `GRADE`, `EL_STATUS`, demographics/program fields, `ELP_FORM` — **no measurement columns**; the only place students are counted |
| Academic | student × year × subject | `academic_long_sim` | `STUDENT_ID`, `YEAR`, `GRADE`, `SUBJECT`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`, `ACHIEVEMENT_LEVEL_ALT` |

Rules a session must not break (each is a test):

- **Headline via the base table.** The base table is the single source of
  ELP measurement; the roster caches nothing. All stages read the headline
  through one accessor — `elp_headline(data)`: base filtered to
  `spec$elp$headline` on the annual form, joined to roster keys, banded per
  `spec$elp$bands` — and Checks 3–4 run on its scale score. No `compute_*()`
  behind Prep 2 or Checks 1–4 names a component; only Prep 1, exit-rule work,
  and Type-gated Additional checks read the base table below the headline.
  Enforced by the ELPA21-headline smoke fixture (`headline =
  "determination"` must resolve through the same accessor).
- **`COMPONENT` vocabulary** (from the Report Views doc): `listening`,
  `speaking`, `reading`, `writing`, `oral`, `written`, `comprehension`,
  `expressive`, `overall`, plus `determination`. Character, never factor.
  Instrument-native names map in the spec (`native_name`: WIDA "Literacy" →
  `written`); never add `literacy` or `composite_overall` to the vocabulary. A
  component may lack a scale (`NA`) or a level (`NA`).
- **Key names differ by table**: `COMPONENT` vs `SUBJECT`, asserted — or
  WIDA's `reading` domain and an academic Reading subject conflate in a join.
  `kind` holds the vendor words; the key never does.
- **Counting.** The roster is the denominator for anything counted per
  student-year; never count students from the academic table (×subjects) or
  the base table (×8 for WIDA). The roster exists because the base table only
  has rows where a test happened: untested-but-enrolled years and the
  former/never reference groups live only there. Person-period data for
  Check 1's models is **derived, never stored**.
- **`EL_STATUS`** keeps three values: `current` / `former` / `never`.
  Monitoring years are sub-states of `former`; sim formers leave the panel
  after monitoring year 4, so "Former ELs" means former-within-four-years and
  captions say so. **"English-only (EO)"** in Checks 3–4 maps to
  `EL_STATUS == "never"`; former ELs are never in the EO group.
- **Exit observation.** The sim flags exit on the **last current** row; the
  package's decided convention dates exit to the **first former** year. The
  package recomputes `YEAR_IN_STATUS`, `CENSORED`, and the exit indicators
  from `EL_STATUS` transitions; the sim's D-tier columns are validation inputs
  only; a test asserts package exit year = flagged row's `YEAR + 1`.
- **Formers and never-ELs carry no ELP.** They have no base-table rows at
  all (contract §4.4). Alternate-form students never enter the headline, are
  excluded from the exit rule and from band distributions unless the spec
  declares an alternate `level_scale`, are reported as a group, and never exit
  in this sim — a modeling limitation, not a finding.
- **Structural missingness is by design** (contract §3.6): waived domains are
  absent rows; academic rows exist only for grades 3–11 and **not at all for
  2020** (a missing year, not an empty one); outcome columns grade-12 only;
  never-EL rows have every EL-program field `NA`. Do not "fix" any of it.
  Consequence for Check 1's Model 2: the lagged-ELA covariate is `NA` for
  2021 person-period rows (no 2020 score exists); those rows drop from
  Model 2 with a rendered note — **never imputed**.
- **`SAMPLE_WEIGHT`.** Never-EL rows are a 10% sample, weight 10. Every
  statewide share weights; every within-EL statistic does not; EO group
  *means* are unaffected by a constant weight but EO *n*s are always reported
  weighted or labeled. A test asserts no unweighted never-EL total appears.
- **Suppression.** `min_cell` from `policy$suppression` (10). Suppressed cells
  are `NA`, never dropped rows. The sim is tuned so one K cohort clears every
  Check 2 cell at `k_cohort_n = 2400`; never lower `min_cell` or pool grades
  to make cells appear.
- **The sim cannot validate fairness or selection findings.** Every EL/EO gap,
  the exited-EL advantage, and any Model 2 hazard ratio in the sim are
  parameters of the generator (contract §5.2–5.3). Stage `.qmd`s built on sim
  data must not present a fairness-check gap or a skimming-bias result as a
  finding — the machinery is real, the numbers are by construction.
- **Minimum viable delivery**: a roster (`STUDENT_ID`, `YEAR`,
  `GRADE`, `EL_STATUS`) plus base-table rows for at least the headline
  component run Checks 1 (Model 1) and 2 in full; the headline scale score +
  the academic table (with `GRADE` — required, no fallback, decided
  2026-08-26) unlock Check 1 Model 2, Check 3, and Check 4; the
  disaggregation set unlocks the crosstab rows; parental education is absent
  from the sim, so the matched fairness check is crude-SES-only there.
- If `data-raw/el_long_sim.R` is missing, stop and say so; never re-simulate.

**Derived variables the package adds** (recomputed, never trusted from a
delivery; ALL CAPS like every other column): `YEAR_IN_STATUS`, cohort key
`COHORT_LABEL` (excluded when `LEFT_TRUNCATED`), `CENSORED` / `CENSOR_TYPE`,
`EXIT_EVER` ⊇ `EXIT_MAX` ⊇ `EXIT_ONTIME` + `NO_EXIT` + `DISAPPEARED`,
`ON_TIME` / `PROLONGED` (Check 2's headline split, from `policy$timelines`),
`LT_EL` (from `policy$timelines$lt_el_years`), the banded headline `ELP_BAND`
(via `elp_headline()` and `elp$bands`), person-period frames for Check 1.

## 5. `el_spec`

A local R list in the ReportR style (constructor `el_spec()` + validator).
`stateELP` stays standalone: no `Imports` on `stateSpecs` or `stateSummative`.
The shape stays close to the older `assessment_spec` so migration is a
field-by-field diff. Top-level blocks: `schema_version`, `program`, `elp`, `policy`,
`academic`, `data`, `verification`, `source_documents`. The
measurement/policy split is a block boundary drawn where stateSpecs will later
draw a file boundary; classification rule: vendor-changes-the-test → `elp`;
agency/legislature-changes-a-rule → `policy`; state-changes-file-delivery →
`data`. The validator enforces the fixed policy field list under `policy` and
nowhere else, and no `compute_*()` reads a policy value from `elp`.

```r
elp = list(
  instrument = "WIDA ACCESS",
  administration_window = "Jan–Mar",
  components = list(                       # weights [DERIVED — VERIFY]
    listening     = list(kind = "domain",    measures = c("scale", "level")),
    speaking      = list(kind = "domain",    measures = c("scale", "level")),
    reading       = list(kind = "domain",    measures = c("scale", "level")),
    writing       = list(kind = "domain",    measures = c("scale", "level")),
    oral          = list(kind = "composite", measures = c("scale", "level"),
                         from = c(listening = .5, speaking = .5)),
    written       = list(kind = "composite", measures = c("scale", "level"),
                         from = c(reading = .5, writing = .5), native_name = "Literacy"),
    comprehension = list(kind = "composite", measures = c("scale", "level"),
                         from = c(listening = .3, reading = .7)),
    overall       = list(kind = "composite", measures = c("scale", "level"),
                         from = c(listening = .15, speaking = .15, reading = .35, writing = .35))
  ),
  headline     = "overall",
  level_scale  = list(type = "numeric", range = c(1, 6)),
  bands        = list(breaks = c(1, 2, 3, 4, 4.5, 5, 7),
                      labels = c("1.0-1.9","2.0-2.9","3.0-3.9","4.0-4.4","4.5-4.9","5.0+")),
  grade_clusters = c("K","1","2-3","4-5","6-8","9-12"),
  cuts         = NULL,                     # never used to derive a level
  alternate    = NULL
)
```

Component rules unchanged: iterate `names(spec$elp$components)`, filter on
`kind`, **nothing hardcodes a component name** (grep test); `from` weights
informational; vendor-reported levels only — no level is ever derived from a
scale score; non-WIDA `bands` default is one band per level.

`policy` fields (Prep 1's and Prep 3's question lists + what Checks 1–4
consume): `exit` (pathways with display `rule` + structured `terms`
[component / measure / op / value / `exempt_if_absent`], `combine`,
`combination`, `set_date`, `set_by`; validated against `elp$components`,
**never evaluated** `[PROPOSED — CONFIRM]`; plus **`types`** — an integer
vector from the Process Overview's typology, below), `timelines`
(`max_years_to_exit`, the ESSA timeline that defines Check 2's on-time split,
`on_time_rule` keyed by start grade and/or initial band, `lt_el_definition`
plus its structured `lt_el_years`,
`exit_dating = "first_former_year"` `[DERIVED — VERIFY]`,
`initial_elp_source`), `growth` (targets + `how_set`),
`accountability_indicator`, `suppression` (`min_cell`), `academic_benchmark`,
`district_size_bands` (deck default `<100 / 101–999 / 1,000+`
`[PROPOSED — CONFIRM]`), **`program`** (Prep 3's inventory: what EL status
gates — program models, course access, credentialing — as labeled fields
`[PROPOSED — CONFIRM]`), and **`error_tolerance`** (Prep 3's elicited values
judgment: which error is costlier, `premature_exit` / `prolonged_retention` /
`equal`, with who was asked and when; consumed by Check 4's interpretation and
the Decision stage `[PROPOSED — CONFIRM]`). Every policy value carries a
`source`; `[DERIVED — VERIFY]` is the display form of
`source = "provisional"`; the validator rejects a value without one. Prep 1
renders verification status beside each value.

**Exit-criteria typology** (Process Overview, named so stages can gate on it):
Type 1 single overall composite; 2 composite-plus-floor; 3
composite-plus-other-source; 4 compensatory combination; 5
tiered/discretionary; 6 domain profile. `policy$exit$types` declares which
apply (a state can be several); the validator cross-checks the declaration
against the shape of `terms`/`pathways` (a Type 6 declaration with a single
overall term is an error) `[PROPOSED — CONFIRM]`. **Every "Additional check"
in §6 is a Type-gated results slot: `NULL` unless the spec declares a relevant
Type.** A Type 1 state skips them all — the sim does exactly this.

`academic` block: inline for SIM (ELA/Math, grades 3–11, cuts
`700 / 735 / 750 / 790` as level minimums, `>=`, level 4 proficient
`[PROPOSED — CONFIRM]` `[DERIVED — VERIFY]`) or a pointer to the general
spec's `spec_id`; same resolved shape either way; EOC subjects `grades = NULL`
with the real tested grade on the academic row.

**Spec diff table (old `assessment_spec` → `el_spec`)** — migration depends
on it:

| old `assessment_spec` field | `el_spec` field | Change |
|---|---|---|
| `subjects` | `elp$components` | rename at migration; add `kind`, `measures`, `from`, `native_name`; domain `grades` → `elp$grade_clusters` |
| `achievement_levels` | `elp$level_scale` | numeric + labels; drop `policy_benchmark` (an exit rule, not a label); `data_column` → crosswalk |
| `scale_scores` | `elp$cuts` | optional, `NULL` in shell; never fills a level |
| `elp$band_scheme` | `elp$bands` | labels → breaks + labels |
| `instrument`/`domains`/`composites`/`composite_weights` | `elp$components` + `headline` | folded |
| `exit_criteria` | `policy$exit` | moved across the boundary; `terms` + `types` added |
| `growth_targets`, `timelines` | `policy$growth`, `policy$timelines` | moved; on-time rule becomes a structure; `exit_dating`, `initial_elp_source` added (the old prompt's `elp` sketch carried `initial_elp_source`; the classification rule puts it in `policy`) `[PROPOSED — CONFIRM]` |
| *(absent)* | `policy$program`, `policy$error_tolerance` | new, from Prep 3 `[PROPOSED — CONFIRM]` |
| `years$cohort_anchor_grade` | *(dropped)* | starting cohort is `start_grade = "K"` on Check 1/2 functions |
| `data$columns` | `data$columns` | extended: roster, per-component entries, academic table |
| `as_assessment_spec()` | `el_spec()` | shell constructor; shape stays acceptable to stateSpecs later |

**Three instantiations** under `inst/specs/`, all through `test-spec.R` — and
nothing else in that directory, because `el_spec_files()` lists every `.R` there
and treats it as a spec id. Display labels live outside the spec tree in
`inst/lookups/Display_Labels.R`; no spec points at them:

1. `00_SIM_EL_Spec.R` — WIDA-shaped, matches the sim; **Type 1**; identity
   crosswalk; `verification$status = "synthetic"`.
2. `00_TEMPLATE_EL_Spec.R` — the migration test case and the starting point
   for a real state spec. WIDA-shaped, **no data, and no state's policy**:
   every value under `policy` is a placeholder, `verification$status =
   "unverified"` permanently. It earns its place by being the *second shape*
   the suite can test — a **non-identity `data$columns` crosswalk** (the SIM
   spec's is identity, so it cannot exercise resolution at all) and
   **renamed components** (`literacy` → `written`, `composite_overall` →
   `overall`, keeping the vendor label as `native_name`, which the SIM spec's
   already-canonical keys cannot test either).

   *It replaced `NJ_elp_2023.R` on 2026-10-09.* That file carried real New
   Jersey attribution — department, N.J.A.C. citations, codebook column names
   — against policy values CLAUDE.md itself recorded as unverified, which a
   reader of a **public** repo could take for that state's actual exit
   criteria. It is now local-only beside its own pre-migration original in
   `internal_docs/source/` (`NJ_elp_2023.R` and
   `NJ_elp_2023_migrated.R`), gitignored, and **rewritten out of published
   history**. `inst/specs/NJ_elp_2023.R` is kept in `.gitignore` as a guard:
   a spec carrying a real state's values must not sit in a directory that
   `el_spec_files()` enumerates and that ships.
3. `00_ELPA21_EL_Spec.R` — no data; `headline = "determination"`, a level-only
   component, a domain-level exit rule with `exempt_if_absent`; **Type 6** —
   it now exercises the typology as well as the component abstraction, and it
   is the guardrail against WIDA assumptions leaking into `spec.R` or any
   `compute_*()`. Do not pre-enumerate other instruments.

## 6. Per-stage blocks

Common: `run_<stage>()` takes `(data, spec, ...)` where `data` is the
`as_el_data()` bundle — except `run_prep1(spec)` and `run_prep3(spec)` (spec
only; a data argument is an error) and `run_decision(results, spec)` (a named
list of prior stage results). Slots degrade to `NULL` with a comment when a
dependency or a Type gate is absent — never a crash, never an invented
denominator. Every `.qmd` renders, in order: the **Sample** callout, the
stage's authored Looking at / Looking for / Interpreting text, its figures and
tables, its Checkpoint, any Type-gated Additional check, and its "Decide
before moving on" as a closing confirm box. *Departure (2026-09-20): Check 1
inserts its "Data fields used" box directly after the Sample callout, before
Looking at.*

**Prep 1 — Policy documentation** (`run_prep1(spec)`). Sample: N/A. The
review-and-confirm table: assessment and component structure with weights,
level scale, bands, every exit pathway with who holds decision authority,
timelines/growth with `how_set`, LTEL definition, accountability indicator,
`initial_elp_source`, exit-dating convention as an explicit confirm question,
and **Type identification** (`policy$exit$types` rendered with the typology
definitions so the state confirms which apply — this gates every Additional
check downstream). Flag any discretionary/committee pathway distinctly: the
Indiana review found selection effects concentrate there, and Checks 1–2 track
it separately. Slots: `$policy_table`, `$components_table`, `$exit_table`,
`$types`, `$confirm_questions`.

**Prep 2 — Population analysis** (`run_prep2(data, spec)`). Sample: full
statewide EL population (current + former where present), ≥5 years. Figures
per the deck: EL counts by year (+ average); EL % of total enrollment
(**weighted**; `NULL` + note when no never-ELs); K–5 vs 6–12 stacked bars;
per-grade bars + average; ranked district bars with the two-thirds-share and
`<100` reference lines (slides 33–34); binned counts on
`policy$district_size_bands` (never the sim's tertile column); entry grade;
initial ELP distribution (banded; alternate-form excluded with note); profile
shares (home language, service model, disability, sex, SES, alt-assessment;
newcomer / LTEL / heritage where derivable — heritage `NULL` in sim);
monitoring counts as a proposed view `[PROPOSED — CONFIRM]`. Two "looking
for" framings render as authored text: the population's own shape (policy
lands unevenly where ELs concentrate) and the compositional facts that make
later stages downweight grades or split new vs long-term ELs — Prep 2 is the
interpretive backdrop every later stage reads against. "Decide" box: which
cohorts/subgroups are large and stable enough (is there a clean K cohort for
Check 1); where pooled all-EL analysis would obscure. **Additional check
(Types 2–6, gated):** the criteria crosswalk table — pass/fail cross-tab per
criterion near the overall cut; blocked-by-one-criterion counts; the
**contested zone** (ELP range where criteria disagree), carried to Checks 2–3.
Slots: the figure slots + `$analytic_sample_memo`, `$criteria_crosswalk`,
`$contested_zone`. Guardrail: slide-38 "resist the urge to average" renders.

**Prep 3 — Program implementation** (`run_prep3(spec)`). Sample: N/A (the
students it matters for are where Prep 2 showed enrollment concentrates —
cross-reference rendered). Inventory table from `policy$program`: what EL
status gates on the ground — program models, access to grade-level content
and advanced coursework, educator credentialing, anything conditioned on EL
status or ELP level. The stage's product is a plain statement of **what
changes for a student the moment they exit**, plus the **error-tolerance
elicitation**: which error stakeholders see as costlier — premature exit or
prolonged retention — rendered with the Process Overview's framing that this
is a values judgment the data can't answer, that Youden's J assumes the two
equal, and that the answer licenses landing the final threshold off the
J-optimal point. Stored answer in `policy$error_tolerance`; `unknown` renders
as an open confirm item, and Check 4 and Decision then caption their outputs
accordingly. Slots: `$program_table`, `$error_tolerance`,
`$confirm_questions`.

**Check 1 — Cohort survival analysis** (`run_check1(data, spec, start_grade =
"K")`). Sample: a stable entry cohort (same starting grade, same year, ideally
K/1) tracked forward — derived from the panel range and
`policy$timelines$max_years_to_exit`, never year literals (sim: five usable K
cohorts, K-2015…K-2019); `LEFT_TRUNCATED` excluded by construction. **One
cohort is analyzed, the most complete — the qualifying candidate with the
longest observed follow-up (D1, 2026-09-20)**; `compute_cohorts()` is
exported so Check 2 takes the same cohort instead of selecting again, and
`$meta$sample` names the selected cohort, its N and its window from
`$cohorts`. **A "Data fields used" box renders directly after the Sample
callout (D2, 2026-09-20)**, generated from one internal field declaration
that the compute functions validate against, so the box and the code cannot
disagree. **Exit
dating: first former year, counted as a year in status; exit absorbing;
disappeared = first year absent with no prior exit** — fixed by the deck's A–J
example; slide-52's "exited at year's end" for row *t* means observed exited
in year *t+1*'s data; a student EL six years and reclassified in the seventh
is *not* within a six-year timeline. Two named models (the rest of the
process refers to them by name):

- **Model 1 — unconditional survival. Two halves, both built (2026-09-21).**
  *Descriptive*: the slide-52 life table (columns **A** enrolled at year's
  beginning, **B** exited, **C** disappeared, **D** = B/A at-risk annual exit
  rate, **E** = C/A, **F** = ΣB/A_K original-cohort cumulative — the headline,
  a lower bound; recurrence A_{t+1} = A_t − B_t − C_t; last year B empty,
  C = A), the cumulative exit curve overall and by initial ELP band, the
  discrete-time survival estimate 1 − Π(1 − D_t) as a labeled companion
  column **with a Greenwood interval on the complementary log-log scale**
  (the estimate only — F is a censoring-biased count, not an estimator, and
  is never banded), median years to exit, % within the ESSA timeline, and %
  still classified at the window's edge. No predictor anywhere in it; the
  band split groups students, it does not model them. *Fitted*, in two steps:
  **`$model1`** is unconditional — `EXITED ~ factor(YEAR_IN_STATUS)`, no
  covariate, saturated, so its fitted hazards **are** column D (a test
  asserts the identity); it needs no score and runs wherever a cohort does.
  **`$model1_baseline`** adds the student's **entry** score, fixed for their
  whole run, and is `NULL` with a note when the headline carries no scale
  (ELPA21). Censored ≠ non-exiter; **no mean/average time-in-status
  anywhere**.
- **Model 2 — ELA-augmented survival.** `$model1_baseline` (derived frame,
  never stored) with **lagged ELA/content achievement** added alongside the
  entry score — the formal test for skimming bias. All three fits run through
  one internal fitter, so the dropped-row accounting and the standardisation
  mean the same thing in each; the frames differ on purpose (Model 2 needs a
  prior-year score and keeps far fewer rows), and the rendered comparison
  states both n's rather than implying a like-for-like refit.

**No covariate is ever the current year's ELP score** (decided 2026-09-21,
after the first build used one; `internal_docs/NOTES-check1.md` has the
evidence, a test is the guardrail). The exit rule is applied to that year's
result, so a contemporaneous score is the decision restated: it separates the
fit, and — the reason this matters — it conditions every other covariate on
the outcome's own cause. Beside it, lagged achievement came out at 1.05
whatever was true, so Model 2's "ratio ≈ 1 ⇒ exit is driven by ELP as
intended" was guaranteed by the specification rather than found. Every
covariate is fixed at entry or lagged. **Standard errors are cluster-robust
(CR1) on `STUDENT_ID`**, computed in-package rather than via `sandwich`;
estimates are unchanged, intervals count students, and the rendered n leads
with students. Headline: the hazard ratio on lagged ELA; ≈1 means exit
  timing is driven by ELP as intended and Checks 3–4 can be read at face
  value; away from 1 means academic performance is shaping exit independent
  of English proficiency, caught at the source. Needs the academic table
  (slot `NULL` without it); 2021 person-period rows drop (no 2020 lag exists)
  with a rendered note, never imputed. On sim data the HR is a generator
  parameter — machinery demo only, captioned as such.

"Decide" box: how much weight later grade-level findings carry, from both
models together — the Process Overview's candidate two-part index
(**retention share** off Model 1's curve as a hard floor, the same logic as
min-N suppression; **selection strength** as |Model 2 HR − 1| as a continuous
downweight above it). **Explicitly untested** — slots `$retention_share`,
`$selection_strength`, `$grade_weights` exist, render only with an
experimental label, and go nowhere mechanically until validated against the
Indiana pipeline `[PROPOSED — CONFIRM]`. Checkpoint: cohort large and stable
(no pandemic-era enrollment anomaly). **Additional check (Type 3, gated):**
the non-ELP criterion as a Model 2 covariate — does it predict exit timing
independent of ELP; if it's an achievement measure also used in the exit
rule, Checks 3–4 "consistency" with it is partly circular and captioned so.
Slots: `$cohorts`, `$life_table`, `$exit_curve`, `$exit_curve_by_initial_band`,
`$survival_est`, `$years_to_50`, `$at_max_timeline`, `$model1`,
`$model1_baseline`, `$model2`,
`$grade_weights` (+ components above), `$type3_covariate`.

**Check 2 — On-time vs. prolonged exiters** (`run_check2(data, spec,
start_grade = "K")`). Sample: the same cohort as Check 1, headline-split into
**on-time** (exited within `policy$timelines`' ESSA timeline) vs **prolonged**
(late exiters + never-exited within the window); disappeared/censored students
are their own column, never silently folded into either group
`[PROPOSED — CONFIRM]`. Machinery: the nested indicators `Exit_Ever ⊇
Exit_Max ⊇ Exit_OnTime` + `No_Exit` from transitions + spec (window-relative;
captions say so). Outputs: the slide-63/64 crosstab (columns All / Exit_Ever /
Exit_Max / Exit_OnTime / No_Exit / Disappeared; rows Total, % of all, then
column-percent blocks for primary language, district size band, sex / low SES
/ special education, service type, initial ELP band; suppression `NA` not
dropped) — plus the new **descriptive academic summary**: median ELA
percentile or proficiency rate for on-time vs prolonged at comparable
trajectory points (comparable = same `YEAR_IN_STATUS` `[PROPOSED — CONFIRM]`;
`NULL` without the academic table). Interpretation text: the human face of
the stuck population — if disability status drives prolongation, a threshold
change alone won't address it. "Decide" box: which subgroups get separate
treatment in Checks 3–4; which equity considerations the final recommendation
must address. Slide-81 caution as guardrail: never plot cohort mean ELP over
time without conditioning on exit group. "Trajectories" view: headline level
× year-in-status faceted by exit category `[PROPOSED — CONFIRM]`.
**Additional check (Types 2–6, gated):** Prep 2's blocked-by-one-criterion
groups as extra comparison columns — a group that looks systematically
different means that criterion screens on something other than what the main
score screens on. Slots: `$flags`, `$crosstab`, `$academic_summary`,
`$trajectories`, `$blocked_groups`.

**Check 3 — Academic patterns: box plots + logistic regression**
(`run_check3(data, spec)`). Sample — **different from Checks 1–2 and stated
first**: cross-sectional, **all ELs per grade and year** (not the K cohort),
restricted to grades with summative academic data (from
`spec$academic$<subject>$grades`; 3–11 in sim; EOC panels by subject when
`grades = NULL`); **each grade analyzed separately, never pooled**
(structure-tested); plus two EO comparison groups: SES-matched-or-filtered EO
for the box plots, full EO for the average-score benchmark. Universe: the
academic table joined to roster status (**academic ⟕ roster** — row-count test
guards the direction; roster ⟕ academic silently drops the reference groups).
Same-year pairing only; caption pairs `elp$administration_window` with the
academic window; 2020 panels `NULL` (missing year, not empty). Core outputs
per grade × subject:

- **Box plots** in the slide-73 grammar (bands + Former + Never/EO on x,
  scale score on y, red labeled proficiency-cut line from
  `policy$academic_benchmark` + `academic` cuts, median line, mean marker),
  with the slide-71 summary table (n, min, p25, median, mean, p75, max, IQR;
  alternate-academic takers in `n`, excluded from distributions; weighted EO
  n; spec-gated "screened but not classified" slot, `NULL` in sim).
- **Logistic regression** (promoted from later-additions to core): estimated
  probability of academic proficiency as a function of the headline **ELP
  scale score**; report the scale score where fitted probability crosses 50%.
- **Landing statistics**, per grade, examined together across grades: the
  band where the share at/above the cut first reaches 50% (equivalently the
  first band whose median clears the cut — supersedes the earlier
  |median − cut| operationalization) and the logistic 50%-crossing score.
  Convergence across methods and grades is corroborating; divergence is
  informative, read against Prep 2's composition findings and Check 1's
  attrition/selection findings — a grade with a heavy long-term-EL share or
  heavy attrition may show a different apparent cut for reasons unrelated to
  the language threshold.
- **Two fairness checks** — is the EL bar one EO peers themselves don't
  clear? (a) box plots re-run on the SES/parental-education-matched EO group
  — crude SES filter in the shell (sim has `FRL`, no parental
  education), matching function a later addition; (b) the identical logistic
  re-run with the binarization threshold swapped from the state's proficiency
  cut to the **EO group's average ELA score**, read at its 50% crossing. A
  persistent gap (fairness threshold materially below the standard one) is a
  construct-validity signal — WIDA measures English proficiency; a content
  cut is not automatically a language-access cut — surfaced explicitly to the
  Decision stage, never resolved here. On sim data the gap is a parameter;
  caption it.

Checkpoint: per-grade estimates never pooled; EO group large enough at every
grade after any SES exclusion. "Decide" box: where the standard and
fairness-check versions land, held as provisional. **Additional check (Types
2/4/5/6, gated):** within Prep 2's contested zone (checked slightly wider),
split by the second criterion — box plots within band; one logistic with ELP
+ cleared-second-criterion indicator + interaction, presented as separate
predicted curves (dual-curve support: later addition). Curve shift = the
second criterion does independent work. Slots: `$summary_table`, `$box_data`,
`$logistic`, `$landing`, `$fairness_boxes`, `$fairness_logistic`,
`$contested_zone_models`, `$coverage`.

**Check 4 — Decision consistency + dependence** (`run_check4(data, spec)`).
Sample: identical to Check 3's paired cross-section, by grade and year. Core:
**decision consistency** (agreement of ELP-based and content-proficiency
classifications across candidate cuts on the headline scale) and **Youden's
J**, with J as the primary optimal-cut statistic: when EL academic-proficiency
rates sit well below 50% (typical), raw consistency's peak is pulled toward
the majority classification and can be distorted or fail to appear in a
realistic range; J weights both error types equally regardless of base rates.
**Checkpoint, encoded as a test:** confirm J is computed, not raw consistency
relabeled — the shell ships a skewed-base-rate fixture where the two optima
demonstrably diverge (this is a demonstrated gap, not theoretical).
Interpretation: convergence between Checks 3 and 4 is the strongest evidence;
Prep 3's error-tolerance answer is rendered beside the J-optimal point — J
assumes equal costs, so a stated asymmetry is a documented reason to land
above or below it, a values input, not a departure from evidence. Output:
a **candidate region**, not a single number, annotated with which grades/years
carry the most and least reliable evidence per Check 1. The **copula /
Kendall's-tau dependence analysis** (tail structure: low-end independence vs
high-end coupling) has no package support yet — the `.qmd` states plainly
what is turnkey (consistency, J: the Cook et al. methods already built) vs
custom (copula), and `$dependence` is `NULL` pending it. **Additional check
(Types 2–6, gated):** the combined policy's own pass/fail outcome as the
classifier input — how the policy as a whole performs; which criterion drives
it comes from Prep 2's crosswalk and Check 3's contested zone, not from here.
Slots: `$consistency_curve`, `$youden_j`, `$j_optimal`, `$candidate_region`,
`$dependence`, `$policy_as_whole`.

**Decision & next steps** (`run_decision(results, spec)`). Sample: N/A —
synthesis. Renders the **evidence map**: one row per stage — what it found,
its sample, its reliability annotation (Check 1's weights, experimental-
labeled), whether it supports keep / adjust / restructure — plus the
beyond-the-data considerations as authored text (other states' practice,
field feedback, equity around any discretionary pathway given Checks 1–2).
The honest output is a **defensible region, not a single true number**:
communicate a range, name which evidence drove the landing point, including
the error-tolerance answer and any fairness-check gap. "Decide" box: (1)
change anything at all; (2) if yes, the specific numbers/bands including any
discretionary pathway's fate; (3) a **monitoring plan** against the ESSA
timeline regardless of the decision. **Additional check (Types 2–6, gated):**
state the recommendation in terms of every piece of the rule and which pieces
the evidence supported changing. Slots: `$evidence_map`,
`$defensible_region`, `$monitoring_plan`, `$recommendation_scaffold`. More
criteria always reduce exit rates — the restructuring trade-off renders as
authored text.

**Row universes.** Each stage declares its own (the Sample line); no stage
requires both assessments unless defined on the pairing. Checks 1 (Model 1)
and 2 live on the roster; Check 1 Model 2, Check 3, and Check 4 live on the
pairing — a student with no score in a tested grade is not in that universe,
not missing from it. The coverage table reports the overlap; no filter
decides it silently.

## 7. `as_el_data()` and delivery forms

`as_el_data(elp, roster = NULL, academic = NULL, spec = NULL)` returns a
validated bundle — the base table first, the roster as the counting frame, the
spec carried when given (so `elp_headline(data)` resolves without it being
passed again); every data-taking `run_<stage>()` takes the bundle, never raw
tables. `roster = NULL` is the merged-single-file form.
The three tables are the analysis model; the delivered form is whatever a
state hands over, and `as_el_data()` is the adapter.

Accepted forms in the shell:

1. **Separate objects** — the sim's own call:
   `as_el_data(el_long_sim, roster = el_roster_sim, academic = academic_long_sim)`. A fully merged single long file (roster attributes repeated per component row, SGP LONG_DATA style) is also accepted and split on intake.
2. **SGP LONG_DATA** — `VALID_CASE`, `CONTENT_AREA`, `YEAR`, `ID`, `GRADE`,
   `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`, demographics per row. `CONTENT_AREA`
   **values** map to components through `data$columns`, never by label
   (`READING` = the overall composite in `WIDA_NV_Data_LONG`, the reading
   test in `NV_Data_LONG`); `GRADE "0"` → `"K"`; `ACHIEVEMENT_LEVEL` parsed
   by the level scale, **warn on integer-only levels** (the 4.0–4.4 / 4.5–4.9
   bands need the decimal from the raw ACCESS file); `VALID_CASE` honored.
   `SGP`/target columns out of scope, recorded as the natural later roster
   addition. Exercised by a hand-reshaped fixture, never a simulation.
3. ~~Single wide student-year table~~ — dropped to Later additions
   `[PROPOSED — CONFIRM]`.

Validator: key `STUDENT_ID × YEAR`; academic `GRADE` present and equal
to the roster's; base-table `ELP_FORM` equal to the roster's (same name on
both tables since contract 0.4.0; sixth column, redundant on purpose);
the **coverage table** by year × grade (academic-scored; current ELs with
same-year ELP; former; never; alternate takers) — Check 3's
data-considerations block. `as_academic_long()` is now the
`standardize_student_results()` equivalent: for ReportR-shaped input it is a
near-identity (the academic table *is* `student_results_long` +
`ACHIEVEMENT_LEVEL_ALT`); for SGP-long input it applies the NOTES alias map
(`ID` → `STUDENT_ID`, `CONTENT_AREA` → `SUBJECT`, `"YYYY_YYYY"` → end-year
integer, `VALID_CASE` filtered), with `CONTENT_AREA` *values* recoded to spec
keys through `data$columns`. stateELP does not import stateSummative or
ReportR; any residual handoff is a `rename()` at the boundary ("data crosses,
code doesn't").
Person-period frames for Check 1 are built by a `check1_`-internal helper
from the bundle, on demand.

## 8. Function and results contract

`compute_*()` returns plain data, no plotting, no I/O, `stopifnot()` at the
top. `plot_*()` takes the matching `compute_*()` output and returns a ggplot
without recomputing. `run_<stage>()` assembles; `render_<stage>()` renders
from computed results. Never blur the four. Never build a ggplot in a
function with student-level data in scope — pre-compute and delegate to a
package-level `.build_*()` helper. Absent dependency or unmet Type gate →
`NULL` slot + one-line comment, never a crash. Every results list carries
`$meta`: spec id, **`sample`** (the stage's Sample statement),
`sim_contract_version` (from data attributes), package version, run date.

## 9. QMD and coding conventions

QMD (ReportR pattern): params `pkg_path`, `spec_file`, `results_file`
(optional), `min_n`; setup switches `devtools::load_all()` vs `library()`.
**Dev mode default**: the `.qmd` calls `run_<stage>()` directly;
`results_file` switches to RDS mode `[PROPOSED — CONFIRM]` — the Decision
stage is the natural RDS consumer (it reads the other stages' saved results).
Analysis logic lives in `R/`, never the `.qmd`. Render: copy the `.qmd` to
the output directory, render there, save RDS beside it, remove the copy;
`_files/` stays out of the source tree. `flextable` for every table (Word
primary, HTML secondary); `fig-dpi: 300`. Dev helper: `dev/render_step.R` +
exported `render_step("check1")` — the fifty-times-a-day loop.

**Reader-facing text names the component, never "headline"** (decided
2026-09-21). "Headline" is this package's word for *which* component a
state's analyses run on — load-bearing in code, meaningless to a policy
reader. Documents, figure legends, table cells and `$meta` notes say what the
state reports instead, through `spec_headline_label()`: the component's
`native_name` when the spec gives one ("Overall Composite", "Overall
Performance Determination"), otherwise its vocabulary key and `kind`. The
name is resolved from the spec, so the rule survives the no-component-literals
guardrail and reads correctly for any instrument. Introduce it once per
document, then use the short form. A test in `test-render.R` greps every
`.qmd` for the word and fails on it. The abstraction keeps its name in code,
roxygen and this file.

Coding: tidyverse conventions throughout; `data.table`/`collapse` only after
a measured bottleneck; `Imports:` individual packages only, never
`tidyverse`; explicit namespacing; `weighted.mean` argument order inside
`summarize()`; `facet_wrap` zero-row guards; no `isTRUE()` on vectors; no
`if_else()` for scalars; no `recode(!!!list)`; American English. Roxygen on
every export; NSE names in `R/globals.R`. R 4.3.3, Quarto CLI,
`renv::init()` at shell creation. **The GitHub repo is to be public**
(decided 2026-09-23, superseding "private" as recorded here through
2026-09-22) — see §3's publication state for what that costs and what has
and has not happened.

**Sibling conventions that do NOT apply** (convention bleed is the risk):
ReportR's domain (four lines of analysis, `assessment_spec`,
scale-score/EOC lookups) — its `student_results_long` column naming, ALL CAPS,
*does* apply (§4); its `summarize_*()` auto-narrative; ELReportR's modules, report templates, ingest
protocol, wide `elp_*` columns, `el_results$…` slots; stateSpecs'
`as_assessment_spec()` (shape-compatible, not called).

## 10. Testing expectations

One test file per stage + `test-spec.R` + `test-el_data.R`. Guardrails are
failing tests, not comments; unbuilt functions get existing, `skip()`ped
tests. Every `compute_*()` gets the ReportR triple: happy path, structure,
small-n suppression (`NA`, not dropped). `devtools::check()` 0/0 every
session.

Fixtures that must exist in the shell:

- **A–J** (deck slides 43–51): 50% exited at year 7 (grade 6) and 40% within
  six years under first-former dating; the last-current alternative
  demonstrably fails — the convention test.
- **Slide-52 worked numbers**: D, E, F to two decimals from the slide's own A
  column; the recurrence at **±1 tolerance** (two slide A cells are off by
  one: 19,306 vs 19,305; 8,339 vs 8,340); one callout typo (0.06 for 0.58) —
  **test the cells, not the callouts.**
- **J-vs-consistency skewed fixture** (new): a small paired sample with a
  proficiency base rate well below 50% where the raw-consistency peak and the
  J-optimal cut diverge; the Check 4 code must return the J optimum. This
  encodes the Process Overview's checkpoint as executable proof.
- ELPA21-headline fixture (a base + roster pair, a few rows): every built `compute_*()` runs — the
  no-WIDA-assumptions smoke test; the ELPA21 spec also exercises Type 6
  gating (its Additional-check slots populate structurally; the SIM Type 1
  spec's are `NULL`).
- SGP-long fixture and stateSummative-shaped academic fixture; identical
  bundle output across accepted forms.
- Structural: `COMPONENT` ≠ `SUBJECT`; Check 3 join direction; sim-vs-package
  exit year `+1`; no unweighted never-EL total; per-grade results never
  pooled (Check 3/4 output keyed by grade); 2021 Model-2 rows dropped, never
  imputed; policy fields only under `policy`; `types` declaration consistent
  with `terms` shape; `R/data.R` columns match `sim_contract.md`.

## 11. Shell definition of done

The shell is built across **nine scoped sessions** — the foundation session (`1_`), then one per stage in §15's order (`2_`–`9_`) — and is done when all of the following hold. Every session ends with `devtools::check()` clean and a §3 update; no session builds beyond its scope:

1. Skeleton: `DESCRIPTION`, generated `NAMESPACE`, `R/globals.R`, `R/data.R`,
   `R/spec.R`, `R/el_data.R`, `inst/specs/` (SIM, TEMPLATE, ELPA21),
   `inst/lookups/Display_Labels.R`, `tests/testthat/` (per stage + spec + el_data),
   `internal_docs/` (`source/` extractions, `steps/` instruction files),
   `dev/render_step.R`, `.Rbuildignore`/`.gitignore` in the ReportR pattern,
   `renv::init()`.
2. Data: `Rscript data-raw/el_long_sim.R` runs, all guardrails PASS, three
   `.rda` in `data/`, `sim_contract.md` placeholders filled, version 0.4.0;
   `R/data.R` documents the actual columns.
3. `as_el_data()` + validator + coverage table, tested against the
   separate-objects form and the SGP-long fixture with identical output;
   `as_academic_long()` tested against a stateSummative-shaped fixture.
4. `test-spec.R`: all three specs pass; every policy value carries a
   `source`; `types` validates against `terms`; ELPA21 (Type 6) constructs.
5. **All eight `.qmd` render to docx and html** against the sim + SIM spec,
   each opening with its Sample callout, carrying its authored stage text,
   and holding **≥1 real element**: Prep 1 the spec review table (with
   Types); Prep 2 EL count by year; Prep 3 the program/error-tolerance
   confirm table from the SIM spec; Check 1 the slide-52 life table (A–F) for
   one K cohort; Check 2 the crosstab's Total and %-of-all rows; Check 3 one
   slide-73 box-plot panel; Check 4 the decision-consistency + Youden's J
   curve for one grade × subject with the J optimum marked; Decision the
   evidence-map skeleton listing the stages and their run status. Everything
   else a labeled placeholder that renders without error. Model 2, the
   fairness checks, the weighting index, and all Additional checks are
   placeholders with `skip()`ped tests.
6. The A–J, slide-52, and J-vs-consistency fixtures and the guardrail tests
   exist; everything runnable passes; `devtools::check()` clean.
7. **Nothing beyond this.** Stages are filled in close-in afterward.

## 12. Open decisions

Data-side items live in `sim_contract.md` §11. Package-side, each proceeding
on its default:

| # | Decision | Default |
|---|---|---|
| 1 | Dev-mode vs RDS-mode `.qmd` | dev mode; `results_file` optional; Decision stage reads RDS |
| 2 | Standalone `.qmd` vs Quarto book | standalone; combined render later |
| 3 | SIM spec doubles as TEMPLATE spec | both, as in ReportR |
| 4 | Computed inline values in shell | wait for stage build — **resolved for Check 1's Sample line (2026-09-20)**, which carries the selected cohort, its N and its window from `$cohorts` |
| 5 | A second, non-SIM spec ships in the repo | yes — `00_TEMPLATE_EL_Spec.R`, no data and no state attribution (2026-10-09; was the NJ spec, see §5) |
| 6 | `academic` block inline vs pointer | inline for SIM; pointer documented |
| 7 | Exit rule: `terms` + display string | adopt; validated, never evaluated |
| 8 | `policy$exit$types` + validator cross-check vs `terms` | adopt |
| 9 | `policy$program` and `policy$error_tolerance` fields | adopt (Prep 3 needs a home) |
| 10 | Wide single-table delivery form | dropped to Later additions |
| 11 | Disappeared column in Check 2 outputs | include; columns sum to All |
| 12 | Check 2 "comparable points" for the academic summary | same `YEAR_IN_STATUS` |
| 13 | Check 2 trajectories view | level × year-in-status, faceted by exit group |
| 14 | Check 1 weighting index (`$grade_weights`) | build as experimental-labeled slots; no mechanical use until validated vs the Indiana pipeline |
| 15 | Auto-narrative | not in shell |
| 16 | Companion `ARCHITECTURE.md` | not yet; `sim_contract.md` carries data rationale |
| 17 | HS grades individually vs bucket | individual (sim has 3–11) |

## 13. Later additions (recorded, out of scope)

~~Dual predicted curves from one covariate-adjusted logistic~~ — **built
with Check 3** (2026-09-22): `compute_contested_models()$model$curves`
returns them, and the other Type additional checks can reuse that pattern
rather than reinventing it.

Copula / Kendall's-tau dependence + tail-structure analysis (no package
support yet — the Cook et al. consistency/J methods are what's built; the
`.qmd` states turnkey vs custom);
propensity-score matching for the fairness box plots (crude SES filter
built in Check 3; matching needs parental education, which this delivery
does not carry); a **year dimension for Check 3** — each panel takes the most
recent year the pairing is available, and whether the landing moves year to
year is a question the stage could ask and does not; validation of the
retention-share / selection-strength weighting
index against the Indiana pipeline; a worked example for the lagged-ELA
hazard model (the newest, least standard piece); designing a new
multi-criterion combination from scratch (audit-only for now);
audience-differentiated (technical vs policy) rendering; later start grades
and grouped spans; preliminary cohorts; starting grade as crosstab rows;
old-vs-new-standards box plots; the wide single-table delivery form; SGP
growth/target columns on the roster; screener-based initial ELP; ingest of
real state files; empirical-quantile district bins; exit-rule *evaluation*
(rule-implied eligibility vs observed exits); whether the Additional checks
stay inline or become a companion document (Molly's open format question);
migration to stateSpecs / stateSummative report layer.

## 14. Close-in development mode

One stage at a time; within a stage, one `compute_*`/`plot_*` pair at a time.
Build what is asked, render with `render_step()`, stop. **Each stage is built
in its own Claude Code session**, launched with
`dev/prompts/stage_session_template.md` (fill `<STAGE>` and the one-line
Scope) and governed by its instruction file in
`internal_docs/steps/` (numbered in run order: `2_prep1_policies.md` …
`9_decision_synthesis.md`) — question → sample → data needs → method
→ output slots → figure list → guardrails → done-when → later passes. All
eight instruction files ship with the repo and are binding; refine them as
stages evolve, in their own commits, never mid-session by reinterpretation.
Run-order numbers live on prompt and instruction files only — `.qmd` names,
`R/` prefixes, and `render_step()` ids carry no numbers.

Cross-stage discipline: a stage session may **call** exported functions from
`R/utils.R` and earlier stages' prefixes; it may not edit them. Shared
derivations (exit dating, flags, cohorts, suppression, banding, weighting)
live in `R/utils.R` and are built in the foundation session — if a stage genuinely needs
a shared helper changed, the session stops and says which and why.
*Departure (2026-09-20): cohort **membership** stays shared in `R/utils.R`
(`build_cohort()`, called unchanged); cohort **selection** lives in the
`check1_` prefix because it is Check 1's sample decision, and Check 2 follows
it by calling the exported `compute_cohorts()`, which this section allows.* A session
reads this file + its instruction file and nothing it can't verify in-repo.

`internal_docs/` may also hold working material beyond `source/`, `steps/`,
`reference/`, and `NOTES-<stage>.md` — drafts, meeting notes, planning docs.
Its designated home is `internal_docs/working/`, which is deny-listed in the
repo's `.claude/settings.json`: never attempt to read that folder, and treat
a permission refusal on its path as by-design, not as an error or a gap in
the repo. None of it is build guidance: this file, `sim_contract.md`, the stage
instruction files, and `internal_docs/source/` are the only governing
documents, and where working material disagrees with them it is out of date
by definition. Superseded artifacts in particular (an older CLAUDE.md, the
V2 codebook, sim contract v0.1.0, the original drafting prompt) are retired:
they belong in `internal_docs/working/` or outside the repo; if found
elsewhere, do not read them for guidance, and flag them for removal in the
session summary.

## 15. Getting started for Claude Code

1. Read this file, then `sim_contract.md` v0.4.0, then
   `internal_docs/source/process_overview.md` end to end.
2. **Session 1 — foundation** — paste `dev/prompts/1_foundation.md`: skeleton,
   spec layer + three specs, data generation (contract placeholders filled),
   `as_el_data()` + adapters, shared derivations in `R/utils.R` (with the
   A–J dating tests), `render_step()`. No `.qmd`s.
3. **Stage sessions, one each.** The file numbers give the intended order —
   `2_prep1_policies` → `3_prep2_population` → `4_prep3_program` →
   `5_check1_survival` → `6_check2_flows` → `7_check3_academics` →
   `8_check4_consistency` → `9_decision_synthesis` — but it has been departed
   from deliberately and twice, because the Checks depend only on the
   foundation session's shared flags while the Prep stages depend on nothing:
   Check 1 was built before Prep 1–3 (2026-09-20), and Checks 2 and 3
   followed, leaving Prep 1 and Prep 3 unbuilt. The Prep stages are neither
   built nor stubbed by that; their tests are still the `skip()`ped
   placeholders.

   **Built: Prep 2, Checks 1, 2, 3. Remaining: Prep 1, Prep 3, Check 4,
   Decision.** Of those four, the real dependency is that **Check 4 reads
   Prep 3's `error_tolerance`** — it can run without it, captioning its
   J-optimal point as assuming equal costs, but Prep 3 is spec-only and short
   and is the cheaper thing to do first. Prep 1 depends on nothing. Decision
   renders whatever has run; it degrades gracefully but is only meaningful
   last. Launch each by pasting the filled `stage_session_template.md` with
   Scope = “first pass: the instruction file's Done-when (shell)”.

   **Prep 3 needs you, not the code.** Its two slots are
   `policy$program` (what EL status gates on the ground) and
   `policy$error_tolerance` (which error stakeholders find costlier —
   premature exit or prolonged retention, elicited from the team and the
   stakeholders whose buy-in the result needs). Both are `NA`/`"unknown"` in
   the SIM spec, and the stage is designed to render that way, so it can be
   built before the answers exist.
4. **Later passes**: the same template with a narrower Scope naming specific
   slots from the instruction file's “Later passes” (e.g. “build
   `$exit_curve` + its plot”). One or two slots per session.
5. Every session: `render_step()` clean in both formats,
   `devtools::check()` 0/0, update §3, list `[PROPOSED — CONFIRM]` calls,
   stop.
