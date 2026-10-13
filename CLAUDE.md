# CLAUDE.md — `stateELP`

R package. It follows the conventions of
[`ReportR`](https://github.com/ndadey/ReportR) (summative-assessment
reporting, itself preliminary) and does not import it. A wider **stateR**
family (`stateSpecs`, `stateSummative`, `stateAccountability`,
`stateScoring`) is **planned, not built** (corrected 2026-10-09, `5f4c86f`;
none of the four exists under `github.com/ndadey`, checked 2026-10-10).
Wherever this file says what stateSpecs or stateSummative has or "will" do,
read it as a shape-compatibility
constraint on a package that does not exist yet: never something to call,
import or check against. Read this file, then the stage's instruction file,
before writing anything. If a rule here would be violated by the change you
are about to make, stop and say so.

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
audience report templates, no ingest pipeline. Every stage is meant to
render end-to-end against the shipped simulated data from day one (§11 item
5). As of 2026-10-10 Prep 1, Prep 3 and Decision have no `.qmd` yet: they
have `skip()`ped tests and no document, `stage_qmd_path()` returns `NA`, and
`rs_all()` passes over them. Analyses are then developed close-in, one
`compute_*`/`plot_*` pair at a time.

**Two public companions sit beside the stage documents** (2026-10-09). They
are maintained but are not deliverables, not audience templates, and never a
home for analysis logic, which stays in `R/`: (1) `README.md`, the public
summary, which `rs_site_index()` also turns into the GitHub Pages landing
page `docs/index.md`; (2) the **walkthrough guide**, `guide/<page>.qmd`
(ten pages: index, setup, one per stage) rendered by `rs_guide()` to
`docs/guide/` — analyst-facing pages that run package code against the sim
and describe by hand the stages not built yet. §9 has the rules for both.

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

Eight stages — three Prep steps, four Data Checks, one Decision stage (§1;
the table below and `stage_ids()` agree) — and eight `.qmd` files (Prep 1
and 3 are spec/inventory-driven; the Decision stage synthesizes prior
results). *This line said "nine stages" through 2026-10-09; the nine in §11
counts build sessions (foundation + one per stage). `README.md`'s "nine-step
process", which inherited the old wording, now says eight stages
(2026-10-13); whether the Process Overview numbers its own steps differently
is still to confirm `[PROPOSED — CONFIRM]`.* Prep 3, skipped in the old plan, is
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

"Results object" names the saved file, `results/<stage>_results.rds`
(`render_step(save_results = TRUE)`, `R/render_step.R:233`, §9); no object
of that name exists in code, and the templates call the object `res`
(clarified 2026-10-13). The same column in `inst/templates/README.md:6-15`
means the same file. Elsewhere in this file, "results object" means the list
`run_<stage>()` returns.

**Every stage `.qmd` opens with its Sample statement** (Process Overview v1.5:
the analytic sample is called out on its own line, never buried in prose), and
every results object carries `$meta$sample` with the same text. The sample
shifts across stages — full statewide population (Prep 2), a single entry
cohort (Checks 1–2), cross-sectional all-ELs-plus-comparison-groups
(Check 3), and the same cross-section restricted to current ELs (Check 4,
§6) — and misstating it is the canonical way to misread a stage.

Shared infrastructure: `R/spec.R` (constructor + validator), `R/data.R`,
`R/el_data.R` (`as_el_data()`, `as_academic_long()`, accessors, coverage
table), `R/utils.R` (suppression, banding, weighting), `R/render_step.R`
(`render_step()`, `stage_ids()`, `stage_qmd_path()`), `R/disclosure.R`
(the render-boundary identifier scan, `d63e2eb`), `R/globals.R`,
`R/stateELP-package.R`. Flat
`R/`, no subdirectories; one session edits one prefix; no session edits
`NAMESPACE` or `man/`. Build notes in `internal_docs/NOTES-<stage>.md`, never
non-`.R` files in `R/`.

## 3. Current package state

Maintain this table. It starts as the shell inventory.

| Piece | Status | Notes |
|---|---|---|
| Package skeleton, `DESCRIPTION`, `renv` | **built** | `renv::init()`, explicit snapshot — which pins `Imports`/`Depends`/`LinkingTo` only (`renv/settings.json`), so no `Suggests` is in `renv.lock` (75 entries); `officer` and `tidyr` are there only because `flextable` pulls them in. `officer` and `openxlsx` joined `Suggests` (edited 2026-10-09, committed with this file) for `test-disclosure.R` (their absence was the one `check()` WARNING in `4f402c3`); `openxlsx` is not in the project library, so that file's `.xlsx` case skips here — `renv::install("openxlsx")` runs it. `NAMESPACE`/`man` generated |
| `data-raw/el_long_sim.R` + `sim_contract.md` | **written, RUN** | contract **v0.4.0**; every `[FILL IN]` filled from the 2026-09-16 build; values verified identical to the 0.2.0 build after renaming (§2.1) |
| `data/` (three `.rda`) | **built** | `el_long_sim` (base) / `el_roster_sim` / `academic_long_sim`, 6.84 MB, `sim_contract_version` 0.4.0; `elp_components_sim` retired; regenerate, don't hand-edit |
| `R/spec.R` + three specs | **built** | SIM (Type 1) / TEMPLATE / ELPA21 (Type 6); ALL-CAPS `data$columns` crosswalks + `value_maps`. No `demographics_spec`: display labels moved out of the spec tree to `inst/lookups/Display_Labels.R` and no spec points at them |
| `R/el_data.R` (bundle, adapter, coverage) | **built** | `as_el_data()` (separate objects, merged single file), `as_el_data_sgp()`, `as_academic_long()`, `elp_headline()`, coverage table |
| `R/utils.R` (shared derivations) | **built** | suppression, banding, weighting, `YEAR_IN_STATUS`, `LT_EL`, spans / exit dating, exit flags, cohorts — all ALL CAPS |
| `R/render_step.R` + `dev/render_step.R` | **built** | smoke-tested to docx and html on the v0.4.0 bundle; see the Dropbox note below. `render_step()` renders a copy in `tempdir()` and copies only the finished files to `out_dir` (default `reports/`) — since the dev-loop commit `3356ad8` (2026-09-15; environment fact 3). Since `d63e2eb` (2026-10-09): `save_results` writes to `results_dir` (default `results/`, gitignored, never `out_dir`); `results_file` reaches RDS mode; `check_disclosure` scans every output for student ids and errors on a hit (§9). Dev helpers in `dev/render_step.R`: `rs()`, `rs_all()`, `rs_site()` (example reports into `docs/`), `rs_site_index()` (`docs/index.md` from the README), `rs_guide()` (guide pages into `docs/guide/`, disclosure-scanned). Known gaps in this path are under "Known issues" below |
| `internal_docs/steps/` (8 files, `2_`–`9_`) + `dev/prompts/` (`1_foundation.md`, `5_check1_survival.md`, `stage_session_template.md`, `stage_session_template_fast.md`) | **written** | run-order numbers on these files only; binding per-stage contracts; refine as stages evolve. `1_foundation.md` and `5_check1_survival.md` are records of sessions already run (§14) |
| `R/check1_*.R` + `check1_survival.qmd` | **complete** | Every slot the instruction file names is filled: `compute_cohorts()` (D1 selection, exported for Check 2), `compute_life_table()` (slide-52 A–F), `compute_exit_curve()` (overall and by initial ELP band), `compute_survival_est()` + `compute_years_to_50()` (with a Greenwood interval on the estimate, cloglog scale; F never banded), `compute_at_max_timeline()`, **`compute_model1()`** (the ELP-only person-period hazard model — Model 1's fitted half, the baseline Model 2 is nested in) and `compute_model2()` (the same through one shared `.check1_fit_hazard()`, lagged achievement added, quasi-separation flagged), `compute_grade_weights()` (experimental, labeled, used nowhere), the Type 3 gate, `plot_exit_curve()` (optional banded estimate on the whole-cohort curve), `ft_check1_*()`, `run_check1()`. No `skip()`ped tests remain in `test-check1.R`. Decisions beyond the instruction file are in `internal_docs/NOTES-check1.md` `[PROPOSED — CONFIRM]`. Not built, and recorded as such in the document: per-grade selection strength, later start grades |
| `R/prep2_*.R` + `prep2_population.qmd` | **full port built** | ported from the standalone draft (`stateData/MT/prep2_population_V2.qmd`), not just the shell's Done-when: every figure and table in the instruction file's list, plus entry dating, time in status and the Type-gated crosswalk; `compute_*()` / `plot_*()` / `ft_prep2_*()` / `run_prep2()`; renders docx + html. Slots and calls beyond the instruction file are in `internal_docs/NOTES-prep2.md` `[PROPOSED — CONFIRM]` |
| `R/check2_*.R` + `check2_flows.qmd` | **complete** | Every slot the instruction file names is filled: `compute_exit_flags()` (the nested indicators on Check 1's cohort via the exported `compute_cohorts()`, plus the three-way `EXIT_GROUP`), `compute_cohort_profile()` (characteristics at entry; district size and initial ELP band derived, never delivered), `compute_crosstab()` (the slide-63/64 table: Total block plus five characteristic blocks, `BLOCK_TYPE` distinguishing a partition from independent indicators and `N_BASE` carrying every denominator), `compute_academic_summary()` (proficiency at the same `YEAR_IN_STATUS`), `compute_trajectories()` (`$levels` + `$composition`, the slide-81 mechanism as data), `compute_blocked_groups()` (Types 2–6, via Prep 2's `compute_criteria_crosswalk()`), `plot_academic_summary()`, `plot_trajectories()`, `ft_check2_*()`, `run_check2()`; renders docx + html. No `skip()`ped tests remain in `test-check2.R`. **First code in the package to read `inst/lookups/Display_Labels.R`** (render-side only; slots stay in the delivery's own codes). Decisions beyond the instruction file are in `internal_docs/NOTES-check2.md` `[PROPOSED — CONFIRM]`. Not built, and recorded as such in the document: later start grades, starting grade as crosstab rows, the logistic regression for correlated characteristics |
| `R/check3_*.R` + `check3_academics.qmd` | **complete** | instruction file's Done-when: `compute_panel()` (subject and grade from the spec (an EOC subject's grade from its academic rows, `R/check3_compute.R:72-75`), the year the most recent the delivery carries for that pairing (`:82-85`) — never chosen; *corrected 2026-10-13, this said all three came from the spec*), `compute_box_data()` (slide-73 box statistics, not student rows), `plot_box_panel()`, `run_check3()`; `$coverage` filled from the existing `el_coverage()`. Later passes all built: `compute_summary_table()` (slide-71, from the box data so figure and table cannot disagree), `compute_logistic()` (P(proficient) on the headline scale score, fitted on current ELs, 50% crossing reported, `CROSSING_IN_RANGE` flagging an extrapolation), `compute_landing()` (first band reaching half + the crossing, with `EXIT_BAND` read from the rule and `LANDING_AT_OR_ABOVE_EXIT` flagged), both fairness checks, `plot_landing()`, `ft_check3_*()`. Every declared grade x subject is its own panel; slots are long and keyed, never pooled. Every named slot is built, the Type-gated `compute_contested_models()` included — **computed but never rendered** (Known issues, 2026-10-13): it derives per-student criterion outcomes in `check3_` because Prep 2 computes but does not expose them, with a test asserting the derivation reproduces Prep 2's own `blocked_groups` exactly. Delta-method intervals on the logistic crossing. No `skip()`ped tests remain. Renders docx + html. The band-axis hazard -- the top bands hold students the exit rule is about to remove -- renders as a warning callout; see `internal_docs/NOTES-check3.md` `[PROPOSED — CONFIRM]` |
| `R/check4_*.R` + `check4_consistency.qmd` | **first pass (shell) built 2026-10-09** (`4f402c3`) | instruction file's Done-when, on the headline **level** axis (decided 2026-10-09, `a98ad3d`), for one grade × subject panel taken from Check 3's exported `compute_panel()`: `compute_consistency_curve()` (the four cells + raw agreement at every observed level inside the declared scale; current ELs with both scores, rows rebuilt in `check4_` with a test proving band-by-band agreement with Check 3), `compute_youden_j()` (from the cells, never from agreement), `compute_j_optimal()` (the J optimum, the highest reportable agreement, the current rule read from the exit term on the reported level and judged by the cut it classifies as, `IMPLIED_COST_RATIO`, Prep 3's tolerance read from the spec with `LEAN`, `NO_OPTIMUM` reasons), `plot_consistency_curve()`, `ft_check4_*()`, `check4_*_note()`, `run_check4()`. The J-vs-consistency skewed fixture is the checkpoint test. Suppression covers a row's own cells, the step to every neighbouring published count (Check 3's band breaks included), and the whole panel when Check 3's unshown thin bands would let the margins give a split away — conservative: about half the cuts show on the full sim, and most panels under ~200 students report no optimum. `$candidate_region` is a later pass (two `skip()`ped tests); `$dependence` `NULL` (no copula support); `$policy_as_whole` `NULL` and Type-gated, a `skip()`ped placeholder for Types 2–6. Renders docx + html. `check()` on an export of HEAD + this stage: 0 errors, 2 known NOTEs and 1 WARNING not this stage's (`test-disclosure.R` used `officer`/`openxlsx` undeclared; fixed by adding both to `Suggests`, edited 2026-10-09 and committed with this file). Reviewed adversarially three times, each finding re-checked by a skeptic (commit message); decisions in `internal_docs/NOTES-check4.md` `[PROPOSED — CONFIRM]` — first among them what J's "equal costs" means per student, which changes how the Process Overview's wording reads |
| Eight `.qmd` shells | **5 of 8 built** | `check1_survival.qmd`, `prep2_population.qmd`, `check2_flows.qmd`, `check3_academics.qmd`, `check4_consistency.qmd`; one stage session each for the rest — Prep 1, Prep 3, Decision (§15) |
| Fixtures: A–J, slide-52, J-vs-consistency | **all three built** | A–J and its last-current counter-fixture in `helper-utils.R` (`:24`, `:60`), tested in `test-utils.R`; slide-52 worked numbers and the cohort rosters in `helper-check1.R`; ELPA21 base + roster pair and SGP / summative fixtures in `helper-el_data.R`; the J-vs-consistency skewed fixture (base rate 16%, J optimum one cut below the agreement peak, worked by hand) in `helper-check4.R` |
| `R/disclosure.R` | **built 2026-10-09** (`d63e2eb`) | `scan_output_for_ids()` (tokenizes the output once and checks every id against it, never a sample; `.docx`/`.xlsx` scanned as unzipped XML) and `check_output_disclosure()`. The rule, decided 2026-10-09: student-level rows may sit on disk in results objects but never reach a `.docx`, `.html` or `.xlsx` (§9) |
| `README.md` | **written** (`28a0626`, `5f4c86f`; Check 4 and guide sections edited 2026-10-09, committed with this file) | the public summary. Its stage-status table, "What the built stages produce", minimum viable delivery and example-report table mirror this section and §4 — change them together. Its Check 4 sample reads "Check 3's cross-section, limited to current ELs with both a same-year ELP level and a content score" (2026-10-13; it said "Same as Check 3"), matching §6. Source of `docs/index.md` |
| `docs/` (GitHub Pages) | **live 2026-10-09 for Prep 2 and Checks 1–3** (`934fa12`, `f4b2e41`); the Check 4 report and `guide/` ship with this commit | https://ndadey.github.io/stateELP/. Example reports (html + docx): Prep 2 and Checks 1–3 rendered from a clean export of `5f4c86f`, Check 4 from `f4b2e41`; `index.md` generated from the README, never hand-edited; `guide/` pages. Every report and guide page scanned: no simulated student id appears. `.Rbuildignore`d. `docs/index.md` regenerated from the current README 2026-10-13 and committed with this file. Every `docs/` output (example reports and guide pages) was scanned with `check_output_disclosure()` against `el_roster_sim` before that commit |
| `guide/` → `docs/guide/` | **10 pages, re-rendered 2026-10-13** | the walkthrough guide (§1): `index`, `setup`, one page per stage. Drafted and adversarially reviewed page by page against the code (2026-10-09), then a cross-page consistency pass (2026-10-13, prose only); all ten re-rendered after it from a clean export of `f4b2e41` (no `R/` change since, so it equals the committed code), every code chunk executed. `.Rbuildignore`d. Refresh with `rs_guide()` from the repo root, everything committed (it `load_all()`s the working tree and disclosure-scans each page) |
| `dev/bootstrap_sandbox.sh` | **written 2026-09-20, untracked until 2026-10-10** | R + Quarto for a Claude cloud sandbox only; refuses to run outside a root Linux container with `/mnt/user-data` (environment fact 6) |
| `dev/prompts/stage_session_template_fast.md` | **written 2026-09-21, untracked until 2026-10-10** | deadline variant: no `devtools::check()`, Word only (§14) |
| Stage analyses (all others) | not built | Prep 1, Prep 3, Decision; Check 4's later passes (`$candidate_region`, `$policy_as_whole`, `$dependence`) — per instruction file |
| Git remote / publication | **done 2026-10-09; history rewritten twice** | public at `github.com/ndadey/stateELP`, 43 commits at `f4b2e41`; Pages live — see below |

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
   (the skeleton: `3230733` before this rewrite, `1d0b2d3` since it — the
   second rewrite below left `1d0b2d3` unchanged), which carried much else, so
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

**Second rewrite, the same day.** `inst/specs/NJ_elp_2023.R` was replaced by
`00_TEMPLATE_EL_Spec.R` (`e295323`, §5), then rewritten across the 35
published commits (all but the first two, `1d0b2d3` and `567564e`, changed;
`dc694e0`, the spec layer, is the first that did) and force-pushed as
`e295323` at 11:13:40 on 2026-10-09 (GitHub push event: before `07ee468`,
head `e295323`). The local `origin/main` reflog's 11:13:09 "forced-update"
is the fetch that reset `origin/main` to the old tip `07ee468`; the
force-push is the next entry. Verified 2026-10-10: neither that path nor
`internal_docs/source/` appears in any commit of any local ref
(`git log --all -- <path>` is empty), and no `refs/original/` backup
remains. **Any clone made before the rewrite has diverged: re-clone it,
never pull.**

**The NJ spec is not fully unpublished** (found 2026-10-13). GitHub still
serves the pre-rewrite chain by SHA — `07ee468`, `b01ad94`, `d28f182`,
`2603d0b`, `0fd258f` and the rest of the 33 replaced commits —
and `inst/specs/NJ_elp_2023.R` is retrievable at those refs
(`gh api 'repos/ndadey/stateELP/contents/inst/specs/NJ_elp_2023.R?ref=07ee468'`
returns it, 10,402 bytes); the public events feed names `07ee468` as the
force-push's `before`. Removing the dangling commits takes a request to
GitHub Support; making it is an open action `[PROPOSED — CONFIRM]`. Locally
the reflogs still reach the same commits (`git log --reflog --oneline --
inst/specs/NJ_elp_2023.R` lists `d6b3851`, `2793c34`, `c9cb211`);
`git reflog expire --expire=now --all` followed by `git gc --prune=now`
clears them.

**Dead hashes.** `3230733`, `2ba4e0c` and `452c6e6` died in the **first**
rewrite (`internal_docs/source/`, before publication): they were written
into this file by the publication-state commit (now `6b65e8b`) before
anything was pushed, and GitHub returns 422 for all three — they never
existed in the public repo.
Their published first-rewrite equivalents (`0fd258f` for `2ba4e0c`,
`2603d0b` for `452c6e6`) then died in the second rewrite; the skeleton's
`1d0b2d3` dates from the first rewrite and survived the second. Each dead
hash is annotated with its current equivalent where it appears (`2ba4e0c` →
`5ab178e`, `452c6e6` → `c7b85c2`). Every other hash cited here, in the
README, the NOTES and the steps files resolves.

**GitHub Pages (enabled 2026-10-09):** https://ndadey.github.io/stateELP/,
source `main` `/docs`, legacy Jekyll build, HTTPS enforced. `docs/` holds
`index.md` (generated from `README.md` by `rs_site_index()`; never
hand-edited), the example reports (`prep2_population`, `check1_survival`,
`check2_flows`, `check3_academics` rendered from a clean export of
`5f4c86f`; `check4_consistency` from `f4b2e41`; html + docx each) and
`guide/*.html`. `check4_consistency`, `guide/*.html` and the regenerated
`index.md` ship with this file's commit, the guide re-rendered and the
landing page regenerated 2026-10-13 immediately before it. All were scanned
for simulated student ids: none appears. `docs/` and `guide/` are `.Rbuildignore`d, so
neither ships in the package. Jekyll would choke on Liquid tags (`{{`,
`{%`) in the README; there are none today. **How it is refreshed** —
locally, not by CI (GitHub Actions rendering is deferred, §13): render from a
clean export of the *committed* code (environment fact 4 for where), run
`rs_site()`, update the README's rendered-from commit and date, run
`rs_site_index()`, `rs_guide()` for any changed guide page, and commit
`docs/` together with the README. Pages are self-contained (embedded
resources), so a full refresh adds roughly 9 MB (example reports) plus 14 MB
(guide) to history: refresh what changed, not everything.

**Local artifacts in the public tree (found 2026-10-10).**
`.claude/settings.local.json` — a per-machine permission list with local
scratch paths, tracked from `0d7ac6d`, evidently swept in by the
`git add -A` its own allow list records — is untracked
(`git rm --cached`) and gitignored with this file's commit (the
`.gitignore` edit dates from 2026-10-09), but remains in published
history. `dev/stateELP.lnk`, a Windows shortcut tracked since `aca7529`,
carries this machine's absolute repo path and host name; it is untracked
(`git rm --cached`, the file stays on disk) and `*.lnk` is gitignored with
this file's commit (2026-10-13), but it too remains in published history.
Neither is judged worth a third history rewrite `[PROPOSED — CONFIRM]`.

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

**Current, as of 2026-10-10 (HEAD `f4b2e41` plus the site / guide /
`Suggests` changes committed with this file): the whole suite is 1,565
passing, 22 `skip()`ped, 0 failing, 0 warnings.** Per file: check1 261,
check2 148, check3 167, check4 370, data 44, disclosure 26, el_data 88,
prep2 283, render 16, spec 97, utils 65. The 22 skips: the three stages with
no code (`test-prep1.R` 7, `test-prep3.R` 4, `test-decision.R` 7); Check 4's
later passes (`test-check4.R` 3: `$candidate_region` ×2, `$policy_as_whole`);
and `test-disclosure.R` 1 — the `.xlsx` scan, which needs `openxlsx`, a
`Suggests` the project library lacks (`officer` is installed). Prep 2 and
Checks 1–3 skip nothing. `devtools::document()` changes nothing in
`NAMESPACE` or `man/`. `devtools::check()` on a staging export of exactly the
files to be committed: **0 errors, 0 warnings, 3 NOTEs** — the 6.5 MB
`data/`; "unable to verify current time" (no network on this machine); and
"Package suggested but not available for checking: 'openxlsx'", which goes
away once it is installed. Examples are slow because each rebuilds the full
sim bundle (`plot_landing` ~2.5 min, `run_check3` ~1.5 min); budget for it,
and keep the machine awake — this run started 2026-10-10 09:40 and finished
2026-10-13 01:24 because the machine slept mid-examples; its 2d 15h duration
(227,356 s "elapsed" on one example) is a wall-clock artifact, not a
timing.

Run `tools::checkDocFiles(dir = ".")` before `check()` — it takes seconds and
catches an undocumented argument, which is the only warning this package has
produced twice.

**Known issues — verified in code 2026-10-10 (items marked 2026-10-13 added
then, from a code and test survey), unfixed.** Most were found by
the guide review and the Check 4 session (`NOTES-check4.md`, "Found in other
prefixes"). Each belongs to the session that owns its prefix (§14); a
session that fixes one strikes it through (`~~…~~`) with the fixing
commit's hash and date, as §13 does for built items — never deletes it.

- *Render path* (`R/render_step.R` + every stage template's setup chunk).
  (a) `render_step()` passes `pkg_path = getwd()` (`:82`, `:125`), and every
  template `load_all()`s any `pkg_path` that has an `R/` subfolder, so
  rendering from an ordinary R project loads that project instead of the
  package. (b) `results_file` is normalized (`:131-134`) but `spec_file` is
  not, so a relative spec path fails once the document runs in `tempdir()`.
  (c) `spec_file` alone does not switch the data: without `results_file`
  every template builds the simulated bundle. (d) The roxygen header
  (`:10-12`) still says the document renders in the output directory and
  saves results beside it; the file-header comment (`:3-6`) repeats the
  render-in-`out_dir` half (2026-10-13).
  The guide (`guide/index.qmd`) documents the
  workarounds for (a)–(c); remove them there when the code is fixed.
  *Added 2026-10-13:* (e) `min_n` does nothing: `render_step()` passes it
  (`:126`) and every template declares it, but none reads `params$min_n` —
  each takes the spec's `min_cell` — so the roxygen's "`NULL` lets the
  document read it from the spec" (`:23-24`) implies an override that does
  not exist (which suits §4's never-lower-`min_cell`). (f)
  `.save_stage_results()` always runs the stage on the shipped sim under
  whatever `spec_file` is given (`:226-231`); for `"decision"` it would call
  `run_decision(bundle, spec)`, though its comment says Decision is not
  saved this way (`:221-222`) and the example
  `render_step("decision", save_results = TRUE)` (`:56`) does exactly that —
  dormant until `run_decision()` exists. (g) When `.render_bundle()` cannot
  build the sim bundle under the given spec, the disclosure scan is skipped
  (`:155-160`), silently at the default `quiet = TRUE`, so §9's "after every
  render … scans" does not always hold.
- *`check3_`.* `ft_check3_fairness()` has no guard for a `NULL`
  `$logistic`: the `STATE_CROSSING` column drops out and the `GAP`
  assignment errors (`R/check3_render.R:87-109`). `ft_check3_landing()`
  labels `TOP_BAND_PCT` "Highest band's share", but it is the best share
  across bands (`max(b$PCT_AT_CUT)`, `R/check3_models.R:188`); its roxygen
  (`:156`) and `NOTES-check3.md` repeat the misreading. `run_check3()`'s
  roxygen still says "This is the shell pass" with later slots `NULL`
  (`R/check3_run.R:13-16`). `.check3_share_at()` is defined twice
  (`R/check3_compute.R:186, 266`; the second wins). "headline" appears in a
  reader-facing note (`R/check3_contested.R:251`; the render test greps
  `.qmd` only). **ELPA21 fails Check 3**, and `test-check3.R` has no ELPA21
  test: `compute_panel()` errors with "subscript out of bounds" on a spec
  with no academic subject (`:61-62`) while `run_check3()` guards only on
  the academic table; without declared bands every current EL is
  `GROUP = NA` (`.check3_group_levels()`); `.check3_exit_band()` takes the
  first level term whatever its component, so on ELPA21 it reads a domain
  floor as the exit threshold. `.check3_cut()` (`R/check3_compute.R:245`)
  errors on an `NA` `proficient_level` — TEMPLATE ships one — as soon as a
  spec inlines its subjects (today the pointer's `NULL` cuts short-circuit
  it; Check 4's restatement guards it). `NOTES-check3.md` cites the sim's 5.0+ median
  falling below the 4.5–4.9 median as visible band-axis selection, but the
  generator's academic score has no exit-timing term
  (`data-raw/el_long_sim.R:515-523`), so in the sim the dip cannot be that
  effect; the structural point (every current EL at ≥ 4.5 is in their exit
  year) stands. *Added 2026-10-13, `check3_academics.qmd`:*
  `$contested_zone_models` has **no render path** — no `ft_`/`plot_` reads
  it and the template only lists it (`:42`) — so a Type 2/4/5/6 run computes
  it and the document drops it, while "What is not here" says
  unconditionally "this spec is Type 1" (`:240-244`). The same section lists
  the crossing intervals as absent (`:246-248`) though `ft_check3_landing()`
  renders a 95% CI column (`R/check3_render.R:63, 72`). "Four things are
  still absent" (`:231`), the Decide box's "once the later passes report it
  across every grade" (`:268`), the Sample's "this pass renders grade …"
  (`R/check3_run.R:121-123`) and `compute_panel()`'s roxygen ("The shell
  renders one panel", `R/check3_compute.R:34`) all predate the later passes.
- *`check1_`.* `$type3_covariate` enters the criterion subject's
  **same-year** score (`add_current_subject`, `R/check1_compute.R:873-879,
  924-926`). Under Type 3 that score is the exit rule's own input, the
  circularity §6's covariate rule forbids for ELP; the guardrail test covers
  ELP only. Dormant (SIM is Type 1). `.check1_initial_band()` is unexported
  but called from `check2_` (`R/check2_compute.R:210`, guarded by an
  agreement test) — export it or move it to `utils.R`. *Added 2026-10-13:*
  the template's "Interpreting it" says median years to exit and the
  within-timeline share "arrive in a later pass" (`check1_survival.qmd:438-441`);
  both render above it (`:192-212`) and its own "What is not here" says
  "Check 1 is complete" (`:456`). Its footer prints "Simulated data,
  contract …" whatever the spec (`:501-503`), where Prep 2, Check 2 and
  Check 4 gate that phrase on `spec_id == "SIM"` (§12 #19). In
  `run_check1()` an unfittable `$model1_baseline` becomes `NULL` with no
  note (`R/check1_run.R:107-112`), and the `$type3_covariate` refit sits
  outside the `check1_unfittable` handler (`:128-131`), so it can crash
  instead of degrading. Both dormant on the sim.
- *`utils.R` (foundation owner).* `.on_time_years()` reads the *delivered*
  `INITIAL_ELP` for a `by_initial_band` on-time rule (`R/utils.R:341-347`),
  which `build_student_spans()` copies from the roster (`:309-314`),
  against §4's "never trusted from a delivery"; dormant, no shipped spec
  keys on band — but two tests lock the behavior in, so the fix rewrites
  them: `test-utils.R:116-137` (`INITIAL_ELP` set on the spans by hand, and
  the "need INITIAL_ELP" error) and `test-check2.R:509-551` (the band-clock
  note, on the sim's delivered column) (2026-10-13). `apply_suppression()` blanks true zeros (`:41`), so Prep 2
  and Checks 1–4 each re-implement "a zero is shown" in their own prefix
  (Check 3 only un-suppresses empty groups, `R/check3_compute.R:175-178`;
  §12 #20).
- *Specs.* The SIM spec's header still says it "doubles as the TEMPLATE a
  state copies" (`inst/specs/00_SIM_EL_Spec.R:9`; §12 #3). Its
  `district_size_bands` label `101-999` sits on left-closed breaks
  `c(0, 100, 1000, Inf)`, so a district of exactly 100 is labeled `101-999`.
  The TEMPLATE and ELPA21 `academic` blocks are unresolved pointers (§5);
  with an academic table delivered, `compute_academic_summary()`
  (`R/check2_compute.R:482-483`, owner `check2_`) and `compute_model2()`
  (`R/check1_compute.R:886-887`, owner `check1_`; reached on TEMPLATE, whose
  headline has a scale, and not caught: `run_check1()` catches only
  `check1_unfittable`) error on them exactly as `compute_panel()` does —
  each run function guards only on the table. Only `run_check4()` checks for
  a declared subject.
- ~~*Build.* `results/` (saved stage results, student-level) is gitignored but
  not `.Rbuildignore`d (`^results$` missing), so `devtools::build()` /
  `install()` from the repo root would bundle it.~~ Fixed 2026-10-13:
  `^results$` added to `.Rbuildignore` with this file's commit.
- *Documents.* `sim_contract.md` prose has drifted from decisions made after
  0.4.0: its header says R 4.3.3; §1 (line 113) says "Checks 3–4 run on the
  headline scale score"; §9 calls the repo private and says `tidyr` is not
  in the project library. Read literally, the contract's own rule (its lines
  12-16) ties a version bump to a generator change that alters a column, a
  grain or a distribution, so a prose-only fix need not bump it
  `[PROPOSED — CONFIRM]` (`NOTES-check4.md` left it for the next bump).
  ~~`README.md`'s "nine-step process" (§2), and its Check 4 sample (§3 README
  row).~~ Both fixed 2026-10-13 with this file's commit.
  `internal_docs/steps/8_check4_consistency.md`'s Sample line (§14
  Known lag). `dev/prompts/1_foundation.md` and `5_check1_survival.md` still
  name `NJ_elp_2023.R` and the NJ spec's crosswalk; whether to scrub a
  published prompt is the user's call.
  `NOTES-check3.md`'s "Still not built" lists the contested-zone models and
  the crossing intervals, both built 2026-09-22.
- *Tests (added 2026-10-13; gaps against §4 and §10 are recorded there).*
  `test-decision.R` has no `skip()`ped placeholder for
  `$recommendation_scaffold` (Decision's Type-gated Additional check), and
  `test-prep3.R` none of its own for `$confirm_questions`; those sessions add
  them. Stale comments, to go with the session that owns each file:
  `test-render.R:1-3` ("The stage documents do not exist yet"; five do) and
  `test-check2.R:206` (the ELPA21 Additional check "is not built";
  `compute_blocked_groups()` is — the slot is `NULL` because nobody is
  blocked, §10). `helper-spec.R:71` defines `spec_merge()`, the one fixture
  builder not named `make_<module>_*`; it shares the package's `spec_*`
  accessor prefix and would mask a future export of that name inside tests.
  Seven source-scanning tests list files recursively from the package root
  (`test-check1.R:107, 125`, `test-check3.R:209`, `test-prep2.R:260, 503,
  815`, `test-spec.R:371`), so they read the whole working tree —
  `docs/`, `guide/`, `renv/`, `internal_docs/working/` — and a stray
  `prep2_*.R` anywhere changes `test-prep2.R:815`'s count, one expectation
  per file.

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
`2ba4e0c` (Check 1 complete + Check 2's first pass; `5ab178e` after both
2026-10-09 rewrites): `devtools::check()` on a
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
`2ba4e0c` (`5ab178e` after both 2026-10-09 rewrites), clearing the debt above, and the full loop — `document()` →
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

**Recorded as current on 2026-10-09, before Check 4** (HEAD `452c6e6`,
`c7b85c2` after both rewrites): 1,168 passing, 25 `skip()`ped — the four
unbuilt stages only (`test-prep1.R` 7, `test-prep3.R` 4, `test-check4.R` 7,
`test-decision.R` 7) — 0 failing, 0 warnings; `check()` on a staging export
0 errors / 0 warnings / 2 known NOTEs.

**2026-10-09, publication day.** Eight commits after the publication edit
(`909b6f3`): the NJ spec replaced by a de-attributed template (`e295323`)
and the second history rewrite; a README (`28a0626`), corrected the same day
to describe the ReportR relationship rather than a family that does not
exist (`5f4c86f`); Check 4 moved to the level axis before any code
(`a98ad3d`); disclosure control at the render boundary (`d63e2eb`); example
reports on GitHub Pages (`934fa12`) and the README-generated landing page
(`f4b2e41`); and Check 4's first pass (`4f402c3`), whose message records
`test-check4.R` 370 expectations / 3 skipped, the suite 1,587 passing / 22
skipped (testthat's `nb`, skips included: 1,565 passed), and `check()` 0 errors / 2 NOTEs / **1 WARNING** —
`test-disclosure.R` used `officer` and `openxlsx` without declaring them.
Two of the day's pushes carried another session's unpushed commits
(environment fact 7).

**Site, guide, `Suggests` (edited 2026-10-09; guide sources revised
2026-10-10).** Committed with this file:
`officer` + `openxlsx` in `Suggests` (clears the WARNING) and
`skip_if_not_installed("officer")` in `test-disclosure.R`;
`.claude/settings.local.json` untracked and gitignored; `^guide$` in
`.Rbuildignore`; `rs_guide()` in `dev/render_step.R` (`rs_site()` and
`rs_site_index()` are already committed, `934fa12` and `f4b2e41`); the
README brought up to Check 4 with a walkthrough
guide section; `docs/check4_consistency.{html,docx}`; the ten `guide/*.qmd`
pages and their `docs/guide/*.html` (re-rendered 2026-10-13 after a
cross-page consistency pass); the regenerated `docs/index.md`; two files
written in September and untracked until now, `dev/bootstrap_sandbox.sh`
(2026-09-20) and `dev/prompts/stage_session_template_fast.md` (2026-09-21);
and, 2026-10-13, `^results$` in `.Rbuildignore`, `dev/stateELP.lnk`
untracked with `*.lnk` gitignored, and the README's stage count and Check 4
sample corrected. This CLAUDE.md update itself came from a survey of the
whole repository against this file, five areas each read by its own agent,
with every edit then reviewed independently for accuracy and for anything
lost (2026-10-10 to 2026-10-13). Tests and `check()` as in
"Current" above. `4f402c3`'s 1,587 is the same suite, counted as
testthat's `nb` total, which includes the 22 skipped tests (1,565 passed +
22 skipped = 1,587; the working tree's per-file `nb` column sums to the same
1,587). That commit message reported the suite's `nb` as "passing" while
giving `test-check4.R`'s passed count (370 of `nb` 373) (corrected
2026-10-13). *Re-run 2026-10-13:* 1,565 passing, 22 skipped, per file as
above, 7.7 min; since `4f402c3` only `test-disclosure.R`'s `officer` skip
line and `DESCRIPTION`'s two `Suggests` lines differ
(`git diff --stat 4f402c3 -- tests/ R/ inst/ DESCRIPTION`).

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
Pushing has its own hazard now that the repo is public — environment fact 7
has the procedure (2026-10-09).

**Column naming.** ALL CAPS for every column of every data frame, delivered,
derived and summary alike — the rule and its rationale live in §4. Confirmed
2026-09-20.

**Environment facts a later session will otherwise rediscover the hard
way** (1–3 first recorded 2026-09-15, still true; 4–7 added 2026-10-10):

1. **R 4.3.3 is not installed on this machine** (4.2.1 / 4.4.2 / 4.5.3 are).
   The shipped data is built on **4.4.2** (`sim_contract.md` §2.1 records the
   cross-version verification). Only R 4.4.2 has a working dev toolchain,
   through the renv project library; on a fresh clone run
   `renv::hydrate(c("devtools", "roxygen2"))` after `renv::restore()`, and
   install the other `Suggests` by hand (`openxlsx` at least; the explicit
   snapshot does not record `Suggests`). The project library lives at
   `%LOCALAPPDATA%/R/cache/R/renv/library/stateELP-0f58ad44/`, not in
   `renv/library/`. The generator (`dplyr`, `tidyr`) runs as
   `Rscript --vanilla data-raw/el_long_sim.R` against the user library — the
   invocation `sim_contract.md` §2.1 verified. *Correction 2026-10-10:* the
   reason given here, that `tidyr` is not in the project library, no longer
   holds — it has been in the lockfile since `458ee9b` (through `flextable`
   → `officer`). Do not switch the generator to the renv library without
   re-verifying the §2.1 reference values.
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
4. **Windows path limits break renders outside the repo** (2026-10-09). From
   a deep folder (a long scratch path), Quarto's figure writes overflow the
   260-character `MAX_PATH` (`png()`: "unable to open file"). When the
   working directory uses the long user-folder name and `tempdir()` the 8.3
   short name (`NATHAN~1`), Quarto builds a broken relative path to the
   input ("file does not exist"). To render a clean export (the site refresh,
   §3 Publication state), use a **short** temp folder spelled with the long
   name, set `TMPDIR`/`TMP`/`TEMP` to it, and `setwd()` there.
   `render_step()` from the repo root is unaffected.
5. **Run `devtools::check()` from the repo root (renv active), pointed at the
   staging export** of fact 2. Run from anywhere else, R falls back to the
   user library (`fs` 1.6.5) and fails.
6. **Cloud sandbox** (`dev/bootstrap_sandbox.sh`, written 2026-09-20,
   committed with this file): installs R
   4.3.3 from Ubuntu noble, Quarto 1.8.24 and a sandbox-only stand-in for
   the `quarto` R binding (never commit it into `R/` or `inst/`). It refuses
   to run outside a root Linux container with `/mnt/user-data`. CRAN, P3M,
   r-universe and codeload are 403 there, so every R package must exist as
   an `r-cran-*` deb. The sandbox is for iteration, not reproduction:
   numbers that matter come from a machine where `renv::restore()` has run.
   The script predates the 2026-09-21 `Imports` and is out of date, not
   wrong: its "renv.lock is 17 packages deep" was true at `aace80a`
   (2026-09-20) and is now 75; its apt list lacks `r-cran-ggplot2` and
   `r-cran-flextable` because neither was an `Import` yet (`ggplot2` joined
   with Prep 2, `efb4c1d`; `flextable` with Check 1, `458ee9b`), so
   `load_all()` should fail unless the base image supplies them (read, not
   executed). Recorded, not updated.
7. **Concurrent sessions and pushes** (2026-10-09). Several sessions commit
   to the same local `main`, and a push publishes everything between
   `origin/main` and `main`. Twice a push carried another session's unpushed
   commits to the public repo (`a98ad3d` went out with `5f4c86f`'s push;
   the 14:34 push of `934fa12` also carried `4f402c3`, committed at 14:28
   between `934fa12`'s commit and its push, and `d63e2eb`, which had sat
   unpushed since 12:40), and `git add -A` is the likely way
   `.claude/settings.local.json` reached history. **Procedure:** commit only
   your own paths (`git commit -o <paths>`; never `git add -A` /
   `git add .`); then, as a separate step, list `git log origin/main..main`
   and confirm it is exactly the approved commits; push by hash,
   `git push origin <hash>:main`.

## 4. Data contract — three long tables

**The authority on the shipped data is `sim_contract.md` (repo root), v0.4.0.**
Its §3.5 / §4.2 / §5.1 column tables are the codebook; `R/data.R` mirrors them
and a test asserts agreement. Bump the contract version and this citation
together. What follows is the working summary; read the contract for detail.

ELP instruments don't share a score structure (WIDA: 4 domains + 3 composites
+ overall, each scale + level; ELPA21: 4 domain scales with levels plus a
categorical determination that is a profile rule over domains). The package
handles this the way ReportR handled subjects: **the thing that varies is a
row key, not a set of columns** — the same atomic-layer-plus-resolver split
intended for stateSpecs' `tests` + `map` (planned, not built; see the
header). **Column names conform to ReportR's `student_results_long`
conventions** (the authority is `internal_docs/NOTES-student_results_long.md`
in the ReportR repo): ALL CAPS, `STUDENT_ID × YEAR` keys, canonical names adopted
exactly where they exist, EL-specific names coined in the same idiom. Values
are unchanged; the contract's v0.4.0 note records the three documented
divergences (character `GRADE` carrying `"K"`; numeric/integer
`ACHIEVEMENT_LEVEL` with labels in the spec; lowercase `COMPONENT`/`SUBJECT`
values as spec keys) and the alias-collision decisions (`school_code` dropped;
`FRL_DETAIL`; the derived binary `EL` beside the three-level `EL_STATUS`).

**Every column of every data frame is ALL CAPS**, following the SGP
`LONG_DATA` convention ReportR uses — the three delivered tables, every
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

Rules a session must not break (each is a test, except — *corrected
2026-10-13* — these clauses, which no test asserts: former ELs never enter
the EO group (the code holds it, `R/check3_compute.R:223`; `test-check3.R:75,
89` only assert the groups exist); the former-within-four-years caption;
the sim caption on fairness and Model 2 results — `test-check3.R:526` is
named for it but asserts only `check3_fairness_note()`'s construct-validity
wording, which never mentions simulated data, and the captions are authored
text no test reads (`check3_academics.qmd:257`, `check1_survival.qmd:323`);
`kind`'s validation; person-period frames never stored. The
`data-raw` rule at the end and "Do not 'fix' any of it" are instructions,
not tests; "never lower `min_cell` or pool grades" is tested only in part —
`test-spec.R:21` pins the SIM spec's `min_cell` at 10, and
`test-check3.R:278` / `test-check4.R:652` forbid pooling grades there):

- **Headline via the base table.** The base table is the single source of
  ELP measurement; the roster caches nothing. All stages read the headline
  through one accessor — `elp_headline(data)`: base filtered to
  `spec$elp$headline` on the annual form, joined to roster keys, banded per
  `spec$elp$bands` — Check 3 runs on its scale score, Check 4 on its level
  (decided 2026-10-09: the exit rule is written on the level, and only the
  level scale is declared in the spec; `NOTES-check4.md`; `sim_contract.md`
  §1 predates this and still says Checks 3–4 run on the scale score — a
  prose fix, no version bump, §3 Known issues). No `compute_*()`
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
  `native_name` holds the vendor's words (WIDA "Literacy" on `written`);
  `kind` is only `domain` / `composite` (validated); the key holds neither.
  *Corrected 2026-10-10: this line said `kind` held the vendor words.*
- **Counting.** The roster is the denominator for anything counted per
  student-year; never count students from the academic table (×subjects) or
  the base table (×8 for WIDA). The roster exists because the base table only
  has rows where a test happened: untested-but-enrolled years and the
  former/never reference groups live only there. Person-period data for
  Check 1's models is **derived, never stored**.
- **`EL_STATUS`** keeps three values: `current` / `former` / `never`.
  Monitoring years are sub-states of `former`; sim formers leave the panel
  after monitoring year 4, so "Former ELs" means former-within-four-years and
  captions say so. **"English-only (EO)"** in Check 3 maps to
  `EL_STATUS == "never"`; former ELs are never in the EO group. Check 4 has
  no EO group (its sample is current ELs only, §6). *Corrected 2026-10-13:
  this line said "Checks 3–4".*
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
  to make cells appear. **A true zero is shown as 0**, not suppressed: it
  discloses no student, and a blank reads as hidden. Prep 2 and Checks 1–4
  apply this in their own prefixes (Check 3 only un-suppresses empty
  groups), because the shared
  `apply_suppression()` still blanks zeros `[PROPOSED — CONFIRM]` (§12 #20).
  Check 4 adds complementary suppression (§3 row, §12 #21). Suppression
  protects aggregates; the separate guard against *rows* reaching a document
  is the render-boundary disclosure scan (§9).
- **The sim cannot validate fairness or selection findings.** Every EL/EO gap,
  the exited-EL advantage, and any Model 2 hazard ratio in the sim are
  parameters of the generator (contract §5.2–5.3). Stage `.qmd`s built on sim
  data must not present a fairness-check gap or a skimming-bias result as a
  finding — the machinery is real, the numbers are by construction.
- **Minimum viable delivery**: a roster (`STUDENT_ID`, `YEAR`,
  `GRADE`, `EL_STATUS`), plus `ELP_FORM` on the roster or the base table
  (`"annual"` throughout for an instrument with no alternate form; carried
  from the roster when the base lacks it; `as_el_data()` rejects a delivery
  with neither), plus base-table rows for at least the headline
  component run Checks 1 (Model 1) and 2 in full; the headline scale score +
  the academic table (with `GRADE` — required, no fallback, decided
  2026-08-26) unlock Check 1 Model 2 and Check 3, and the headline level +
  the academic table unlock Check 4; the
  disaggregation set unlocks the crosstab rows; parental education is absent
  from the sim, so the matched fairness check is crude-SES-only there.
- If `data-raw/el_long_sim.R` is missing, stop and say so; never re-simulate.

**Derived variables the package adds** (recomputed, never trusted from a
delivery; ALL CAPS like every other column): `YEAR_IN_STATUS`, cohort key
`COHORT_LABEL` (excluded when `LEFT_TRUNCATED`; when the roster has no
`LEFT_TRUNCATED`, entrants in the panel's first year are treated as
truncated `[PROPOSED — CONFIRM]`), `CENSORED` / `CENSOR_TYPE`
(`disappeared` / `window_edge`),
`EXIT_EVER` ⊇ `EXIT_MAX` ⊇ `EXIT_ONTIME` + `NO_EXIT` (= `window_edge`) +
`DISAPPEARED`, `ON_TIME_YEARS` and
`ON_TIME` / `PROLONGED` (Check 2's headline split, from `policy$timelines`),
`LT_EL` (from `policy$timelines$lt_el_years`), the banded headline `ELP_BAND`
(via `elp_headline()` and `elp$bands`), person-period frames for Check 1.
*Two exceptions, both recorded:* Prep 2 dates **entry** from delivered
fields (the first available of `LEP_PROGRAM_START_DATE`,
`COHORT_START_YEAR`, `YEAR_IN_STATUS`, `ENTRY_GRADE`, capped at the first
observed current year), because the panel cannot see entry before it opens;
every disagreement with the panel is reported in `$meta$notes$entry`
(`NOTES-prep2.md` #3, §12 #22). And `.on_time_years()` reads the delivered
`INITIAL_ELP` for a `by_initial_band` rule — a defect (§3 Known issues).
Exit dating and cohort membership never read a delivered field.

## 5. `el_spec`

A local R list in the ReportR style (constructor `el_spec()` + validator).
`stateELP` stays standalone: no `Imports` on `stateSpecs` or `stateSummative`
(neither exists yet; see the header). The shape stays close to the older
`assessment_spec` so migration is a field-by-field diff. Top-level blocks:
`schema_version`, `program`, `elp`, `policy`, `academic`, `data`,
`verification`, `source_documents`. Two things are named `program`: the
top-level block (assessment metadata; `program$short_name` — `"SIM"`,
`"ACCESS"`, `"ELPA21"` — is what every `$meta$spec_id` carries, not the
`inst/specs/` file id) and `policy$program` (Prep 3's inventory). The
measurement/policy split is a block boundary drawn where a future stateSpecs
would draw a file boundary; classification rule: vendor-changes-the-test → `elp`;
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
                         from = c(listening = .15, speaking = .15, reading = .35, writing = .35),
                         native_name = "Overall Composite")   # spec_headline_label() reads it (§9)
  ),
  headline     = "overall",
  level_scale  = list(type = "numeric", range = c(1, 6),
                      labels = c("Entering","Emerging","Developing","Expanding","Bridging","Reaching")),
                      # labels: what a labeled SGP delivery is parsed against (§7).
                      # categorical: list(type = "categorical", levels = c(...)); stored level = index
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
consume): `exit` — `pathways`, each with `name`, display `rule`,
`authority` (who decides; for Prep 1 to render, which is unbuilt — nothing
in `R/` reads it today), `discretionary` (`TRUE` flags
a committee/discretionary route: Check 4 skips it when reading the current
threshold, and Type 5 accepts it), `combine` (`"all"` / `"any"`) and
structured `terms` [`component` / `measure` / `op` (`>=`, `>`, `<=`, `<`,
`==`) / scalar `value` / `exempt_if_absent`, plus `from` — `"elp"` by
default; `"academic"` names a subject in `component` and is what Type 3
requires]; on `exit` itself `combination` (`"any_pathway"` /
`"all_pathways"`), `set_date`, `set_by`, the optional `zone_half_width`
(Prep 2's near-cut window, default 0.5, `NOTES-prep2.md` #4
`[PROPOSED — CONFIRM]`), and **`types`** — an integer vector from the
Process Overview's typology, below. Terms are validated against
`elp$components` and **never evaluated by the validator**
`[PROPOSED — CONFIRM]`. *Clarified 2026-10-10:* two Type-gated stages do
evaluate individual ELP terms, pass/fail per student-year with
`exempt_if_absent` honored — Prep 2's criteria crosswalk and Check 3's
contested-zone models, both on the first pathway with ≥ 2 ELP terms, criteria
combined conjunctively, `from = "academic"` terms skipped
`[PROPOSED — CONFIRM]` — and Check 4 reads the headline term's `op`/`value`
as the current cut. Whole-rule evaluation (pathway combination,
rule-implied eligibility vs observed exits) is still a later addition (§13).
`timelines` — `max_years_to_exit`; `essa_timeline_years` (the ESSA
timeline; Check 2's on-time split and Check 1's "within the ESSA timeline"
row use it only when `on_time_rule$default_years` is absent,
`R/utils.R:329-330`, clarified 2026-10-13); `on_time_rule = list(default_years,
by_start_grade, by_initial_band)`, named whole-year vectors whose
`by_initial_band` names must be `elp$bands$labels`, the default falling back
from `default_years` to `essa_timeline_years` to `max_years_to_exit`,
`by_start_grade` overriding it and `by_initial_band` overriding both
`[PROPOSED — CONFIRM]`, returned as `ON_TIME_YEARS`; `lt_el_definition`
(display text) plus its structured `lt_el_years` (whole number; `LT_EL` is
`NA` without it); `exit_dating` — the validator accepts only
`"first_former_year"` `[DERIVED — VERIFY]`, and a delivery using another
convention is translated at the boundary; `initial_elp_source` — Check 1
supports `"first_annual"` (absent is read as that) and errors on any other
declared value (`R/check1_compute.R:271-273`), while Prep 2 reads anything
else, or absence, as the entry year. `growth` (targets + `how_set`),
`accountability_indicator`, `suppression` (`min_cell`), `academic_benchmark`,
`district_size_bands` (deck default `<100 / 101–999 / 1,000+`
`[PROPOSED — CONFIRM]`), **`program`** (Prep 3's inventory: what EL status
gates — `models`, `course_access`, `credentialing`, `gated_by_el_status` —
as labeled text fields, all `NA` in the shipped specs
`[PROPOSED — CONFIRM]`), and **`error_tolerance`** (Prep 3's elicited values
judgment: `costlier_error` ∈ `premature_exit` / `prolonged_retention` /
`equal` / `unknown`, validated, with `asked_of`, `asked_date` and
`rationale`; every shipped spec says `unknown`; consumed by Check 4's
interpretation — already read from the spec there — and the Decision stage
`[PROPOSED — CONFIRM]`). **Required under `policy`: `exit`, `timelines`,
`suppression`**; the other six are optional (ELPA21 omits `growth`,
`accountability_indicator`, `district_size_bands` and `program`). Each
policy field present carries one `source` string — per field, not per
scalar; `[DERIVED — VERIFY]` is the display form of
`source = "provisional"`; the validator rejects a field without one. The
fixed list is enforced in both directions: nothing outside it under
`policy`, none of it under `elp` (the leak check covers `elp` only). Prep 1
renders verification status beside each field. `verification$status` ∈
`verified` / `unverified` / `provisional` / `synthetic` (validated).

The vocabulary, the policy field list and the typology each have one source:
`el_component_vocabulary()`, `el_policy_fields()`, `el_exit_typology()`.
`R/spec.R` is the only file allowed to type component literals (the grep
test exempts it by name). Stage code reads the spec through the accessor
layer — `spec_components()`, `spec_headline()`, `spec_headline_label()`,
`spec_bands()`, `spec_min_cell()`, `spec_max_years()`, `spec_essa_years()`,
`spec_exit_types()`, `spec_has_type()`, `spec_academic_subjects()`,
`spec_academic_grades()` — rather than walking the list. `read_el_spec(id)`
sources the file into a fresh environment and requires exactly one
`el_spec` object in it; `el_spec_path()` accepts a file path as well as an
id, so a real state spec can live outside `inst/specs/` (pass it to
`render_step()` as a **full** path: §3 Known issues).

`data` block: `columns` — `roster` / `base` / `academic` crosswalks
(ALL-CAPS canonical = delivered name; a canonical name the crosswalk omits
keeps its name) and `value_maps` (`CONTENT_AREA$base`: values → component
keys; `CONTENT_AREA$academic`: values → subject keys; `GRADE`, e.g. `"0"` →
`"K"`). `columns$components` (per-component wide column names) is validated
but **read by no adapter** — it belongs to the dropped wide form (§12 #10),
and only TEMPLATE carries it. Prep 2 also reads three optional fields the
validator does not restrict: `entry_sources`, `entry_date_rule`
(`"academic_end_year"` default / `"calendar_year"`; SIM sets the latter
because the sim stamps entry dates on the spring-year label) and
`years_in_status_first_value` `[PROPOSED — CONFIRM]` (`NOTES-prep2.md` #4).

**Exit-criteria typology** (Process Overview, named so stages can gate on it):
Type 1 single overall composite; 2 composite-plus-floor; 3
composite-plus-other-source; 4 compensatory combination; 5
tiered/discretionary; 6 domain profile. `policy$exit$types` declares which
apply (a state can be several); it is **required** (integer, within 1:6),
and the validator cross-checks the declaration against the shape of
`terms`/`pathways` `[PROPOSED — CONFIRM]`: Type 1 — one pathway with one
term, on the headline; Type 2 — a pathway with ≥ 2 terms; Type 3 — a term
with `from = "academic"`; Type 5 — ≥ 2 pathways or one
`discretionary = TRUE`; Type 6 — ≥ 2 terms on `kind = "domain"` components,
and not a single headline term. Type 4 (compensatory) is accepted as
declared until the spec can represent a compensatory schedule. Each checked
Type (1, 2, 3, 5, 6) has a test in `test-spec.R`, but two rejection branches have no failing-case
test yet: Type 6's "fewer than two domain terms" (`R/spec.R:507-509`) and
Type 1's "single term not on the headline" (`R/spec.R:483-486`). The same
holds for most of the validator outside the typology (checked 2026-10-13:
54 `add()` branches in `R/spec.R`, 27 `expect_error()` in `test-spec.R`):
no failing-case test for a bad `kind`, a missing required policy field,
`combination`, `combine`, a term's `from` or `op`, `exempt_if_absent`,
`max_years_to_exit` / `essa_timeline_years`, `min_cell`, `costlier_error` or
`verification$status`. "Validated" in this file means the validator checks
it, not that a test proves it does. **Every "Additional check"
in §6 is a Type-gated results slot: `NULL` unless the spec declares a relevant
Type.** A Type 1 state skips them all — the sim does exactly this.

`academic` block: inline for SIM (ELA/Math, grades 3–11, cuts
`700 / 735 / 750 / 790` as level minimums, `>=`, level 4 proficient
`[PROPOSED — CONFIRM]` `[DERIVED — VERIFY]`) or a pointer to the general
spec's `spec_id`; meant to resolve to the same shape either way (not yet:
below); EOC subjects `grades = NULL`
with the real tested grade on the academic row. **The pointer is recorded,
not resolved** (found 2026-10-10): no code fetches a general spec — none
exists to fetch — and `.academic_subjects()` ignores `spec_id`, so a
pointer-only spec declares **no subjects**. With an academic table
delivered, only Check 4 guards it (its slots go `NULL` with a note); Check 1
Model 2 (when the headline carries a scale — the `tryCatch` catches only
`check1_unfittable`), Check 2's academic summary and Check 3 take
`subjects[[1]]` and **error** with "subscript out of bounds" (§3 Known
issues). TEMPLATE and ELPA21 are pointer-form. **A real
state spec copied from TEMPLATE must inline its subjects** (`instrument`,
`grades`, `cuts`, `op`, `level_labels`, `source` per subject).

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
| `as_assessment_spec()` | `el_spec()` | shell constructor; shape stays acceptable to stateSpecs later (`as_assessment_spec()` is a local, unpublished draft, not an old field; §9) |

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
   already-canonical keys cannot test either). **To start a real state
   spec:** copy it (outside `inst/specs/` is fine: `el_spec_path()` takes a
   file path), replace every `source = "provisional"` value with a sourced
   one, set `verification$status`, and inline the `academic` subjects — its
   `spec_id` pointer resolves to nothing (above).

   *It replaced `NJ_elp_2023.R` on 2026-10-09.* That file carried real New
   Jersey attribution — department, N.J.A.C. citations, codebook column names
   — against policy values CLAUDE.md itself recorded as unverified, which a
   reader of a **public** repo could take for that state's actual exit
   criteria. It is now local-only beside its own pre-migration original in
   `internal_docs/source/` (`NJ_elp_2023.R` and
   `NJ_elp_2023_migrated.R`), gitignored, and rewritten out of the
   published branch's history — **but GitHub still serves the replaced
   commits by SHA**, so the file is not fully unpublished until GitHub
   Support removes them (§3 Publication state). `inst/specs/NJ_elp_2023.R` is kept in `.gitignore` as a guard:
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
denominator. Every `.qmd` opens with the **Sample** callout and closes with
its "Decide before moving on" confirm box, followed only by the spec/run
footer line (and, on the SIM spec, its synthetic-data line; Check 3 has no
footer, §12 #19); between them it carries the
stage's authored Looking at / Looking for / Interpreting text, its figures and
tables, its Checkpoint and any Type-gated Additional check. *Corrected
2026-10-13 (this said "in order"; `grep -n '^## '` on the templates):* the
order between varies by stage. Prep 2 and Check 2 put Looking at and Looking
for before the figures; Checks 3 and 4 put Looking at before the figures and
Looking for / Interpreting after them, and Check 4's curve table and
Checkpoint follow Interpreting (`check4_consistency.qmd:265`, `:275`);
Check 1 puts its "Data fields used" box
directly after the Sample callout (D2, 2026-09-20) and all three text blocks
after its figures (`check1_survival.qmd:377`). Only Check 4 has a
`## Checkpoint` heading (`check4_consistency.qmd:275`); Check 1's checkpoint is prose at the top of
its Life table section, before the cohort and life tables
(`check1_survival.qmd:84-88`), and Prep 2 and Checks 2–3 carry none by that
name. Prep 2's box is titled "Decide", inside a "Decide before moving on"
section that first prints the analytic-sample memo
(`prep2_population.qmd:441-454`).
Check 1's Type 3 check renders inside its Model 2 section, only when the
slot is filled; Check 3 renders no Additional check (§3 Known issues).
Whether to normalize the order is open (§12 #24) `[PROPOSED — CONFIRM]`.
Until it is decided, a new stage document (Prep 1, Prep 3, Decision) follows
the original order: Sample → Looking at / Looking for / Interpreting →
figures and tables → Checkpoint → Type-gated Additional check → Decide.

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
newcomer (`NEWCOMER_FLAG`, when delivered) / LTEL (`$profile$TIME_IN_STATUS`,
which needs `lt_el_years` and entry dating) / heritage — `$profile$HERITAGE`
is `NULL` for **every** delivery, not only the sim: no field is read for it
until a heritage indicator is defined, `R/prep2_compute.R:963-964`,
corrected 2026-10-13);
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
`$contested_zone`; built beyond the instruction file (`NOTES-prep2.md`):
`$entry_sources`, `$entry_by_year`, `$entry_cohorts`, `$time_in_status`,
and `$blocked_groups`, one row per student-year blocked by exactly one
criterion within the near-cut window (every current annual student-year when
the rule has no headline term) (`STUDENT_ID`, `YEAR`, `HEADLINE_LEVEL`,
`BLOCKED_BY`; Types 2–6 only, `R/prep2_compute.R:1100-1101, 1136-1140`). *Corrected
2026-10-13 (this said Check 2 consumes the slot):* nothing reads it — Check
2's `compute_blocked_groups()` calls `compute_criteria_crosswalk()` itself
(`R/check2_compute.R:634`), as Check 3's contested zone does
(`R/check3_contested.R:179-181`). Guardrail:
slide-38 "resist the urge to average" renders.

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
accordingly. **Before writing that framing, read the open conflict under
Check 4 below (§12 #18):** J weights the two error *rates* equally, not two
students, and Check 4 as built reads Prep 3's answer per student — so the
elicitation must say whether it asks per student or per rate, and "J
assumes the two equal" cannot be rendered per student without contradicting
the Check 4 report. The user answers §12 #18 before Prep 3 renders its
elicitation; a Prep 3 session raises it and does not choose. The
placeholder test it will un-skip (`test-prep3.R:16-20`) predates the
conflict and says Check 4 and Decision "caption their outputs to say the
J-optimal point assumes equal costs"; Check 4 does not (§15) — correct that comment with the test rather than
building to it (2026-10-13). Slots: `$program_table`, `$error_tolerance`,
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
  is never banded), median years to exit, % within the on-time timeline
  (`max(ON_TIME_YEARS)`, labeled "ESSA timeline" in the table,
  `R/check1_compute.R:507-511`; clarified 2026-10-13), and %
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
**Open conflict (found 2026-10-10):** as built, `$type3_covariate` enters
the criterion subject's *same-year* score (`add_current_subject`) — under
Type 3 the exit rule's own input, the circularity the covariate rule above
forbids for ELP. Dormant (SIM is Type 1); a `check1_` session lags it or
records why same-year is meant here, and adds the matching guardrail test.
Slots: `$cohorts`, `$life_table`, `$exit_curve`, `$exit_curve_by_initial_band`,
`$survival_est`, `$years_to_50`, `$at_max_timeline`, `$model1`,
`$model1_baseline`, `$model2`,
`$grade_weights` (+ components above), `$type3_covariate`.

**Check 2 — On-time vs. prolonged exiters** (`run_check2(data, spec,
start_grade = "K", subject = NULL)`; `subject` picks the academic subject for
`$academic_summary`, the spec's first by default — *signature corrected
2026-10-13*). Sample: the same cohort as Check 1, headline-split into
**on-time** (exited within the student's `ON_TIME_YEARS`, from
`policy$timelines$on_time_rule`, which falls back to the ESSA timeline; §5 —
they coincide only when the rule is flat and its `default_years` is absent or
equal to `essa_timeline_years`; the sim sets both to 5,
`inst/specs/00_SIM_EL_Spec.R:117-119`) vs **prolonged**
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
score screens on. Slots: `$flags`, `$profile` (characteristics at the entry
year; built beyond the instruction file, `NOTES-check2.md`), `$crosstab`,
`$academic_summary`, `$trajectories`, `$blocked_groups`. `$flags` and
`$profile` are one row per student — the reason saved results never sit
beside the documents (§9); under a Type 2–6 spec so is Prep 2's
`$blocked_groups`, one row per student-year blocked by exactly one criterion.

**Check 3 — Academic patterns: box plots + logistic regression**
(`run_check3(data, spec, subject = NULL, grade = NULL, year = NULL)`; each
override is derived when omitted and sets the focal panel passed to
`compute_panel()` — the one the figure draws — and `subject` and `year` also
restrict the all-panel loop; *signature corrected 2026-10-13*). Sample —
**different from Checks 1–2 and stated first**: cross-sectional, **all ELs per grade and year** (not the K cohort),
restricted to grades with summative academic data (from
`spec$academic$<subject>$grades`; 3–11 in sim; EOC panels by subject when
`grades = NULL`); **each grade analyzed separately, never pooled**
(structure-tested); plus two EO comparison groups. *Corrected 2026-10-13
(this said the box plots used the SES-filtered group):* the **full** EO group
is drawn in the slide-73 box plots and the summary table and supplies the
average-score benchmark (the fairness logistic's bar is its weighted mean,
`R/check3_models.R:299-312`); the FRL-filtered EO group appears only in
`$fairness_boxes`, `NULL` when the roster carries no `FRL` (`:254`). Universe: the
academic table joined to roster status (**academic ⟕ roster** — row-count test
guards the direction; roster ⟕ academic silently drops the reference groups).
Same-year pairing only; caption pairs `elp$administration_window` with the
academic window; 2020 panels `NULL` (missing year, not empty). Core outputs
per grade × subject:

- **Box plots** in the slide-73 grammar (bands + "No annual ELP score" +
  Former + Never/EO on x — current ELs untested or on the alternate form keep
  a column of their own, the label Check 1 uses, so the join drops no
  academic row with a roster match; scale score on y, red labeled
  proficiency-cut line from `policy$academic_benchmark` + `academic` cuts,
  median line, mean marker), with the slide-71 summary table (n, min, p25,
  median, mean, p75, max, IQR; alternate-academic takers in `n`, excluded
  from distributions; weighted EO n). A spec-gated "screened but not
  classified" column is **not built** — no spec field declares it and no
  delivery carries it (§13; corrected 2026-10-10, this line called it a
  `NULL` slot).
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
predicted curves (`compute_contested_models()$model$curves`, built
2026-09-22; §13). Curve shift = the
second criterion does independent work. Slots: `$summary_table`, `$box_data`,
`$logistic`, `$landing`, `$fairness_boxes`, `$fairness_logistic`,
`$contested_zone_models`, `$coverage`.

**Check 4 — Decision consistency + dependence** (`run_check4(data, spec,
subject = NULL, grade = NULL, year = NULL)`, the same focal-panel overrides;
an explicit `subject` or `grade` that `compute_panel()` cannot derive
errors, while a derived panel that fails degrades to `NULL` with the reason
in `$meta$panel_error`, `R/check4_run.R:56-67` — *signature corrected
2026-10-13*). Sample: Check 3's paired cross-section, by grade and year, **restricted to
current ELs** — every current EL with both a same-year headline level and a
content score. Formers and never-ELs carry no ELP level to classify, so
Check 3's reference columns are not here and the Sample line says so
(`NOTES-check4.md` #9, which quotes the Process Overview's own Check 4
sample wording in support, `[PROPOSED — CONFIRM]`; *this line said
"identical to Check 3's" until 2026-10-10*). The instruction file
(`internal_docs/steps/8_check4_consistency.md:7`) still says "identical to"
Check 3 (§14 Known lag); the README was brought into line 2026-10-13. The panel comes from Check 3's exported
`compute_panel()`, called unchanged. Core:
**decision consistency** (agreement of ELP-based and content-proficiency
classifications across candidate cuts on the headline **level** scale — the
level, not the scale score, decided 2026-10-09) and **Youden's
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
above or below it, a values input, not a departure from evidence.
**Open conflict with that sentence — the first `[PROPOSED — CONFIRM]` in
`NOTES-check4.md`, left for the user (§12 #18):** J weights the two error
*rates* equally, not two students. Maximising J minimises FP + R·FN with
R = (1 − π)/π, so at a base rate below one half a held-back proficient
student is priced at R premature exits (7.7 at the sim's grade 3). As built,
`$j_optimal` carries `IMPLIED_COST_RATIO`, and `LEAN` reads Prep 3's answer
*per student* against it: premature exit costlier → above; equal per
student → above (agreement's maximum) at π < ½; retention costlier → `NA`
at π < ½ (the side depends on whether a hold is judged more than R times as
costly, a magnitude Prep 3 does not record), "below" only when R ≤ 1
(`R/check4_compute.R:728-731`). Every reader-facing sentence says "equal as
rates". The sentence above, the instruction file and the Process Overview
are deliberately **not** edited; if Prep 3 is meant to elicit a preference
over rates, the paraphrase was right and `.check4_lean()` reverts in one
line. Output:
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
`$dependence`, `$policy_as_whole`. *As built (2026-10-13,
`R/check4_run.R:91-99`):* only the first three are filled, for one grade ×
subject panel (`4f402c3`). `$candidate_region` is `NULL` until its later
pass, which `$meta$pending` names — the candidate region above is the
stage's eventual output, not built yet; `$dependence` is `NULL` pending
copula support (`$meta$dependence_note`); `$policy_as_whole` is `NULL` under
every Type — under Type 1 it does not apply, under Types 2–6 it is a later
pass `$meta$pending` names (`$meta$policy_as_whole_note`).

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
tables. `roster = NULL` is the merged-single-file form. The bundle has class
`el_data` and holds `$elp` (the base table — the element is named `elp`,
not `base`), `$roster`, `$academic` (`NULL` when absent), `$coverage` (from
`el_coverage_table()`), `$spec` and `$meta` (`sim_contract_version`,
`spec_id` = `spec$program$short_name`, `n_students`, `years`, `built`).
Stage code reads it through `el_base()` / `el_roster()` / `el_academic()` /
`el_coverage()` and the headline through `elp_headline()`.
The three tables are the analysis model; the delivered form is whatever a
state hands over, and `as_el_data()` is the adapter.

Accepted forms in the shell:

1. **Separate objects** — the sim's own call:
   `as_el_data(el_long_sim, roster = el_roster_sim, academic = academic_long_sim)`. A fully merged single long file (roster attributes repeated per component row, SGP LONG_DATA style) is also accepted and split on intake.
2. **SGP LONG_DATA**, through `as_el_data_sgp(long_data, spec, academic =
   NULL)` — the spec is required; the function recodes into the merged form
   and hands it to `as_el_data(roster = NULL)`. Columns `VALID_CASE`,
   `CONTENT_AREA`, `YEAR`, `ID`, `GRADE`, `SCALE_SCORE`,
   `ACHIEVEMENT_LEVEL`, demographics per row. `CONTENT_AREA` **values** map
   to components through `data$columns$value_maps$CONTENT_AREA$base`
   (required; an unmapped value is an error), never by label
   (`READING` = the overall composite in `WIDA_NV_Data_LONG`, the reading
   test in `NV_Data_LONG`); `GRADE "0"` → `"K"` through
   `value_maps$GRADE`, never hardcoded; labeled `ACHIEVEMENT_LEVEL`s parse
   against `elp$level_scale$labels` (numeric; index = level) or `$levels`
   (categorical); **warn on integer-only levels** when the spec's bands split
   on a decimal (the 4.0–4.4 / 4.5–4.9 bands need the decimal from the raw
   ACCESS file); `VALID_CASE` honored; `academic` passes through
   `as_academic_long()` (`CONTENT_AREA$academic` map, per-subject
   `level_labels`).
   `SGP`/target columns out of scope, recorded as the natural later roster
   addition. Exercised by a hand-reshaped fixture, never a simulation.
3. ~~Single wide student-year table~~ — dropped to Later additions
   `[PROPOSED — CONFIRM]`.

Validator: keys unique (`STUDENT_ID × YEAR` on the roster, `× COMPONENT`
on base, `× SUBJECT` on academic); the swapped-key check (`SUBJECT` on
base, `COMPONENT` on academic) first; `EL_STATUS` ∈ `current` / `former` /
`never`; base carries measurement columns only, `COMPONENT` character and
inside the spec's model when a spec is given, `ACHIEVEMENT_LEVEL` numeric;
every base and academic row has a roster row; `YEAR` may arrive as
`"YYYY_YYYY"` in any form; a crosswalk renames only where the delivered name
is present and the canonical one absent; academic `GRADE` present and equal
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
keys through `data$columns`. stateELP does not import ReportR (and
stateSummative does not exist; "stateSummative-shaped" in §10/§11 means
ReportR's `student_results_long` shape); any residual handoff is a
`rename()` at the boundary ("data crosses, code doesn't").
Person-period frames for Check 1 are built by a `check1_`-internal helper
from the bundle, on demand.

## 8. Function and results contract

`compute_*()` returns plain data, no plotting, no I/O, `stopifnot()` at the
top. `plot_*()` takes the matching `compute_*()` output and returns a ggplot
without recomputing. `run_<stage>()` assembles. The render side is
`ft_<stage>_*()` flextables in `R/<stage>_render.R` and `<stage>_*_note()`
sentence builders (exported or `.`-internal, under the stage's prefix), each
taking computed results and called from the `.qmd`;
`render_step("<stage>")` renders the document. There is no per-stage
`render_<stage>()` (*corrected 2026-10-10; this line named one*). Never
blur compute / plot / run / render. A stage may split compute across files
under its prefix (`check1_fields.R`; `check3_models.R`,
`check3_contested.R`). Never build a ggplot in a
function with student-level data in scope — pre-compute and delegate to a
package-level `.build_*()` helper — none exists today, because every
`plot_*()` takes aggregate `compute_*()` output and never student rows; the
helper is the pattern for a figure that would need rows in scope
(2026-10-13). Absent dependency or unmet Type gate →
`NULL` slot + one-line comment, never a crash. Every results list carries
`$meta`: `stage` (the stage id), `spec_id` (`spec$program$short_name`, e.g.
`"SIM"` — not the `inst/specs/` file id), **`sample`** (the stage's Sample
statement), `sim_contract_version` (copied from the bundle's `$meta`, which
`as_el_data()` reads from the delivered tables' attribute — `NA` on any
delivery that is not the sim, `R/el_data.R:121-128`), `package_version`,
`run_date`, and the stage's own notes beside them (checked on all five built
stages, 2026-10-13).

## 9. QMD and coding conventions

QMD (ReportR pattern): params `pkg_path`, `spec_file`, `results_file`
(optional), `min_n`; setup switches `devtools::load_all()` vs `library()`.
**Dev mode default**: the `.qmd` calls `run_<stage>()` directly;
`results_file` switches to RDS mode `[PROPOSED — CONFIRM]` — the Decision
stage is the natural RDS consumer (it reads the other stages' saved results).
Analysis logic lives in `R/`, never the `.qmd`. Render: copy the `.qmd` to
the output directory, render there, save RDS beside it *(superseded
2026-10-09, `d63e2eb`: results go to `results/`, never beside the
documents — below)*, remove the copy;
`_files/` stays out of the source tree. *As built (§3 environment fact 3 and
`d63e2eb`, 2026-10-09):* `render_step()` copies the `.qmd` into a scratch
folder under `tempdir()`, renders there, copies only the finished `.docx` /
`.html` into `out_dir` (default `reports/`) and deletes the scratch.
`save_results = TRUE` writes `<stage>_results.rds` to `results_dir`
(default `results/`, gitignored) and **never beside the documents**: Check
2's `$flags` / `$profile` are one row per student, under a Type 2–6 spec
Prep 2's `$blocked_groups` is one row per student-year blocked by exactly
one criterion (`render_step()`'s
roxygen names only Check 2's), and a zipped `reports/` must not carry
either. After every render `check_output_disclosure()` scans
each produced file for `STUDENT_ID`s and **errors** on a hit. The bundle it
scans against is always the shipped sim's, so on a real delivery pass
`check_disclosure = FALSE` and call `check_output_disclosure(files,
my_bundle)` yourself. **The rule** (decided 2026-10-09): student-level rows
may live on disk in results objects; they never reach a `.docx`, `.html` or
`.xlsx`. `flextable` for every table (Word primary, HTML secondary);
`fig-dpi: 300`. Dev helper: `dev/render_step.R` + exported
`render_step("check1")` — the fifty-times-a-day loop. Known gaps in this
path are in §3 Known issues.

**README, site and guide** (2026-10-09). `docs/index.md` is generated from
`README.md` by `rs_site_index()` (links rewritten for the site, GitHub
alert lines dropped); **never hand-edit it**, and commit every README change
together with a regenerated `docs/index.md` (the rule `dev/render_step.R`
states above `rs_site()`, `934fa12` / `f4b2e41`). `docs/guide/*.html` is
generated from `guide/*.qmd` by `rs_guide()`; a guide source change ships
with its re-rendered page `[PROPOSED — CONFIRM]` — no recorded decision
makes the guide a standing obligation yet. `rs_site()` renders every built stage into
`docs/` and regenerates the index. All of it renders from **committed**
code — a clean export, per §3's Publication state — so each page matches
what GitHub shows. `docs/` and `guide/` are `.Rbuildignore`d; never add a
build-time dependency on them. The guide is analyst-facing and runs package
code against the sim (`load_all(params$pkg_path)`), so it goes stale when a
stage changes: §14 says who updates it.

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
stage `.qmd` in `inst/templates/` for the word and fails on it — not R
code (a reader-facing note in `R/` slipped through: §3 Known issues) and not
the guide, which names the spec field `spec$elp$headline` and its accessors
as code for analysts (32 occurrences); whether the guide's *prose* should be
held to the rule is open `[PROPOSED — CONFIRM]`. The abstraction keeps its
name in code, roxygen and this file.

Coding: tidyverse conventions throughout; `data.table`/`collapse` only after
a measured bottleneck; `Imports:` individual packages only, never
`tidyverse`; explicit namespacing; `weighted.mean` argument order inside
`summarize()`; `facet_wrap` zero-row guards; no `isTRUE()` on vectors; no
`if_else()` for scalars; no `recode(!!!list)`; American English. Roxygen on
every export; NSE names in `R/globals.R`. R 4.3.3, Quarto CLI,
`renv::init()` at shell creation. *As of 2026-10-10:* `DESCRIPTION` requires
R ≥ 4.3.0; development, `renv.lock` and the shipped data are on 4.4.2 (4.3.3
is not installed here, §3 environment fact 1); code and `.qmd`s must stay
runnable on 4.3.3, which the cloud sandbox gets from Ubuntu noble. Quarto
CLI is 1.6.40 here and 1.8.24 in the sandbox — when a render differs between
them, suspect the version before calling it a bug. **The GitHub repo is
public** (decided 2026-09-23, superseding "private" as recorded here through
2026-09-22; published 2026-10-09) — see §3's publication state for how, and
what that costs.

**Sibling conventions that do NOT apply** (convention bleed is the risk):
ReportR's domain (four lines of analysis, `assessment_spec`,
scale-score/EOC lookups) — its `student_results_long` column naming, ALL CAPS,
*does* apply (§4); its `summarize_*()` auto-narrative; ELReportR's modules, report templates, ingest
protocol, wide `elp_*` columns, `el_results$…` slots; the planned
stateSpecs constructor `as_assessment_spec()` — drafted locally outside any
repository (`Git/AssessmentSpecs/assessment_spec.R:248`, a 2026-07-02
folder that is not a git repository or a package, unpublished; its header
says its ELP schema supersedes `el_spec`); keep `el_spec` shape-compatible,
never call it; ReportR's own specs are plain `assessment_spec` lists read by its
`R/spec_helpers.R` — keep `el_spec` shape-compatible, never call it.

## 10. Testing expectations

One test file per stage, plus six for the shared layers (*corrected
2026-10-13; this named two*): `test-spec.R`, `test-el_data.R`,
`test-utils.R` (shared derivations; the A–J convention test), `test-data.R`
(the shipped objects against `sim_contract.md` and `R/data.R`),
`test-render.R` (the render loop and the "headline" grep) and
`test-disclosure.R` (the render-boundary scan). Fixtures live in
`helper-<module>.R` (`utils`, `spec`, `el_data`, `prep2`, `check1`–`check4`),
builders named `make_<module>_*` because testthat sources every helper into
one environment (`spec_merge()`, §3 Known issues, is the exception). A later
stage borrows an earlier stage's builders rather than copying them
(`test-check2.R` uses `make_check1_elpa21_bundle()`; `helper-check4.R:12`
wraps `make_check3_sim_bundle()`, and `test-check4.R:606` uses
`make_check3_type2_spec()`). Guardrails are
failing tests, not comments; unbuilt functions get existing, `skip()`ped
tests. Every `compute_*()` gets the ReportR triple: happy path, structure,
small-n suppression (`NA`, not dropped). Known gaps (2026-10-13): no
small-n test for `compute_life_table()` or `compute_trajectories()` (no test
reads either's `SUPPRESSED`), and `compute_life_table()`,
`compute_at_max_timeline()`, `compute_model2()`, `compute_trajectories()`
and `compute_blocked_groups()` are tested only through their stage's
`run_*()` results. `devtools::check()` 0/0 every
session, except a session the user launches with the fast template (§14).

Fixtures that must exist in the shell:

- **A–J** (deck slides 43–51; `helper-utils.R:24, 60` → `test-utils.R:16,
  31, 47`): 50% exited at year 7 (grade 6) and 40% within
  six years under first-former dating; the last-current alternative
  demonstrably fails — the convention test.
- **Slide-52 worked numbers** (`helper-check1.R:16` → `test-check1.R:37,
  49`): D, E, F to two decimals from the slide's own A
  column; the recurrence at **±1 tolerance** (two slide A cells are off by
  one: 19,306 vs 19,305; 8,339 vs 8,340); one callout typo (0.06 for 0.58) —
  **test the cells, not the callouts.**
- **J-vs-consistency skewed fixture** (`helper-check4.R:84-104` →
  `test-check4.R:7, 41`; built with `4f402c3`): a small paired sample with a
  proficiency base rate well below 50% where the raw-consistency peak and the
  J-optimal cut diverge; the Check 4 code must return the J optimum. This
  encodes the Process Overview's checkpoint as executable proof.
- ELPA21-headline fixture (a base + roster pair, a few rows): every built `compute_*()` runs — the
  no-WIDA-assumptions smoke test; the ELPA21 spec also exercises Type 6
  gating (its Additional-check slots populate structurally; the SIM Type 1
  spec's are `NULL`). *Corrected 2026-10-13:* under the shipped ELPA21 spec
  only Prep 2's crosswalk, contested zone and (zero-row) `$blocked_groups`
  populate (`test-prep2.R:704-716`; `R/prep2_run.R:106`).
  Check 2's `$blocked_groups` opens its gate but stays `NULL`: the domain
  terms ask for level ≥ 4 on a three-level scale
  (`inst/specs/00_ELPA21_EL_Spec.R:71, 95-101`), so nobody is blocked by
  exactly one (on a bundle with domain rows, `test-check2.R:484-496`; the
  smoke test at `test-check2.R:198-209` has no domain rows, so every term
  passes via `exempt_if_absent`, `R/prep2_compute.R:1075`, and again nobody
  is blocked), and `make_check2_elpa21_spec()` lowers them to test the
  populated case (`helper-check2.R:12-27`, `test-check2.R:457-496`). Check
  4's `$policy_as_whole` opens its gate as a pending `NULL`
  (`test-check4.R:589-595`) — on `make_check4_elpa21_spec()`, the ELPA21
  spec with the SIM academic block grafted on (`helper-check4.R:109`), not
  the shipped one. **Not yet true for Check 3** (found 2026-10-09 by the
  Check 4 session): `test-check3.R` has no ELPA21 test and Check 3 fails it
  (§3 Known issues). Checks 1 and 2 have ELPA21 tests on bundles with no
  academic table, so neither reaches the shipped spec's empty academic
  block. Check 4's main ELPA21 tests graft the SIM academic block onto the
  ELPA21 spec (`make_check4_elpa21_spec()`), and one
  (`test-check4.R:738-744`) runs the shipped pointer-only spec with an
  academic table, asserting the `NULL`-with-note guard.
- SGP-long fixture and stateSummative-shaped (i.e. ReportR
  `student_results_long`-shaped; §7) academic fixture; identical bundle
  output across accepted forms.
- Structural: `COMPONENT` ≠ `SUBJECT`; Check 3 join direction; sim-vs-package
  exit year `+1`; no unweighted never-EL total; per-grade results never
  pooled (Check 3/4 output keyed by grade); 2021 Model-2 rows dropped, never
  imputed; policy fields only under `policy`; `types` declaration consistent
  with `terms` shape; `R/data.R` columns match `sim_contract.md`.

## 11. Shell definition of done

The shell is built across **nine scoped sessions** — the foundation session (`1_`), then one per stage in §15's order (`2_`–`9_`) — and is done when all of the following hold. Every session ends with `devtools::check()` clean (except a session the user launches with the fast template, §14) and a §3 update; no session builds beyond its scope:

1. Skeleton: `DESCRIPTION`, generated `NAMESPACE`, `R/globals.R`, `R/data.R`,
   `R/spec.R`, `R/el_data.R`, `inst/specs/` (SIM, TEMPLATE, ELPA21),
   `inst/lookups/Display_Labels.R`, `tests/testthat/` (per stage + spec +
   el_data + utils + data + render, with a `helper-<module>.R` per built
   module, §10; `test-disclosure.R` arrived with the post-shell disclosure
   control, item 7),
   `internal_docs/` (`source/` extractions, `steps/` instruction files),
   `dev/render_step.R`, `.Rbuildignore`/`.gitignore` in the ReportR pattern,
   `renv::init()`.
2. Data: `Rscript data-raw/el_long_sim.R` runs, all guardrails PASS, three
   `.rda` in `data/`, `sim_contract.md` placeholders filled, version 0.4.0;
   `R/data.R` documents the actual columns.
3. `as_el_data()` + validator + coverage table, tested against the
   separate-objects form and the SGP-long fixture with identical output;
   `as_academic_long()` tested against a stateSummative-shaped fixture.
4. `test-spec.R`: all three specs pass; every policy field carries a
   `source` (per field, not per scalar, §5); `types` validates against
   `terms`; ELPA21 (Type 6) constructs.
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
   *Additions beyond the shell, made 2026-10-09 — not scope creep to
   revert:* the README (`28a0626`), disclosure control at the render boundary
   (`d63e2eb`) and the Pages site with example reports (`934fa12`,
   `f4b2e41`), all committed; and the walkthrough guide (`guide/`, committed
   with this file), whose standing as a maintained companion is
   `[PROPOSED — CONFIRM]` — no recorded decision makes it one yet. Item 1's
   `source/` extractions are local-only and gitignored (§3), so a clone of
   the public repo can never meet it in full.

**Where the shell stands (2026-10-10):** items 1–4 hold; item 6 holds
except the ELPA21 smoke test for Check 3 (§10, §3 Known issues) and — found
2026-10-13 — the sim-caption guardrail (§4), which `test-check3.R:526`
names but does not assert (re-run 2026-10-13: everything runnable passes);
item 5 holds
for the five built documents (Prep 2 and Checks 1–3 complete, Check 4's
first pass) and not yet for Prep 1, Prep 3 and Decision, which have no
`.qmd`.

## 12. Open decisions

Data-side items live in `sim_contract.md` §11. Package-side, each proceeding
on its default:

| # | Decision | Default |
|---|---|---|
| 1 | Dev-mode vs RDS-mode `.qmd` | dev mode; `results_file` optional; Decision stage reads RDS |
| 2 | Standalone `.qmd` vs Quarto book | standalone; combined render later |
| 3 | Which spec a state copies | `00_TEMPLATE_EL_Spec.R` (supersedes "SIM spec doubles as TEMPLATE spec — both, as in ReportR"; TEMPLATE introduced in `e295323`, 2026-10-09, this row updated 2026-10-10): placeholder policy values and a non-identity crosswalk; the SIM spec only mirrors the sim. The SIM spec's header still claims the role and should change with this `[PROPOSED — CONFIRM]` |
| 4 | Computed inline values in shell | wait for stage build — **resolved for Check 1's Sample line (2026-09-20)**, which carries the selected cohort, its N and its window from `$cohorts` |
| 5 | A second, non-SIM spec ships in the repo | yes — `00_TEMPLATE_EL_Spec.R`, no data and no state attribution (2026-10-09; was the NJ spec, see §5) |
| 6 | `academic` block inline vs pointer | inline; the `spec_id` pointer is a placeholder until a general-spec resolver exists — it resolves to no subjects today (§5, 2026-10-10) *(was: "inline for SIM; pointer documented")* `[PROPOSED — CONFIRM]` |
| 7 | Exit rule: `terms` + display string | adopt; the validator never evaluates; Type-gated stages evaluate single ELP terms only (§5, clarified 2026-10-10) *(was: "adopt; validated, never evaluated")* |
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
| 18 | What J's "equal costs" means | **open, for the user** (Check 4, 2026-10-09): built as equal error *rates*, with Prep 3's answer read per student against `IMPLIED_COST_RATIO`; the Process Overview, the instruction files and §6's own sentence are unedited pending the answer. The user answers this before Prep 3 renders its elicitation; a Prep 3 session raises it and does not choose (§6) |
| 19 | Simulated-data banner at the top of every stage report | **open**, raised 2026-10-09: today the caveat sits near the end of each report, and Check 3's report has no closing caveat line |
| 20 | Zero counts | shown as 0, never suppressed — Prep 2 and Checks 1–4, each in its own prefix (Check 3 only un-suppresses empty groups); `apply_suppression()` still blanks zeros, a `utils.R` change for the foundation owner `[PROPOSED — CONFIRM]` |
| 21 | Check 4's suppression standard | conservative (§3 Check 4 row): about half the cuts show on the full sim and most panels under ~200 students report no optimum; the J optimum is the best *reportable* cut. A less conservative band-break exemption is recorded, not taken (`NOTES-check4.md`) `[PROPOSED — CONFIRM]` |
| 22 | Prep 2 entry dating | delivered entry fields are *sources*, every disagreement with the panel reported — a deliberate exception to §4's "recomputed, never trusted" (`NOTES-prep2.md` #3) `[PROPOSED — CONFIRM]` |
| 23 | The README's attribution of Molly Faulkner-Bond under "Source documents" (and this file's own references to the deck, now public) | **open**, raised 2026-10-09 |
| 24 | Section order inside stage documents | **open**, raised 2026-10-13: the five built templates differ (§6 Common); new stages follow the original order until decided `[PROPOSED — CONFIRM]` |

## 13. Later additions (recorded, out of scope)

~~Dual predicted curves from one covariate-adjusted logistic~~ — **built
with Check 3** (2026-09-22): `compute_contested_models()$model$curves`
returns them, and the other Type additional checks can reuse that pattern
rather than reinventing it. ~~Confidence intervals on Check 3's logistic
crossing~~ — **built** 2026-09-22 (delta method).

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
migration to stateSpecs / stateSummative report layer (both planned, not
built).

*Added 2026-10-10, each recorded in a NOTES file, a prompt or the site
work and missing here:* **CI rendering of `docs/` via GitHub Actions**
(deferred: `docs/` is rendered locally from a clean export and committed,
§3); a resolver for the `academic` `spec_id` pointer (§5); per-grade
selection strength — an interaction or per-grade refit; Check 1 reports one
cohort-level `|ratio − 1|` (`NOTES-check1.md`), and Check 4's candidate
region and the Decision map want per-grade annotations, with no mapping yet
from Check 1's year-in-status index to Checks 3–4's cross-sectional grades;
a logistic regression controlling for correlated characteristics in Check 2
(slide 66; `NOTES-check2.md` says it is on this list, and now it is); a
Prolonged column in Check 2's crosstab (one entry in
`.CHECK2_FLAG_COLUMNS`); mobility (no roster field carries it); a Type 3
circularity gate on Check 2's academic summary (`NOTES-check2.md`); a
shared home for the `Display_Labels.R` reader (only `check2_` reads it); a
cohort override for Checks 1–2 (a selected cohort that fails the Checkpoint
cannot be swapped, and any override must reach Check 2); the "Data fields
used" box for every stage (Check 1 only today, its helper moved to
`utils.R` if extended); a spec-gated "screened but not classified" column in
Check 3's summary table (§6); Check 4's tied-run rule and the less
conservative band-break suppression option (both with the candidate
region); a year dimension for Check 4 as for Check 3.

## 14. Close-in development mode

One stage at a time; within a stage, one `compute_*`/`plot_*` pair at a time.
Build what is asked, render with `render_step()`, stop. **Each stage is built
in its own Claude Code session**, launched with
`dev/prompts/stage_session_template.md` (fill `<STAGE>` with the
`render_step()` id and the one-line Scope; the instruction file is the one
whose name contains `_<STAGE>_` — the template's "ending `_<STAGE>.md`"
matches no file, a slip the fast variant fixes) and governed by its
instruction file in
`internal_docs/steps/` (numbered in run order: `2_prep1_policies.md` …
`9_decision_synthesis.md`) — question → sample → data needs → method
→ output slots → figure list → guardrails → done-when → later passes. All
eight instruction files ship with the repo and are binding; refine them as
stages evolve, in their own commits, never mid-session by reinterpretation.
Run-order numbers live on prompt and instruction files only — `.qmd` names,
`R/` prefixes, and `render_step()` ids carry no numbers. **Known lag:**
`internal_docs/steps/5_check1_survival.md` (last changed 2026-09-20,
`aace80a`) predates the 2026-09-21
Model 1 / covariate / CR1 / Greenwood decisions; until it is refined in its
own commit, §6 Check 1 governs those points, and `INITIAL_ELP` in its Data
needs is a validation input, never the source (Check 1 derives the initial
band from the first annual headline). `internal_docs/steps/8_check4_consistency.md`'s
Sample line ("Identical to Check 3's paired cross-section", `:7`) lags the
same way, as does `test-check4.R:2`'s header comment (2026-10-13); §6's
current-ELs-only Sample governs until it is refined.

**`dev/prompts/stage_session_template_fast.md`** (written 2026-09-21,
committed with this file) is a deadline variant, used only when the user
directs it for a session, as on 2026-09-21 `[PROPOSED — CONFIRM]`: no
`devtools::check()`, Word only. It is that day's directed loop's own prompt
(§3), and its override is "for this session only". The session records in
its §3 row that `check()` and the HTML render were not run (the template
asks for nothing in the commit; saying so there too is good practice, as
the 2026-09-21 loop required), and `check()` plus the HTML render run once
after the last fast-mode stage lands, before any stage is called done.
`dev/prompts/1_foundation.md` and `5_check1_survival.md` are records of
sessions already run; their NJ-spec instructions were superseded by
`00_TEMPLATE_EL_Spec.R` (2026-10-09), so do not re-run them as written.

**Public surfaces follow a stage change** (2026-10-09). When a stage is
built or its output changes, update the README's stage table and "What the
built stages produce" and the stage's guide page (`guide/<stage>.qmd`,
"Status in the package"; the guide half is `[PROPOSED — CONFIRM]`, §9),
then refresh `docs/` from a clean export of the
committed code: `rs_site()` (or one stage's report), the README's
rendered-from commit and date, `rs_site_index()`, `rs_guide("<stage>")`
(§3 Publication state; §9). Commit `docs/` with the README. A session may
defer this to a separate site-refresh commit, but must say so.

Cross-stage discipline: a stage session may **call** exported functions from
`R/utils.R` and earlier stages' prefixes; it may not edit them. Shared
derivations (exit dating, flags, cohorts, suppression, banding, weighting)
live in `R/utils.R` and are built in the foundation session — if a stage genuinely needs
a shared helper changed, the session stops and says which and why.
*Departure (2026-09-20): cohort **membership** stays shared in `R/utils.R`
(`build_cohort()`, called unchanged); cohort **selection** lives in the
`check1_` prefix because it is Check 1's sample decision, and Check 2 follows
it by calling the exported `compute_cohorts()`, which this section allows.*
Check 4 takes Check 3's exported `compute_panel()` the same way (2026-10-09).
Where a needed helper is *internal* to another prefix, the pattern is
**restate it in your own prefix and add an agreement test against the
original** — Check 4's rows, cut and exit band against Check 3; Check 3's
`compute_criterion_pass()` against Prep 2's `blocked_groups` — never a call
to the internal. **One violation is open:** `check2_` calls the unexported
`.check1_initial_band()` (`R/check2_compute.R:210`, guarded by an agreement
test); a `check1_` session exports it or moves it to `utils.R` (§3 Known
issues). A session
reads this file + its instruction file and nothing it can't verify in-repo.

`internal_docs/` holds `steps/` (binding), `NOTES-<stage>.md` (build notes,
so far for prep2 and check1–check4; not governing — everything in them is
`[PROPOSED — CONFIRM]` unless it cites a governing file — and partly stale
against later passes, so check them against the code), `source/` (local
only, gitignored) and `working/` (deny-listed). There is no `reference/`
folder. It may also hold working material beyond these — drafts, meeting
notes, planning docs.
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
   `internal_docs/source/process_overview.md` end to end (local only — a
   clone of the public repo lacks it; ask rather than infer, §3).
2. **Session 1 — foundation** — paste `dev/prompts/1_foundation.md`: skeleton,
   spec layer + three specs, data generation (contract placeholders filled),
   `as_el_data()` + adapters, shared derivations in `R/utils.R` (with the
   A–J dating tests), `render_step()`. No `.qmd`s.
3. **Stage sessions, one each.** The file numbers give the intended order —
   `2_prep1_policies` → `3_prep2_population` → `4_prep3_program` →
   `5_check1_survival` → `6_check2_flows` → `7_check3_academics` →
   `8_check4_consistency` → `9_decision_synthesis` — but it has been departed
   from deliberately and twice by 2026-09-22 (a third time with Check 4,
   below), because the Checks depend only on the
   foundation session's shared flags while the Prep stages depend on nothing:
   Check 1 was built before Prep 1–3 (2026-09-20), and Checks 2 and 3
   followed, leaving Prep 1 and Prep 3 unbuilt. The Prep stages are neither
   built nor stubbed by that; their tests are still the `skip()`ped
   placeholders.

   *As of 2026-10-09, before Check 4. Its Built / Remaining lists and its
   equal-costs caption are superseded by the next paragraph; its launch
   instruction (last sentence) stands:* **Built: Prep 2, Checks 1, 2, 3.
   Remaining: Prep 1, Prep 3, Check 4, Decision.** Of those four, the real dependency is that **Check 4 reads
   Prep 3's `error_tolerance`** — it can run without it, captioning its
   J-optimal point as assuming equal costs, but Prep 3 is spec-only and short
   and is the cheaper thing to do first. Prep 1 depends on nothing. Decision
   renders whatever has run; it degrades gracefully but is only meaningful
   last. Launch each by pasting the filled `stage_session_template.md` with
   Scope = “first pass: the instruction file's Done-when (shell)”.

   **As of 2026-10-10 (supersedes the lists above):** **Built: Prep 2 and Checks 1, 2, 3 (complete);
   Check 4 (first pass, 2026-10-09, `4f402c3`). Remaining: Prep 1, Prep 3,
   Decision, and Check 4's later passes in the instruction file's order —
   `$candidate_region` across every grade with Check 1's annotations (the
   per-grade loop lands with it), then the Type-gated `$policy_as_whole`
   (needs a Type 2–6 spec), then `$dependence` (blocked on copula
   support).** The numbered order has now been departed from three times:
   Check 4 went ahead of Prep 3 because it reads
   `policy$error_tolerance$costlier_error` straight from the spec, and while
   the SIM spec says `"unknown"` it captions the question as open and prints
   the per-student cost ratio (it does not say "assuming equal costs"). When
   Prep 3 lands and echoes the answer into `$meta`, nothing in Check 4
   changes. Prep 3 is still the cheaper next session; the user answers the
   per-student vs per-rate question (§12 #18) before Prep 3 renders its
   elicitation, and a Prep 3 session raises it rather than choosing (§6
   Check 4). Prep 1
   depends on nothing; Decision is meaningful only last.

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
   `devtools::check()` 0/0 (except a session the user launches with the
   fast template, §14), update §3, list `[PROPOSED — CONFIRM]` calls,
   stop. Since 2026-10-09 also: bring the README, the stage's guide page
   (`[PROPOSED — CONFIRM]`, §9) and `docs/` along, or say the site refresh
   is deferred (§14); commit only
   your own paths and push by hash (§3 environment fact 7).
