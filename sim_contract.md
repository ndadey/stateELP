# `sim_contract.md` — stateELP simulated data contract

| | |
|---|---|
| **Version** | 0.4.0 |
| **Date** | 2026-08-26 |
| **Generator** | `data-raw/el_long_sim.R` |
| **Seed** | `20260727` |
| **R** | 4.3.3 |
| **Status** | `[PROPOSED — CONFIRM]` — §11 carries the open items; 0.3.0 renames the objects and moves the headline out of the roster (§1) |

`CLAUDE.md` cites this file by path and version. When the generator changes in a
way that changes a column, a grain, or a distribution, bump the version here and
update the citation in `CLAUDE.md`. A session that regenerates data without
bumping the version produces a package whose documentation silently describes a
different dataset than the one shipped.

**What changed in 0.4.0.** Column names now conform to ReportR's
`student_results_long` conventions (`internal_docs/NOTES-student_results_long.md`
in the ReportR repo, whose alias map is the authority): ALL CAPS throughout,
`STUDENT_ID × YEAR` keys, and the canonical names adopted exactly where they
exist — `GRADE`, `SUBJECT`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`, `SCHOOL_ID`,
`DISTRICT_ID`, `GENDER`, `RACE_ETHNICITY`, `FRL`, `EL`, `IEP`. EL-specific
columns are coined in the same idiom (`EL_STATUS`, `ELP_FORM`, `COHORT_LABEL`,
…). The academic table is now literally `student_results_long` plus
`ACHIEVEMENT_LEVEL_ALT`. Values are unchanged; renames happen once at the
generator's output boundary, so the pass is RNG-neutral. Three documented
divergences: (1) `GRADE` stays character and carries `"K"` — EL analysis needs
K–2, ReportR's integer contract assumed 3–12; the SGP-WIDA `"0"` maps to `"K"`
at intake; (2) `ACHIEVEMENT_LEVEL` is stored numeric (base, 1.0–6.0) / integer
(academic, 1–5) with labels in the spec, not as the label strings ReportR
stores; (3) `COMPONENT` and `SUBJECT` values remain lowercase spec keys —
SGP-style caps values map at intake. Alias-collision decisions: `school_code`
is **dropped** (both `SCHOOL_CODE` and `SCHOOL_NUMBER` are ReportR aliases of
`SCHOOL_ID`); the F/R/N detail is named `FRL_DETAIL` because `FRL_STATUS` is an
alias of `FRL`; the canonical binary `EL` ships (derived: `current` → `"Y"`)
alongside the three-level `EL_STATUS`, which must never be run through
`standardize_student_results()` (the alias map would collapse it to `EL`).
Roster is 52 columns (− `school_code`, + `RACE_ETHNICITY`, + `EL`).

**What changed in 0.3.0.** The component-long table is now **the base for all
analysis** and takes the name `el_long_sim`; the former spine is renamed
`el_roster_sim` and sheds its cached headline columns, so the base table is
the single source of ELP measurement. The
headline is resolved from the base table through one accessor
(`elp_headline()`), never read from a cached copy. This revises the
2026-08-26 restructuring instruction, which had the spine carry the headline;
directed 2026-09-16. RNG-neutral: only names and shipped columns change, no
random draw moves. Roster is 51 columns.

**What changed in 0.2.0.** The data is restructured around the long components
table. Three objects ship, not two; the spine carries only the headline; the
components table is stored long, not materialized from wide columns. The
previous version's item 5 ("the generator is the one that is wrong") is
reversed: the three-table generator is the target, and this document now
describes it. One generator bug is fixed on the way (§4.4). Reference values
that the fix or the version's other edits change are marked
`[FILL IN after regeneration]`; the rest are carried from the 0.1.0 run and are
expected to reproduce bit for bit (§2.1).

---

## 0. Scope and non-goals

This contract governs the synthetic data that ships with `stateELP`. It exists so
that a session can answer "what is actually in this data" without reading the
generator, and so that the five walkthrough `.qmd` files render against a known
shape on day one. Its column tables (§3.5, §4.2, §5.1) are the codebook; there is
no separate codebook document to keep in sync. `R/data.R` documents the three
objects against these tables and a test asserts the column sets agree.

**In scope:** one simulated roster, the ELP measurement model over it as the
long base table, the academic score model conditional on that roster, and the
three dataframes emitted.

**Explicitly not in scope:**

- **No raw-data layer.** Messy multi-file fixtures are not simulated. There is no
  de-cleaning step, no per-year vendor file families, no missing-value sentinels,
  no duplicate resolution, no ingest oracle. Ingest of real state deliveries is a
  later and separate concern and must not shape the package shell. The V2
  `inst/extdata/raw_sim/` fixture and its `mess_manifest.csv` are retired.
- **No `format_el_long()` target.** The simulation emits the clean post-ingest
  state directly. Nothing in `R/` reads a file from `inst/extdata/`.
- **No claim to external validity.** This is a fixture for exercising code and
  rendering documents. Distributions are anchored to published sources where
  anchors exist (§7) and are otherwise modelling choices (§10). Row counts buy
  view coverage and performance signal, not inferential credibility. Any figure
  produced from this data and shown outside the repo must say so.

---

## 1. Outputs

Three clean R dataframes from one simulated population. All ship in `data/`.

| Object | Grain | Rows (0.1.0 run) | Cols | Contents |
|---|---|---|---|---|
| `el_long_sim` | one row per `STUDENT_ID × YEAR × COMPONENT` | `[FILL IN after regeneration]` (annual rows shrink under §4.4; alternate rows stay at 6,595) | 6 | **the base table for all analysis**: every reported ELP component as a row |
| `el_roster_sim` | one row per `STUDENT_ID × YEAR` | 461,040 | 52 | the **roster / enrollment frame**: grade, EL status, demographics, program fields, outcomes — no measurement columns |
| `academic_long_sim` | one row per `STUDENT_ID × YEAR × SUBJECT` | 568,202 | 7 | content-assessment scores |

This is the three-table analysis model the package is built on:

- **Base table** (`el_long_sim`). The base for all analysis: every ELP
  measurement is a row here and nowhere else. Which component is the
  **headline** is one spec field (`spec$elp$headline`, `overall` here), and
  every stage reads it through one accessor — `elp_headline(data)` filters
  the base table to the headline component on the annual form, joins the
  roster keys, and bands the level per `spec$elp$bands`. Prep 2's initial-ELP
  bars, Check 1's disaggregation, and Check 3's x-axis are the banded
  headline; Checks 3–4 run on the headline scale score. **No `compute_*()`
  behind Prep 2 or Checks 1–4 may name a component**: everything routes
  through the accessor and the spec. (Type-gated additional checks are the
  recorded exception; this Type 1 sim has none.)
- **Roster** (`el_roster_sim`). One row per student-year, and the only place
  students are counted. Grade, `EL_STATUS`, demographics, program fields,
  outcomes, `ELP_FORM` — and **no measurement columns**. It exists because
  the base table only has rows where a test happened: enrolled-but-untested
  years (participation dips, 2020–21), former ELs, and never-ELs live only
  here, and dissolving the frame into the base table turns the 2021
  participation dip into fake attrition and erases Check 3's reference
  groups.
- Below the headline, the base table serves Prep 1, exit-rule evaluation, and
  the Type-gated / domain-level work (the "which domain blocks exit"
  question). `COMPONENT` is a row key over a controlled vocabulary (§4.1):
  WIDA fills eight rows per tested student-year; an instrument reporting four
  domains and a determination fills five; the table's shape never changes.
- **Academic.** `student_results_long` in stateSummative's sense — one row per
  student-year-subject — in this package's lowercase naming. Consumed by Check 3
  only.

This is the same atomic-layer-plus-resolver split as `stateSpecs`' `tests` +
`map`, and the same move `stateSummative` made with `SUBJECT`: the thing that
varies across instruments is a row key, not a set of columns.

**Counting rules, and the failures they prevent.**

- `el_roster_sim` is the denominator for anything counted per student-year.
  `academic_long_sim` is never used to count students (it double-counts every
  tested student, once per subject), and the base table `el_long_sim` is never
  used to count students (it counts a WIDA-tested student eight times).
- The join key everywhere is `STUDENT_ID × YEAR`. Counting joins go roster
  → academic (left). Check 3's universe is the academic table joined to roster
  status (academic ⟕ roster), so `former` and `never` survive as reference groups;
  a roster ⟕ academic join silently drops them because they have no ELP rows.
- The sim's own package call is the separate-files delivery form:

  ```r
  as_el_data(el_long_sim, roster = el_roster_sim, academic = academic_long_sim)
  ```

  A fully merged single long file — roster attributes repeated on every
  component row, SGP LONG_DATA style — is an accepted **delivery** form the
  adapter splits on intake; the analysis model stays base + roster + academic.
  No accessor pivots anything. The wide-on-the-spine layout described in 0.1.0
  no longer exists in this data; the package accepts it only as a delivery form
  exercised by a hand-built fixture.

---

## 2. Seeds and parameters

### 2.1 Determinism

`set.seed(20260727)` at the top of the generator, once. Rerunning
`Rscript data-raw/el_long_sim.R` on R 4.3.3 reproduces all three objects bit for
bit. Anyone can rebuild the shipped data; the generator is the artifact of
record, not the `.rda`.

Two things break determinism and must not be introduced: sampling before the
seed is set, and any use of `parallel::` or a hash-ordered structure in the
generation path.

**Version 0.2.0 is RNG-neutral with respect to 0.1.0.** Every edit is applied
after the random draw it could have displaced (§4.4 filters after the scale
draw; §5.1 adds a deterministic column; §5.4 relabels). Roster rows, academic
rows, monitoring counts, status counts by year, and the cohort exit table are
expected to reproduce exactly. The values that change are the components row
set, the band distribution (both from §4.4), and academic scores and levels on
`former` rows (§5.3) plus levels exactly at a cut (§5.4).

### 2.2 Parameter block

All tunable quantities live in one `P` list at the top. Nothing below reads a
magic number.

| Parameter | Value | Meaning |
|---|---|---|
| `years` | `2015:2025` | spring academic years; 2015 = SY 2014-15 |
| `k_cohort_n` | 2400 | new EL identifications entering at grade K each year; env override `STATEELP_K_COHORT_N` |
| `never_el_n` | 34000 | distinct never-EL students carried (§2.3); env override `STATEELP_NEVER_EL_N` |
| `never_sample_frac` | 0.10 | sampling fraction those students represent |
| `n_districts` | 12 | uneven sizes, so `DISTRICT_SIZE_BAND` varies |
| `min_cell` | 10 | suppression minimum, mirrors `el_spec$policy$suppression$min_cell` |
| `max_years` | 6 | max timeline; mirrors `el_spec$policy$timelines$max_years_to_exit` |
| `academic_cuts` | `c(700, 735, 750, 790)` | minimum scale score of academic levels 2–5; level 4 is proficient |
| `prof_cut` | 750 | `= academic_cuts[3]`, asserted; the academic score model's center and Check 3's reference line |

**Span rationale.** `max_years = 6` requires at least seven years of follow-up.
2015–2025 gives five K cohorts with full follow-up (K-2015 … K-2019). A shorter
span supports one cohort and the life table runs on tens of students.

**Scale rationale.** `k_cohort_n` is set by the binding constraint, which is the
Check 2 flows table: it splits **one** K cohort by each disaggregation variable
and needs `min_cell` on both the exited and still-in-status sides. The smallest
`HOME_LANGUAGE` category is ~3% and the smallest `SPED_CLASSIFICATION` ~0.9% of a
cohort. At 2400 every category clears; below roughly 1800 `SPED_CLASSIFICATION`
starts suppressing. If Check 1 and Check 2 are changed to **pool** the five
followable K cohorts, `k_cohort_n` can drop to ~500 for the same power and the
fixture shrinks by roughly a factor of four. `[PROPOSED — CONFIRM]`

**Fixture-sized build.** `STATEELP_K_COHORT_N=300 STATEELP_NEVER_EL_N=4250`
keeps the never/ever ratio (and so the implied EL share) and yields a roster of
roughly 60k rows `[FILL IN after regeneration]`. It will not populate every
disaggregation, and it is not supposed to. **A session must not lower `min_cell`
or pool grades to make cells appear in the fixture build.**

### 2.3 The never-EL decision — weighted sample, not full population

`el_roster_sim` carries **a 10% weighted sample of never-EL students**, not the
full non-EL population. 34,000 distinct never-EL students, ~10,300 enrolled in
any given year, each row carrying `sample_weight = 1 / never_sample_frac = 10`.
Ever-EL rows carry `sample_weight = 1`.

Carrying the full non-EL population would multiply the fixture several times over
for what is only a reference distribution in Prep 2 and Check 3.

**The rule this creates, and the failure it prevents.** In SY 2023 the roster
holds 24,762 current EL, 11,995 former EL, and 10,265 never-EL rows.

- Unweighted, current ELs are **52.7%** of enrollment.
- Weighted, current ELs are **17.8%** of enrollment.

So: **any statewide share must weight; any within-EL statistic must not.** A
`compute_*()` that reports EL % of total enrollment without `SAMPLE_WEIGHT`
reports 52.7%, and nobody reading it will believe the rest of the document.
Check 3's never-EL column reports its `n` weighted or labels the group as a
weighted sample; a test asserts no unweighted never-EL total appears in any
output. Conversely, weighting a duration or exit statistic inflates nothing —
those are ever-EL-only, all weights are 1 — but weighting them is still wrong in
principle and will break the moment the sampling design changes.

**Two independent knobs.** `never_sample_frac` sets the *implied* statewide EL
share and costs nothing. `never_el_n` sets how many distinct students back the
reference distribution and costs rows. To target a ~6% EL share (roughly New
Jersey), set `never_sample_frac ≈ 0.027` and leave `never_el_n` alone.

The current 17.8% is `[DERIVED — VERIFY]` — it was not calibrated to any state.
**Open: confirm the target EL share.** Prep 2 reports this number directly.

**The enrollment denominator is not a population.** Never-ELs are a sample and
former ELs leave the panel four years after exit (§3.3). Prep 2's "EL % of total
enrollment" is therefore illustrative on this data and its caption says so; for a
real delivery containing only ever-EL students the slot degrades to `NULL`.

---

## 3. The roster — `el_roster_sim`

### 3.1 Grain and keys

One row per `STUDENT_ID × YEAR`. Uniqueness on that pair is asserted by
the generator and must stay asserted; every count in Prep 2 and both Check 1
denominators assume it.

`STUDENT_ID` is `S######` for ever-EL students and `N######` for never-EL.
Do not parse the prefix in analysis code — use `EVER_EL`. The prefix is a
generation artifact and will not survive contact with real data.

### 3.2 Population construction

Three sub-populations, one roster:

1. **Standing stock.** Ever-EL students already in status when the panel opens,
   with entry years 2009–2014. Their trajectories are simulated from their true
   entry year and then clipped to the panel window, so their exits fall where
   they actually fall. **Rule: pre-panel entrants carry `left_truncated = TRUE`
   and `cohort_label = NA`.** Cohort analyses filter on `!left_truncated`. A
   session that includes them in a K cohort measures duration from an
   unobserved start and understates it by up to six years. Some of them exited
   before 2015 and appear only as `former` monitoring rows; `EXIT_OBSERVED` is
   never 1 for them.
2. **New identifications**, `k_cohort_n / 0.55` per year, entry grade drawn from
   an entry-grade distribution that is K-dominant (~55%) with a flat 3–7% tail
   across 1–8 and a thin secondary-newcomer tail at 9–11.
3. **Never-EL sample**, §2.3.

Students are assigned one district and one school for their whole span. **There
is no mobility and no grade retention.** Anything about transfers, school
changes, or repeated grades cannot be studied in this data and must not be
simulated in a `.qmd`.

### 3.3 EL status, exit, and monitoring

`EL_STATUS` has **three** levels — `current`, `former`, `never` — and keeps them,
because every reference-group filter depends on it.

The four federal monitoring years ride alongside in `EL_STATUS_DETAIL`
(`monitoring_1` … `monitoring_4`) and `EL_MONITOR_YEAR` (1–4, `NA` otherwise).
Counts run 30,877 / 27,507 / 23,637 / 19,327. **After the fourth monitoring year
the student leaves the panel.** A real delivery keeps former ELs indefinitely;
here "Former ELs" as a Check 3 reference group means former-within-four-years,
and Check 3's caption states it.

**The exit-year row is still `current`.** A student observed to meet the exit
criterion in year *t* has `el_status == "current"` and `exit_observed == 1L` in
year *t*, and `former` from *t+1*. This is the sim's convention and it is
**not** the package's: the package dates exit to the first year observed as
`former` (the deck's A–J convention, in which student E exits in year 4), so the
package's exit year equals the flagged row's `school_year + 1`. A test asserts
that relationship so the off-by-one is visible rather than silent. The sim's
`YEAR_IN_STATUS` and `EXIT_OBSERVED` are validation inputs for the package's own
derivations, never the source of them.

**Exit is absorbing.** Once `exit_observed == 1`, a student is an exiter in every
subsequent year regardless of later absence. Without this the cumulative exit
curve in Check 1 can decrease.

**Exit requires the annual form.** `exit_eligible` is `tested & elp_form ==
"annual" & headline >= threshold`. Alternate-form students therefore never exit
in this data and all become long-term ELs by construction; a `.qmd` must not
read that as a finding.

### 3.4 Derived variables carried on the roster

| Column | Derivation | Package treatment |
|---|---|---|
| `YEAR_IN_STATUS` | years since entry, `NA` unless `el_status == "current"`; for `LEFT_TRUNCATED` students this counts from the true pre-panel entry, which is **unobservable** in a real delivery | recomputed from observed transitions under the deck convention; sim value used to validate on `!left_truncated` rows only |
| `COHORT_START_YEAR` | entry year; `NA` for never-EL; populated for `LEFT_TRUNCATED` students with their true pre-panel entry year | never used to build cohorts — use `COHORT_LABEL` |
| `COHORT_LABEL` | `<entry_grade>-<entry_year>`, e.g. `K-2017`; `NA` when `LEFT_TRUNCATED` or never-EL | the cohort key |
| `ENTRY_GRADE` | grade of first EL year; for `LEFT_TRUNCATED` students the true pre-panel grade | used with `COHORT_LABEL`; the package's own "first observed grade" differs for left-truncated rows by design |
| `LEFT_TRUNCATED` | entry year precedes the panel; `FALSE` for never-EL | Check 1 cohorts exclude by construction; Check 2's initial-ELP rows label them |
| `EXIT_OBSERVED` | 1 in the year the exit criterion is first met (still a `current` row) | validation input; see §3.3 |
| `INITIAL_ELP` | banded headline at the first `current` year with a valid value; source `first_annual` per spec; `NA` for alternate-form students and for never-EL | "first observed" is the operable definition for left-truncated students too |
| `FRL` | `frpl_status %in% c("F","R")` | |
| `DISTRICT_SIZE_BAND` | tertiles (`small` / `medium` / `large`) of mean annual current-EL count per district | validation input only; the package bands districts from `spec$policy$district_size_bands` (deck default: `<100`, `101–999`, `1,000+`) or an empirical method, never from this column `[PROPOSED — CONFIRM]` |
| `NEWCOMER_FLAG` | immigrant and within two years of EL entry | |
| `SAMPLE_WEIGHT` | §2.3 | |

The four nested exit indicators (`Exit_Ever ⊇ Exit_Max ⊇ Exit_OnTime`, plus
`No_Exit`) and `censored` are **not** stored. They are computed by Checks 1–2
from `EL_STATUS` transitions and `el_spec`, because `Exit_Max` and `Exit_OnTime`
depend on spec fields that a state can change without regenerating data.

### 3.5 Column reference

Tier follows the V2 codebook's scheme: **R** required (Checks 1–2 cannot run
without it), **D** derived (a state may deliver it; the package revalidates),
**S** supplemental (enables a view; absent ⇒ `NULL` slot). "Reads" names the
steps that consume the column in the shell.

| Column | Type | Values | Tier | Reads | Notes |
|---|---|---|---|---|---|
| `STUDENT_ID` | chr | `S######` / `N######` | R | all | never parse the prefix |
| `YEAR` | int | 2015–2025 | R | all | spring year |
| `GRADE` | chr | `"K"`, `"1"`–`"12"` | R | all | HS grades individual, not a bucket |
| `DISTRICT_ID` | chr | `D01`–`D12` | S | P2, C2 | |
| `SCHOOL_ID` | chr | `D01-S03` | S | — | static per student; `SCHOOL_CODE`/`SCHOOL_NUMBER` are ReportR aliases of this column, so no separate within-district code ships |
| `GENDER` | chr | M / F / X | S | P2, C2 | X ≈ 0.3%, expected to suppress |
| `RACE_ETHNICITY` | chr | federal 7-category labels | D | P2, C2 | ReportR canonical categorical, derived from `HISPANIC` + the `RACE_*` flags |
| `HISPANIC` | chr | Y / N | S | — | |
| `RACE_AMERICAN_INDIAN`, `RACE_ASIAN`, `RACE_BLACK`, `RACE_PACIFIC`, `RACE_WHITE` | chr | Y / N | S | — | multiple may be Y |
| `COUNTRY_OF_BIRTH` | chr | 3-letter; `USA`; `XXX` for `oth` | S | — | |
| `IMMIGRANT_STATUS` | chr | Y / N | S | P2 | newcomer proxy |
| `FIRST_ENTRY_US_SCHOOL_DATE` | Date | | S | — | ever-EL only |
| `FRL_DETAIL` | chr | F / R / N | S | P2, C2 | |
| `FRL` | chr | Y / N | D | P2, C2 | |
| `SPED_CLASSIFICATION` | chr | SLD, SLI, OHI, AUT, MD, OTH, `NA` | S | P2, C2 | `NA` = not classified |
| `IEP` | chr | Y / N | S | P2, C2 | |
| `GIFTED_FLAG`, `MIGRANT_STATUS` | chr | Y / N | S | — | sparse |
| `HOME_LANGUAGE` | chr | spa, por, ara, guj, hat, cmn, kor, urd, tgl, oth; never-EL: eng, spa, oth | S | P2, C2 | lowercase 3-char |
| `SERVICE_MODEL` | chr | esl_pullout, esl_pushin, sheltered, bilingual_tw, bilingual_trans, newcomer_program, refused_services; `NA` never-EL | S | P2, C2 | static per student |
| `LEP_PROGRAM_START_DATE` | Date | Sept 15 of entry year | S | — | ever-EL only |
| `LEP_PROGRAM_COMPLETION_DATE` | Date | June 15 of the `EXIT_OBSERVED` year | S | — | **static per student**: present on every row of an exiter, including rows before the exit; validates timing, never the timing source |
| `TITLE3_LEP_FLAG` | chr | Y / N | S | — | Y iff current and not refused |
| `EL` | chr | Y / N | D | — | ReportR canonical binary: `EL_STATUS == "current"`; ships for cross-package flows |
| `EVER_EL` | chr | Y / N | D | all | |
| `EL_STATUS` | chr | current / former / never | R | all | §3.3 |
| `EL_STATUS_DETAIL` | chr | current / never / monitoring_1–4 | D | P2 (proposed) | sub-state of former |
| `EL_MONITOR_YEAR` | int | 1–4 / `NA` | D | — | |
| `EXIT_OBSERVED` | int | 0 / 1 | D | validation | §3.3 |
| `YEAR_IN_STATUS` | int | 1… / `NA` | D | validation | §3.4 |
| `ENTRY_GRADE` | chr | `"K"`–`"11"` / `NA` | D | P2, C1 | §3.4 |
| `COHORT_START_YEAR` | int | | D | — | §3.4 |
| `COHORT_LABEL` | chr | `K-2017` … / `NA` | D | C1, C2 | |
| `LEFT_TRUNCATED` | lgl | | D | C1, C2 | |
| `INITIAL_ELP` | chr | headline band label / `NA` | D | P2, C1, C2 | |
| `NEWCOMER_FLAG` | chr | Y / N | D | P2 | |
| `SIFE_FLAG` | chr | Y / N | S | P2 | |
| `ELP_FORM` | chr | annual / alternate / `NA` | S | P2, C3 | gates the headline (§4.4); `NA` never-EL |
| `ELP_GROWTH_TARGET_MET` | chr | Y / N / `NA` | S | — | the state's ELP growth measure; out of scope for the five steps |
| `GRAD_4YR_STATUS`, `GRAD_5YR_STATUS` | chr | Y / N / `NA` | S | — | grade-12 rows only |
| `ELA_GRAD_PATHWAY`, `MATH_GRAD_PATHWAY` | chr | assessment / portfolio / substitute | S | — | grade-12 rows only |
| `SCHOOL_EXIT_DATE` | Date | June 20 | S | — | grade-12 rows only |
| `SEAL_OF_BILITERACY` | chr | Y / N | S | — | grade-12 rows only |
| `SEAL_LANGUAGES` | chr | home-language code | S | — | awardees only |
| `DISTRICT_SIZE_BAND` | chr | small / medium / large | D | validation | §3.4 |
| `SAMPLE_WEIGHT` | dbl | 1 / 10 | R (for this sim) | P2, C3 | §2.3 |

**Minimum viable delivery.** A roster of four columns (`STUDENT_ID`,
`YEAR`, `GRADE`, `EL_STATUS`) plus base-table rows for at least the
headline component run Checks 1–2 in full — the package bands the headline
level via the spec. The academic table unlocks Check 3.
Strongly recommended in a first delivery: `DISTRICT_ID` and the disaggregation
set (`HOME_LANGUAGE`, `FRL_DETAIL`, `SPED_CLASSIFICATION`, `GENDER`,
`SERVICE_MODEL`); without them "resist the urge to average" can only be honored
partially.

### 3.6 Structural missingness on the roster

These `NA` patterns are by design, not data problems. A session must not "fix"
them.

| Pattern | Rule |
|---|---|
| Never-EL rows | every EL-program field is `NA` (`ENTRY_GRADE`, `cohort_*`, `YEAR_IN_STATUS`, `INITIAL_ELP`, `SERVICE_MODEL`, LEP dates, `ELP_FORM`, `elp_*`); `left_truncated = FALSE`; `exit_observed = 0`; `ever_el = "N"` |
| Former rows | no base-table rows exist — formers do not test (§4.4); `YEAR_IN_STATUS` is `NA`; `exit_observed = 0` |
| Current untested rows | no annual base-table rows when the student was not tested that year (participation, §6); the student-year exists on the roster only |
| Outcome columns | grade-12 rows only; `GRAD_5YR_STATUS` `NA` where not yet observable |
| `ELP_GROWTH_TARGET_MET` | `NA` unless current with a valid headline this year and last |
| `LEP_PROGRAM_COMPLETION_DATE` | exiters only, all rows of the student |

---

## 4. The base table — `el_long_sim`

### 4.1 Vocabulary and shape

```
student_id | school_year | component | form | scale_score | level
```

`COMPONENT` is a **row key over a controlled vocabulary**, not a set of
instrument-named columns. The vocabulary is the Report Views doc's own list:
`listening`, `speaking`, `reading`, `writing`, `oral` (L+S), `written` (R+W),
`comprehension` (L+R), `expressive` (W+S), `overall`, plus `determination` for
profile-based headlines. The SIM data is WIDA-shaped and carries eight;
`expressive` and `determination` are admitted by the vocabulary and appear in no
SIM row. **`COMPONENT` is stored as character**, not factor: the spec supplies
display order and labels, and a factor would coerce on `bind_rows()` with a
real delivery's character column.

Instrument-native names map onto the vocabulary **in the spec**, never in the
data: WIDA's "Literacy" is `written`, via `native_name` on that component. A
session must not add `literacy` or `composite_overall` to the vocabulary; that
is the mapping working as intended. The key column is named `COMPONENT`. The
academic table's key is `SUBJECT`; the two must never share a key name, or
WIDA's `reading` domain and an academic `Reading` subject conflate in a join.

**The table's shape never changes.** WIDA fills eight rows per tested
student-year. An instrument reporting four domain scale scores with levels, an
overall scale score with no level, and a categorical determination with no scale
fills six. A component that reports a level but no scale score has `NA` in
`SCALE_SCORE`; a component that reports a scale score but no level has `NA` in
`ACHIEVEMENT_LEVEL`. Both cases are legitimate rows, and the SIM data exercises the first
(§4.4).

**`ELP_FORM`** is the sixth column, carried because the alternate form reports on a
different scale (§4.4) and a domain-level analysis that forgot to join the roster
would otherwise average alternate ordinals with annual decimals. It is
functionally dependent on the student-year — a student takes one form in a year —
so it is redundant with the roster's `ELP_FORM`, and the generator asserts the two
agree on every row. For an instrument with no alternate form it is constant
`"annual"`. `[PROPOSED — CONFIRM]` — the alternative is to drop it and gate every
base-table read through `roster$ELP_FORM`.

### 4.2 Column reference

| Column | Type | Values | Notes |
|---|---|---|---|
| `STUDENT_ID` | chr | | key |
| `YEAR` | int | | key |
| `COMPONENT` | chr | vocabulary (§4.1) | key; SIM carries the eight WIDA components |
| `ELP_FORM` | chr | annual / alternate | equals the roster's `ELP_FORM` for the student-year, asserted (same name on both tables since 0.4.0) |
| `SCALE_SCORE` | dbl | ≈100–600; `NA` on alternate rows | `round(180 + 42·level + 7.5·grade_n + N(0, 9))` — `[DERIVED — VERIFY]` synthetic map |
| `ACHIEVEMENT_LEVEL` | dbl | annual: 1.0–6.0 in tenths; alternate: ordinal 1–6 | numeric always: the instrument's native value where one exists, an ordinal index otherwise, with labels in the spec |

**Row population.** A row exists for (ever-EL student) × (year with
`el_status == "current"`) × (tested that year) × (component with a non-`NA`
value). So a student-year may carry fewer than eight rows: waived domains are
absent rows and every composite drawing on them is absent (§4.4); alternate
takers carry the four domains and the headline only. There are no rows for
`former` or `never` student-years and no rows for untested years.

### 4.3 Composites and the headline accessor

Component-first: the model generates the four domain levels (§4.6) and builds
composites up from them by iterating each component's `from` weights; it never
generates an overall score directly.

```
oral          = 0.50 listening + 0.50 speaking
written       = 0.50 reading   + 0.50 writing
comprehension = 0.30 listening + 0.70 reading
overall       = 0.15 listening + 0.15 speaking + 0.35 reading + 0.35 writing
```

**A composite is `NA` whenever any contributing component is `NA`.** This is
computed from the weight names, so no code names a domain to know that a waived
listening score invalidates both `oral` and `comprehension`.

`from` weights are informational for reporting: Prep 1 renders them and a later
QC check may recompute and compare, but reported values are the vendor's.

**Headline resolution.** `spec$elp$headline` is `overall`. The package
accessor `elp_headline(data)` filters the base table to the headline
component on the annual form, joins the roster keys, and bins the level per
§4.5, returning `STUDENT_ID`, `YEAR`, `ACHIEVEMENT_LEVEL`, `SCALE_SCORE`,
`ELP_BAND` — one filter, one join, nothing pivots, nothing cached. The generator
asserts internally that this resolution is well-defined and that the shipped
roster carries no headline columns.

### 4.4 Population rule, alternate form, and waivers

**Component rows exist only for `current` student-years.** Former ELs do not
take the ELP assessment; they have no base-table rows at all.
*This is the 0.2.0 fix.* The 0.1.0 generator kept the acquisition curve running
through the four monitoring years and populated component rows and the headline
on `former` rows, which is why 0.1.0's band distribution had 86,258 rows in
`5.0+` — more than any other band — from K entrants who leave status at 4.5. A
Check 3 built on that data places one student in `5.0+` and in "Former ELs" at
once. The fix is a status filter applied after the scale draw, so the RNG stream
is unchanged (§2.1). The generator now asserts that no `former` student-year has
a component row.

`ELP_FORM` on the roster is `annual`, `alternate`, or `NA` (never-EL).

- **Alternate** (~1% of ELs, concentrated in MD/AUT classifications) reports
  P1–P3 / A1–A3, stored as ordinal 1–6 with **no scale score** — the
  level-without-scale case in the vocabulary. 6,595 component rows (unchanged by
  the fix: alternate students never exit, §3.3).
- **Waivers** (~2% of ELs, a documented sensory disability) mean one or more
  domains were not administered. Those domain rows are absent and every composite
  drawing on them is absent. No `waived` column ships; absence *is* the signal.
  A validator must not read a missing domain row as a data problem, and any exit
  rule evaluated over domain levels must treat an absent row as exempt (ELPA21's
  "non-exempt domains" arriving in WIDA data).

**Alternate levels never enter the headline.** They are a different scale;
banding them on the annual `1.0–1.9 … 5.0+` scheme silently mixes two
measurement systems. Alternate student-years never resolve through
`elp_headline()` (its annual-form filter), carry `initial_elp = NA` with the
roster's `ELP_FORM` saying why, and are excluded from the exit rule. Check 3
reports them as a group (counted in `n`) and excludes them from band
distributions unless the spec declares an alternate `level_scale`. This is
asserted.

### 4.5 Bands

From `spec$elp$bands`, never a literal:
breaks `c(1, 2, 3, 4, 4.5, 5, 7)`, labels `1.0-1.9 / 2.0-2.9 / 3.0-3.9 /
4.0-4.4 / 4.5-4.9 / 5.0+`. Left-closed. For any instrument without a declared
`bands` entry the package default is one band per level.

The 4.5 boundary **is** the exit threshold, so `4.5-4.9` and `5.0+` contain only
students in their exit year. For K entrants, any band at 4.0 or above is
near-empty by construction (`theta_1 ≤ 3.9`); suppression there is expected, not
a sizing failure.

Headline distribution across the six bands (via `elp_headline()`): `[FILL IN after regeneration]` (the
0.1.0 values 31,480 / 58,860 / 67,380 / 35,360 / 40,907 / 86,258 included
`former` rows and are wrong under §4.4; the top two bands will shrink to
exit-year rows).

### 4.6 Acquisition model

```
theta_1 ~ N(1.9 + 0.075 * entry_grade_n, 0.62), clamped to [1.0, 3.9]
plateau ~ Bernoulli(0.14 + 0.13*SWD + 0.10*SIFE + 0.05*(theta_1 < 1.8))
rate    ~ lognormal(log 0.055, 0.45) if plateau, else lognormal(log 0.335, 0.42)
theta_t  = 6 - (6 - theta_1) * exp(-rate * (t - 1)) + N(0, 0.17), clamped [1, 6]

level_d  = round(theta_t + offset_d + N(0, 0.45), 1), clamped [1, 6]
           offsets: listening +0.35, speaking +0.20, reading -0.15, writing -0.30
scale    = round(180 + 42 * level + 7.5 * grade_n + N(0, 9))
```

`theta_1` is capped at **3.9**: a student screening at or above the exit
threshold would not be identified as an EL at all, and `el_spec`'s on-time rule
defines initial bands only up to 3.9.

The `plateau` subgroup is what produces the long-term-EL tail that Check 2 is
about. Without it, "who gets stuck" has no signal to find.

### 4.7 Exit

`exit_spec` is `list(rule = "overall level >= 4.5", component = "overall",
threshold = 4.5)` — read as component + threshold, not parsed from the string.
Exit requires the student to have been tested, on the annual form, with a
non-`NA` headline. The 2021 participation dip therefore genuinely depresses
reclassification rather than being cosmetic.

Resulting duration for full-follow-up K cohorts (unchanged by 0.2.0):

| Cohort | n | % exit ≤5 yrs | % exit ≤6 yrs | % never |
|---|---|---|---|---|
| K-2015 | 2,373 | 59.8 | 69.6 | 17.2 |
| K-2016 | 2,360 | 57.3 | 66.8 | 18.8 |
| K-2017 | 2,411 | 54.4 | 68.3 | 20.3 |
| K-2018 | 2,403 | 57.1 | 69.8 | 21.7 |
| K-2019 | 2,374 | 59.4 | 69.9 | 25.4 |

These percentages use the sim's last-current-year dating. Under the package's
first-former dating every "≤ k years" figure shifts to "≤ k+1 years"; the Check 1
fixture tests that shift explicitly.

---

## 5. The academic table — `academic_long_sim`

### 5.1 Columns and coverage

```
student_id | school_year | grade | subject | scale_score | perf_level | alt_perf_level
```

| Column | Type | Values | Notes |
|---|---|---|---|
| `STUDENT_ID` | chr | | key |
| `YEAR` | int | | key; **no 2020 rows** (§6) |
| `GRADE` | chr | `"3"`–`"11"` | *added in 0.2.0*; equals the roster's grade for the student-year, asserted. Carried because `student_results_long` carries it, because an EOC delivery reports a real tested grade on the row, and because a validator cannot check grade agreement on a column that isn't there |
| `SUBJECT` | chr | ELA / Math | key; matches `spec$academic` keys |
| `SCALE_SCORE` | dbl | ≈650–850; `NA` on alternate rows | |
| `ACHIEVEMENT_LEVEL` | int | 1–5; `NA` on alternate rows | *integer in 0.2.0*, was character; level 4 is proficient; labels live in the spec |
| `ACHIEVEMENT_LEVEL_ALT` | int | 1–4; `NA` otherwise | *integer in 0.2.0* |

Grades **3–11**, subjects **ELA** and **Math**. K–2 and grade 12 are genuinely
untested and have no rows. **Check 3 must drive its grade loop from
`spec$academic$<subject>$grades`, not from the observed grade span**, or it
renders empty panels for K, 1, 2, and 12. Never-EL and former students have
academic rows (they are the reference groups); alternate ELP-form students have
rows with `scale_score = NA` and `ACHIEVEMENT_LEVEL_ALT` populated. Alternate academic
participation is tied to the ELP alternate form — never-EL SWDs do not take the
alternate academic assessment in this data.

### 5.2 Score model, conditional on ELP level and status

```
scale_score = prof_cut
            + 4 * (grade_n - 6)              # grade drift
            + status_shift                   # current -34, former +6, never 0
            + elp_shift                      # (headline - 3.0) * 9; 0 when there is no headline (formers)
            + subj_shift                     # Math +3, ELA 0
            + swd_shift                      # SWD -18
            + N(0, 33)
```

This produces the Data View 3 gradient: score rises monotonically with ELP band
among current ELs, exited ELs land just above never-ELs, and the proficiency cut
sits inside the distribution rather than at its edge.

### 5.3 Known simplification — the exited-EL advantage is imposed

Former ELs are not tested for ELP, so `elp_level` is `NA` and `elp_shift` is 0
for them. Their position above never-ELs comes entirely from the flat
`status_shift = +6`. (In 0.1.0 this sentence was false: because of the §4.4 bug,
formers also received `elp_shift` from a level that kept rising after exit.
Their scores drop under 0.2.0.)

The earlier ELReportR sim derived that advantage **endogenously**, by correlating
academic ability with ELP growth rate, so faster exiters were also stronger
students and the RFEP advantage emerged rather than being asserted. That is the
better model and this one is a regression from it. `[PROPOSED — CONFIRM]`:
restore the latent-ability correlation and drop the flat `+6`.

Until then, **Check 3 must not be used to make any claim about whether exiting
causes academic gains**, because the answer is a parameter.

### 5.4 The cut convention

`academic_cuts = c(700, 735, 750, 790)` are the **minimum** scale scores of
levels 2–5, so `perf_level = findInterval(scale_score, academic_cuts) + 1L` and a
score of exactly 750 is level 4 (proficient). The 0.1.0 generator used
`cut(..., right = TRUE)`, under which 750 was level 3 and `prof_cut` was the
*highest failing* score, contradicting its role as Check 3's reference line ("the
band whose median is closest to the proficiency cut"). ReportR derives levels as
`sum(score > cuts) + 1`, which is the 0.1.0 reading; the two packages disagree at
the boundary and the family needs one convention. That is a stateSpecs decision
(the MS TAC plan's "cut score rounding consistency" item is the same question);
stateELP takes `>=` and declares it in the SIM spec's `academic` block.
`[PROPOSED — CONFIRM]`

The V2 codebook's `700 / 725 / 750 / 790` was a documentation error; the
generator has always used 735.

---

## 6. Calendar effects

| Year | ELP participation | Academic participation |
|---|---|---|
| 2015–2019 | 0.97 | 0.96 |
| 2020 | 0.93 | **0.00** |
| 2021 | **0.81** | 0.72 |
| 2022 | 0.97 | 0.94 |
| 2023–2025 | 0.97 | 0.96 |

Spring 2020 produced **no state summative anywhere**, so `academic_long_sim` has
no 2020 rows at all — the year is absent from the table, not present-and-empty.
ACCESS 2020 largely completed in its January–February window and is present at
reduced participation.

Any year-over-year academic comparison must handle a missing year, not an empty
one. A `.qmd` that assumes contiguous years drops or misaligns everything after
2019. Check 3's 2020 panel degrades to `NULL` with a note.

An untested year is **not** a disappearance: `EL_STATUS` is carried on every
roster row whether or not the student tested, so nobody enters the life table's
column C in 2020 or 2021 for lack of a test. The life table needs no gap-year
rule; the only place the participation dips show is in the coverage table and in
the exit rate (§4.7).

---

## 7. Calibration anchors

Where a distribution is anchored to a published source, that source is named
here. Everything else is §10.

| Quantity | Anchor |
|---|---|
| ELP participation 2020-21 / 2021-22 | ~81% / ~97%, OELA Title III Biennial Report to Congress, SY 2020–22 |
| Four monitoring years after exit | OELA Exhibit 3.12 |
| LIEP / service-model taxonomy | OELA Exhibit 4.1 (dual language / two-way, transitional bilingual, content-based integrated ESL, ESL/ELD, newcomer, other) |
| ELs with disabilities ≈ 15% | OELA identified-ELs-with-disabilities exhibits |
| Home languages present | OELA most-commonly-spoken-languages exhibits |
| Duration shape | New Jersey, 15,048 ELs (21.8%) not proficient within five years, SY 2021–22. **Denominator includes students with fewer than five years in status**, so this is a plausibility direction, not a target to reproduce. |

---

## 8. Guardrails the generator asserts

These run on every regeneration and print PASS/FAIL. A FAIL means the shipped
data is wrong, not that a threshold needs relaxing.

1. Roster unique on `STUDENT_ID × YEAR`.
2. Components unique on `STUDENT_ID × YEAR × COMPONENT`.
3. Academic unique on `STUDENT_ID × YEAR × SUBJECT`.
4. Internal headline resolution is well-defined (the headline component row
   equals the generator's working headline wherever non-`NA`).
5. The shipped roster carries no headline columns, in either naming case.
6. Composites reproduce the spec `from` weights, and `NA` propagates.
7. No alternate-form student-year resolves an internal headline.
8. No `elp_band` outside the spec labels.
9. Every Check 2 disaggregation clears `min_cell` on both sides of the exit split,
   excluding the residual missing bucket, `sex = "X"`, and initial bands ≥ 4.0.
10. No component name appears as a string literal below the spec block.
11. *(0.2.0)* No component row belongs to a `former` or `never` student-year;
    equivalently, the internal headline is `NA` on every non-current student-year.
12. *(0.2.0)* Base-table `ELP_FORM` equals the roster's on every row (same name on both tables since 0.4.0).
13. *(0.2.0)* Every academic row has a roster row, and its `GRADE` equals the
    roster's grade for that student-year.
14. *(0.2.0)* `academic_long_sim` has no 2020 rows.
15. *(0.2.0)* `COMPONENT` is character and every value is in the vocabulary.

**No mean or average time-in-status is computed anywhere in the generator**, and
none may be computed downstream. Time-in-status is censored; its mean is not
defined on this data.

---

## 9. Regeneration

```bash
Rscript data-raw/el_long_sim.R                                     # full scale
STATEELP_K_COHORT_N=300 STATEELP_NEVER_EL_N=4250 Rscript data-raw/el_long_sim.R   # fixture build
```

Writes `data/el_long_sim.rda` (the base table), `data/el_roster_sim.rda`, and
`data/academic_long_sim.rda`, all `compress = "xz"`. Runtime is a few minutes at
`k_cohort_n = 2400`. Each object carries `attr(, "sim_contract_version")`, and
`as_el_data()` reads it to label outputs.

Size: roughly 9 MB across the shipped objects at full scale
`[FILL IN after regeneration]`. That is above CRAN's 5 MB guidance, which is a
non-issue for a private GitHub repo and a blocker for submission. **Default:
ship full scale in the private repo**, because developing the steps against
suppressed cells hides rendering bugs behind blank tables; ship the fixture
build only if CRAN becomes a target. `[PROPOSED — CONFIRM]`

If `data-raw/el_long_sim.R` is missing, stop and say so rather than
re-simulating.

---

## 10. `[DERIVED — VERIFY]` register

Every quantity below is a modelling choice with no official source. Each is
marked in the generator at its point of use.

- Entry-grade distribution (K ≈ 55%, tail across 1–11).
- Home-language percentages. Chosen so each clears `min_cell` in one K cohort;
  the *set* is anchored, the *proportions* are not.
- SPED classification proportions; rare categories pooled into `OTH`.
- SIFE prevalence among secondary newcomers; alternate-form rate; waiver rate.
- The entire acquisition model in §4.6 — `theta_1` mean and SD, plateau
  probability and its modifiers, both rate lognormals, domain offsets, the
  level→scale map.
- The entire academic model in §5.2, including every shift coefficient, and the
  cuts `700 / 735 / 750 / 790` with the `>=` convention (§5.4).
- The exit threshold 4.5 on `overall`, and the six-year maximum timeline.
- Seal of Biliteracy award rate (~14% of eligible ever-ELs); graduation rates.
- District count, size weights, and the tertile banding.
- The implied statewide EL share of 17.8% (§2.3).
- The 0.4-level annual growth target behind `ELP_GROWTH_TARGET_MET`.

---

## 11. Open items

| # | Item | Default | Section |
|---|---|---|---|
| 1 | Target statewide EL share; set `never_sample_frac` accordingly | leave at 0.10 (17.8%) | §2.3 |
| 2 | Pool the five K cohorts in Checks 1–2, allowing `k_cohort_n ≈ 500` | keep single-cohort at 2400 | §2.2 |
| 3 | Restore the endogenous exited-EL advantage; drop flat `status_shift = +6` | keep the flat shift, do not interpret Check 3 causally | §5.3 |
| 4 | sixth base-table column carrying the form | **resolved by 0.4.0:** named `ELP_FORM` on both tables, redundant on purpose, asserted equal | §4.1 |
| 5 | Which build ships in `data/` | full scale (2400 / 34000); fixture build by env var | §9 |
| 6 | `GRADE` required on the academic table | **decided 2026-08-26:** required; a delivery without it is rejected by the validator, there is no fill-from-roster fallback, and the roster-agreement check always runs | §5.1 |
| 7 | Cut convention: cuts are level minimums, `>=` | adopt; refer the ReportR `>` discrepancy to stateSpecs | §5.4 |
| 8 | District size bands for Prep 2 and Check 2 | spec-declared deck breaks; an empirical-quantile method as an alternative; never the sim's `DISTRICT_SIZE_BAND` column | §3.4 |
| 9 | Retire the separate codebook in favor of §3.5 / §4.2 / §5.1 | retire; `R/data.R` mirrors these tables and a test asserts the column sets agree | §0 |

Resolved in this version: 0.1.0's item 5 (stored wide vs long). Components are
stored long; there is no wide layout and no pivot.

---

## 12. Change log

| Version | Date | Change |
|---|---|---|
| 0.1.0 | 2026-08-25 | Initial contract. Two-table output, component vocabulary, weighted never-EL sample, COVID calendar, five full-follow-up K cohorts. |
| 0.4.0 | 2026-09-16 | ReportR naming conformance: ALL-CAPS columns per NOTES-student_results_long, `STUDENT_ID × YEAR` keys, canonical `GRADE`/`SUBJECT`/`SCALE_SCORE`/`ACHIEVEMENT_LEVEL`/`SCHOOL_ID`/`DISTRICT_ID`/`GENDER`/`RACE_ETHNICITY` (new, derived)/`FRL`/`FRL_DETAIL`/`EL` (new, derived)/`IEP`; `school_code` dropped (alias collision); academic table = `student_results_long` + `ACHIEVEMENT_LEVEL_ALT`; values unchanged; RNG-neutral. Roster 52 cols. |
| 0.3.0 | 2026-09-16 | Primacy flip at Nathan's direction: the component-long table is the base for all analysis and takes the name `el_long_sim`; the spine becomes `el_roster_sim` (51 cols) and sheds `elp_level`/`elp_scale`/`elp_band`; headline resolved only via `elp_headline()`; a fully merged single long file accepted as a delivery form, split on intake; generator assertions updated. RNG-neutral. |
| 0.2.0 | 2026-08-26 | Three-table output with the components table stored long (reverses 0.1.0 item 5). Component rows and the headline restricted to `current` student-years (fixes formers carrying ELP levels through monitoring years). `COMPONENT` stored as character. `GRADE` added to the academic table; `ACHIEVEMENT_LEVEL` and `ACHIEVEMENT_LEVEL_ALT` integer; cuts read as level minimums with `>=`. Env-var overrides for the fixture build. Column references added (§3.5, §4.2, §5.1) and the separate codebook retired. Five new generator assertions. |
