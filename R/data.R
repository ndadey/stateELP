# Documentation for the three shipped simulated objects.
#
# The codebook is sim_contract.md 3.5 / 4.2 / 5.1 -- there is no separate
# codebook document. This file mirrors those tables and test-data.R asserts
# that the contract, this file, and the shipped objects name the same columns,
# so a column added to the generator without being documented fails the build.
#
# Column names are ALL CAPS per ReportR's student_results_long conventions
# (sim_contract.md 0.4.0). Values are unchanged: codes such as `current`,
# `annual`, `spa` and the COMPONENT / SUBJECT keys stay as they were.

#' Simulated ELP base table: one row per student-year-component
#'
#' **The base table for all analysis.** Every ELP measurement is a row here and
#' nowhere else. `COMPONENT` is a row key over a controlled vocabulary, not a
#' set of instrument-named columns, so an instrument reporting four domains and
#' a categorical determination fills the same table shape as WIDA's eight. See
#' [el_component_vocabulary()].
#'
#' @section The headline:
#' Which component is the headline is one spec field (`spec$elp$headline`).
#' Every stage reads it through [elp_headline()] -- this table filtered to the
#' headline component on the annual form, joined to the roster keys, and
#' banded per `spec$elp$bands`. No `compute_*()` behind Prep 2 or Checks 1-4
#' names a component; only Prep 1, exit-rule work, and the Type-gated
#' Additional checks read below the headline.
#'
#' @section Counting:
#' Never count students here: it counts a WIDA-tested student eight times.
#' [el_roster_sim] is the only counting frame.
#'
#' @section Row population:
#' A row exists for an ever-EL student, in a year they were `current`, if they
#' were tested and the component has a non-`NA` value. So a student-year may
#' carry fewer than eight rows: a waived domain is an **absent row**, and every
#' composite drawing on it is absent too. Absence *is* the signal; there is no
#' waived column, and a validator must not read a missing domain row as a
#' data problem. There are no rows for former or never student-years, and none
#' for untested years.
#'
#' @format A data frame with 1,824,716 rows and 6 columns:
#' \describe{
#'   \item{STUDENT_ID}{Character; key.}
#'   \item{YEAR}{Integer spring year; key.}
#'   \item{COMPONENT}{Character; key. One of the controlled vocabulary; the sim
#'     carries the eight WIDA components. Stored as character, never a factor:
#'     the spec supplies display order, and a factor would coerce on
#'     `bind_rows()` with a real delivery. The academic table's key is
#'     `SUBJECT` -- the two must never share a key name, or a `reading` domain
#'     and a Reading subject conflate in a join.}
#'   \item{ELP_FORM}{Character annual / alternate; equals the roster's
#'     `ELP_FORM` for the student-year, asserted. Redundant on purpose, so a
#'     domain-level analysis that forgot the roster cannot average alternate
#'     ordinals with annual decimals.}
#'   \item{SCALE_SCORE}{Numeric, about 100-600; `NA` on alternate rows. A
#'     component reporting a level but no scale score is a legitimate row.}
#'   \item{ACHIEVEMENT_LEVEL}{Numeric always. Annual 1.0-6.0 in tenths;
#'     alternate an ordinal 1-6 on a different scale, with labels in the spec.
#'     6,595 alternate rows carry a level and no scale score. Vendor-reported:
#'     no level is ever derived from a scale score.}
#' }
#' @source `data-raw/el_long_sim.R` under `sim_contract.md` v0.4.0, seed
#'   20260727. Synthetic: no figure from it describes any real state.
#' @seealso [el_roster_sim], [academic_long_sim], [as_el_data()],
#'   [elp_headline()]
"el_long_sim"

#' Simulated EL roster: one row per student-year
#'
#' The **roster / enrollment frame** and the only place students are counted:
#' grade, EL status, demographics, program fields, outcomes, `ELP_FORM` -- and
#' **no measurement columns**. It exists because the base table only has rows
#' where a test happened: enrolled-but-untested years, former ELs and never-ELs
#' live only here.
#'
#' @section Counting:
#' This is the denominator for anything counted per student-year. Never count
#' students from [academic_long_sim] (it double-counts, once per subject) or
#' from [el_long_sim] (it counts a student up to eight times).
#'
#' @section Weighting:
#' Never-EL rows are a 10% sample carrying `SAMPLE_WEIGHT = 10`; ever-EL rows
#' carry 1. Every statewide share must weight and every within-EL statistic
#' must not. Unweighted, current ELs are 52.7% of SY2023 enrollment; weighted,
#' 17.8%.
#'
#' @section Structural missingness:
#' By design, not a data problem (contract 3.6). Never-EL rows have every
#' EL-program field `NA`. Former rows have no base-table rows -- formers do not
#' test. Outcome columns are grade-12 only.
#'
#' @section Exit dating:
#' `EXIT_OBSERVED` is 1 on the **last current** row, which is the simulator's
#' convention and **not** the package's. The package dates exit to the first
#' year observed as `former`, so the package's exit year is this row's
#' `YEAR + 1`. The sim's `YEAR_IN_STATUS` and `EXIT_OBSERVED` are validation
#' inputs, never the source of the package's own derivations.
#'
#' @format A data frame with 461,040 rows and 52 columns:
#' \describe{
#'   \item{STUDENT_ID}{Character. `S######` ever-EL, `N######` never-EL. Never
#'     parse the prefix -- use `EVER_EL`; the prefix will not survive contact
#'     with real data.}
#'   \item{YEAR}{Integer spring year, 2015-2025.}
#'   \item{GRADE}{Character `"K"`, `"1"`-`"12"`. High school grades are
#'     individual, not a bucket.}
#'   \item{DISTRICT_ID}{Character `D01`-`D12`.}
#'   \item{SCHOOL_ID}{Character `D01-S03`; static per student. No separate
#'     within-district code ships: `SCHOOL_CODE` and `SCHOOL_NUMBER` are
#'     ReportR aliases of this column.}
#'   \item{GENDER}{Character M / F / X. X is about 0.3% and is expected to
#'     suppress.}
#'   \item{RACE_ETHNICITY}{Character federal seven-category label, plus
#'     "Not Reported"; ReportR canonical, derived from `HISPANIC` and the
#'     `RACE_*` flags.}
#'   \item{HISPANIC}{Character Y / N.}
#'   \item{RACE_AMERICAN_INDIAN}{Character Y / N; more than one race may be Y.}
#'   \item{RACE_ASIAN}{Character Y / N.}
#'   \item{RACE_BLACK}{Character Y / N.}
#'   \item{RACE_PACIFIC}{Character Y / N.}
#'   \item{RACE_WHITE}{Character Y / N.}
#'   \item{COUNTRY_OF_BIRTH}{Character three-letter code; `USA`; `XXX` for
#'     other.}
#'   \item{IMMIGRANT_STATUS}{Character Y / N; the newcomer proxy.}
#'   \item{FIRST_ENTRY_US_SCHOOL_DATE}{Date; ever-EL only.}
#'   \item{FRL_DETAIL}{Character F / R / N. Not `FRL_STATUS`, which is a
#'     ReportR alias of `FRL`.}
#'   \item{FRL}{Character Y / N; derived as `FRL_DETAIL` in F or R.}
#'   \item{SPED_CLASSIFICATION}{Character SLD, SLI, OHI, AUT, MD, OTH or `NA`;
#'     `NA` means not classified.}
#'   \item{IEP}{Character Y / N.}
#'   \item{GIFTED_FLAG}{Character Y / N; sparse.}
#'   \item{MIGRANT_STATUS}{Character Y / N; sparse.}
#'   \item{HOME_LANGUAGE}{Character lowercase three-character code: spa, por,
#'     ara, guj, hat, cmn, kor, urd, tgl, oth; never-EL eng, spa, oth.}
#'   \item{SERVICE_MODEL}{Character esl_pullout, esl_pushin, sheltered,
#'     bilingual_tw, bilingual_trans, newcomer_program, refused_services;
#'     `NA` for never-EL. Static per student.}
#'   \item{LEP_PROGRAM_START_DATE}{Date, 15 September of the entry year.}
#'   \item{LEP_PROGRAM_COMPLETION_DATE}{Date, 15 June of the `EXIT_OBSERVED`
#'     year. **Static per student**: present on every row of an exiter,
#'     including rows before the exit. Validates timing; never the source of
#'     it.}
#'   \item{TITLE3_LEP_FLAG}{Character Y / N; Y if and only if current and not
#'     refused services.}
#'   \item{EL}{Character Y / N; ReportR's canonical binary, `EL_STATUS ==
#'     "current"`. Ships for cross-package flows; `EL_STATUS` must never be
#'     run through ReportR's alias map, which would collapse it to this.}
#'   \item{EVER_EL}{Character Y / N.}
#'   \item{EL_STATUS}{Character current / former / never. Three values, kept,
#'     because every reference-group filter depends on it. "English-only (EO)"
#'     in Checks 3-4 means `never`; former ELs are never in the EO group.}
#'   \item{EL_STATUS_DETAIL}{Character current / never / monitoring_1-4.
#'     Monitoring years are sub-states of former, not a fourth status. Students
#'     leave the panel after monitoring year 4, so "Former ELs" here means
#'     former-within-four-years and captions must say so.}
#'   \item{EL_MONITOR_YEAR}{Integer 1-4, else `NA`.}
#'   \item{EXIT_OBSERVED}{Integer 0 / 1 on the last current row; see the Exit
#'     dating section.}
#'   \item{YEAR_IN_STATUS}{Integer; `NA` unless current. Validation input; the
#'     package recomputes it with [add_year_in_status()].}
#'   \item{ENTRY_GRADE}{Character `"K"`-`"11"`, else `NA`.}
#'   \item{COHORT_START_YEAR}{Integer entry year; `NA` for never-EL. Never used
#'     to build cohorts.}
#'   \item{COHORT_LABEL}{Character `K-2017` etc.; `NA` when `LEFT_TRUNCATED` or
#'     never-EL. Validation input for [build_cohort()].}
#'   \item{LEFT_TRUNCATED}{Logical; entry precedes the panel. Check 1 cohorts
#'     exclude these by construction: their duration is measured from an entry
#'     nobody observed.}
#'   \item{INITIAL_ELP}{Character headline band label, else `NA`; the band at
#'     the first current year with a valid annual headline.}
#'   \item{NEWCOMER_FLAG}{Character Y / N; immigrant and within two years of
#'     entry.}
#'   \item{SIFE_FLAG}{Character Y / N.}
#'   \item{ELP_FORM}{Character annual / alternate / `NA`. Gates the headline:
#'     alternate student-years never resolve through [elp_headline()].}
#'   \item{ELP_GROWTH_TARGET_MET}{Character Y / N / `NA`.}
#'   \item{GRAD_4YR_STATUS}{Character Y / N / `NA`; grade-12 rows only.}
#'   \item{GRAD_5YR_STATUS}{Character Y / N / `NA`; grade-12 rows only.}
#'   \item{ELA_GRAD_PATHWAY}{Character assessment / portfolio / substitute;
#'     grade-12 rows only.}
#'   \item{MATH_GRAD_PATHWAY}{Character assessment / portfolio / substitute;
#'     grade-12 rows only.}
#'   \item{SCHOOL_EXIT_DATE}{Date, 20 June; grade-12 rows only.}
#'   \item{SEAL_OF_BILITERACY}{Character Y / N; grade-12 rows only.}
#'   \item{SEAL_LANGUAGES}{Character home-language code; awardees only.}
#'   \item{DISTRICT_SIZE_BAND}{Character small / medium / large, from tertiles
#'     of mean annual current-EL count. **Validation input only**: the package
#'     bands districts from `spec$policy$district_size_bands`, never from this
#'     column.}
#'   \item{SAMPLE_WEIGHT}{Numeric 1 or 10; see the Weighting section.}
#' }
#' @source `data-raw/el_long_sim.R` under `sim_contract.md` v0.4.0, seed
#'   20260727. Synthetic: no figure from it describes any real state.
#' @seealso [el_long_sim], [academic_long_sim], [as_el_data()]
"el_roster_sim"

#' Simulated academic results: one row per student-year-subject
#'
#' ReportR's `student_results_long` plus `ACHIEVEMENT_LEVEL_ALT`. Consumed by
#' Check 1's Model 2, Check 3 and Check 4.
#'
#' @section Coverage:
#' Grades 3-11 and subjects ELA and Math only; K-2 and grade 12 are genuinely
#' untested and have no rows. **There are no 2020 rows at all** -- spring 2020
#' produced no state summative, so the year is absent from the table, not
#' present and empty. Any year-over-year comparison must handle a missing year.
#'
#' Check 3 must drive its grade loop from `spec$academic$<subject>$grades`, not
#' from the observed grade span, or it renders empty panels for K, 1, 2 and 12.
#'
#' Never-EL and former students have rows -- they are the reference groups.
#' Check 3's universe is therefore this table joined to roster status
#' (academic left-joined to roster); joining the other way silently drops
#' them, because they have no ELP rows.
#'
#' @format A data frame with 568,202 rows and 7 columns:
#' \describe{
#'   \item{STUDENT_ID}{Character; key.}
#'   \item{YEAR}{Integer; key. No 2020 rows.}
#'   \item{GRADE}{Character `"3"`-`"11"`; equals the roster's grade for the
#'     student-year, asserted. Required in a delivery, with no fill-from-roster
#'     fallback.}
#'   \item{SUBJECT}{Character ELA / Math; key; matches `spec$academic` names.}
#'   \item{SCALE_SCORE}{Numeric, about 650-850; `NA` on alternate rows.}
#'   \item{ACHIEVEMENT_LEVEL}{Integer 1-5; `NA` on alternate rows. Cuts are
#'     level **minimums** read with `>=`, so a score exactly at the proficiency
#'     cut is level 4 (proficient). Labels live in the spec.}
#'   \item{ACHIEVEMENT_LEVEL_ALT}{Integer 1-4; `NA` otherwise. Alternate
#'     academic participation is tied to the alternate ELP form.}
#' }
#' @source `data-raw/el_long_sim.R` under `sim_contract.md` v0.4.0.
#' @seealso [el_long_sim], [el_roster_sim]
"academic_long_sim"
