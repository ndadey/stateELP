# Documentation for the three shipped simulated objects.
#
# The codebook is sim_contract.md 3.5 / 4.2 / 5.1 -- there is no separate
# codebook document. This file mirrors those tables and test-data.R asserts
# that the contract, this file, and the shipped objects name the same columns,
# so a column added to the generator without being documented fails the build.

#' Simulated EL spine: one row per student-year
#'
#' The **spine** and the only place students are counted. It carries the
#' headline ELP columns and nothing else about ELP measurement; which component
#' feeds the headline is one spec field (`spec$elp$headline`).
#'
#' @section Counting:
#' This is the denominator for anything counted per student-year. Never count
#' students from `academic_long_sim` (it double-counts, once per subject) or
#' from `elp_components_sim` (it counts a student up to eight times).
#'
#' @section Weighting:
#' Never-EL rows are a 10% sample carrying `sample_weight = 10`; ever-EL rows
#' carry 1. Every statewide share must weight and every within-EL statistic
#' must not. Unweighted, current ELs are 52.7% of SY2023 enrollment; weighted,
#' 17.8%.
#'
#' @section Structural missingness:
#' By design, not a data problem (contract 3.6). Never-EL rows have every
#' EL-program field `NA`. Former rows have `elp_level`, `elp_scale` and
#' `elp_band` `NA` -- formers do not test. Current rows are `NA` when the
#' student was not tested that year or took the alternate form. Outcome columns
#' are grade-12 only.
#'
#' @section Exit dating:
#' `exit_observed` is 1 on the **last current** row, which is the simulator's
#' convention and **not** the package's. The package dates exit to the first
#' year observed as `former`, so the package's exit year is this row's
#' `school_year + 1`. The sim's `year_in_status` and `exit_observed` are
#' validation inputs, never the source of the package's own derivations.
#'
#' @format A data frame with 461,040 rows and 54 columns:
#' \describe{
#'   \item{student_id}{Character. `S######` ever-EL, `N######` never-EL. Never
#'     parse the prefix -- use `ever_el`; the prefix will not survive contact
#'     with real data.}
#'   \item{school_year}{Integer spring year, 2015-2025.}
#'   \item{grade}{Character `"K"`, `"1"`-`"12"`. High school grades are
#'     individual, not a bucket.}
#'   \item{district_id}{Character `D01`-`D12`.}
#'   \item{school_code}{Character within-district school code.}
#'   \item{school_id}{Character `D01-S03`; static per student.}
#'   \item{sex}{Character M / F / X. X is about 0.3% and is expected to
#'     suppress.}
#'   \item{hispanic}{Character Y / N.}
#'   \item{race_american_indian}{Character Y / N; more than one race may be Y.}
#'   \item{race_asian}{Character Y / N.}
#'   \item{race_black}{Character Y / N.}
#'   \item{race_pacific}{Character Y / N.}
#'   \item{race_white}{Character Y / N.}
#'   \item{country_of_birth}{Character three-letter code; `USA`; `XXX` for
#'     other.}
#'   \item{immigrant_status}{Character Y / N; the newcomer proxy.}
#'   \item{first_entry_us_school_date}{Date; ever-EL only.}
#'   \item{frpl_status}{Character F / R / N.}
#'   \item{econ_disadv}{Character Y / N; derived as `frpl_status` in F or R.}
#'   \item{sped_classification}{Character SLD, SLI, OHI, AUT, MD, OTH or `NA`;
#'     `NA` means not classified.}
#'   \item{swd_flag}{Character Y / N.}
#'   \item{gifted_flag}{Character Y / N; sparse.}
#'   \item{migrant_status}{Character Y / N; sparse.}
#'   \item{home_language}{Character lowercase three-character code: spa, por,
#'     ara, guj, hat, cmn, kor, urd, tgl, oth; never-EL eng, spa, oth.}
#'   \item{service_model}{Character esl_pullout, esl_pushin, sheltered,
#'     bilingual_tw, bilingual_trans, newcomer_program, refused_services;
#'     `NA` for never-EL. Static per student.}
#'   \item{lep_program_start_date}{Date, 15 September of the entry year.}
#'   \item{lep_program_completion_date}{Date, 15 June of the `exit_observed`
#'     year. **Static per student**: present on every row of an exiter,
#'     including rows before the exit. Validates timing; never the source of
#'     it.}
#'   \item{title3_lep_flag}{Character Y / N; Y if and only if current and not
#'     refused services.}
#'   \item{ever_el}{Character Y / N.}
#'   \item{el_status}{Character current / former / never. Three values, kept,
#'     because every reference-group filter depends on it. "English-only (EO)"
#'     in Checks 3-4 means `never`; former ELs are never in the EO group.}
#'   \item{el_status_detail}{Character current / never / monitoring_1-4.
#'     Monitoring years are sub-states of former, not a fourth status. Students
#'     leave the panel after monitoring year 4, so "Former ELs" here means
#'     former-within-four-years and captions must say so.}
#'   \item{el_monitor_year}{Integer 1-4, else `NA`.}
#'   \item{exit_observed}{Integer 0 / 1 on the last current row; see the Exit
#'     dating section.}
#'   \item{year_in_status}{Integer; `NA` unless current. Validation input; the
#'     package recomputes it.}
#'   \item{entry_grade}{Character `"K"`-`"11"`, else `NA`.}
#'   \item{cohort_start_year}{Integer entry year; `NA` for never-EL. Never used
#'     to build cohorts -- use `cohort_label`.}
#'   \item{cohort_label}{Character `K-2017` etc.; `NA` when `left_truncated` or
#'     never-EL. This is the cohort key.}
#'   \item{left_truncated}{Logical; entry precedes the panel. Check 1 cohorts
#'     exclude these by construction: their duration is measured from an entry
#'     nobody observed.}
#'   \item{initial_elp}{Character band label, else `NA`; the band at the first
#'     current year with a valid band.}
#'   \item{newcomer_flag}{Character Y / N; immigrant and within two years of
#'     entry.}
#'   \item{sife_flag}{Character Y / N.}
#'   \item{elp_form}{Character annual / alternate / `NA`. Gates the headline.}
#'   \item{elp_level}{Numeric 1.0-6.0 in tenths, else `NA`. **The headline.**}
#'   \item{elp_scale}{Numeric, about 100-600, else `NA`. The headline scale
#'     score; Check 3's logistic regression and Check 4's consistency and
#'     Youden's J run on it.}
#'   \item{elp_band}{Character spec band label, else `NA`.}
#'   \item{elp_growth_target_met}{Character Y / N / `NA`.}
#'   \item{grad_4yr_status}{Character Y / N / `NA`; grade-12 rows only.}
#'   \item{grad_5yr_status}{Character Y / N / `NA`; grade-12 rows only.}
#'   \item{ela_grad_pathway}{Character assessment / portfolio / substitute;
#'     grade-12 rows only.}
#'   \item{math_grad_pathway}{Character assessment / portfolio / substitute;
#'     grade-12 rows only.}
#'   \item{school_exit_date}{Date, 20 June; grade-12 rows only.}
#'   \item{seal_of_biliteracy}{Character Y / N; grade-12 rows only.}
#'   \item{seal_languages}{Character home-language code; awardees only.}
#'   \item{district_size_band}{Character small / medium / large, from tertiles
#'     of mean annual current-EL count. **Validation input only**: the package
#'     bands districts from `spec$policy$district_size_bands`, never from this
#'     column.}
#'   \item{sample_weight}{Numeric 1 or 10; see the Weighting section.}
#' }
#' @source `data-raw/el_long_sim.R` under `sim_contract.md` v0.2.0, seed
#'   20260727. Synthetic: no figure from it describes any real state.
#' @seealso [elp_components_sim], [academic_long_sim], [as_el_data()]
"el_long_sim"

#' Simulated ELP components: one row per student-year-component
#'
#' `component` is a **row key over a controlled vocabulary**, not a set of
#' instrument-named columns, so an instrument reporting four domains and a
#' categorical determination fills the same table shape as WIDA's eight. See
#' [el_component_vocabulary()].
#'
#' @section Who reads this:
#' Prep 1, exit-rule work, and the Type-gated Additional checks. **No
#' `compute_*()` behind Prep 2 or Checks 1-4 may read this table** -- those
#' stages read the headline off the spine. Never count students here: it counts
#' a student once per component.
#'
#' @section Row population:
#' A row exists for an ever-EL student, in a year they were `current`, if they
#' were tested and the component has a non-`NA` value. So a student-year may
#' carry fewer than eight rows: a waived domain is an **absent row**, and every
#' composite drawing on it is absent too. Absence *is* the signal; there is no
#' `waived` column, and a validator must not read a missing domain row as a
#' data problem. There are no rows for former or never student-years.
#'
#' @format A data frame with 1,824,716 rows and 6 columns:
#' \describe{
#'   \item{student_id}{Character; key.}
#'   \item{school_year}{Integer; key.}
#'   \item{component}{Character; key. One of the controlled vocabulary. Stored
#'     as character, never a factor: the spec supplies display order, and a
#'     factor would coerce on `bind_rows()` with a real delivery. The academic
#'     table's key is `subject` -- the two must never share a key name, or a
#'     `reading` domain and a Reading subject conflate in a join.}
#'   \item{form}{Character annual / alternate; equals the spine's `elp_form`
#'     for the student-year, asserted.}
#'   \item{scale_score}{Numeric, about 100-600; `NA` on alternate rows. A
#'     component reporting a level but no scale is a legitimate row.}
#'   \item{level}{Numeric. Annual 1.0-6.0 in tenths; alternate an ordinal 1-6
#'     on a different scale. 6,595 alternate rows carry a level and no scale.}
#' }
#' @source `data-raw/el_long_sim.R` under `sim_contract.md` v0.2.0.
#' @seealso [el_long_sim], [academic_long_sim]
"elp_components_sim"

#' Simulated academic results: one row per student-year-subject
#'
#' `student_results_long` in stateSummative's sense, in this package's
#' lowercase naming. Consumed by Check 1's Model 2, Check 3 and Check 4.
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
#' Check 3's universe is therefore this table joined to spine status
#' (academic left-joined to spine); joining the other way silently drops them,
#' because they have no ELP rows.
#'
#' @format A data frame with 568,202 rows and 7 columns:
#' \describe{
#'   \item{student_id}{Character; key.}
#'   \item{school_year}{Integer; key. No 2020 rows.}
#'   \item{grade}{Character `"3"`-`"11"`; equals the spine's grade for the
#'     student-year, asserted. Required in a delivery, with no fill-from-spine
#'     fallback.}
#'   \item{subject}{Character ELA / Math; key; matches `spec$academic` names.}
#'   \item{scale_score}{Numeric, about 650-850; `NA` on alternate rows.}
#'   \item{perf_level}{Integer 1-5; `NA` on alternate rows. Cuts are level
#'     **minimums** read with `>=`, so a score exactly at the proficiency cut
#'     is level 4 (proficient).}
#'   \item{alt_perf_level}{Integer 1-4; `NA` otherwise. Alternate academic
#'     participation is tied to the alternate ELP form.}
#' }
#' @source `data-raw/el_long_sim.R` under `sim_contract.md` v0.2.0.
#' @seealso [el_long_sim], [elp_components_sim]
"academic_long_sim"
