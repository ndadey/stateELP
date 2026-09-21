# Check 1 -- Model 1's analysis. Plain data out, no plotting, no I/O.
#
# Cohort MEMBERSHIP is shared and stays in R/utils.R (build_cohort(), called
# unchanged). Cohort SELECTION lives here because it is Check 1's sample
# decision; Check 2 follows it by calling compute_cohorts() (CLAUDE.md 14,
# departure recorded 2026-09-20).

#' Candidate entry cohorts, and the one Check 1 analyzes
#'
#' One row per usable entry cohort, and which one the stage selects. The table
#' answers only *which cohort and why*: it carries no outcome columns, because
#' outcomes are the life table's job, for the selected cohort alone.
#'
#' **Candidates** are what [build_cohort()] returns with its default
#' `entry_years` -- entry years with the maximum timeline plus a year of
#' follow-up inside the panel, `LEFT_TRUNCATED` excluded. **Selection** is the
#' most complete candidate: the one with the longest observed follow-up
#' (`FOLLOWUP_YEARS`), which is the number of rows its life table will have,
#' so it is the cohort with most of its trajectory observed rather than
#' censored at the window's edge. Because follow-up shrinks with every later
#' entry year, in practice this is the earliest usable cohort.
#'
#' A candidate below the spec's `min_cell` is suppressed -- its `N_ENTRY` is
#' blanked, the row stays -- and can never be selected. With no qualifying
#' candidate nothing is selected, and [run_check1()] degrades every analysis
#' slot to `NULL` rather than analyzing a cohort too small to report.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param start_grade Entry grade defining the cohorts, e.g. `"K"`.
#' @return A tibble: `COHORT_LABEL`, `COHORT_START_YEAR`, `N_ENTRY`,
#'   `FOLLOWUP_YEARS`, `SELECTED`, `SUPPRESSED`. Zero rows when the panel
#'   supports no cohort at all.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' compute_cohorts(bundle, spec)
compute_cohorts <- function(data, spec, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(start_grade), length(start_grade) == 1L)
  roster <- el_roster(data)
  .check1_require_fields(roster, "cohorts")

  panel_last <- max(roster$YEAR, na.rm = TRUE)
  members <- build_cohort(roster, spec, start_grade = start_grade)

  out <- members |>
    dplyr::count(.data$COHORT_LABEL, .data$COHORT_START_YEAR, name = "N_ENTRY") |>
    dplyr::mutate(FOLLOWUP_YEARS =
                    as.integer(.env$panel_last - .data$COHORT_START_YEAR + 1L)) |>
    dplyr::arrange(.data$COHORT_START_YEAR)

  # cols = character(0): the default would blank COHORT_START_YEAR and
  # FOLLOWUP_YEARS, which are not disclosive and are what the row is for.
  out <- apply_suppression(out, spec, n_col = "N_ENTRY", cols = character(0),
                           blank_n = TRUE)

  out$SELECTED <- FALSE
  qualifies <- !out$SUPPRESSED
  if (any(qualifies)) {
    pick <- which(qualifies)[which.max(out$FOLLOWUP_YEARS[qualifies])]
    out$SELECTED[pick] <- TRUE
  }
  out[c("COHORT_LABEL", "COHORT_START_YEAR", "N_ENTRY", "FOLLOWUP_YEARS",
        "SELECTED", "SUPPRESSED")]
}

# The deck's slide-52 arithmetic, split out so the worked example can be
# tested against the slide's own A column rather than through a simulated
# cohort that would have to reproduce 27,511 students.
#
# The final interval has no follow-up: no exit can be observed in it, so B is
# empty, every student still at risk is censored (C = A), the observed exit
# rate is 0 and the censor rate 1, and the cumulative column is left blank
# rather than repeating the previous row -- exactly as the slide prints it.
.life_table_rates <- function(A, B, C) {
  stopifnot(is.numeric(A), is.numeric(B), is.numeric(C),
            length(A) == length(B), length(B) == length(C), length(A) > 0L)
  observed_b <- ifelse(is.na(B), 0, B)
  tibble::tibble(
    D = ifelse(A > 0, observed_b / A, NA_real_),
    E = ifelse(A > 0, C / A, NA_real_),
    F = ifelse(is.na(B), NA_real_, cumsum(observed_b) / A[[1]])
  )
}

#' The slide-52 life table for one cohort
#'
#' Discrete-time life table by year in status, in the deck's own columns:
#' **A** students enrolled at the year's beginning (at risk), **B** students
#' who exited at the year's end, **C** students who disappeared at the year's
#' end, **D** = B/A the at-risk annual exit rate, **E** = C/A the annual
#' censor rate, **F** = sum(B)/A_1 the original-cohort cumulative exit rate.
#'
#' The recurrence A_(t+1) = A_t - B_t - C_t holds by construction. Under the
#' package's exit dating -- first former year, counted as a year in status --
#' "exited at the end of year *t*" means observed as former in year *t+1*'s
#' data, and "disappeared at the end of year *t*" means last seen in year *t*
#' (CLAUDE.md 6). The last row is the window's edge: B is empty because no
#' following year exists to observe an exit in, and every remaining student is
#' censored there.
#'
#' **F is a lower bound**, not a completion rate: censored students are not
#' evidence that a student stayed in status, and no mean time in status is
#' computed anywhere -- it is not defined on censored data.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param cohort A single `COHORT_LABEL`, normally the selected row of
#'   [compute_cohorts()].
#' @param start_grade The entry grade those cohorts were built on.
#' @return A tibble, one row per year in status: `YEAR_IN_STATUS`, `GRADE`,
#'   `A`, `B`, `C`, `D`, `E`, `F`, `SUPPRESSED`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' compute_life_table(bundle, spec, cohort = co$COHORT_LABEL[co$SELECTED])
compute_life_table <- function(data, spec, cohort, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(cohort), length(cohort) == 1L, !is.na(cohort),
            is.character(start_grade), length(start_grade) == 1L)
  roster <- el_roster(data)
  .check1_require_fields(roster, "life_table")

  out <- .check1_life_counts(roster, spec, cohort, start_grade)
  out <- dplyr::bind_cols(out, .life_table_rates(out$A, out$B, out$C))
  apply_suppression(out, spec, n_col = "A",
                    cols = c("B", "C", "D", "E", "F"))
}

# A, B and C before any suppression, with the row's grade label. Split out
# because the survival estimate multiplies (1 - D) down the column: a
# suppressed D is NA, and reading NA as "no exits" would quietly flatten the
# curve. Everything that needs the counts takes them from here.
.check1_life_counts <- function(roster, spec, cohort, start_grade) {
  co <- .check1_cohort_spans(roster, spec, cohort, start_grade)
  spans <- co$spans
  n_rows <- co$n_rows

  # Last year each student was current: the year before the exit year for an
  # exiter, the last observed year for anyone censored.
  last_current <- ifelse(spans$EXITED, spans$EXIT_YEAR_IN_STATUS - 1L,
                         spans$LAST_YEAR_IN_STATUS)
  t <- seq_len(n_rows)
  A <- vapply(t, function(i) sum(last_current >= i, na.rm = TRUE), 0L)
  B <- vapply(t, function(i) {
    sum(spans$EXITED & spans$EXIT_YEAR_IN_STATUS == i + 1L, na.rm = TRUE)
  }, 0L)
  C <- vapply(t, function(i) {
    sum(spans$CENSOR_TYPE %in% "disappeared" &
          spans$LAST_YEAR_IN_STATUS == i, na.rm = TRUE)
  }, 0L)
  B[n_rows] <- NA_integer_          # no year n_rows + 1 to observe an exit in
  C[n_rows] <- A[n_rows]            # everyone still at risk is censored here

  tibble::tibble(YEAR_IN_STATUS = t,
                 GRADE = .cohort_grade_labels(co$cohort_rows, co$start_year, t),
                 A = A, B = B, C = C)
}

# The label for students who never resolve an annual headline, and so have no
# initial band: alternate-form takers (a different scale entirely, contract
# 4.4) and anyone untested in their first years. They are shown as their own
# group, never folded into a band and never silently dropped.
#
# It appears in a figure legend, so it says what a reader can check against
# their own data -- no annual score -- rather than naming this package's
# abstraction.
.CHECK1_NO_BAND <- "No annual ELP score"

#' The cumulative exit curve for one cohort
#'
#' The share of the entry cohort observed to have exited by each year in
#' status -- the life table's **F** column as a curve, overall or split by a
#' student-level group.
#'
#' `by = "INITIAL_ELP"` splits on the **initial ELP band**, derived here
#' rather than read from the delivery: the banded headline at the student's
#' first year with a valid annual headline, which is what
#' `policy$timelines$initial_elp_source = "first_annual"` declares. A
#' delivered `INITIAL_ELP` column is a validation input, never the source
#' (the same treatment `YEAR_IN_STATUS` and `COHORT_LABEL` get). Students with
#' no annual headline form their own group.
#'
#' **The curve is a lower bound.** Censored students -- those who disappeared,
#' and those still classified at the window's edge -- are not exiters, and
#' nothing here imputes what they would have done. No mean time in status is
#' computed: on censored data it is not defined.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param cohort A single `COHORT_LABEL`, normally the selected row of
#'   [compute_cohorts()].
#' @param start_grade The entry grade those cohorts were built on.
#' @param by `NULL` for the whole cohort, or `"INITIAL_ELP"` to split by
#'   initial ELP band.
#' @return A tibble, one row per group and year in status: `GROUP`,
#'   `YEAR_IN_STATUS`, `N_AT_ENTRY`, `N_EXITED_CUM`, `CUM_EXIT_RATE`,
#'   `SUPPRESSED`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' compute_exit_curve(bundle, spec, cohort = co$COHORT_LABEL[co$SELECTED])
compute_exit_curve <- function(data, spec, cohort, start_grade = "K",
                               by = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(cohort), length(cohort) == 1L, !is.na(cohort),
            is.character(start_grade), length(start_grade) == 1L,
            is.null(by) || (is.character(by) && length(by) == 1L))
  slot <- if (is.null(by)) "exit_curve" else "exit_curve_by_initial_band"
  roster <- el_roster(data)
  .check1_require_fields(roster, slot)

  co <- .check1_cohort_spans(roster, spec, cohort, start_grade)
  spans <- co$spans

  if (is.null(by)) {
    spans$GROUP <- "All"
  } else if (identical(by, "INITIAL_ELP")) {
    band <- .check1_initial_band(data, spec)
    spans <- dplyr::left_join(spans, band, by = "STUDENT_ID")
    spans$GROUP <- ifelse(is.na(spans$ELP_BAND), .CHECK1_NO_BAND,
                          spans$ELP_BAND)
  } else {
    stop("`by` must be NULL or \"INITIAL_ELP\"; got \"", by, "\".",
         call. = FALSE)
  }

  groups <- .check1_group_order(unique(spans$GROUP), spec)
  grid <- expand.grid(YEAR_IN_STATUS = seq_len(co$n_rows), GROUP = groups,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  out <- tibble::tibble(
    GROUP = grid$GROUP,
    YEAR_IN_STATUS = as.integer(grid$YEAR_IN_STATUS),
    N_AT_ENTRY = vapply(grid$GROUP,
                        function(g) sum(spans$GROUP == g), 0L,
                        USE.NAMES = FALSE),
    N_EXITED_CUM = vapply(seq_len(nrow(grid)), function(i) {
      sum(spans$GROUP == grid$GROUP[[i]] & spans$EXITED &
            spans$EXIT_YEAR_IN_STATUS <= grid$YEAR_IN_STATUS[[i]],
          na.rm = TRUE)
    }, 0L)
  )
  out$CUM_EXIT_RATE <- ifelse(out$N_AT_ENTRY > 0,
                              out$N_EXITED_CUM / out$N_AT_ENTRY, NA_real_)

  # cols given explicitly: the default would blank YEAR_IN_STATUS, which is
  # the row's own index.
  apply_suppression(out, spec, n_col = "N_AT_ENTRY",
                    cols = c("N_EXITED_CUM", "CUM_EXIT_RATE"), blank_n = TRUE)
}

# The initial ELP band per student: the banded headline at the first year one
# resolves. Read through elp_headline(), the only route to the headline, so
# the annual-form filter and the spec's bands both apply (CLAUDE.md 4).
.check1_initial_band <- function(data, spec) {
  .check1_initial_elp(data, spec)[, c("STUDENT_ID", "ELP_BAND"), drop = FALSE]
}

# The same first annual headline, keeping the scale score as well: the band
# for the curves, the score for the fitted models. One derivation, so the
# figure and the model cannot end up disagreeing about where a student
# started.
.check1_initial_elp <- function(data, spec) {
  src <- spec$policy$timelines$initial_elp_source
  if (!is.null(src) && !identical(src, "first_annual")) {
    stop("`policy$timelines$initial_elp_source` is \"", src, "\"; this ",
         "package derives the initial band as the first annual headline ",
         "only.", call. = FALSE)
  }
  h <- elp_headline(data, spec)
  h <- h[!is.na(h$ELP_BAND),
         c("STUDENT_ID", "YEAR", "ELP_BAND", "SCALE_SCORE"), drop = FALSE]
  h <- h[order(h$STUDENT_ID, h$YEAR), , drop = FALSE]
  out <- h[!duplicated(h$STUDENT_ID), , drop = FALSE]
  names(out)[names(out) == "SCALE_SCORE"] <- "INITIAL_ELP"
  out[, c("STUDENT_ID", "ELP_BAND", "INITIAL_ELP"), drop = FALSE]
}

# Display order: the spec's own band order, then the no-band group last. Band
# labels are never typed here.
.check1_group_order <- function(groups, spec) {
  known <- spec_bands(spec)$labels
  # An instrument with no declared bands gets one band per level, so the
  # level scale supplies the order instead (CLAUDE.md 5).
  if (is.null(known) && identical(spec$elp$level_scale$type, "categorical")) {
    known <- spec$elp$level_scale$levels
  }
  ordered <- c(intersect(known, groups),
               sort(setdiff(groups, c(known, .CHECK1_NO_BAND))))
  c(ordered, intersect(.CHECK1_NO_BAND, groups))
}

# One cohort's membership, spans and window. Shared by the life table and the
# exit curves so the two slots cannot drift apart: the same students, the same
# dating, the same number of rows.
.check1_cohort_spans <- function(roster, spec, cohort, start_grade) {
  members <- build_cohort(roster, spec, start_grade = start_grade)
  members <- members[members$COHORT_LABEL == cohort, , drop = FALSE]
  if (!nrow(members)) {
    stop("No cohort labelled '", cohort, "' among the ", start_grade,
         " entry cohorts this panel supports.", call. = FALSE)
  }
  panel_last <- max(roster$YEAR, na.rm = TRUE)
  start_year <- members$COHORT_START_YEAR[[1]]
  cohort_rows <- roster[roster$STUDENT_ID %in% members$STUDENT_ID, , drop = FALSE]

  list(
    members     = members,
    cohort_rows = cohort_rows,
    start_year  = start_year,
    panel_last  = panel_last,
    n_rows      = as.integer(panel_last - start_year + 1L),
    # The window is the panel's, not the cohort's: a student last seen in the
    # panel's final year is still enrolled, not disappeared.
    spans       = build_student_spans(cohort_rows, last_year = panel_last)
  )
}

# The grade each life-table row falls in, read from the cohort's own rows
# (the modal grade in that year) rather than assumed from the start grade, so
# a delivery with retention shows what it shows.
.cohort_grade_labels <- function(cohort_rows, start_year, t) {
  tab <- cohort_rows |>
    dplyr::mutate(YEAR_IN_STATUS = as.integer(.data$YEAR - .env$start_year + 1L)) |>
    dplyr::filter(.data$YEAR_IN_STATUS %in% .env$t) |>
    dplyr::count(.data$YEAR_IN_STATUS, .data$GRADE) |>
    dplyr::slice_max(.data$n, n = 1L, by = "YEAR_IN_STATUS", with_ties = FALSE)
  tab$GRADE[match(t, tab$YEAR_IN_STATUS)]
}

# ---- Model 1: the companion estimate and the timeline statistics ------------

#' The discrete-time survival estimate
#'
#' The life table's **F** column counts exits against the cohort as it
#' started, so every censored student holds it down: it is a lower bound. The
#' companion here is the standard discrete-time estimate,
#' `1 - prod(1 - D_t)`, which compounds each year's at-risk exit rate and so
#' answers a different question -- what share would have exited had nobody
#' been lost to follow-up.
#'
#' Both are reported side by side, labeled, because neither is the right
#' number alone: the observed share is what happened, the estimate is what the
#' hazards imply. The gap between them is the price of censoring.
#'
#' **Only the estimate carries a confidence interval.** `SE`, `CI_LO` and
#' `CI_HI` come from Greenwood's variance, taken through the complementary
#' log-log scale so the bounds stay inside \[0, 1\]. The observed column has
#' none: it counts exits against the cohort as it started, which is a
#' censoring-biased lower bound rather than an estimator of anything, and an
#' interval around it would advertise precision about the wrong quantity. The
#' interval covers sampling variability in this cohort's exit rates only --
#' not whether the cohort represents any other.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param cohort A single `COHORT_LABEL`.
#' @param start_grade The entry grade the cohorts were built on.
#' @return A tibble by `YEAR_IN_STATUS`: `A`, `HAZARD` (D), `SURVIVING`,
#'   `CUM_EXIT_EST`, `SE`, `CI_LO`, `CI_HI`, `CUM_EXIT_OBS`, `SUPPRESSED`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(b, spec)
#' compute_survival_est(b, spec, cohort = co$COHORT_LABEL[co$SELECTED])
compute_survival_est <- function(data, spec, cohort, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(cohort), length(cohort) == 1L, !is.na(cohort))
  roster <- el_roster(data)
  .check1_require_fields(roster, "survival_est")

  counts <- .check1_life_counts(roster, spec, cohort, start_grade)
  rates <- .life_table_rates(counts$A, counts$B, counts$C)

  # The final interval observes no exits, so it leaves the estimate where it
  # was rather than dropping it.
  hazard <- ifelse(is.na(rates$D), 0, rates$D)
  surviving <- cumprod(1 - hazard)
  band <- .check1_greenwood(counts$A, counts$B, surviving)

  out <- tibble::tibble(
    YEAR_IN_STATUS = counts$YEAR_IN_STATUS,
    A = counts$A,
    HAZARD = rates$D,
    SURVIVING = surviving,
    CUM_EXIT_EST = 1 - surviving,
    SE = band$SE,
    CI_LO = band$CI_LO,
    CI_HI = band$CI_HI,
    CUM_EXIT_OBS = rates$F
  )
  apply_suppression(out, spec, n_col = "A",
                    cols = c("HAZARD", "SURVIVING", "CUM_EXIT_EST",
                             "SE", "CI_LO", "CI_HI", "CUM_EXIT_OBS"))
}

# Greenwood's variance for the product-limit estimate, and a 95% interval
# around it on the complementary log-log scale.
#
# sigma^2_t = sum_{j<=t} B_j / (A_j (A_j - B_j)) is the variance of log S_t;
# the standard error of S itself -- and so of the cumulative exit share
# 1 - S, which is what the table shows -- is S * sigma.
#
# The interval is taken through log(-log S) rather than on S directly, which
# is `survfit`'s default and the reason to bother: a symmetric interval on S
# runs outside [0, 1] near the ends of the curve, where a cohort that has
# nearly all exited actually is.
#
# Two degeneracies are left as they are rather than papered over. An interval
# that exhausts its own risk set (A = B) has no defined variance, and NA
# propagates forward from it, because nothing after it is known either. A
# stretch with no exits at all has sigma = 0 and S = 1: the band is the point,
# and widening it would invent uncertainty about something the data settles.
.check1_greenwood <- function(A, B, surviving, z = stats::qnorm(0.975)) {
  b <- ifelse(is.na(B), 0, B)
  at_risk_after <- A - b
  term <- ifelse(b == 0, 0,
                 ifelse(A > 0 & at_risk_after > 0,
                        b / (A * at_risk_after), NA_real_))
  sigma <- sqrt(cumsum(term))

  se <- surviving * sigma
  neg_log_s <- -log(surviving)
  half <- ifelse(neg_log_s > 0, z * sigma / neg_log_s, 0)
  lo <- surviving^exp(half)
  hi <- surviving^exp(-half)

  # The table reports the cumulative exit share, so the bounds swap.
  tibble::tibble(SE = se, CI_LO = 1 - hi, CI_HI = 1 - lo)
}

#' Median years to exit
#'
#' The first year in status by which half the cohort has exited, on each
#' basis: the observed share (a lower bound, which may never reach half) and
#' the at-risk estimate. `NA` means half was not reached inside the window,
#' which is itself the finding.
#'
#' **The answer is on the exit-dating basis used everywhere else** -- exit
#' year is the first year observed as former, the deck's A-J convention. A
#' life-table row *t* accumulates exits dated *t + 1* ("exited at year *t*'s
#' end"), so the row index is converted here rather than reported raw. That
#' conversion is what keeps this number equal to the year the exit curve
#' crosses half, and a test holds the two together.
#'
#' **No mean is reported**, here or anywhere in this package: time in status
#' is censored, and its mean is not defined on censored data.
#'
#' @param survival_est Output of [compute_survival_est()].
#' @return A tibble: `BASIS`, `YEARS_TO_50`, `REACHED`.
#' @export
compute_years_to_50 <- function(survival_est) {
  stopifnot(is.data.frame(survival_est),
            all(c("YEAR_IN_STATUS", "CUM_EXIT_EST", "CUM_EXIT_OBS") %in%
                  names(survival_est)))
  first_half <- function(x) {
    hit <- which(!is.na(x) & x >= 0.5)
    if (!length(hit)) return(NA_integer_)
    as.integer(survival_est$YEAR_IN_STATUS[[hit[[1]]]] + 1L)
  }
  obs <- first_half(survival_est$CUM_EXIT_OBS)
  est <- first_half(survival_est$CUM_EXIT_EST)

  tibble::tibble(
    BASIS = c("Observed exits, against the cohort as it started",
              "At-risk estimate, allowing for censoring"),
    YEARS_TO_50 = c(obs, est),
    REACHED = c(!is.na(obs), !is.na(est))
  )
}

#' Where the cohort ended up
#'
#' The timeline statistics the Process Overview asks for: what share exited
#' within the ESSA timeline, what share within the maximum timeline, and what
#' share is still classified at the window's edge. Read from the shared exit
#' flags, so the timelines are whatever `policy$timelines` says, including a
#' rule keyed by start grade or initial band.
#'
#' The first three rows are **nested** (on time is a subset of within the
#' maximum, which is a subset of exited at all). The last three -- exited,
#' still classified, disappeared -- partition the cohort.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param cohort A single `COHORT_LABEL`.
#' @param start_grade The entry grade the cohorts were built on.
#' @return A tibble: `OUTCOME`, `NESTED`, `N`, `PCT`, `SUPPRESSED`.
#' @export
compute_at_max_timeline <- function(data, spec, cohort, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(cohort), length(cohort) == 1L, !is.na(cohort))
  roster <- el_roster(data)
  .check1_require_fields(roster, "at_max_timeline")

  co <- .check1_cohort_spans(roster, spec, cohort, start_grade)
  flags <- add_exit_flags(co$spans, spec)
  n <- nrow(flags)
  essa <- max(flags$ON_TIME_YEARS, na.rm = TRUE)

  out <- tibble::tibble(
    OUTCOME = c(
      sprintf("Exited within the ESSA timeline (%d years)", essa),
      sprintf("Exited within the maximum timeline (%d years)",
              spec_max_years(spec)),
      "Exited at any point observed",
      "Still classified at the window's edge",
      "Disappeared from the data"),
    NESTED = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    N = c(sum(flags$EXIT_ONTIME), sum(flags$EXIT_MAX), sum(flags$EXIT_EVER),
          sum(flags$NO_EXIT), sum(flags$DISAPPEARED))
  )
  out$PCT <- out$N / n
  raw <- out$N
  out <- apply_suppression(out, spec, n_col = "N", cols = "PCT",
                           blank_n = TRUE)

  # A zero is shown, not suppressed. Suppression protects students, and a
  # count of none discloses nobody -- while blanking it would read as
  # "hidden" when the finding is "this did not happen". In the shipped sim
  # nobody disappears, and that is worth seeing.
  none <- raw == 0L
  out$N[none] <- 0L
  out$PCT[none] <- 0
  out$SUPPRESSED[none] <- FALSE

  attr(out, "n_cohort") <- n
  attr(out, "essa_years") <- essa
  out
}

# ---- The fitted models: one frame, one engine, two right-hand sides ---------

# The person-period frame: one row per student per year they were at risk,
# derived on demand and never stored (CLAUDE.md 4). The outcome is the life
# table's B in student form -- exited at the end of this year, observed as
# former in the next -- so the models and the table count the same events.
#
# `subject = NULL` builds the frame without the lagged content score, which is
# what Model 1 needs: it fits on ELP alone, so requiring a prior-year academic
# score would throw away rows for a covariate it does not use. It is also the
# only form that works at all when the delivery carries no academic table.
.check1_person_period <- function(data, spec, cohort, start_grade,
                                  subject = NULL) {
  roster <- el_roster(data)
  co <- .check1_cohort_spans(roster, spec, cohort, start_grade)
  spans <- co$spans

  at_risk <- ifelse(spans$EXITED, spans$EXIT_YEAR_IN_STATUS - 1L,
                    spans$LAST_YEAR_IN_STATUS)
  keep <- !is.na(at_risk) & at_risk >= 1L
  spans <- spans[keep, , drop = FALSE]
  at_risk <- at_risk[keep]

  pp <- tibble::tibble(
    STUDENT_ID = rep(spans$STUDENT_ID, at_risk),
    YEAR_IN_STATUS = as.integer(sequence(at_risk))
  )
  pp$YEAR <- as.integer(co$start_year + pp$YEAR_IN_STATUS - 1L)
  eyis <- spans$EXIT_YEAR_IN_STATUS[match(pp$STUDENT_ID, spans$STUDENT_ID)]
  pp$EXITED_THIS_YEAR <- as.integer(!is.na(eyis) &
                                      eyis == pp$YEAR_IN_STATUS + 1L)

  # The score the student STARTED at -- one value per student, fixed for
  # their whole run.
  #
  # Not that year's score, which was the first build's mistake: the exit rule
  # is applied to that year's result, so regressing exit on it restates the
  # rule and separates the fit by construction. Worse, any other covariate
  # entered beside it is then conditioned on the outcome's own cause, which
  # is what drove Model 2's achievement ratio to 1 and left the skimming test
  # unable to detect skimming. Decided 2026-09-21; see NOTES-check1.md.
  init <- .check1_initial_elp(data, spec)
  pp$INITIAL_ELP <- init$INITIAL_ELP[match(pp$STUDENT_ID, init$STUDENT_ID)]

  # ... and, for Model 2 only, the content score from the year before, never
  # the same year: the question is whether prior achievement predicts this
  # year's exit.
  if (!is.null(subject)) {
    ac <- el_academic(data)
    ac <- ac[ac$SUBJECT %in% subject, , drop = FALSE]
    pp$ACADEMIC_LAG <- ac$SCALE_SCORE[match(paste(pp$STUDENT_ID, pp$YEAR - 1L),
                                            paste(ac$STUDENT_ID, ac$YEAR))]
  }
  pp
}

# The fitting half, shared by both models: a ready frame and a right-hand side
# in, the same result shape out. Model 1 is Model 2 without the lag term, and
# keeping one engine is what stops the two drifting apart -- the dropped-row
# accounting, the standardisation and the separation check have to mean the
# same thing in both or the nested comparison between them is not a
# comparison.
#
# `fr` must already carry the standardised covariates named in `rhs`, and must
# already have had its uninformative rows removed: what is dropped, and why,
# is the caller's question, not the fitter's.
.check1_fit_hazard <- function(fr, rhs = character(), focal = character()) {
  stopifnot(is.data.frame(fr), nrow(fr) > 0L)

  if (length(unique(fr$YEAR_IN_STATUS)) > 1L) {
    rhs <- c("factor(YEAR_IN_STATUS)", rhs)
  }
  # No covariate and a single year in status leaves an intercept, which is
  # still a model: the one exit rate there is to estimate.
  if (!length(rhs)) rhs <- "1"
  fml <- stats::as.formula(paste("EXITED_THIS_YEAR ~",
                                 paste(rhs, collapse = " + ")))
  # glm's separation warning is caught rather than printed: it states the same
  # fact `quasi_separated` carries, and carried there it can be read beside
  # the ratio it applies to instead of scrolling past in a console. Every
  # other warning is left alone.
  fit <- withCallingHandlers(
    stats::glm(fml, data = fr, family = stats::binomial()),
    warning = function(w) {
      if (grepl("fitted probabilities numerically 0 or 1",
                conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    })
  cf <- summary(fit)$coefficients
  est <- cf[, "Estimate"]
  se <- .check1_cluster_se(fit, fr$STUDENT_ID)
  z <- est / se
  crit <- stats::qnorm(0.975)

  coefficients <- tibble::tibble(
    TERM = rownames(cf),
    ESTIMATE = unname(est),
    SE = unname(se),
    P_VALUE = 2 * stats::pnorm(-abs(unname(z))),
    RATIO = exp(unname(est)),
    CI_LO = exp(unname(est - crit * se)),
    CI_HI = exp(unname(est + crit * se)),
    FOCAL = rownames(cf) %in% focal
  )

  # Fitted probabilities pinned at 0 or 1 mean a covariate separates the
  # outcome almost perfectly. That happens whenever exit is close to a
  # deterministic function of the score -- which is exactly how the shipped
  # simulation is built -- and it makes that term's ratio uninterpretable
  # even though the fit converges.
  extreme <- any(fit$fitted.values < 1e-8 | fit$fitted.values > 1 - 1e-8)

  list(coefficients = coefficients,
       n_person_periods = nrow(fr),
       n_events = sum(fr$EXITED_THIS_YEAR),
       quasi_separated = extreme)
}

# Cluster-robust (CR1) standard errors on the student.
#
# A student contributes one row per year at risk -- about four and a half on
# the shipped sim -- and the model-based errors would count each as an
# independent observation. That is exact only if nothing unmeasured makes a
# student a faster or slower exiter than their covariates say, which is not a
# thing anyone believes. The estimates are unchanged; the intervals widen to
# reflect the number of students rather than the number of rows.
#
# Computed here rather than through `sandwich` to keep the dependency list as
# it is: for a binomial GLM the score contribution of a row is
# x_i (y_i - mu_i), so summing those within a student and sandwiching the
# model's own covariance is the whole estimator.
.check1_cluster_se <- function(fit, cluster) {
  bread <- stats::vcov(fit)
  x <- stats::model.matrix(fit)
  u <- x * as.numeric(stats::residuals(fit, type = "response"))

  g <- factor(cluster[as.integer(rownames(x))])
  scores <- rowsum(u, g, reorder = FALSE)
  meat <- crossprod(scores)

  n <- nrow(x)
  k <- ncol(x)
  n_g <- nlevels(g)
  # CR1: the usual finite-cluster correction. With one cluster per row it
  # reduces to the familiar HC1.
  adj <- if (n_g > 1L && n > k) (n_g / (n_g - 1L)) * ((n - 1L) / (n - k)) else 1
  v <- bread %*% meat %*% bread * adj
  sqrt(pmax(diag(v), 0))
}

# "There is nothing here to fit" is a condition, not a bug: a cohort with no
# observed exits, or none that survive a covariate's requirements, has no
# model in it. Signalling it with its own class lets run_check1() degrade that
# one slot to NULL while every other error still surfaces as an error -- a
# blanket tryCatch() there would hide real ones.
.check1_unfittable <- function(model, detail = "") {
  stop(structure(
    class = c("check1_unfittable", "error", "condition"),
    list(message = paste0("No person-period rows with an exit survive the ",
                          "covariate requirements, so ", model, " cannot be ",
                          "fitted on this cohort", detail, "."),
         call = NULL)))
}

# Years in status where nobody exited identify no baseline term and carry no
# information about the covariates either; both models drop them and report
# the count. Returns the kept frame and how many rows went.
.check1_drop_eventless <- function(fr) {
  events_by_year <- tapply(fr$EXITED_THIS_YEAR, fr$YEAR_IN_STATUS, sum)
  informative <- names(events_by_year)[events_by_year > 0]
  keep <- as.character(fr$YEAR_IN_STATUS) %in% informative
  list(frame = fr[keep, , drop = FALSE], dropped = sum(!keep))
}

#' Model 1 -- the fitted ELP-only hazard model
#'
#' Model 1 has two halves, and this is the second of them. The first is
#' descriptive and carries no model at all: the life table
#' ([compute_life_table()]), the exit curves ([compute_exit_curve()]) and the
#' product-limit estimate ([compute_survival_est()]) are computed from counts,
#' and ELP enters them only as a way of splitting the curve, never as a
#' predictor. This function is the fitted half the Process Overview asks for:
#' the probability of exiting in each year in status, with the ELP score as
#' the only covariate.
#'
#' It exists mainly to be the baseline [compute_model2()] is nested in. On its
#' own the ELP ratio says something obvious -- students with higher scores exit
#' sooner, because that is what the rule does. What is not obvious is whether
#' the ratio *moves* when lagged achievement joins it, and that comparison is
#' only available if both models are fitted.
#'
#' **It fits on more data than Model 2.** The frame is built without the
#' lagged content score, so no row is lost for a covariate this model does not
#' use; on the shipped simulation Model 2 keeps about a fifth of the
#' person-years Model 1 keeps. Both n's are reported so the difference is
#' visible rather than implied.
#'
#' **What is dropped, and never imputed**: a year with no ELP score (the
#' student was not tested that year) and any year in status where nobody
#' exited, whose baseline term is not identified. Both are counted in
#' `$dropped`.
#'
#' **A level-only headline cannot be fitted.** Where the state's reported
#' result is a determination or a level with no scale behind it, there is no
#' covariate to fit and this function stops; [run_check1()] guards ahead of it
#' and leaves the slot `NULL` with a note.
#'
#' **On simulated data this is machinery, not a finding**: exit in the
#' simulation is a rule applied to the ELP score, so the fit is close to
#' deterministic and `$quasi_separated` will be `TRUE`.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param cohort A single `COHORT_LABEL`.
#' @param start_grade The entry grade the cohorts were built on.
#' @param baseline_elp `FALSE` fits the unconditional model -- one term per
#'   year in status and no covariate. `TRUE` adds the student's entry score.
#'   Never the current year's score: [run_check1()] fits both and stores them
#'   as `$model1` and `$model1_baseline`.
#' @return A list: `$coefficients`, `$ratio` (`NA` when unconditional), `$ci`,
#'   the `n_*` counts, `$dropped`, `$quasi_separated`, `$baseline_elp`,
#'   `$note`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(b, spec)
#' compute_model1(b, spec, cohort = co$COHORT_LABEL[co$SELECTED],
#'                baseline_elp = TRUE)$ratio
compute_model1 <- function(data, spec, cohort, start_grade = "K",
                           baseline_elp = FALSE) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(cohort), length(cohort) == 1L, !is.na(cohort),
            is.logical(baseline_elp), length(baseline_elp) == 1L)
  roster <- el_roster(data)
  .check1_require_fields(roster, "model1")

  pp <- .check1_person_period(data, spec, cohort, start_grade, subject = NULL)
  n_all <- nrow(pp)
  n_students <- length(unique(pp$STUDENT_ID))

  # The unconditional fit asks nothing of a student beyond being at risk, so
  # it keeps every row. Only the baseline-ELP fit needs a score, and only it
  # loses the students who never had one.
  drop_elp <- 0L
  dropped_reasons <- character()
  rhs <- character()
  focal <- character()

  if (baseline_elp) {
    drop_elp <- sum(is.na(pp$INITIAL_ELP))
    if (drop_elp == n_all) {
      stop("Model 1 with baseline ELP needs a headline scale score, and this ",
           "delivery has none for the cohort: the reported result carries a ",
           "level only.", call. = FALSE)
    }
    pp <- pp[!is.na(pp$INITIAL_ELP), , drop = FALSE]
    dropped_reasons <- "No first annual ELP score (the student was never tested)"
    rhs <- "INITIAL_ELP_Z"
    focal <- "INITIAL_ELP_Z"
  }

  cut <- .check1_drop_eventless(pp)
  fr <- cut$frame

  if (!nrow(fr) || sum(fr$EXITED_THIS_YEAR) == 0L) {
    .check1_unfittable(if (baseline_elp) "Model 1 (baseline ELP)" else
                       "Model 1 (unconditional)")
  }

  if (baseline_elp) fr$INITIAL_ELP_Z <- .check1_z(fr$INITIAL_ELP)
  fit <- .check1_fit_hazard(fr, rhs = rhs, focal = focal)

  lead <- if (length(focal)) match(focal, fit$coefficients$TERM) else NA_integer_
  c(fit,
    list(
      baseline_elp = baseline_elp,
      ratio = if (is.na(lead)) NA_real_ else fit$coefficients$RATIO[[lead]],
      ci = if (is.na(lead)) c(NA_real_, NA_real_) else
             c(fit$coefficients$CI_LO[[lead]], fit$coefficients$CI_HI[[lead]]),
      n_person_periods_all = n_all,
      n_students = n_students,
      dropped = tibble::tibble(
        REASON = c(dropped_reasons,
                   "Year in status with no exits (no baseline term to identify)"),
        N = c(if (length(dropped_reasons)) drop_elp, cut$dropped)),
      note = if (baseline_elp) paste0(
        "The score is the student's first annual result, standardised, so ",
        "the ratio is per standard deviation of where students started. It ",
        "is fixed for a student across their whole run: the current year's ",
        "score is what the exit rule is applied to, and regressing exit on ",
        "it would restate the rule rather than test anything. Rows with a ",
        "missing score are dropped, never imputed.")
        else paste0(
        "No covariate: one term per year in status and nothing else. The ",
        "fitted hazards are the life table's annual exit rates, which is ",
        "the point -- this is the descriptive half written as a model, and ",
        "the baseline the others are measured against.")))
}

#' Model 2 -- the ELA-augmented survival model
#'
#' A discrete-time hazard model on the person-period frame: does prior-year
#' content achievement tell you anything about who exits, once the ELP score
#' is already in the model? That is the formal test for skimming bias, caught
#' at the source rather than inferred from downstream results. It is
#' [compute_model1()] with one term added, and the two are meant to be read
#' against each other.
#'
#' The headline is the ratio on lagged achievement. At 1 it adds nothing and
#' exit timing is driven by ELP as intended; away from 1 in either direction,
#' something other than the language test is shaping who exits and when.
#' Strictly this is an odds ratio on the discrete-time hazard, which the
#' Process Overview calls a hazard ratio; both covariates are standardised, so
#' it reads per standard deviation.
#'
#' **What is dropped, and never imputed**: a year with no ELP score (the
#' student was not tested), a year whose prior-year content score does not
#' exist -- which includes every row from the year after the missing
#' assessment year -- and any year in status where nobody exited, because a
#' baseline term for it is not identified. Each is counted in `$dropped` and
#' rendered.
#'
#' **On simulated data this is machinery, not a finding**: the relationship
#' between achievement and exit is a parameter of the generator
#' (`sim_contract.md` 5.2-5.3), so the ratio reports what was built in.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param cohort A single `COHORT_LABEL`.
#' @param start_grade The entry grade the cohorts were built on.
#' @param subject Academic subject to use. Defaults to the spec's first.
#' @param add_current_subject Optional subject whose *same-year* score enters
#'   as an extra covariate -- the Type 3 additional check.
#' @return A list: `$coefficients`, `$ratio`, `$ci`, the `n_*` counts,
#'   `$dropped`, `$subject`, `$note`.
#' @export
compute_model2 <- function(data, spec, cohort, start_grade = "K",
                           subject = NULL, add_current_subject = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(cohort), length(cohort) == 1L, !is.na(cohort))
  if (is.null(el_academic(data))) {
    stop("Model 2 needs the academic table; without it the slot is NULL.",
         call. = FALSE)
  }
  subjects <- spec_academic_subjects(spec)
  if (is.null(subject)) subject <- subjects[[1]]
  if (!subject %in% subjects) {
    stop("Subject '", subject, "' is not in the spec's academic block.",
         call. = FALSE)
  }
  roster <- el_roster(data)
  .check1_require_fields(roster, "model2")

  pp <- .check1_person_period(data, spec, cohort, start_grade, subject)
  n_all <- nrow(pp)
  n_students <- length(unique(pp$STUDENT_ID))

  gap <- .check1_gap_year(data)
  drop_elp <- sum(is.na(pp$INITIAL_ELP))
  missing_lag <- !is.na(pp$INITIAL_ELP) & is.na(pp$ACADEMIC_LAG)
  # The two reasons a lag is missing are worth separating: one is the
  # calendar (a year with no assessment anywhere), the other is structural
  # (content testing starts in grade 3, so an early-grade cohort has no
  # prior-year score for its first years at all).
  drop_gap <- if (is.na(gap)) 0L else sum(missing_lag & pp$YEAR == gap)
  drop_lag_other <- sum(missing_lag) - drop_gap
  fr <- pp[!is.na(pp$INITIAL_ELP) & !is.na(pp$ACADEMIC_LAG), , drop = FALSE]

  cut <- .check1_drop_eventless(fr)
  drop_no_events <- cut$dropped
  fr <- cut$frame

  if (!nrow(fr) || sum(fr$EXITED_THIS_YEAR) == 0L) {
    .check1_unfittable(
      "Model 2",
      " (it also needs a prior-year content score on every row it keeps)")
  }

  fr$INITIAL_ELP_Z <- .check1_z(fr$INITIAL_ELP)
  fr$ACADEMIC_LAG_Z <- .check1_z(fr$ACADEMIC_LAG)
  rhs <- c("INITIAL_ELP_Z", "ACADEMIC_LAG_Z")

  if (!is.null(add_current_subject)) {
    ac <- el_academic(data)
    ac <- ac[ac$SUBJECT %in% add_current_subject, , drop = FALSE]
    fr$CRITERION_Z <- .check1_z(
      ac$SCALE_SCORE[match(paste(fr$STUDENT_ID, fr$YEAR),
                           paste(ac$STUDENT_ID, ac$YEAR))])
    fr <- fr[!is.na(fr$CRITERION_Z), , drop = FALSE]
    rhs <- c(rhs, "CRITERION_Z")
  }

  fit <- .check1_fit_hazard(fr, rhs = rhs,
                            focal = c("INITIAL_ELP_Z", "ACADEMIC_LAG_Z",
                                      "CRITERION_Z"))
  coefficients <- fit$coefficients
  lagged <- match("ACADEMIC_LAG_Z", coefficients$TERM)

  list(
    coefficients = coefficients,
    subject = subject,
    ratio = coefficients$RATIO[[lagged]],
    ci = c(coefficients$CI_LO[[lagged]], coefficients$CI_HI[[lagged]]),
    n_person_periods = fit$n_person_periods,
    n_person_periods_all = n_all,
    n_students = n_students,
    n_events = fit$n_events,
    quasi_separated = fit$quasi_separated,
    dropped = tibble::tibble(
      REASON = c(
        "No first annual ELP score (the student was never tested)",
        sprintf(paste0("No prior-year %s score: content testing does not ",
                       "reach that grade, or the student was untested"),
                subject),
        if (is.na(gap)) {
          sprintf("No prior-year %s score for any other reason", subject)
        } else {
          sprintf(paste0("No prior-year %s score because %d has no ",
                         "assessment anywhere (every %d row)"),
                  subject, gap - 1L, gap)
        },
        "Year in status with no exits (no baseline term to identify)"),
      N = c(drop_elp, drop_lag_other, drop_gap, drop_no_events)),
    note = paste0(
      "Both covariates are standardised, so each ratio is per standard ",
      "deviation. Rows with a missing score are dropped, never imputed."))
}

.check1_z <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x - mean(x, na.rm = TRUE))
  (x - mean(x, na.rm = TRUE)) / s
}

# The year whose lag is missing because the year before it has no academic
# rows at all -- read from the data, never a literal.
.check1_gap_year <- function(data) {
  ac <- el_academic(data)
  if (is.null(ac)) return(NA_integer_)
  yrs <- sort(unique(ac$YEAR))
  gaps <- yrs[which(diff(yrs) > 1L) + 1L]
  if (length(gaps)) as.integer(gaps[[1]]) else NA_integer_
}

# ---- the experimental weighting index ---------------------------------------

#' The candidate grade-weighting index (experimental)
#'
#' The Process Overview's sketch for deciding how much weight a later grade's
#' findings can carry: **retention share**, the fraction of the entry cohort
#' still at risk, as a hard floor -- the same logic as minimum-cell
#' suppression -- and **selection strength**, how far Model 2's ratio sits
#' from 1, as a continuous downweight above that floor.
#'
#' **This is not an established statistic.** It is untested against real data,
#' nothing in this package uses it mechanically, and everything it produces
#' renders with an experimental label until it has been validated against the
#' Indiana pipeline. `[PROPOSED - CONFIRM]`
#'
#' @param survival_est Output of [compute_survival_est()].
#' @param model2 Output of [compute_model2()], or `NULL`.
#' @param spec An `el_spec`; `min_cell` sets the floor.
#' @return A list of three slots: `$retention_share`, `$selection_strength`,
#'   `$grade_weights`.
#' @export
compute_grade_weights <- function(survival_est, model2, spec) {
  stopifnot(is.data.frame(survival_est), inherits(spec, "el_spec"),
            all(c("YEAR_IN_STATUS", "A") %in% names(survival_est)))

  retention <- tibble::tibble(
    YEAR_IN_STATUS = survival_est$YEAR_IN_STATUS,
    A = survival_est$A,
    RETENTION_SHARE = survival_est$A / survival_est$A[[1]]
  )
  strength <- if (is.null(model2)) NA_real_ else abs(model2$ratio - 1)
  floor_n <- spec_min_cell(spec)

  weights <- retention
  weights$BELOW_FLOOR <- weights$A < floor_n
  weights$SELECTION_STRENGTH <- strength
  weights$WEIGHT <- ifelse(weights$BELOW_FLOOR, 0, 1 / (1 + strength))
  weights$EXPERIMENTAL <- TRUE

  list(
    retention_share = retention,
    selection_strength = list(
      value = strength,
      basis = if (is.null(model2)) NA_character_
              else sprintf("|ratio on lagged %s - 1|", model2$subject),
      experimental = TRUE),
    grade_weights = weights
  )
}
