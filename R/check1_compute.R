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

  co <- .check1_cohort_spans(roster, spec, cohort, start_grade)
  spans <- co$spans
  cohort_rows <- co$cohort_rows
  start_year <- co$start_year
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

  out <- tibble::tibble(YEAR_IN_STATUS = t,
                        GRADE = .cohort_grade_labels(cohort_rows, start_year, t),
                        A = A, B = B, C = C)
  out <- dplyr::bind_cols(out, .life_table_rates(out$A, out$B, out$C))
  apply_suppression(out, spec, n_col = "A",
                    cols = c("B", "C", "D", "E", "F"))
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
  src <- spec$policy$timelines$initial_elp_source
  if (!is.null(src) && !identical(src, "first_annual")) {
    stop("`policy$timelines$initial_elp_source` is \"", src, "\"; this ",
         "package derives the initial band as the first annual headline ",
         "only.", call. = FALSE)
  }
  h <- elp_headline(data, spec)
  h <- h[!is.na(h$ELP_BAND), c("STUDENT_ID", "YEAR", "ELP_BAND"), drop = FALSE]
  h <- h[order(h$STUDENT_ID, h$YEAR), , drop = FALSE]
  h[!duplicated(h$STUDENT_ID), c("STUDENT_ID", "ELP_BAND"), drop = FALSE]
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
