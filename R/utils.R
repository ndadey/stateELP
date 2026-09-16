# Shared derivations. Built once here, called by every stage; a stage session
# may call these and may not edit them (CLAUDE.md 14).
#
# Every derivation reads the roster -- the counting frame -- and its ALL-CAPS
# columns (sim_contract.md 0.4.0), and every column it adds is ALL CAPS too.
# Nothing here names a component or reads the base table below the headline.
#
# The exit-dating convention is the load-bearing decision in this file. The
# package dates exit to the FIRST year observed as `former`, counts that year
# as a year in status, and treats exit as absorbing. That is the deck's A-J
# convention (slides 43-51), in which student E -- EL in K, 1 and 2, R in
# grade 3 -- exits in year 4, not year 3. A delivery that flags exit on the
# last `current` row (as the simulator does) is off by one against this; see
# test-utils.R, which encodes both the convention and its counter-test.

# ---- suppression -------------------------------------------------------------

#' Suppress small cells
#'
#' Sets value columns to `NA` where the cell count is below the spec's
#' `min_cell`. Suppressed cells stay as rows: dropping them would silently
#' change a denominator and hide which groups were suppressed, which is the
#' opposite of what suppression is for.
#'
#' @param x A data frame carrying a count column.
#' @param spec An `el_spec`; `min_cell` is read from `policy$suppression` and
#'   nowhere else.
#' @param n_col Name of the count column tested against `min_cell`.
#' @param cols Columns to blank. Defaults to every numeric column except
#'   `n_col` itself, so the count that *caused* the suppression stays visible.
#' @param blank_n Whether to blank `n_col` as well. `TRUE` when even the count
#'   is disclosive.
#' @return `x` with suppressed cells set to `NA` and a logical `SUPPRESSED`
#'   column.
#' @export
apply_suppression <- function(x, spec, n_col = "N", cols = NULL,
                              blank_n = FALSE) {
  stopifnot(is.data.frame(x), inherits(spec, "el_spec"),
            n_col %in% names(x))
  min_cell <- spec_min_cell(spec)
  hit <- !is.na(x[[n_col]]) & x[[n_col]] < min_cell

  if (is.null(cols)) {
    cols <- names(x)[vapply(x, is.numeric, TRUE)]
    cols <- setdiff(cols, n_col)
  }
  for (cl in intersect(cols, names(x))) x[[cl]][hit] <- NA
  if (blank_n) x[[n_col]][hit] <- NA

  x$SUPPRESSED <- hit
  x
}

# ---- banding -----------------------------------------------------------------

#' Bin a headline level into spec bands
#'
#' Bands are left-closed: a level exactly at a break belongs to the band above.
#' The 4.5 break in the WIDA-shaped scheme *is* the exit threshold, so the top
#' bands hold only students in their exit year.
#'
#' When the spec declares no `bands` the package default is one band per
#' level. For a categorical level scale the base table stores the ordinal
#' index (sim_contract.md 4.2), so the band is that level's label from
#' `level_scale$levels`; a level already delivered as its label passes through.
#'
#' @param level Headline `ACHIEVEMENT_LEVEL`: numeric, or labels for a
#'   categorical scale.
#' @param spec An `el_spec`.
#' @return A character vector of band labels, `NA` where `level` is `NA`.
#' @export
assign_elp_band <- function(level, spec) {
  stopifnot(inherits(spec, "el_spec"))
  bands <- spec_bands(spec)
  ls <- spec$elp$level_scale

  if (is.null(bands)) {
    # One band per level.
    if (identical(ls$type, "categorical") && is.numeric(level) &&
        !is.null(ls$levels)) {
      return(ls$levels[level])
    }
    return(as.character(level))
  }
  if (!is.numeric(level)) {
    stop("`level` must be numeric to bin against numeric band breaks; ",
         "this spec declares bands but a non-numeric level was supplied.",
         call. = FALSE)
  }
  as.character(cut(level, breaks = bands$breaks, labels = bands$labels,
                   right = FALSE, include.lowest = TRUE))
}

# ---- weighting ---------------------------------------------------------------

#' Weighted counts and shares
#'
#' Never-EL rows in a delivery may be a weighted sample; every statewide share
#' must weight and every within-EL statistic must not. These wrappers exist so
#' that a weighted total is visibly a weighted total at the call site.
#'
#' @param w A weight vector, typically `SAMPLE_WEIGHT`. `NULL` is treated as
#'   unweighted.
#' @param x A logical or numeric vector to total.
#' @param na.rm Whether to drop `NA`s.
#' @return A single number.
#' @export
weighted_n <- function(w = NULL, na.rm = TRUE) {
  if (is.null(w)) return(NA_real_)
  sum(w, na.rm = na.rm)
}

#' @rdname weighted_n
#' @export
weighted_share <- function(x, w = NULL, na.rm = TRUE) {
  if (is.null(w)) w <- rep(1, length(x))
  keep <- if (na.rm) !is.na(x) & !is.na(w) else rep(TRUE, length(x))
  if (!any(keep)) return(NA_real_)
  sum(as.numeric(x[keep]) * w[keep]) / sum(w[keep])
}

# ---- status transitions ------------------------------------------------------

# min()/max() over an empty slice returns -Inf/Inf with a warning, and being
# double it promotes an otherwise-integer year column. dplyr also evaluates
# summarise() expressions once on an empty slice to infer the result type, so
# this fires even for data where no group is actually empty.
.min_year <- function(x) if (length(x)) as.integer(min(x)) else NA_integer_
.max_year <- function(x) if (length(x)) as.integer(max(x)) else NA_integer_

.roster_cols <- function(roster, need = c("STUDENT_ID", "YEAR", "EL_STATUS")) {
  stopifnot(is.data.frame(roster))
  miss <- setdiff(need, names(roster))
  if (length(miss)) {
    stop("Roster is missing: ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  }
}

# First current year and first former year per student. The first former
# year only counts once a student has been current: a former row with no
# earlier current row is an exit nobody observed.
.status_marks <- function(roster) {
  first_cur <- roster |>
    dplyr::filter(.data$EL_STATUS %in% "current") |>
    dplyr::group_by(.data$STUDENT_ID) |>
    dplyr::summarise(FIRST_CURRENT_YEAR = .min_year(.data$YEAR),
                     .groups = "drop")
  first_fmr <- roster |>
    dplyr::filter(.data$EL_STATUS %in% "former") |>
    dplyr::inner_join(first_cur, by = "STUDENT_ID") |>
    dplyr::filter(.data$YEAR > .data$FIRST_CURRENT_YEAR) |>
    dplyr::group_by(.data$STUDENT_ID) |>
    dplyr::summarise(FIRST_FORMER_YEAR = .min_year(.data$YEAR),
                     .groups = "drop")
  dplyr::left_join(first_cur, first_fmr, by = "STUDENT_ID")
}

#' Recompute year in status from observed transitions
#'
#' Years since the first year observed as `current`, counting that year as 1
#' and counting the **exit year** -- the first year observed as `former` -- as
#' a year in status (the deck's A-J convention). Monitoring years after the
#' exit year are not years in status and are `NA`; so is every row before a
#' student's first `current` year and every never-EL row. Exit is absorbing:
#' a `current` row after the exit year is also `NA`.
#'
#' Computed from `YEAR` arithmetic rather than row position, so a gap year
#' does not renumber the years that follow it.
#'
#' A delivered `YEAR_IN_STATUS` -- the sim ships one -- is **replaced**: the
#' package recomputes every derivation and never trusts a delivered one. Keep
#' a copy first if it is wanted as a validation input.
#'
#' @param roster A roster with `STUDENT_ID`, `YEAR`, `EL_STATUS`.
#' @return `roster` with an integer `YEAR_IN_STATUS`.
#' @export
add_year_in_status <- function(roster) {
  .roster_cols(roster)
  marks <- .status_marks(roster)
  out <- dplyr::left_join(roster[setdiff(names(roster), "YEAR_IN_STATUS")],
                          marks, by = "STUDENT_ID")
  exit_row <- !is.na(out$FIRST_FORMER_YEAR) &
    out$YEAR == out$FIRST_FORMER_YEAR
  in_status <- !is.na(out$FIRST_CURRENT_YEAR) &
    out$YEAR >= out$FIRST_CURRENT_YEAR &
    (is.na(out$FIRST_FORMER_YEAR) | out$YEAR <= out$FIRST_FORMER_YEAR) &
    (out$EL_STATUS %in% "current" | exit_row)
  out$YEAR_IN_STATUS <- dplyr::if_else(
    in_status, as.integer(out$YEAR - out$FIRST_CURRENT_YEAR + 1L), NA_integer_)
  out[setdiff(names(out), c("FIRST_CURRENT_YEAR", "FIRST_FORMER_YEAR"))]
}

#' Flag long-term EL student-years
#'
#' `LT_EL` is `TRUE` on a `current` row at or beyond
#' `policy$timelines$lt_el_years` years in status. A spec that records the
#' long-term-EL definition only as text (`lt_el_definition`) cannot be
#' evaluated, so `LT_EL` is then `NA` throughout rather than a guess.
#'
#' @param roster A roster carrying `YEAR_IN_STATUS` from [add_year_in_status()].
#' @param spec An `el_spec`.
#' @return `roster` with a logical `LT_EL`.
#' @export
add_lt_el <- function(roster, spec) {
  .roster_cols(roster, c("EL_STATUS", "YEAR_IN_STATUS"))
  stopifnot(inherits(spec, "el_spec"))
  yrs <- spec$policy$timelines$lt_el_years
  roster$LT_EL <- if (is.null(yrs)) {
    NA
  } else {
    roster$EL_STATUS %in% "current" &
      !is.na(roster$YEAR_IN_STATUS) & roster$YEAR_IN_STATUS >= yrs
  }
  roster
}

#' Collapse a roster to one row per student
#'
#' The event table both cohort checks run on: when each student was first
#' observed in status, when they exited, when they were last seen, and whether
#' they disappeared.
#'
#' Definitions, fixed by the deck's A-J example:
#' * **exit year** -- the first year observed as `former`, counted as a year in
#'   status. Exit is absorbing: nothing after it matters.
#' * **disappeared** -- the first year absent from the data with no prior exit,
#'   provided that year is inside the observation window. Rows after an
#'   absence are not read: once censored, a student has left the risk set, so
#'   the life table's recurrence holds.
#' * **window edge** -- still `current` in the window's last year. Disjoint
#'   from disappeared, so that Check 2's columns sum to the total.
#'
#' Censored is not did-not-exit: both censor types are unobserved outcomes.
#' Students observed only as `former` (they exited before the data began) are
#' not at risk in the panel and have no span.
#'
#' @param roster A roster with `STUDENT_ID`, `YEAR`, `GRADE`, `EL_STATUS`;
#'   `LEFT_TRUNCATED` and `INITIAL_ELP` are carried onto the span when present.
#' @param last_year Last year of the observation window. Defaults to the last
#'   year present. Pass it explicitly when the window extends past the last
#'   year anyone was seen, or every student whose final row is that year is
#'   counted as still enrolled rather than disappeared.
#' @return One row per student with a `current` row: `STUDENT_ID`,
#'   `START_GRADE`, `FIRST_CURRENT_YEAR`, `EXIT_YEAR`, `EXIT_YEAR_IN_STATUS`,
#'   `LAST_OBSERVED_YEAR`, `LAST_YEAR_IN_STATUS`, `CENSOR_TYPE`,
#'   `CENSOR_YEAR`, `CENSORED`, `EXITED`, plus `LEFT_TRUNCATED` and
#'   `INITIAL_ELP` when the roster has them.
#' @export
build_student_spans <- function(roster, last_year = NULL) {
  .roster_cols(roster, c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS"))
  if (is.null(last_year)) last_year <- max(roster$YEAR, na.rm = TRUE)
  last_year <- as.integer(last_year)

  ever <- roster[roster$EL_STATUS %in% c("current", "former"),
                 c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS")]
  marks <- .status_marks(ever)

  ev <- ever |>
    dplyr::inner_join(marks[c("STUDENT_ID", "FIRST_CURRENT_YEAR")],
                      by = "STUDENT_ID") |>
    dplyr::filter(.data$YEAR >= .data$FIRST_CURRENT_YEAR) |>
    dplyr::arrange(.data$STUDENT_ID, .data$YEAR)

  # The continuous run from the first current year: the first absent year ends
  # observation. Vectorised rather than grouped -- a row stays in the run when
  # no gap has occurred since its student's first row.
  n <- nrow(ev)
  if (n) {
    first_row <- c(TRUE, ev$STUDENT_ID[-1L] != ev$STUDENT_ID[-n])
    gap <- !first_row & c(FALSE, diff(ev$YEAR) != 1L)
    gaps_so_far <- cumsum(gap)
    ev <- ev[gaps_so_far == gaps_so_far[first_row][cumsum(first_row)], ]
  }

  run <- ev |>
    dplyr::group_by(.data$STUDENT_ID) |>
    dplyr::summarise(
      FIRST_CURRENT_YEAR = dplyr::first(.data$FIRST_CURRENT_YEAR),
      START_GRADE        = dplyr::first(.data$GRADE),
      EXIT_YEAR          = .min_year(.data$YEAR[.data$EL_STATUS == "former"]),
      LAST_RUN_YEAR      = .max_year(.data$YEAR),
      .groups = "drop"
    )

  spans <- run |>
    dplyr::mutate(
      EXITED = !is.na(.data$EXIT_YEAR),
      EXIT_YEAR_IN_STATUS = as.integer(
        .data$EXIT_YEAR - .data$FIRST_CURRENT_YEAR + 1L),
      LAST_OBSERVED_YEAR = dplyr::if_else(.data$EXITED, .data$EXIT_YEAR,
                                          .data$LAST_RUN_YEAR),
      LAST_YEAR_IN_STATUS = as.integer(
        .data$LAST_OBSERVED_YEAR - .data$FIRST_CURRENT_YEAR + 1L),
      CENSOR_TYPE = dplyr::case_when(
        .data$EXITED                          ~ NA_character_,
        .data$LAST_RUN_YEAR < .env$last_year  ~ "disappeared",
        TRUE                                  ~ "window_edge"
      ),
      CENSOR_YEAR = dplyr::if_else(.data$CENSOR_TYPE %in% "disappeared",
                                   .data$LAST_RUN_YEAR + 1L, NA_integer_),
      CENSORED = !is.na(.data$CENSOR_TYPE)
    )

  if ("LEFT_TRUNCATED" %in% names(roster)) {
    truncated <- unique(roster$STUDENT_ID[roster$LEFT_TRUNCATED %in% TRUE])
    spans$LEFT_TRUNCATED <- spans$STUDENT_ID %in% truncated
  }
  if ("INITIAL_ELP" %in% names(roster)) {
    ie <- roster[!is.na(roster$INITIAL_ELP), c("STUDENT_ID", "YEAR", "INITIAL_ELP")]
    ie <- ie[order(ie$STUDENT_ID, ie$YEAR), ]
    ie <- ie[!duplicated(ie$STUDENT_ID), c("STUDENT_ID", "INITIAL_ELP")]
    spans <- dplyr::left_join(spans, ie, by = "STUDENT_ID")
  }

  spans[c("STUDENT_ID", "START_GRADE", "FIRST_CURRENT_YEAR", "EXIT_YEAR",
          "EXIT_YEAR_IN_STATUS", "LAST_OBSERVED_YEAR", "LAST_YEAR_IN_STATUS",
          "CENSOR_TYPE", "CENSOR_YEAR", "CENSORED", "EXITED",
          intersect(c("LEFT_TRUNCATED", "INITIAL_ELP"), names(spans)))]
}

# The on-time timeline per student. `on_time_rule$default_years`, falling back
# to the ESSA timeline and then the maximum; `by_start_grade` overrides the
# default, and `by_initial_band` overrides both. [PROPOSED - CONFIRM] the
# precedence when a spec keys on both.
.on_time_years <- function(spans, spec) {
  tl <- spec$policy$timelines
  rule <- tl$on_time_rule
  default <- rule$default_years %||% tl$essa_timeline_years %||%
    tl$max_years_to_exit
  out <- rep(as.integer(default), nrow(spans))

  if (!is.null(rule$by_start_grade)) {
    if (!"START_GRADE" %in% names(spans)) {
      stop("The on-time rule is keyed by start grade; spans need START_GRADE.",
           call. = FALSE)
    }
    hit <- spans$START_GRADE %in% names(rule$by_start_grade)
    out[hit] <- as.integer(rule$by_start_grade[spans$START_GRADE[hit]])
  }
  if (!is.null(rule$by_initial_band)) {
    if (!"INITIAL_ELP" %in% names(spans)) {
      stop("The on-time rule is keyed by initial ELP band; spans need ",
           "INITIAL_ELP.", call. = FALSE)
    }
    hit <- spans$INITIAL_ELP %in% names(rule$by_initial_band)
    out[hit] <- as.integer(rule$by_initial_band[spans$INITIAL_ELP[hit]])
  }
  out
}

#' Add the nested exit indicators
#'
#' `EXIT_ONTIME` is a subset of `EXIT_MAX`, which is a subset of `EXIT_EVER`.
#' All three are window-relative: a student who exits in year 7 of a six-year
#' timeline is an ever-exiter and not a max-timeline exiter, and a caption has
#' to say so. `NO_EXIT` and `DISAPPEARED` are disjoint, so exiters, no-exit
#' and disappeared sum to the cohort.
#'
#' The on-time timeline comes from `policy$timelines$on_time_rule` -- its
#' `default_years`, overridden per student by `by_start_grade` and then by
#' `by_initial_band` -- and is returned as `ON_TIME_YEARS`. The on-time /
#' prolonged split is Check 2's headline: `PROLONGED` is late exiters plus the
#' never-exited, and disappeared students are their own column, never folded
#' into either group.
#'
#' @param spans Output of [build_student_spans()].
#' @param spec An `el_spec`; timelines are read from `policy$timelines`.
#' @return `spans` with `ON_TIME_YEARS`, `EXIT_EVER`, `EXIT_MAX`,
#'   `EXIT_ONTIME`, `NO_EXIT`, `DISAPPEARED`, `ON_TIME` and `PROLONGED`.
#' @export
add_exit_flags <- function(spans, spec) {
  stopifnot(is.data.frame(spans), inherits(spec, "el_spec"),
            all(c("EXITED", "EXIT_YEAR_IN_STATUS", "CENSOR_TYPE") %in%
                  names(spans)))
  max_years <- spec_max_years(spec)
  spans$ON_TIME_YEARS <- .on_time_years(spans, spec)

  spans |>
    dplyr::mutate(
      EXIT_EVER   = .data$EXITED,
      EXIT_MAX    = .data$EXITED & !is.na(.data$EXIT_YEAR_IN_STATUS) &
        .data$EXIT_YEAR_IN_STATUS <= .env$max_years,
      EXIT_ONTIME = .data$EXIT_MAX &
        .data$EXIT_YEAR_IN_STATUS <= .data$ON_TIME_YEARS,
      NO_EXIT     = .data$CENSOR_TYPE %in% "window_edge",
      DISAPPEARED = .data$CENSOR_TYPE %in% "disappeared",
      ON_TIME     = .data$EXIT_ONTIME,
      PROLONGED   = (.data$EXIT_EVER & !.data$EXIT_ONTIME) | .data$NO_EXIT
    )
}

#' Build a starting-grade cohort
#'
#' A cohort is students who entered EL status in the same grade in the same
#' year. Left-truncated students -- already in status when the panel opened --
#' are excluded by construction: their duration is measured from an entry
#' nobody observed, and including them understates time in status by up to the
#' whole maximum timeline. When the roster carries no `LEFT_TRUNCATED`, entry
#' in the panel's first year cannot be told from earlier entry, so those
#' students are treated as left-truncated. `[PROPOSED - CONFIRM]`
#'
#' @param roster A roster with `STUDENT_ID`, `YEAR`, `GRADE`, `EL_STATUS`.
#' @param spec An `el_spec`.
#' @param start_grade Entry grade, e.g. `"K"`.
#' @param entry_years Entry years to keep. `NULL` keeps every year with enough
#'   follow-up for the maximum timeline plus the year that observes an exit in
#'   its last year, derived from the panel range rather than named as literals.
#' @return A tibble: `STUDENT_ID`, `COHORT_LABEL`, `START_GRADE`,
#'   `COHORT_START_YEAR`.
#' @export
build_cohort <- function(roster, spec, start_grade = "K", entry_years = NULL) {
  .roster_cols(roster, c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS"))
  stopifnot(inherits(spec, "el_spec"))

  panel_first <- min(roster$YEAR, na.rm = TRUE)
  panel_last <- max(roster$YEAR, na.rm = TRUE)

  cur <- roster[roster$EL_STATUS %in% "current", c("STUDENT_ID", "YEAR", "GRADE")]
  cur <- cur[order(cur$STUDENT_ID, cur$YEAR), ]
  entrants <- cur[!duplicated(cur$STUDENT_ID), ]
  entrants <- tibble::tibble(STUDENT_ID = entrants$STUDENT_ID,
                             START_GRADE = entrants$GRADE,
                             COHORT_START_YEAR = as.integer(entrants$YEAR))

  truncated <- if ("LEFT_TRUNCATED" %in% names(roster)) {
    unique(roster$STUDENT_ID[roster$LEFT_TRUNCATED %in% TRUE])
  } else {
    entrants$STUDENT_ID[entrants$COHORT_START_YEAR == panel_first]
  }
  entrants <- entrants[!entrants$STUDENT_ID %in% truncated, ]

  if (is.null(entry_years)) {
    # sim_contract.md 2.2: a maximum timeline of k years needs k + 1 years of
    # follow-up (the life table's last row reads its exits from the next
    # year's data), so 2015-2025 gives K-2015 ... K-2019 under k = 6.
    entry_years <- sort(unique(entrants$COHORT_START_YEAR[
      entrants$COHORT_START_YEAR + spec_max_years(spec) <= panel_last]))
  }

  out <- entrants[entrants$START_GRADE %in% start_grade &
                    entrants$COHORT_START_YEAR %in% entry_years, ]
  out$COHORT_LABEL <- paste0(out$START_GRADE, "-", out$COHORT_START_YEAR)
  out[c("STUDENT_ID", "COHORT_LABEL", "START_GRADE", "COHORT_START_YEAR")]
}
