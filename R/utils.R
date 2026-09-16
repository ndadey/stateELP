# Shared derivations. Built once here, called by every stage; a stage session
# may call these and may not edit them (CLAUDE.md 14).
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
#' @return `x` with suppressed cells set to `NA` and a `suppressed` flag column.
#' @export
apply_suppression <- function(x, spec, n_col = "n", cols = NULL,
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
  if (blank_n) x[[n_col]][hit] <- NA_integer_

  x$suppressed <- hit
  x
}

# ---- banding -----------------------------------------------------------------

#' Bin a headline level into spec bands
#'
#' Bands are left-closed: a level exactly at a break belongs to the band above.
#' The 4.5 break in the WIDA-shaped scheme *is* the exit threshold, so the top
#' bands hold only students in their exit year.
#'
#' When the spec declares no `bands` the package default is one band per level,
#' which is what a non-WIDA instrument with a categorical headline gets.
#'
#' @param level Headline level: numeric for a numeric level scale, character
#'   for a categorical one.
#' @param spec An `el_spec`.
#' @return A character vector of band labels, `NA` where `level` is `NA`.
#' @export
assign_elp_band <- function(level, spec) {
  stopifnot(inherits(spec, "el_spec"))
  bands <- spec_bands(spec)

  if (is.null(bands)) {
    # One band per level. A categorical headline is already its own band.
    return(as.character(level))
  }
  if (!is.numeric(level)) {
    stop("`level` must be numeric to bin against numeric band breaks; ",
         "this spec declares bands but a non-numeric level was supplied.",
         call. = FALSE)
  }
  out <- cut(level, breaks = bands$breaks, labels = bands$labels,
             right = FALSE, include.lowest = TRUE)
  as.character(out)
}

# ---- weighting ---------------------------------------------------------------

#' Weighted counts and shares
#'
#' Never-EL rows in a delivery may be a weighted sample; every statewide share
#' must weight and every within-EL statistic must not. These wrappers exist so
#' that a weighted total is visibly a weighted total at the call site.
#'
#' @param w A weight vector. `NULL` or all-`NA` is treated as unweighted.
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

#' Recompute year in status from observed transitions
#'
#' Years since the first year observed as `current`, counting the exit year
#' itself. Computed from `school_year` arithmetic rather than row position, so
#' a gap year does not renumber the years that follow it.
#'
#' `NA` for never-EL students, and for years before a student's first
#' `current` year.
#'
#' @param spine A spine data frame with `student_id`, `school_year`,
#'   `el_status`.
#' @return `spine` with an added integer `year_in_status`.
#' @export
add_year_in_status <- function(spine) {
  stopifnot(is.data.frame(spine),
            all(c("student_id", "school_year", "el_status") %in% names(spine)))

  first_cur <- spine |>
    dplyr::filter(.data$el_status == "current") |>
    dplyr::group_by(.data$student_id) |>
    dplyr::summarise(first_current_year = .min_year(.data$school_year),
                     .groups = "drop")

  spine |>
    dplyr::left_join(first_cur, by = "student_id") |>
    dplyr::mutate(
      year_in_status = dplyr::if_else(
        !is.na(.data$first_current_year) &
          .data$school_year >= .data$first_current_year &
          .data$el_status != "never",
        as.integer(.data$school_year - .data$first_current_year + 1L),
        NA_integer_
      )
    ) |>
    dplyr::select(-"first_current_year")
}

#' Collapse a spine to one row per student
#'
#' The event table both cohort checks run on: when each student entered status,
#' when they exited (first `former` year), when they were last seen, and
#' whether they disappeared.
#'
#' Definitions, fixed by the deck's A-J example:
#' * **exit year** -- the first year observed as `former`. Exit is absorbing:
#'   once exited, a student is an exiter in every later year regardless of
#'   later absence.
#' * **disappeared** -- the first year absent from the data with no prior
#'   exit, provided that year is inside the observation window.
#' * **no exit** -- still `current` at the window's last year. Disjoint from
#'   disappeared, so that Check 2's columns sum to the total.
#'
#' @param spine A spine data frame.
#' @param last_year Last year of the observation window. Defaults to the last
#'   year present. Pass it explicitly when the roster extends past the last
#'   year any cohort member was seen, or every student whose final row is the
#'   last observed year is counted as still-enrolled rather than disappeared.
#' @return One row per ever-EL student: `student_id`, `first_current_year`,
#'   `last_el_year`, `exit_year`, `exit_year_in_status`, `last_observed_year`,
#'   `censor_type`, `censored`, `exited`.
#' @export
build_student_spans <- function(spine, last_year = NULL) {
  stopifnot(is.data.frame(spine),
            all(c("student_id", "school_year", "el_status") %in% names(spine)))
  if (is.null(last_year)) last_year <- max(spine$school_year, na.rm = TRUE)

  ever <- spine |>
    dplyr::filter(.data$el_status %in% c("current", "former"))
  if (nrow(ever) == 0L) {
    return(tibble::tibble(
      student_id = character(), first_current_year = integer(),
      last_el_year = integer(), exit_year = integer(),
      exit_year_in_status = integer(), last_observed_year = integer(),
      censor_type = character(), censored = logical(), exited = logical()
    ))
  }

  ever |>
    dplyr::group_by(.data$student_id) |>
    dplyr::summarise(
      first_current_year = .min_year(
        .data$school_year[.data$el_status == "current"]),
      # First FORMER year: the package's exit date.
      exit_year = .min_year(.data$school_year[.data$el_status == "former"]),
      last_el_year = .max_year(.data$school_year),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      exited = !is.na(.data$exit_year),
      exit_year_in_status = as.integer(
        .data$exit_year - .data$first_current_year + 1L),
      last_observed_year = .data$last_el_year,
      censor_type = dplyr::case_when(
        .data$exited                        ~ NA_character_,
        .data$last_el_year < .env$last_year ~ "disappeared",
        TRUE                                ~ "window_edge"
      ),
      censored = !is.na(.data$censor_type)
    ) |>
    dplyr::select("student_id", "first_current_year", "last_el_year",
                  "exit_year", "exit_year_in_status", "last_observed_year",
                  "censor_type", "censored", "exited")
}

#' Add the nested exit indicators
#'
#' `exit_ontime` is a subset of `exit_max`, which is a subset of `exit_ever`.
#' All three are window-relative: a student who exits in year 7 of a six-year
#' window is an ever-exiter and not a max-timeline exiter, and a caption has to
#' say so. `no_exit` and `disappeared` are disjoint, so the four categories sum
#' to the cohort.
#'
#' The on-time / prolonged split is Check 2's headline: prolonged is late
#' exiters plus the never-exited, and disappeared students are their own
#' column, never folded into either group.
#'
#' @param spans Output of [build_student_spans()].
#' @param spec An `el_spec`; `max_years_to_exit` and the ESSA timeline are read
#'   from `policy$timelines`.
#' @return `spans` with `exit_ever`, `exit_max`, `exit_ontime`, `no_exit`,
#'   `disappeared`, `on_time`, `prolonged` and `lt_el`.
#' @export
add_exit_flags <- function(spans, spec) {
  stopifnot(is.data.frame(spans), inherits(spec, "el_spec"),
            "exit_year_in_status" %in% names(spans))
  max_years <- spec_max_years(spec)
  essa <- spec_essa_years(spec)
  if (is.na(essa)) essa <- max_years

  spans |>
    dplyr::mutate(
      exit_ever   = .data$exited,
      exit_max    = .data$exited & .data$exit_year_in_status <= .env$max_years,
      exit_ontime = .data$exited & .data$exit_year_in_status <= .env$essa,
      disappeared = identical_na(.data$censor_type, "disappeared"),
      no_exit     = identical_na(.data$censor_type, "window_edge"),
      on_time     = .data$exit_ontime,
      prolonged   = (.data$exit_ever & !.data$exit_ontime) | .data$no_exit,
      lt_el       = .data$prolonged
    )
}

# case_when over a character column that is NA for exiters; keeps the result
# logical rather than NA-propagating into the flags.
identical_na <- function(x, value) !is.na(x) & x == value

#' Build a starting-grade cohort
#'
#' A cohort is students who entered EL status in the same grade in the same
#' year. Left-truncated students -- already in status when the panel opened --
#' are excluded by construction: their duration is measured from an entry
#' nobody observed, and including them understates time in status by up to the
#' whole maximum timeline.
#'
#' @param spine A spine data frame.
#' @param spec An `el_spec`.
#' @param start_grade Entry grade, e.g. `"K"`.
#' @param entry_years Entry years to keep. `NULL` keeps every year with enough
#'   follow-up for the maximum timeline, derived from the panel range rather
#'   than named as literals.
#' @return A tibble: `student_id`, `cohort_label`, `start_grade`,
#'   `cohort_start_year`.
#' @export
build_cohort <- function(spine, spec, start_grade = "K", entry_years = NULL) {
  stopifnot(is.data.frame(spine), inherits(spec, "el_spec"))

  panel_last <- max(spine$school_year, na.rm = TRUE)
  need <- spec_max_years(spec) + 1L   # max timeline needs one more year to observe

  entrants <- spine |>
    dplyr::filter(.data$el_status == "current") |>
    dplyr::group_by(.data$student_id) |>
    dplyr::summarise(cohort_start_year = min(.data$school_year),
                     start_grade = .data$grade[which.min(.data$school_year)],
                     .groups = "drop")

  if ("left_truncated" %in% names(spine)) {
    lt <- spine |>
      dplyr::group_by(.data$student_id) |>
      dplyr::summarise(left_truncated = any(.data$left_truncated %in% TRUE),
                       .groups = "drop")
    entrants <- entrants |>
      dplyr::left_join(lt, by = "student_id") |>
      dplyr::filter(!.data$left_truncated %in% TRUE) |>
      dplyr::select(-"left_truncated")
  }

  if (is.null(entry_years)) {
    entry_years <- sort(unique(
      entrants$cohort_start_year[
        entrants$cohort_start_year + need - 1L <= panel_last]))
  }

  entrants |>
    dplyr::filter(.data$start_grade == .env$start_grade,
                  .data$cohort_start_year %in% .env$entry_years) |>
    dplyr::mutate(cohort_label = paste0(.data$start_grade, "-",
                                        .data$cohort_start_year)) |>
    dplyr::select("student_id", "cohort_label", "start_grade",
                  "cohort_start_year")
}
