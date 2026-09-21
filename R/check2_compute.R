# Check 2 -- on-time vs prolonged exiters. Plain data out, no plotting, no I/O.
#
# Check 2 does not select a cohort and does not date an exit. The cohort is
# Check 1's (the caller passes the label from the exported compute_cohorts()),
# and the exit dating, the nested indicators and the on-time / prolonged split
# are R/utils.R's build_student_spans() and add_exit_flags(), called unchanged
# (CLAUDE.md 14). What is new here is the crosstab built on them.

# The crosstab's columns, in the deck's order (slides 63-64). ALL is every
# cohort student; the rest are the indicator columns add_exit_flags() returns.
# EXIT_EVER >= EXIT_MAX >= EXIT_ONTIME are nested, so those three do not add
# up; what adds up is EXIT_EVER + NO_EXIT + DISAPPEARED = ALL.
.CHECK2_FLAG_COLUMNS <- c("EXIT_EVER", "EXIT_MAX", "EXIT_ONTIME",
                          "NO_EXIT", "DISAPPEARED")
.CHECK2_COLUMNS <- c("ALL", .CHECK2_FLAG_COLUMNS)

# Typography, from code points: R sources must be ASCII (see check1_render.R).
.CHECK2_EM_DASH <- intToUtf8(0x2014)

#' The exit indicators for one cohort
#'
#' One row per student in the entry cohort, carrying the nested exit
#' indicators and the on-time / prolonged split. Membership is
#' [build_cohort()]'s and the flags are [add_exit_flags()]'s; this function
#' only joins them for one cohort. The cohort is normally the one Check 1
#' analyzes, taken from [compute_cohorts()] rather than selected again.
#'
#' The indicators are **window-relative**: `NO_EXIT` means not observed
#' exiting by the panel's last year, and a student exiting in year 7 of a
#' six-year timeline is `EXIT_EVER` but not `EXIT_MAX`. `PROLONGED` is late
#' exiters plus `NO_EXIT`. Students who `DISAPPEARED` are in neither group:
#' their outcome is unobserved, and censored is not did-not-exit.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param cohort A single `COHORT_LABEL`, normally the selected row of
#'   [compute_cohorts()].
#' @param start_grade The entry grade those cohorts were built on.
#' @return A tibble, one row per student: `STUDENT_ID`, `COHORT_LABEL`,
#'   `EXIT_YEAR_IN_STATUS`, `ON_TIME_YEARS`, `EXIT_EVER`, `EXIT_MAX`,
#'   `EXIT_ONTIME`, `NO_EXIT`, `DISAPPEARED`, `ON_TIME`, `PROLONGED`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' flags <- compute_exit_flags(bundle, spec, co$COHORT_LABEL[co$SELECTED])
#' colSums(flags[c("EXIT_EVER", "EXIT_ONTIME", "NO_EXIT", "DISAPPEARED")])
compute_exit_flags <- function(data, spec, cohort, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(cohort), length(cohort) == 1L, !is.na(cohort),
            is.character(start_grade), length(start_grade) == 1L)
  roster <- el_roster(data)

  members <- build_cohort(roster, spec, start_grade = start_grade)
  members <- members[members$COHORT_LABEL == cohort, , drop = FALSE]
  if (!nrow(members)) {
    stop("No cohort labelled '", cohort, "' among the ", start_grade,
         " entry cohorts this panel supports.", call. = FALSE)
  }

  # The window is the panel's, not the cohort's: a student last seen in the
  # panel's final year is still enrolled (NO_EXIT), not disappeared.
  rows <- roster[roster$STUDENT_ID %in% members$STUDENT_ID, , drop = FALSE]
  spans <- build_student_spans(rows, last_year = max(roster$YEAR, na.rm = TRUE))
  flags <- add_exit_flags(spans, spec)
  flags$COHORT_LABEL <- members$COHORT_LABEL[match(flags$STUDENT_ID,
                                                   members$STUDENT_ID)]

  flags[c("STUDENT_ID", "COHORT_LABEL", "EXIT_YEAR_IN_STATUS", "ON_TIME_YEARS",
          "EXIT_EVER", "EXIT_MAX", "EXIT_ONTIME", "NO_EXIT", "DISAPPEARED",
          "ON_TIME", "PROLONGED")]
}

#' The slide-63/64 crosstab
#'
#' Columns **All / Exit_Ever / Exit_Max / Exit_OnTime / No_Exit /
#' Disappeared**, in long form: one row per block, row and column. This pass
#' builds the `"Total"` block only -- the count of students in each column
#' (`N`) and that count as a share of the whole cohort (`PCT`, the deck's
#' "% of all EL students" row). The characteristic row blocks (primary
#' language, district size band, ...) arrive in later passes and will use the
#' same shape, with `PCT` the column percent within the block.
#'
#' Suppression follows the spec's `min_cell`: a nonzero count below it is
#' `NA`, along with its share, and the row is kept. A count of **zero** is
#' shown as zero rather than suppressed: it is not a small cell, and blanking
#' it would tell the reader a column was hidden when it is simply empty.
#' `[PROPOSED - CONFIRM]`
#'
#' @param flags Output of [compute_exit_flags()].
#' @param spec An `el_spec`.
#' @return A tibble: `BLOCK`, `ROW`, `COLUMN` (`ALL`, `EXIT_EVER`,
#'   `EXIT_MAX`, `EXIT_ONTIME`, `NO_EXIT`, `DISAPPEARED`), `N`, `PCT`,
#'   `SUPPRESSED`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' flags <- compute_exit_flags(bundle, spec, co$COHORT_LABEL[co$SELECTED])
#' compute_crosstab(flags, spec)
compute_crosstab <- function(flags, spec) {
  stopifnot(is.data.frame(flags), inherits(spec, "el_spec"),
            all(.CHECK2_FLAG_COLUMNS %in% names(flags)))

  n_all <- nrow(flags)
  n <- c(n_all, vapply(.CHECK2_FLAG_COLUMNS,
                       function(cl) sum(flags[[cl]] %in% TRUE), 0L))

  out <- tibble::tibble(
    BLOCK  = "Total",
    ROW    = "Total students",
    COLUMN = .CHECK2_COLUMNS,
    N      = as.integer(n),
    PCT    = if (n_all > 0L) as.numeric(n) / n_all else NA_real_
  )

  raw <- out
  out <- apply_suppression(out, spec, n_col = "N", cols = "PCT",
                           blank_n = TRUE)
  empty <- raw$N == 0L
  out$N[empty]          <- raw$N[empty]
  out$PCT[empty]        <- raw$PCT[empty]
  out$SUPPRESSED[empty] <- FALSE
  out
}
