# Fixtures for Check 1.
#
# Module-scoped names (make_check1_*): testthat sources every helper into one
# shared environment, so a generic name would clobber another module's
# builder. The A-J example lives in helper-utils.R and is tested there; what
# this file adds is the deck's slide-52 worked table and small rosters whose
# cohort structure is stated relative to the panel, never as year literals.

# Slide 52, exactly as printed -- including the two A cells that are off by
# one against the slide's own recurrence (19,306 for 19,305 and 8,339 for
# 8,340). D, E and F are the slide's rounded cells. The last row is the
# window's edge: B empty, C = A, and F left blank.
#
# The slide also carries a callout typo, "(6,432 + 5,237 + 4,304) / 27,511 =
# 0.06", where the cell itself reads 0.58. Test the cells, not the callouts.
make_check1_slide52 <- function() {
  tibble::tibble(
    A = c(27511, 19306, 13196, 8339, 5581, 4065),
    B = c(6432, 5237, 4304, 2474, 1305, NA),
    C = c(1774, 873, 552, 284, 211, 4065),
    D = c(0.23, 0.27, 0.33, 0.30, 0.23, 0.00),
    E = c(0.06, 0.05, 0.04, 0.03, 0.04, 1.00),
    F = c(0.23, 0.42, 0.58, 0.67, 0.72, NA)
  )
}

# A roster of K entrants, `n[i]` of them entering in `entry[i]`, every student
# staying current to the end of the panel. Enough to exercise cohort
# candidacy, suppression and selection; exits and disappearances are the A-J
# fixture's job.
#
# `entry` is given as offsets from the panel's first year, so a test can say
# "the first usable year" without naming one.
make_check1_roster <- function(entry_offset = c(0L, 1L), n = c(20L, 20L),
                               panel = 2015:2025, left_truncated = TRUE) {
  stopifnot(length(entry_offset) == length(n))
  panel_first <- min(panel)
  panel_last <- max(panel)

  rows <- list()
  for (i in seq_along(entry_offset)) {
    start <- panel_first + entry_offset[[i]]
    yrs <- start:panel_last
    grades <- c("K", as.character(seq_len(length(yrs) - 1L)))
    for (s in seq_len(n[[i]])) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        STUDENT_ID = sprintf("S%d_%03d", start, s),
        YEAR       = as.integer(yrs),
        GRADE      = grades,
        EL_STATUS  = "current"
      )
    }
  }
  out <- dplyr::bind_rows(rows)
  if (left_truncated) out$LEFT_TRUNCATED <- FALSE
  out
}

# The bundle the stage takes. The base table carries one headline row per
# current student-year -- the minimum viable delivery of CLAUDE.md 4, which
# is all Model 1 needs.
make_check1_bundle <- function(roster, spec = make_check1_spec()) {
  current <- roster[roster$EL_STATUS == "current", , drop = FALSE]
  base <- tibble::tibble(
    STUDENT_ID        = current$STUDENT_ID,
    YEAR              = current$YEAR,
    COMPONENT         = spec_headline(spec),
    ELP_FORM          = "annual",
    SCALE_SCORE       = 400,
    ACHIEVEMENT_LEVEL = 3
  )
  as_el_data(base, roster = roster, spec = spec)
}

make_check1_spec <- function() read_el_spec("00_SIM_EL_Spec")

# An ELPA21-shaped cohort: a categorical headline (an ordinal index into
# level_scale$levels), no declared bands, and a panel long enough for one
# cohort to be followable. The no-WIDA-assumptions smoke test for the curves
# -- component values and level counts come from the spec, never typed here.
make_check1_elpa21_bundle <- function(spec = read_el_spec("00_ELPA21_EL_Spec"),
                                      panel = 2015:2022, n = 30L) {
  headline <- spec_headline(spec)
  n_levels <- length(spec$elp$level_scale$levels)
  panel_last <- max(panel)

  rows <- list()
  for (s in seq_len(n)) {
    # Exit after 3, 5 or 7 years in status; the rest never exit.
    exit_after <- c(3L, 5L, 7L, NA_integer_)[(s %% 4L) + 1L]
    yrs <- min(panel):panel_last
    status <- rep("current", length(yrs))
    if (!is.na(exit_after) && exit_after <= length(yrs)) {
      status[seq(exit_after, length(status))] <- "former"
    }
    rows[[s]] <- tibble::tibble(
      STUDENT_ID     = sprintf("E%03d", s),
      YEAR           = as.integer(yrs),
      GRADE          = c("K", as.character(seq_len(length(yrs) - 1L))),
      EL_STATUS      = status,
      ELP_FORM       = "annual",
      LEFT_TRUNCATED = FALSE
    )
  }
  roster <- dplyr::bind_rows(rows)

  current <- roster[roster$EL_STATUS == "current", , drop = FALSE]
  base <- tibble::tibble(
    STUDENT_ID        = current$STUDENT_ID,
    YEAR              = current$YEAR,
    COMPONENT         = headline,
    ELP_FORM          = "annual",
    SCALE_SCORE       = NA_real_,     # the headline reports a level only
    ACHIEVEMENT_LEVEL = rep_len(seq_len(n_levels), nrow(current))
  )
  as_el_data(base, roster = roster, spec = spec)
}

# The shipped simulation, built once per test run rather than per test: the
# bundle joins 1.8M base rows against the roster.
make_check1_sim_bundle <- local({
  b <- NULL
  function() {
    if (is.null(b)) {
      b <<- as_el_data(el_long_sim, roster = el_roster_sim,
                       academic = academic_long_sim, spec = make_check1_spec())
    }
    b
  }
})

# The selected cohort's label, derived rather than typed: the tests must not
# name a year any more than the package does.
make_check1_selected <- function(bundle = make_check1_sim_bundle(),
                                 spec = make_check1_spec()) {
  co <- compute_cohorts(bundle, spec)
  co$COHORT_LABEL[co$SELECTED]
}

# A Type 3 spec: the SIM rule plus a second criterion drawn from the academic
# table, which is what "composite plus other source" means. The subject comes
# from the spec's own academic block, never typed here.
make_check1_type3_spec <- function() {
  spec <- make_check1_spec()
  subject <- spec_academic_subjects(spec)[[2]]
  spec$policy$exit$types <- 3L
  spec$policy$exit$pathways[[1]]$terms <- c(
    spec$policy$exit$pathways[[1]]$terms,
    list(list(component = subject, from = "academic", measure = "level",
              op = ">=", value = 3))
  )
  validate_el_spec(spec)
  spec
}
