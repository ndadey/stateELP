# Fixtures for Check 2.
#
# Module-scoped names (make_check2_*): testthat sources every helper into one
# shared environment, so a generic name would clobber another module's
# builder. The exit-dating convention itself is tested against the deck's A-J
# example in test-utils.R; what this file adds is a small cohort whose exit
# outcomes are STATED, so every expected count in test-check2.R is known by
# construction rather than read back from the code under test.

make_check2_spec <- function() read_el_spec("00_SIM_EL_Spec")

# A Type 6 spec whose domain rule can actually be passed. The shipped ELPA21
# spec asks for level 4 or higher on a three-level scale, so under it every
# student fails every criterion and nobody is ever blocked by exactly one --
# which is the wrong shape to test the gated Additional check against. Lowering
# the terms to the top level of the declared scale is the smallest change that
# makes the criteria able to disagree, which is what the check is about.
make_check2_elpa21_spec <- function() {
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  top <- length(spec$elp$level_scale$levels)
  spec$policy$exit$pathways[[1]]$terms <-
    lapply(spec$policy$exit$pathways[[1]]$terms, function(t) {
      t$value <- top
      t
    })
  spec
}

# An ELPA21-shaped cohort carrying all four domain levels, arranged so that the
# criteria disagree: some students clear every domain, some fail exactly one,
# some fail two. That is what gives Prep 2's crosswalk a
# blocked-by-one-criterion group for Check 2 to cross with the exit outcomes.
#
# Group sizes sit above min_cell so the Additional check's cells are not all
# suppressed. Every student's domain levels are constant across years, so the
# "most often blocked by" resolution has an unambiguous answer to find.
make_check2_elpa21_blocked_bundle <- function(spec = make_check2_elpa21_spec(),
                                              panel = 2015:2025) {
  top <- length(spec$elp$level_scale$levels)
  domains <- spec_components(spec, "domain")
  start <- min(panel) + 1L
  last <- max(panel)

  # Each profile: which domains fall short, and after how many years in status
  # the student exits (NA = never).
  profiles <- list(
    list(tag = "PASS", n = 20L, short = character(), exit_at = 3L),
    list(tag = "WONLY", n = 15L, short = "writing", exit_at = 8L),
    list(tag = "RONLY", n = 15L, short = "reading", exit_at = NA_integer_),
    list(tag = "TWO", n = 12L, short = c("reading", "writing"),
         exit_at = NA_integer_)
  )

  roster <- list()
  base <- list()
  for (p in profiles) {
    for (i in seq_len(p$n)) {
      id <- sprintf("%s_%03d", p$tag, i)
      if (!is.na(p$exit_at)) {
        exit_year <- start + p$exit_at - 1L
        yrs <- start:min(exit_year + 1L, last)
        status <- ifelse(yrs < exit_year, "current", "former")
      } else {
        yrs <- start:last
        status <- rep("current", length(yrs))
      }
      idx <- yrs - start
      roster[[length(roster) + 1L]] <- tibble::tibble(
        STUDENT_ID = id, YEAR = as.integer(yrs),
        GRADE = ifelse(idx == 0L, "K", as.character(idx)),
        EL_STATUS = status, ELP_FORM = "annual", LEFT_TRUNCATED = FALSE,
        HOME_LANGUAGE = c("spa", "por")[(i %% 2L) + 1L],
        GENDER = c("M", "F")[(i %% 2L) + 1L])

      cur <- as.integer(yrs[status == "current"])
      for (d in domains) {
        base[[length(base) + 1L]] <- tibble::tibble(
          STUDENT_ID = id, YEAR = cur, COMPONENT = d, ELP_FORM = "annual",
          SCALE_SCORE = NA_real_,
          ACHIEVEMENT_LEVEL = if (d %in% p$short) top - 1L else top)
      }
      base[[length(base) + 1L]] <- tibble::tibble(
        STUDENT_ID = id, YEAR = cur, COMPONENT = spec_headline(spec),
        ELP_FORM = "annual", SCALE_SCORE = NA_real_,
        ACHIEVEMENT_LEVEL = if (length(p$short)) top - 1L else top)
    }
  }
  as_el_data(dplyr::bind_rows(base), roster = dplyr::bind_rows(roster),
             spec = spec)
}

# One usable K cohort. Under the SIM spec the maximum timeline is 6 years and
# the on-time timeline 5, so the groups are:
#   on time        exit in year in status 3       (Exit_OnTime, Exit_Max, Exit_Ever)
#   late, in max   exit in year in status 6       (Exit_Max, Exit_Ever)
#   late, past max exit in year in status 8       (Exit_Ever only)
#   never          current through the panel      (No_Exit)
#   disappear      last seen in year in status 4  (Disappeared)
# Exit year is the FIRST FORMER year and counts as a year in status (CLAUDE.md
# 4). The cohort enters one year after the panel opens, so it is not
# left-truncated and has ten observed years -- enough for the year-8 exits.
#
# `n_*` are student counts. Defaults sit above the SIM spec's min_cell of 10 so
# the unsuppressed arithmetic is tested first; the suppression tests override
# one group at a time.
make_check2_roster <- function(n_ontime = 12L, n_late_max = 12L,
                               n_late_ever = 12L, n_never = 12L,
                               n_disappear = 12L, panel = 2015:2025) {
  start <- min(panel) + 1L
  last <- max(panel)

  student <- function(id, exit_at = NA_integer_, vanish_after = NA_integer_) {
    if (!is.na(exit_at)) {
      exit_year <- start + exit_at - 1L
      years <- start:min(exit_year + 1L, last)       # one monitoring year
      status <- ifelse(years < exit_year, "current", "former")
    } else if (!is.na(vanish_after)) {
      years <- start:(start + vanish_after - 1L)
      status <- rep("current", length(years))
    } else {
      years <- start:last
      status <- rep("current", length(years))
    }
    idx <- years - start
    tibble::tibble(
      STUDENT_ID = id, YEAR = as.integer(years),
      GRADE = ifelse(idx == 0L, "K", as.character(idx)),
      EL_STATUS = status, LEFT_TRUNCATED = FALSE)
  }

  groups <- list(
    list(n = n_ontime,    tag = "ON",  exit_at = 3L),
    list(n = n_late_max,  tag = "LM",  exit_at = 6L),
    list(n = n_late_ever, tag = "LE",  exit_at = 8L),
    list(n = n_never,     tag = "NV"),
    list(n = n_disappear, tag = "DIS", vanish_after = 4L))

  rows <- lapply(groups, function(g) {
    lapply(seq_len(g$n), function(i) {
      student(sprintf("%s_%03d", g$tag, i),
              exit_at = g$exit_at %||% NA_integer_,
              vanish_after = g$vanish_after %||% NA_integer_)
    })
  })
  dplyr::bind_rows(rows)
}

# The bundle the stage takes. The base table carries one headline row per
# current student-year -- the minimum viable delivery of CLAUDE.md 4, which is
# all Check 2's crosstab needs.
make_check2_bundle <- function(roster = make_check2_roster(),
                               spec = make_check2_spec()) {
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

# The shipped simulation, built once per test run rather than per test: the
# bundle joins 1.8M base rows against the roster.
make_check2_sim_bundle <- local({
  b <- NULL
  function() {
    if (is.null(b)) {
      b <<- as_el_data(el_long_sim, roster = el_roster_sim,
                       academic = academic_long_sim, spec = make_check2_spec())
    }
    b
  }
})
