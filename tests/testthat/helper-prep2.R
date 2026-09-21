# Fixtures for Prep 2. Module-scoped names (make_prep2_*): testthat sources
# every helper into one shared environment, so a generic name would clobber
# another module's builder.
#
# The small roster is hand-written so that every expected value in
# test-prep2.R can be derived by hand. Panel 2015-2020, min_cell 10:
#
#   A  24 students, K entrants in 2016 (in the panel), current 2016-2020,
#      grades K..4. A01-A12 in district D1, A13-A24 in D2. A01-A10 exit:
#      former in 2019 (monitoring year 1) and 2020 (monitoring year 2).
#      A13-A24 take the ALTERNATE form (no base rows), all years.
#   B  12 students already EL when the panel opens: grade 3 in 2015, current
#      2015-2020, district D1. Nothing dates their entry unless
#      `entry_fields` is TRUE.
#   C  3 K entrants in 2017, district D3 -- too small to report.
#   N  15 never-EL students, grade 1, every year, SAMPLE_WEIGHT 10.
#
# Current ELs per year: 12, 36, 39, 39, 29, 29 (2015-2020).

make_prep2_spec <- function() read_el_spec("00_SIM_EL_Spec")

make_prep2_roster <- function(entry_fields = FALSE, yis_zero = FALSE,
                              left_truncated = FALSE) {
  grades <- c("K", as.character(1:12))
  mk <- function(prefix, n, years, first_grade, district, language) {
    g0 <- match(first_grade, grades)
    do.call(rbind, lapply(seq_len(n), function(i) {
      data.frame(
        STUDENT_ID = sprintf("%s%02d", prefix, i),
        YEAR = as.integer(years),
        GRADE = grades[g0 + seq_along(years) - 1L],
        EL_STATUS = "current",
        DISTRICT_ID = district(i),
        HOME_LANGUAGE = language(i),
        SAMPLE_WEIGHT = 1,
        ELP_FORM = "annual",
        EL_MONITOR_YEAR = NA_integer_,
        stringsAsFactors = FALSE)
    }))
  }
  a <- mk("A", 24L, 2016:2020, "K", function(i) if (i <= 12) "D1" else "D2",
          function(i) "spa")
  b <- mk("B", 12L, 2015:2020, "3", function(i) "D1", function(i) "hat")
  cc <- mk("C", 3L, 2017:2020, "K", function(i) "D3", function(i) "por")

  exited <- a$STUDENT_ID %in% sprintf("A%02d", 1:10) & a$YEAR >= 2019L
  a$EL_STATUS[exited] <- "former"
  a$EL_MONITOR_YEAR[exited] <- ifelse(a$YEAR[exited] == 2019L, 1L, 2L)
  a$ELP_FORM[a$STUDENT_ID %in% sprintf("A%02d", 13:24)] <- "alternate"

  nv <- do.call(rbind, lapply(1:15, function(i) {
    data.frame(STUDENT_ID = sprintf("N%02d", i), YEAR = 2015:2020, GRADE = "1",
               EL_STATUS = "never", DISTRICT_ID = "D1", HOME_LANGUAGE = "eng",
               SAMPLE_WEIGHT = 10, ELP_FORM = NA_character_,
               EL_MONITOR_YEAR = NA_integer_, stringsAsFactors = FALSE)
  }))
  out <- rbind(a, b, cc, nv)

  if (entry_fields) {
    start <- c(A = 2016L, B = 2013L, C = 2017L)[substr(out$STUDENT_ID, 1, 1)]
    out$COHORT_START_YEAR <- unname(start)
    out$YEAR_IN_STATUS <- ifelse(out$EL_STATUS == "current",
                                 out$YEAR - out$COHORT_START_YEAR +
                                   if (yis_zero) 0L else 1L,
                                 NA_integer_)
  }
  if (left_truncated) {
    out$LEFT_TRUNCATED <- substr(out$STUDENT_ID, 1, 1) == "B"
  }
  tibble::as_tibble(out)
}

# One headline row per current annual student-year: level 1.5 in a student's
# first observed year, 3.4 after. The alternate-form students have none.
make_prep2_bundle <- function(roster = make_prep2_roster(),
                              spec = make_prep2_spec()) {
  cur <- roster[roster$EL_STATUS == "current" & roster$ELP_FORM %in% "annual", ]
  first <- stats::ave(cur$YEAR, cur$STUDENT_ID, FUN = min)
  base <- data.frame(
    STUDENT_ID        = cur$STUDENT_ID,
    YEAR              = cur$YEAR,
    COMPONENT         = spec_headline(spec),
    ELP_FORM          = "annual",
    SCALE_SCORE       = 400,
    ACHIEVEMENT_LEVEL = ifelse(cur$YEAR == first, 1.5, 3.4),
    stringsAsFactors = FALSE)
  as_el_data(base, roster = roster, spec = spec)
}

# The shipped simulation, built once per test run: the bundle joins 1.8M base
# rows against the roster, and run_prep2() on it takes several seconds.
make_prep2_sim_bundle <- local({
  b <- NULL
  function() {
    if (is.null(b)) {
      b <<- as_el_data(el_long_sim, roster = el_roster_sim,
                       spec = make_prep2_spec())
    }
    b
  }
})

make_prep2_sim_run <- local({
  r <- NULL
  function() {
    if (is.null(r)) r <<- run_prep2(make_prep2_sim_bundle(), make_prep2_spec())
    r
  }
})

# A Type 2 (composite-plus-floor) variant of the SIM spec: the overall level
# must reach 4.5 AND the reading level 4.0 (a waived domain is exempt). The
# validator checks the declared Type against the shape of the rule.
make_prep2_type2_spec <- function() {
  s <- make_prep2_spec()
  s$policy$exit$pathways[[1]]$terms[[2]] <- list(
    component = "reading", measure = "level", op = ">=", value = 4.0,
    exempt_if_absent = TRUE)
  s$policy$exit$types <- 2L
  validate_el_spec(s)
  s
}
