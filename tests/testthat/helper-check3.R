# Fixtures for Check 3.
#
# Module-scoped names (make_check3_*): testthat sources every helper into one
# shared environment, so a generic name would clobber another module's
# builder.

make_check3_spec <- function() read_el_spec("00_SIM_EL_Spec")

# The shipped simulation, built once per test run rather than per test.
make_check3_sim_bundle <- local({
  b <- NULL
  function() {
    if (is.null(b)) {
      b <<- as_el_data(el_long_sim, roster = el_roster_sim,
                       academic = academic_long_sim,
                       spec = make_check3_spec())
    }
    b
  }
})

# A small hand-built panel: one grade, one year, one subject, with every
# group the axis carries -- current ELs across two bands, a former EL and a
# never-EL. Sizes are given so a test can push a group under `min_cell`
# without touching the others.
#
# Scores are typed, the groups are not: bands come from the spec's own band
# scheme through elp_headline(), as they do in the package.
make_check3_bundle <- function(n_band1 = 20L, n_band2 = 20L,
                               n_former = 20L, n_never = 20L,
                               spec = make_check3_spec(),
                               year = 2024L, grade = NULL) {
  subject <- spec_academic_subjects(spec)[[1]]
  # The grade the package would pick, so the fixture and compute_panel() land
  # on the same panel without the test naming a grade.
  if (is.null(grade)) grade <- spec_academic_grades(spec, subject)[[1]]
  mk <- function(prefix, n, status) {
    if (!n) return(NULL)
    tibble::tibble(STUDENT_ID = sprintf("%s%03d", prefix, seq_len(n)),
                   YEAR = as.integer(year), GRADE = grade,
                   EL_STATUS = status)
  }
  roster <- dplyr::bind_rows(
    mk("A", n_band1, "current"), mk("B", n_band2, "current"),
    mk("F", n_former, "former"), mk("N", n_never, "never"))
  roster$ELP_FORM <- ifelse(roster$EL_STATUS == "current", "annual",
                            NA_character_)
  roster$SAMPLE_WEIGHT <- ifelse(roster$EL_STATUS == "never", 10, 1)

  # Only current ELs carry ELP rows (contract 4.4). Levels are taken from the
  # spec's band breaks so the fixture lands in two distinct bands whatever
  # the scheme is.
  breaks <- spec_bands(spec)$breaks
  cur <- roster[roster$EL_STATUS == "current", , drop = FALSE]
  base <- tibble::tibble(
    STUDENT_ID = cur$STUDENT_ID,
    YEAR = cur$YEAR,
    COMPONENT = spec_headline(spec),
    ELP_FORM = "annual",
    SCALE_SCORE = 400,
    ACHIEVEMENT_LEVEL = ifelse(startsWith(cur$STUDENT_ID, "A"),
                               breaks[[1]] + 0.1, breaks[[3]] + 0.1))

  academic <- tibble::tibble(
    STUDENT_ID = roster$STUDENT_ID,
    YEAR = roster$YEAR,
    GRADE = roster$GRADE,
    SUBJECT = subject,
    SCALE_SCORE = seq(700, 780, length.out = nrow(roster)),
    ACHIEVEMENT_LEVEL = 3L,
    ACHIEVEMENT_LEVEL_ALT = NA_integer_)

  as_el_data(base, roster = roster, academic = academic, spec = spec)
}

# The panel that fixture's bundle resolves to, derived rather than typed so a
# test never names a grade or a year the package did not choose.
make_check3_panel <- function(bundle = make_check3_bundle(),
                              spec = make_check3_spec()) {
  compute_panel(bundle, spec)
}
