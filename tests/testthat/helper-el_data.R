# Fixtures for the delivery-boundary module. Module-scoped names throughout
# (make_el_data_*): testthat sources every helper into one shared environment.
#
# These are hand-written, never simulated: the point of a delivery fixture is
# that it is shaped like something a state actually sent, and a simulator only
# ever produces the shape it was told to.
#
# No component name is typed here. Component values come from the spec -- its
# headline, or its component list -- so the fixtures cannot smuggle a WIDA
# assumption past the ELPA21 smoke test.

make_el_data_sim_spec <- function() read_el_spec("00_SIM_EL_Spec")

# An SGP LONG_DATA delivery. CONTENT_AREA is "READING" and maps to the HEADLINE
# composite through the spec's value map, which is the case that makes
# label-based guessing wrong: in one state's WIDA file READING is the overall
# composite, in another's it is the reading test.
#
# Also exercises: ID for STUDENT_ID, "YYYY_YYYY" years, GRADE "0" recoded to
# "K", an INVALID_CASE row that must be dropped, decimal ACHIEVEMENT_LEVELs,
# and a former student-year carried with no measurement (CONTENT_AREA still
# names the file's ELP content area; SCALE_SCORE and level are NA).
make_el_data_sgp_long <- function() {
  tibble::tibble(
    VALID_CASE        = c(rep("VALID_CASE", 5), "INVALID_CASE"),
    CONTENT_AREA      = "READING",
    YEAR              = c("2014_2015", "2015_2016", "2014_2015", "2015_2016",
                          "2016_2017", "2015_2016"),
    ID                = c("S1", "S1", "S2", "S2", "S2", "S9"),
    GRADE             = c("0", "1", "3", "4", "5", "1"),
    SCALE_SCORE       = c(250, 300, 480, 520, NA, 999),
    ACHIEVEMENT_LEVEL = c(1.5, 2.5, 4.2, 4.6, NA, 6.0),
    EL_STATUS         = c("current", "current", "current", "current",
                          "former", "current"),
    ELP_FORM          = "annual",
    HOME_LANGUAGE     = c("spa", "spa", "por", "por", "por", "oth")
  )
}

# The same records expressed the other accepted way: separate objects. If the
# forms do not produce the same bundle, one of the adapters is quietly
# reshaping the data. S2's former year has a roster row and no base row --
# exactly how the sim carries a former student-year.
make_el_data_expected_roster <- function() {
  tibble::tibble(
    STUDENT_ID    = c("S1", "S1", "S2", "S2", "S2"),
    YEAR          = c(2015L, 2016L, 2015L, 2016L, 2017L),
    GRADE         = c("K", "1", "3", "4", "5"),
    EL_STATUS     = c("current", "current", "current", "current", "former"),
    ELP_FORM      = "annual",
    HOME_LANGUAGE = c("spa", "spa", "por", "por", "por")
  )
}

make_el_data_expected_base <- function(spec) {
  tibble::tibble(
    STUDENT_ID        = c("S1", "S1", "S2", "S2"),
    YEAR              = c(2015L, 2016L, 2015L, 2016L),
    COMPONENT         = spec_headline(spec),
    ELP_FORM          = "annual",
    SCALE_SCORE       = c(250, 300, 480, 520),
    ACHIEVEMENT_LEVEL = c(1.5, 2.5, 4.2, 4.6)
  )
}

# A small stateSummative / ReportR-shaped academic table: student_results_long
# plus ACHIEVEMENT_LEVEL_ALT, already canonical.
make_el_data_summative_academic <- function() {
  tibble::tibble(
    STUDENT_ID            = c("S2", "S2", "S2", "S2"),
    YEAR                  = c(2015L, 2015L, 2016L, 2016L),
    GRADE                 = c("3", "3", "4", "4"),
    SUBJECT               = c("ELA", "Math", "ELA", "Math"),
    SCALE_SCORE           = c(700, 720, 760, 780),
    ACHIEVEMENT_LEVEL     = c(2L, 2L, 4L, 4L),
    ACHIEVEMENT_LEVEL_ALT = NA_integer_
  )
}

# The same academic records as SGP LONG_DATA: ID, CONTENT_AREA values that
# are not the spec's subject keys, "YYYY_YYYY" years, and an INVALID_CASE row.
make_el_data_sgp_academic <- function() {
  tibble::tibble(
    VALID_CASE            = c(rep("VALID_CASE", 4), "INVALID_CASE"),
    ID                    = c("S2", "S2", "S2", "S2", "S2"),
    YEAR                  = c("2014_2015", "2014_2015", "2015_2016",
                              "2015_2016", "2015_2016"),
    GRADE                 = c("3", "3", "4", "4", "4"),
    CONTENT_AREA          = c("ELA", "MATHEMATICS", "ELA", "MATHEMATICS",
                              "ELA"),
    SCALE_SCORE           = c(700, 720, 760, 780, 999),
    ACHIEVEMENT_LEVEL     = c(2L, 2L, 4L, 4L, 5L),
    ACHIEVEMENT_LEVEL_ALT = NA_integer_
  )
}

# The ELPA21-headline pair: a base table and roster whose headline is a
# CATEGORICAL determination stored as an ordinal index, with a level-only
# component and a scale-only one. Built by iterating the spec's components and
# their declared measures, so every row is exactly what the spec says the
# instrument reports. Every compute_*() the package has built must run
# against this without naming a component or assuming a numeric level.
make_el_data_elpa21_roster <- function() {
  tibble::tibble(
    STUDENT_ID     = c("E1", "E1", "E1", "E2", "E2", "E3"),
    YEAR           = c(2015L, 2016L, 2017L, 2015L, 2016L, 2015L),
    GRADE          = c("K", "1", "2", "K", "1", "K"),
    EL_STATUS      = c("current", "current", "former",
                       "current", "current", "current"),
    ELP_FORM       = "annual",
    LEFT_TRUNCATED = FALSE,
    SAMPLE_WEIGHT  = 1
  )
}

make_el_data_elpa21_base <- function(spec) {
  roster <- make_el_data_elpa21_roster()
  tested <- roster[roster$EL_STATUS == "current", c("STUDENT_ID", "YEAR")]
  headline <- spec_headline(spec)
  n_levels <- length(spec$elp$level_scale$levels)
  # Headline determination by student-year: E1 rises, E2 stays, E3 mid.
  det <- c(1, n_levels, 1, 1, 2)

  rows <- list()
  for (cp in spec_components(spec)) {
    m <- spec$elp$components[[cp]]$measures
    rows[[cp]] <- tibble::tibble(
      STUDENT_ID        = tested$STUDENT_ID,
      YEAR              = tested$YEAR,
      COMPONENT         = cp,
      ELP_FORM          = "annual",
      SCALE_SCORE       = if ("scale" %in% m) 300 + 10 * seq_len(nrow(tested))
                          else NA_real_,
      ACHIEVEMENT_LEVEL = if (!"level" %in% m) NA_real_
                          else if (identical(cp, headline)) det
                          else 3
    )
  }
  dplyr::bind_rows(rows)
}
