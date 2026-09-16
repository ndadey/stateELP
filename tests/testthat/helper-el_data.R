# Fixtures for the delivery-boundary module. Module-scoped names throughout.
#
# These are hand-written, never simulated: the point of a delivery fixture is
# that it is shaped like something a state actually sent, and a simulator only
# ever produces the shape it was told to.

# An SGP LONG_DATA delivery. CONTENT_AREA is "READING" and maps to the OVERALL
# composite through the spec, which is the case that makes label-based guessing
# wrong: in one state's WIDA file READING is the overall composite, in another's
# it is the reading test.
#
# Also exercises: GRADE "0" recoded to "K", an INVALID_CASE row that must be
# dropped, and decimal ACHIEVEMENT_LEVELs.
make_el_data_sgp_long <- function() {
  tibble::tibble(
    VALID_CASE        = c("VALID_CASE", "VALID_CASE", "VALID_CASE",
                          "VALID_CASE", "INVALID_CASE"),
    CONTENT_AREA      = "READING",
    YEAR              = c(2015L, 2016L, 2015L, 2016L, 2016L),
    ID                = c("S1", "S1", "S2", "S2", "S9"),
    GRADE             = c("0", "1", "0", "1", "1"),
    SCALE_SCORE       = c(250, 300, 480, 520, 999),
    ACHIEVEMENT_LEVEL = c(1.5, 2.5, 4.2, 4.6, 6.0),
    EL_STATUS         = c("current", "current", "current", "current", "current"),
    ELP_FORM          = "annual",
    HOME_LANGUAGE     = c("spa", "spa", "por", "por", "oth")
  )
}

# The same four records expressed the other accepted way: separate objects.
# If the two forms do not produce the same bundle, one of the adapters is
# quietly reshaping the data.
make_el_data_expected_spine <- function() {
  tibble::tibble(
    student_id    = c("S1", "S1", "S2", "S2"),
    school_year   = c(2015L, 2016L, 2015L, 2016L),
    grade         = c("K", "1", "K", "1"),
    el_status     = "current",
    elp_form      = "annual",
    home_language = c("spa", "spa", "por", "por"),
    elp_scale     = c(250, 300, 480, 520),
    elp_level     = c(1.5, 2.5, 4.2, 4.6)
  )
}

make_el_data_expected_components <- function() {
  tibble::tibble(
    student_id  = c("S1", "S1", "S2", "S2"),
    school_year = c(2015L, 2016L, 2015L, 2016L),
    component   = "overall",
    scale_score = c(250, 300, 480, 520),
    level       = c(1.5, 2.5, 4.2, 4.6),
    form        = "annual"
  )
}

# A stateSummative-shaped academic table: uppercase names, CONTENT_AREA rather
# than subject, ACHIEVEMENT_LEVEL rather than perf_level.
make_el_data_summative_academic <- function() {
  tibble::tibble(
    VALID_CASE        = "VALID_CASE",
    ID                = c("S1", "S1", "S2", "S2"),
    YEAR              = c(2015L, 2016L, 2015L, 2016L),
    GRADE             = c("K", "1", "K", "1"),
    CONTENT_AREA      = c("ELA", "ELA", "ELA", "ELA"),
    SCALE_SCORE       = c(700, 720, 760, 780),
    ACHIEVEMENT_LEVEL = c(2L, 2L, 4L, 4L)
  )
}

# A spine whose headline is a CATEGORICAL determination, for the
# no-WIDA-assumptions smoke test. Every compute_*() the package has built must
# run against this without naming a component or assuming a numeric level.
make_el_data_elpa21_spine <- function() {
  tibble::tibble(
    student_id  = c("E1", "E1", "E1", "E2", "E2", "E3"),
    school_year = c(2015L, 2016L, 2017L, 2015L, 2016L, 2015L),
    grade       = c("K", "1", "2", "K", "1", "K"),
    el_status   = c("current", "current", "former",
                    "current", "current", "current"),
    elp_level   = c("Emerging", "Progressing", NA,
                    "Emerging", "Emerging", "Progressing"),
    sample_weight = 1
  )
}
