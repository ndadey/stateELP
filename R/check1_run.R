# Check 1 -- assembly. run_<stage>() computes nothing itself: it calls the
# compute functions, degrades absent slots to NULL with a comment, and builds
# $meta (CLAUDE.md 8).

#' Run Check 1 -- cohort survival analysis
#'
#' Selects one entry cohort and describes its exit timing. This pass builds
#' `$cohorts` and `$life_table`; every later slot is `NULL` until its own
#' pass, and `$meta$pending` names them.
#'
#' With no qualifying cohort -- a panel too short for any cohort to reach the
#' maximum timeline plus a year, or every candidate below `min_cell` -- the
#' analysis slots stay `NULL`, `$meta$note` says why, and the document renders
#' that note. Never a crash, never an invented denominator.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param start_grade Entry grade defining the cohorts, e.g. `"K"`.
#' @return A named list of results slots plus `$meta`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' res <- run_check1(bundle, spec)
#' res$meta$sample
run_check1 <- function(data, spec, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(start_grade), length(start_grade) == 1L)
  roster <- el_roster(data)
  .check1_require_fields(roster, c("cohorts", "life_table"))

  panel_last <- max(roster$YEAR, na.rm = TRUE)
  cohorts <- compute_cohorts(data, spec, start_grade = start_grade)
  if (!nrow(cohorts)) cohorts <- NULL   # [no candidate cohort in this panel]

  selected <- if (is.null(cohorts) || !any(cohorts$SELECTED)) NA_character_
              else cohorts$COHORT_LABEL[cohorts$SELECTED]

  life_table <- if (is.na(selected)) NULL else {
    compute_life_table(data, spec, cohort = selected, start_grade = start_grade)
  }

  computed <- c(if (!is.null(cohorts)) "cohorts",
                if (!is.null(life_table)) "life_table")

  list(
    cohorts    = cohorts,
    life_table = life_table,

    # [Later passes -- instruction file: curves -> survival column ->
    #  years-to-50 / at-max -> Model 2 -> experimental weights]
    exit_curve                 = NULL,
    exit_curve_by_initial_band = NULL,
    survival_est               = NULL,
    years_to_50                = NULL,
    at_max_timeline            = NULL,
    model2                     = NULL,
    retention_share            = NULL,
    selection_strength         = NULL,
    grade_weights              = NULL,

    # [Type-gated: Type 3 only, and the SIM spec is Type 1]
    type3_covariate = NULL,

    meta = list(
      stage                = "check1",
      spec_id              = spec$program$short_name %||% NA_character_,
      sample               = .check1_sample_text(cohorts, start_grade, panel_last,
                                                 spec),
      start_grade          = start_grade,
      cohort               = selected,
      panel                = c(min(roster$YEAR, na.rm = TRUE), panel_last),
      data_fields          = .check1_data_fields(roster, spec, computed),
      note                 = .check1_note(cohorts, spec),
      pending              = c("exit_curve", "exit_curve_by_initial_band",
                               "survival_est", "years_to_50", "at_max_timeline",
                               "model2", "retention_share",
                               "selection_strength", "grade_weights"),
      sim_contract_version = data$meta$sim_contract_version,
      package_version      = as.character(utils::packageVersion("stateELP")),
      run_date             = Sys.Date()
    )
  )
}

# Spelled out to nine, because "the 5 usable K cohorts" reads like a count of
# something measured rather than a handful.
.small_number <- function(n) {
  words <- c("one", "two", "three", "four", "five",
             "six", "seven", "eight", "nine")
  if (n >= 1L && n <= length(words)) words[[n]] else format(n, big.mark = ",")
}

# The Sample statement, built from $cohorts -- never a literal (CLAUDE.md 12
# #4, resolved for this line 2026-09-20). It is also `$meta$sample`, so the
# callout and the results object cannot disagree.
.check1_sample_text <- function(cohorts, start_grade, panel_last, spec) {
  grade_phrase <- if (identical(start_grade, "K")) "kindergarten"
                  else paste("grade", start_grade)

  if (is.null(cohorts) || !nrow(cohorts)) {
    return(paste0(
      "No entry cohort: this panel is too short for any ", start_grade,
      " cohort to be followed for the ", spec_max_years(spec),
      "-year maximum timeline plus the year that observes an exit in it. ",
      "Every analysis slot is empty."))
  }
  if (!any(cohorts$SELECTED)) {
    return(paste0(
      "No entry cohort: none of the ", .small_number(nrow(cohorts)),
      " candidate ", start_grade, " cohorts reaches the minimum cell size of ",
      spec_min_cell(spec), ". Every analysis slot is empty."))
  }

  sel <- cohorts[cohorts$SELECTED, ]
  paste0(
    "The ", sel$COHORT_LABEL, " entry cohort: ",
    format(sel$N_ENTRY, big.mark = ","), " students who entered EL status in ",
    grade_phrase, " in ", sel$COHORT_START_YEAR, ", followed through ",
    panel_last, " (", sel$FOLLOWUP_YEARS, " years). The most complete of the ",
    .small_number(nrow(cohorts)), " usable ", start_grade,
    if (nrow(cohorts) == 1L) " cohort" else " cohorts",
    " — the longest follow-up.")
}

# Rendered when an analysis slot is empty, so the document says why rather
# than showing a blank.
.check1_note <- function(cohorts, spec) {
  if (is.null(cohorts) || !nrow(cohorts)) {
    return(paste0("No candidate cohort: the panel does not cover the ",
                  spec_max_years(spec), "-year maximum timeline plus the ",
                  "year that observes an exit in it, for any entry year."))
  }
  if (!any(cohorts$SELECTED)) {
    return(paste0("No cohort selected: every candidate falls below the ",
                  "minimum cell size of ", spec_min_cell(spec),
                  ", so none can be reported."))
  }
  NA_character_
}
