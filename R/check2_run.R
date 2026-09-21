# Check 2 -- assembly. run_<stage>() computes nothing itself: it calls the
# compute functions, degrades absent slots to NULL with a comment, and builds
# $meta (CLAUDE.md 8).

#' Run Check 2 -- on-time vs. prolonged exiters
#'
#' Takes the cohort Check 1 analyzes -- the selected row of
#' [compute_cohorts()], not a second selection -- and classifies each student
#' by exit outcome. This pass builds `$flags` (the student-level indicators)
#' and `$crosstab` (the Total block of the slide-63/64 crosstab); every later
#' slot is `NULL` until its own pass, and `$meta$pending` names them.
#'
#' With no qualifying cohort -- the same condition under which Check 1 selects
#' none -- every analysis slot stays `NULL` and `$meta$note` says why. Never a
#' crash, never an invented denominator.
#'
#' `$blocked_groups` is the Type-gated Additional check and is `NULL` for a
#' Type 1 spec. `$meta$type_gate_applies` says whether the spec declares a
#' Type it would apply to, so the document can tell "does not apply" from "not
#' built yet".
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param start_grade Entry grade defining the cohorts, e.g. `"K"`.
#' @return A named list of results slots plus `$meta`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' res <- run_check2(bundle, spec)
#' res$meta$sample
run_check2 <- function(data, spec, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(start_grade), length(start_grade) == 1L)
  roster <- el_roster(data)
  panel_last <- max(roster$YEAR, na.rm = TRUE)

  cohorts <- compute_cohorts(data, spec, start_grade = start_grade)
  selected <- if (!nrow(cohorts) || !any(cohorts$SELECTED)) NA_character_
              else cohorts$COHORT_LABEL[cohorts$SELECTED]

  flags <- if (is.na(selected)) NULL else {
    compute_exit_flags(data, spec, cohort = selected, start_grade = start_grade)
  }
  crosstab <- if (is.null(flags)) NULL else compute_crosstab(flags, spec)

  on_time_text <- if (is.null(flags)) NA_character_
                  else .check2_on_time_text(flags$ON_TIME_YEARS)
  provisional <- spec$policy$timelines$source %in% "provisional"

  list(
    flags    = flags,
    crosstab = crosstab,

    # [Later passes -- instruction file: row blocks one at a time -> academic
    #  summary -> trajectories, with its guardrail test first]
    academic_summary = NULL,   # [REQUIRES the academic table; NULL until built]
    trajectories     = NULL,

    # [Type-gated: Types 2-6 only, and the SIM spec is Type 1. Prep 2's
    #  blocked-by-one-criterion groups as extra comparison columns.]
    blocked_groups = NULL,

    meta = list(
      stage                = "check2",
      spec_id              = spec$program$short_name %||% NA_character_,
      sample               = .check2_sample_text(cohorts, spec, panel_last,
                                                 start_grade, on_time_text,
                                                 provisional),
      start_grade          = start_grade,
      cohort               = selected,
      cohort_n             = if (is.null(flags)) NA_integer_ else nrow(flags),
      panel                = c(min(roster$YEAR, na.rm = TRUE), panel_last),
      max_years            = spec_max_years(spec),
      on_time_text         = on_time_text,
      on_time_provisional  = provisional,
      type_gate_applies    = spec_has_type(spec, 2:6),
      note                 = if (is.na(selected)) .check2_no_cohort_note()
                             else NA_character_,
      pending              = c("academic_summary", "trajectories",
                               "blocked_groups"),
      sim_contract_version = data$meta$sim_contract_version,
      package_version      = as.character(utils::packageVersion("stateELP")),
      run_date             = Sys.Date()
    )
  )
}

# "5 years", or the range when the on-time rule assigns different timelines to
# different students (by start grade or initial band).
.check2_on_time_text <- function(years) {
  years <- sort(unique(years[!is.na(years)]))
  if (!length(years)) return(NA_character_)
  unit <- function(y) paste(y, if (y == 1L) "year" else "years")
  if (length(years) == 1L) return(unit(years))
  paste0("between ", min(years), " and ", max(years),
         " years, depending on the state's on-time rule")
}

# The Sample statement, built from Check 1's own cohort table -- never a
# literal, and the same cohort by construction. It is also `$meta$sample`, so
# the callout and the results object cannot disagree.
.check2_sample_text <- function(cohorts, spec, panel_last, start_grade,
                                on_time_text, provisional) {
  if (!nrow(cohorts) || !any(cohorts$SELECTED)) {
    return(paste0("No entry cohort: Data Check 1 selects none from this ",
                  "panel, so there is nothing to split into on-time and ",
                  "prolonged exiters. Every analysis slot is empty."))
  }
  sel <- cohorts[cohorts$SELECTED, ]
  grade_phrase <- if (identical(start_grade, "K")) "kindergarten"
                  else paste("grade", start_grade)
  flag <- if (provisional) paste0(" [DERIVED ", .CHECK2_EM_DASH, " VERIFY]")
          else ""

  paste0(
    "The ", sel$COHORT_LABEL, " entry cohort ", .CHECK2_EM_DASH, " the same ",
    "cohort Data Check 1 analyzes: ", format(sel$N_ENTRY, big.mark = ","),
    " students who entered EL status in ", grade_phrase, " in ",
    sel$COHORT_START_YEAR, ", followed through ", panel_last, ". They are ",
    "split into on-time exiters (exited within ", on_time_text, flag, ") and ",
    "prolonged cases (exited later, or still classified when the window ",
    "closes). Students who disappeared from the data are shown separately, ",
    "in neither group.")
}

.check2_no_cohort_note <- function() {
  paste0("No cohort selected: Data Check 1 finds no usable entry cohort in ",
         "this panel, so Check 2 has no sample.")
}
