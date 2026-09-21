# Check 3 -- assembly. run_<stage>() computes nothing itself: it calls the
# compute functions, degrades absent slots to NULL with a comment, and builds
# $meta (CLAUDE.md 8).

#' Run Check 3 -- academic patterns
#'
#' **The sample is not Checks 1-2's cohort**, and the stage says so first: it
#' is cross-sectional -- all ELs in a grade and year, beside former ELs and an
#' English-only reference group -- because the question is where academic
#' performance sits relative to language proficiency *now*, not how one entry
#' cohort moved through.
#'
#' This is the shell pass. `$box_data` and the panel it is drawn for are real;
#' the summary table, the logistic and its landing statistics, the two
#' fairness checks and the contested-zone models arrive in later passes and
#' are `NULL` with a comment until then.
#'
#' Grades are **never pooled**: every slot is keyed by one grade x subject x
#' year panel, and the later pass that covers every tested grade loops this
#' same derivation rather than widening it.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param subject,grade,year Optional panel overrides; each defaults to
#'   [compute_panel()]'s derivation.
#' @return A named list of results slots plus `$meta`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                      academic = academic_long_sim, spec = spec)
#' res <- run_check3(bundle, spec)
#' res$meta$sample
run_check3 <- function(data, spec, subject = NULL, grade = NULL, year = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))

  # Every slot in this stage lives on the academic-to-roster pairing, so
  # without an academic table there is no stage, not a thinner one.
  has_academic <- !is.null(el_academic(data))
  panel <- if (!has_academic) NULL else {
    compute_panel(data, spec, subject = subject, grade = grade, year = year)
  }
  box_data <- if (is.null(panel)) NULL else compute_box_data(data, spec, panel)

  list(
    box_data = box_data,

    # [Later pass -- instruction file: summary table, then logistic +
    #  landing, then the two fairness checks]
    summary_table      = NULL,
    logistic           = NULL,
    landing            = NULL,
    fairness_boxes     = NULL,
    fairness_logistic  = NULL,

    # [Type-gated: Types 2/4/5/6 only, and the SIM spec is Type 1. It also
    #  needs Prep 2's contested zone, which is NULL under the same gate.]
    contested_zone_models = NULL,

    coverage = el_coverage(data),

    meta = list(
      stage                = "check3",
      spec_id              = spec$program$short_name %||% NA_character_,
      sample               = .check3_sample_text(panel, spec, has_academic),
      panel                = panel,
      subject              = if (is.null(panel)) NA_character_ else panel$subject,
      grade                = if (is.null(panel)) NA_character_ else panel$grade,
      year                 = if (is.null(panel)) NA_integer_ else panel$year,
      cut                  = if (is.null(panel)) NA_real_ else
                               .check3_cut(spec, panel$subject),
      proficient_level     = spec$policy$academic_benchmark$proficient_level,
      pairing_note         = .check3_pairing_note(spec, panel),
      band_axis_note       = .check3_band_axis_note(spec),
      note                 = if (has_academic) NA_character_ else
                               paste0("No academic table in this delivery. ",
                                      "Every Check 3 slot lives on the ",
                                      "pairing of academic scores with ",
                                      "roster status, so the stage has ",
                                      "nothing to report."),
      pending              = c("summary_table", "logistic", "landing",
                               "fairness_boxes", "fairness_logistic"),
      sim_contract_version = data$meta$sim_contract_version,
      package_version      = as.character(utils::packageVersion("stateELP")),
      run_date             = Sys.Date()
    )
  )
}

# The Sample statement, on its own line at the top of the document
# (CLAUDE.md 2). It names the panel, because "all ELs" without a grade and a
# year is the misreading this stage is most prone to.
.check3_sample_text <- function(panel, spec, has_academic) {
  if (!has_academic || is.null(panel)) {
    return(paste0("Cross-sectional: all ELs in one grade and year, with ",
                  "former ELs and an English-only group as references. No ",
                  "academic table in this delivery, so no panel is drawn."))
  }
  paste0(
    "Cross-sectional, and different from Data Checks 1 and 2: **every ",
    "grade-", panel$grade, " EL with a ", panel$year, " ", panel$subject,
    " score** -- not the entry cohort those stages follow -- with former ELs ",
    "and an English-only group beside them for reference. A student enters ",
    "on having sat the content test, so the universe is the ", panel$subject,
    " table joined to roster status, not the other way round. Each grade is ",
    "a panel of its own and grades are never pooled; this pass renders ",
    "grade ", panel$grade, ", the first grade the spec declares ",
    panel$subject, " tested in.")
}

# The two assessments are paired within a year, and the windows do not
# coincide. Saying so is the difference between "same year" and "same time".
.check3_pairing_note <- function(spec, panel) {
  if (is.null(panel)) return(NA_character_)
  window <- spec$elp$administration_window
  paste0(
    "Scores are paired within the school year, not to the day: the ",
    spec_headline_label(spec), " is administered ",
    if (is.null(window)) "in its own window" else window,
    " and the content test later in the year. A student's language score ",
    "therefore precedes their content score, which is the right order for ",
    "this question but is not a controlled interval.")
}

# The hazard on this stage's x-axis, recorded 2026-09-21. Rendered rather than
# left in the code, because nothing in the figure reveals it.
.check3_band_axis_note <- function(spec) {
  rule <- spec$policy$exit$pathways[[1]]$rule
  paste0(
    "The band axis is not a proficiency gradient across a stable population. ",
    "Its top end is constituted by the exit rule",
    if (is.null(rule)) "" else paste0(" (", rule, ")"),
    ": a student observed as a current EL at or above the exit threshold is, ",
    "in most states, in their final year -- so the highest bands hold ",
    "students on their way out rather than a cross-section of highly ",
    "proficient ELs. Where a state's rule carries a second criterion, the ",
    "same cells instead hold students that criterion is blocking, which is a ",
    "different population again. Read the top of this axis with that in ",
    "mind, and read the Former ELs column beside it.")
}
