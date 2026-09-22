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

  # Every tested grade x subject, each its own panel. Grades are never pooled
  # (instruction file, tested), so the loop binds a keyed long table rather
  # than merging anything: the landing statistics only become evidence when
  # they can be read across grades together, and one grade is an anecdote.
  panels <- if (!has_academic) list() else {
    .check3_all_panels(data, spec, panel, subject = subject, year = year)
  }
  fits <- lapply(panels, function(p) .check3_one_panel(data, spec, p))

  box_data <- .check3_bind(fits, "box_data", panels)
  summary_table <- .check3_bind(fits, "summary_table", panels)
  fairness_boxes <- .check3_bind(fits, "fairness_boxes", panels)
  logistic <- .check3_bind(fits, "logistic", panels)
  landing <- .check3_bind(fits, "landing", panels)
  fairness_logistic <- .check3_bind(fits, "fairness_logistic", panels)

  list(
    # All panels, long and keyed. One grade is a filter of this, which is why
    # the figure takes a selector rather than a separate slot.
    box_data           = box_data,
    summary_table      = summary_table,
    logistic           = logistic,
    landing            = landing,
    fairness_boxes     = fairness_boxes,
    fairness_logistic  = fairness_logistic,

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
      panels               = length(panels),
      pending              = character(),
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

# Every tested grade x subject the spec declares, as panels. The focal panel
# is placed first so a reader of the long tables meets the figure's grade at
# the top; the rest follow in the spec's own order.
.check3_all_panels <- function(data, spec, focal, subject = NULL, year = NULL) {
  subjects <- if (is.null(subject)) spec_academic_subjects(spec) else subject
  out <- list()
  for (sb in subjects) {
    grades <- spec_academic_grades(spec, sb)
    if (is.null(grades)) {
      ac <- el_academic(data)
      grades <- sort(unique(as.character(ac$GRADE[ac$SUBJECT %in% sb])))
    }
    for (g in grades) {
      p <- tryCatch(compute_panel(data, spec, subject = sb, grade = g,
                                  year = year),
                    error = function(e) NULL)
      if (!is.null(p) && !is.na(p$year)) out[[length(out) + 1L]] <- p
    }
  }
  if (is.null(focal)) return(out)

  same <- vapply(out, function(p) {
    identical(p$subject, focal$subject) && identical(p$grade, focal$grade)
  }, TRUE)
  c(out[same], out[!same])
}

# One panel's five results. Each is independent of the others except that the
# summary table and the landing statistics read the box data rather than
# recomputing it.
.check3_one_panel <- function(data, spec, panel) {
  box <- compute_box_data(data, spec, panel)
  logistic <- compute_logistic(data, spec, panel)
  list(
    box_data          = box,
    summary_table     = compute_summary_table(data, spec, panel,
                                              box_data = box),
    logistic          = logistic,
    landing           = compute_landing(box, logistic, spec),
    fairness_boxes    = compute_fairness_boxes(data, spec, panel),
    fairness_logistic = compute_fairness_logistic(data, spec, panel))
}

# Bind one slot across panels into a long table keyed by subject and grade.
# The keys are added here rather than inside each compute function, so a
# single panel's result stays readable on its own.
.check3_bind <- function(fits, slot, panels) {
  parts <- list()
  for (i in seq_along(fits)) {
    x <- fits[[i]][[slot]]
    if (is.null(x) || !nrow(x)) next
    if (!"SUBJECT" %in% names(x)) {
      x <- dplyr::bind_cols(
        tibble::tibble(SUBJECT = panels[[i]]$subject,
                       GRADE = panels[[i]]$grade,
                       YEAR = panels[[i]]$year),
        x)
    }
    parts[[length(parts) + 1L]] <- x
  }
  if (!length(parts)) return(NULL)   # [no panel produced this slot]
  dplyr::bind_rows(parts)
}
