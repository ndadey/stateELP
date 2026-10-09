# Check 4 -- assembly. run_check4() computes nothing itself: it calls the
# compute functions, degrades absent slots to NULL with a comment, and builds
# $meta (CLAUDE.md 8).

#' Run Check 4 -- decision consistency and Youden's J
#'
#' **The sample is Check 3's paired cross-section**, not Checks 1-2's cohort:
#' every current EL in one grade and year with both a same-year ELP level and a
#' content score. The panel is Check 3's [compute_panel()], called unchanged,
#' so the two stages describe the same students.
#'
#' At every candidate cut on the ELP level the stage reports decision
#' consistency (raw agreement with content proficiency) and **Youden's J**,
#' and names the J optimum as its optimal-cut statistic: when ELs' proficiency
#' rate is well below one half, agreement is pulled toward the majority
#' classification and can peak at the top of the scale, where J does not.
#' The state's current exit threshold, read from the spec's exit rule, is
#' evaluated beside it, and Prep 3's error-tolerance answer is carried with it.
#'
#' This is the shell pass, for one grade x subject panel. `$candidate_region`
#' (the region across every grade, annotated with Check 1's reliability) is a
#' later pass; `$dependence` (the copula / Kendall's-tau analysis) has no
#' package support yet; `$policy_as_whole` is Type-gated (Types 2-6) and does
#' not apply to a Type 1 rule. All three are `NULL`, with `$meta` saying why.
#'
#' Without an academic table, or with a spec that declares no academic
#' subject, there is no pairing and so no stage: every slot is `NULL` and
#' `$meta$note` says so.
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
#' res <- run_check4(bundle, spec)
#' res$meta$sample
#' res$j_optimal
run_check4 <- function(data, spec, subject = NULL, grade = NULL, year = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))

  # Every slot lives on the pairing of academic scores with ELP levels, so
  # without an academic table -- or a spec naming no academic subject --
  # there is no stage, not a thinner one.
  has_academic <- !is.null(el_academic(data))
  has_subjects <- length(spec_academic_subjects(spec)) > 0L
  # compute_panel() is Check 3's and stops on a panel it cannot derive -- an
  # end-of-course subject the delivery carries no rows for, say. When the
  # panel is derived from the spec, that is an absent dependency here, not a
  # crash. An explicit subject or grade override is the caller's request and
  # errors exactly as compute_panel() does.
  panel_error <- NA_character_
  panel <- if (!has_academic || !has_subjects) NULL else {
    if (!is.null(subject) || !is.null(grade)) {
      compute_panel(data, spec, subject = subject, grade = grade, year = year)
    } else {
      tryCatch(compute_panel(data, spec, year = year),
               error = function(e) {
                 panel_error <<- conditionMessage(e)
                 NULL
               })
    }
  }

  # A whole-number delivery against a fractional rule is worth a callout in
  # the document as well as a warning at the console.
  coarse_note <- NA_character_
  curve <- if (is.null(panel)) NULL else {
    withCallingHandlers(
      compute_consistency_curve(data, spec, panel),
      check4_coarse_levels = function(w) {
        coarse_note <<- conditionMessage(w)
      })
  }
  youden_j <- compute_youden_j(curve)
  j_optimal <- compute_j_optimal(youden_j, spec)

  thr <- .check4_exit_threshold(spec)
  tol <- .check4_tolerance(spec)
  gate <- spec_has_type(spec, 2:6)

  list(
    consistency_curve = curve,
    youden_j          = youden_j,
    j_optimal         = j_optimal,

    # [Later pass: the region across every grade, annotated with Check 1's
    # reliability -- NULL until built.]
    candidate_region  = NULL,
    # [No package support for the copula / Kendall's-tau analysis yet --
    # NULL pending it.]
    dependence        = NULL,
    # [Type-gated: Types 2-6 only, and the SIM spec is Type 1. Under Types 2-6
    # it is a later pass.]
    policy_as_whole   = NULL,

    meta = list(
      stage                = "check4",
      spec_id              = spec$program$short_name %||% NA_character_,
      sample               = .check4_sample_text(panel, spec, curve,
                                                 has_academic, has_subjects),
      component_label      = spec_headline_label(spec),
      panel                = panel,
      subject              = if (is.null(panel)) NA_character_ else
                               panel$subject,
      grade                = if (is.null(panel)) NA_character_ else
                               panel$grade,
      year                 = if (is.null(panel)) NA_integer_ else panel$year,
      cut                  = if (is.null(panel)) NA_real_ else
                               .check4_cut(spec, panel$subject),
      proficient_level     = spec$policy$academic_benchmark$proficient_level,
      threshold            = thr$value,
      threshold_op         = thr$op,
      threshold_label      = thr$label,
      threshold_rule       = thr$rule,
      threshold_note       = thr$note,
      costlier_error       = tol$costlier_error,
      error_tolerance      = tol,
      resolution_note      = coarse_note,
      axis_note            = .check4_axis_note(thr),
      pairing_note         = .check4_pairing_note(spec, panel),
      n_out_of_scale       = if (is.null(curve)) NA_integer_ else
                               attr(curve, "n_out_of_scale"),
      type_gate_applies    = gate,
      policy_as_whole_note = .check4_policy_note(gate),
      dependence_note      = .check4_dependence_note(),
      note                 = .check4_absent_note(has_academic, has_subjects,
                                                 panel, curve, spec),
      panel_error          = panel_error,
      pending              = c("candidate_region",
                               if (gate) "policy_as_whole"),
      sim_contract_version = data$meta$sim_contract_version,
      package_version      = as.character(utils::packageVersion("stateELP")),
      run_date             = Sys.Date()
    )
  )
}

# The Sample statement, on its own line at the top of the document
# (CLAUDE.md 2). It names the panel and says whose classifications these are,
# because the reference groups Check 3 draws beside its bands are not here.
.check4_sample_text <- function(panel, spec, curve, has_academic,
                                has_subjects = TRUE) {
  component <- spec_headline_label(spec)
  if (is.null(panel)) {
    why <- if (!has_academic) {
      "No academic table in this delivery"
    } else if (!has_subjects) {
      "The spec declares no academic subject"
    } else {
      paste0("No panel could be derived for ",
             spec_academic_subjects(spec)[[1]], " from this delivery")
    }
    return(paste0(
      "Cross-sectional: every current EL in one grade and year with both a ",
      "level on the ", component, " and a content score -- the same paired ",
      "cross-section as Data Check 3, not the entry cohort Data Checks 1 and ",
      "2 follow. ", why, ", so no panel is drawn."))
  }
  n <- if (is.null(curve)) NA else curve$N[[1]]
  paste0(
    "Cross-sectional -- the same paired cross-section as Data Check 3, not ",
    "the entry cohort Data Checks 1 and 2 follow: **every current grade-",
    panel$grade, " EL with both a ", panel$year, " ", component, " level and ",
    "a ", panel$year, " ", panel$subject, " score**",
    if (is.na(n)) "" else paste0(" (n = ", .check4_fmt_n(n), ")"),
    ". Former ELs and English-only students are not in it: the two ",
    "classifications compared here exist only for students still ",
    "classified. Each grade is a panel of its own and grades are never ",
    "pooled; this pass renders one, grade ", panel$grade, " ",
    panel$subject, ".")
}

# Same school year, not the same day. Saying so is the difference between
# "paired" and "simultaneous".
.check4_pairing_note <- function(spec, panel) {
  if (is.null(panel)) return(NA_character_)
  window <- spec$elp$administration_window
  paste0(
    "Scores are paired within the school year, not to the day: the ",
    spec_headline_label(spec), " is administered ",
    if (is.null(window)) "in its own window" else window,
    " and the content test later in the year.")
}

# The hazard at the right-hand end of the axis, the same one Check 3 renders
# for its top bands -- including its caveat for a rule with a second
# criterion, where the same students are the ones that criterion is holding.
.check4_axis_note <- function(thr) {
  if (is.na(thr$value)) return(NA_character_)
  paste0(
    "The right-hand end of the axis is constituted by the exit rule. A ",
    "student still classified as an EL at or above the current rule's ",
    "level (", thr$label, ") is, in most states, in their final year, so the ",
    "cuts at and above it rest on students on their way out rather than on a ",
    "cross-section of highly proficient ELs.",
    if (isTRUE(thr$conjunctive)) paste0(
      " This rule asks for a second criterion alongside the level, so those ",
      "cuts also hold students the other criterion is blocking -- a ",
      "different population again.") else "",
    " Read both curves there with that in mind.")
}

.check4_policy_note <- function(gate) {
  if (!gate) {
    return(paste0(
      "Does not apply. This spec's exit rule is Type 1, a single overall ",
      "criterion, so the policy's own pass/fail outcome is one point on the ",
      "curves above -- the current rule's cut -- and there is nothing ",
      "further to compute."))
  }
  paste0(
    "Applies, and is not built in this pass. This spec's exit rule ",
    "combines more than one criterion, so the policy's own pass/fail ",
    "outcome should be fed through the same consistency and J analysis to ",
    "show how the policy performs as a whole. Which criterion drives that ",
    "performance comes from Prep 2's crosswalk and Data Check 3's contested ",
    "zone, not from here.")
}

.check4_dependence_note <- function() {
  paste0(
    "Decision consistency and Youden's J -- the Cook et al. methods -- are ",
    "turnkey: built here, and they run on any delivery. The copula / ",
    "Kendall's-tau dependence analysis, which asks where and how strongly ",
    "ELP and content performance move together, including whether they ",
    "couple differently at the low and high ends, is custom: it has no ",
    "package support yet, so this slot is empty until it does.")
}

.check4_absent_note <- function(has_academic, has_subjects, panel, curve,
                                spec) {
  if (!has_academic) {
    return(paste0(
      "No academic table in this delivery. Every Check 4 slot lives on the ",
      "pairing of content scores with ELP levels, so the stage has nothing ",
      "to report."))
  }
  if (!has_subjects) {
    return(paste0(
      "The spec declares no academic subject, so there is no proficiency ",
      "cut to classify against and the stage has nothing to report."))
  }
  if (is.null(panel)) {
    # The underlying error stays in $meta$panel_error for the analyst; the
    # document says what it means.
    return(paste0(
      "No panel could be derived for ", spec_academic_subjects(spec)[[1]],
      ", the first academic subject the spec declares: this delivery carries ",
      "no academic rows the panel can be drawn from. The stage has nothing ",
      "to report."))
  }
  if (is.null(curve)) {
    return(paste0(
      "No current EL in grade ", panel$grade, " ", panel$subject, " has both ",
      "scores", if (is.na(panel$year)) "" else paste0(" in ", panel$year),
      ", or the spec gives no proficiency cut for the subject, so there is ",
      "no curve to draw."))
  }
  NA_character_
}
