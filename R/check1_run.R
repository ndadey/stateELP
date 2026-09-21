# Check 1 -- assembly. run_<stage>() computes nothing itself: it calls the
# compute functions, degrades absent slots to NULL with a comment, and builds
# $meta (CLAUDE.md 8).

#' Run Check 1 -- cohort survival analysis
#'
#' Selects one entry cohort and describes its exit timing: the candidate
#' cohorts, the life table, the exit curves, the discrete-time survival
#' estimate and its median, where the cohort ended up against the timelines,
#' both fitted models, and the experimental weighting index.
#'
#' Model 1 is two things, and both are here. `$life_table`, `$exit_curve`,
#' `$survival_est` and their statistics are its descriptive half, computed
#' from counts with no predictor of any kind. `$model1` is the same thing
#' written as a model -- year-in-status terms and no covariate, whose fitted
#' hazards reproduce the life table's annual exit rates -- and
#' `$model1_baseline` adds where the student started. Together they are the
#' nested sequence `$model2` extends.
#'
#' Slots degrade rather than fail. A headline reported as a level with no
#' scale behind it leaves `$model1_baseline` and `$model2` `NULL` with
#' `$meta$model1_note`, since neither has a covariate to fit; the
#' unconditional `$model1` is unaffected, because it asks for no score.
#' Without an academic table there is no Model 2, so `$model2` and the
#' selection-strength half of the weighting index are `NULL` and
#' `$meta$model2_note` says why. `$type3_covariate` is `NULL` unless the spec
#' declares Type 3 and its non-ELP criterion names a subject the delivery
#' carries.
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
  exit_curve <- if (is.na(selected)) NULL else {
    compute_exit_curve(data, spec, cohort = selected, start_grade = start_grade)
  }
  # The split needs a headline to band. A delivery with no annual headline
  # rows -- or an instrument whose headline never resolves for this cohort --
  # loses the split and keeps the overall curve.
  has_band <- !is.na(selected) && nrow(.check1_initial_band(data, spec)) > 0L
  exit_curve_by_initial_band <- if (!has_band) NULL else {
    compute_exit_curve(data, spec, cohort = selected,
                       start_grade = start_grade, by = "INITIAL_ELP")
  }

  survival_est <- if (is.na(selected)) NULL else {
    compute_survival_est(data, spec, cohort = selected,
                         start_grade = start_grade)
  }
  years_to_50 <- if (is.null(survival_est)) NULL else {
    compute_years_to_50(survival_est)
  }
  at_max_timeline <- if (is.na(selected)) NULL else {
    compute_at_max_timeline(data, spec, cohort = selected,
                            start_grade = start_grade)
  }

  # The unconditional fit needs no score at all, so it runs wherever a cohort
  # does; only the two models carrying an ELP term need one. Where the state's
  # reported result is a level or a determination and nothing else, those two
  # stay NULL and everything else is unaffected.
  fittable <- !is.na(selected)
  has_scale <- fittable && .check1_has_headline_scale(data, spec)
  # A cohort with no exits in it has no model in it either, and that is a
  # finding rather than a failure -- so the one condition the compute
  # functions raise for it is caught, and its message becomes the note the
  # document renders in the slot's place. Any other error still stops.
  unfittable1 <- NULL
  unfittable2 <- NULL

  model1 <- if (!fittable) NULL else {
    tryCatch(
      compute_model1(data, spec, cohort = selected, start_grade = start_grade),
      check1_unfittable = function(e) {
        unfittable1 <<- conditionMessage(e)
        NULL
      })
  }

  model1_baseline <- if (!has_scale) NULL else {
    tryCatch(
      compute_model1(data, spec, cohort = selected, start_grade = start_grade,
                     baseline_elp = TRUE),
      check1_unfittable = function(e) NULL)
  }

  # Model 2 needs the academic table as well; without it the slot is NULL and
  # so is everything downstream of it.
  model2 <- if (!has_scale || is.null(el_academic(data))) NULL else {
    tryCatch(
      compute_model2(data, spec, cohort = selected, start_grade = start_grade),
      check1_unfittable = function(e) {
        unfittable2 <<- conditionMessage(e)
        NULL
      })
  }

  # Type-gated: the non-ELP criterion as a Model 2 covariate. Type 3 only,
  # and it needs the criterion's subject to be one the academic table carries.
  type3 <- .check1_type3_subject(spec, data)
  type3_covariate <- if (is.null(model2) || is.na(type3)) NULL else {
    fit <- compute_model2(data, spec, cohort = selected,
                          start_grade = start_grade,
                          add_current_subject = type3)
    list(criterion_subject = type3, model = fit,
         caption = paste0(
           "The exit rule already uses ", type3, ", so it appears again as ",
           "an outcome in Checks 3-4. Agreement between the criteria and ",
           "those methods is partly circular, and is captioned there."))
  }

  weights <- if (is.null(survival_est)) NULL else {
    compute_grade_weights(survival_est, model2, spec)
  }

  computed <- c(if (!is.null(cohorts)) "cohorts",
                if (!is.null(life_table)) "life_table",
                if (!is.null(exit_curve)) "exit_curve",
                if (!is.null(exit_curve_by_initial_band))
                  "exit_curve_by_initial_band",
                if (!is.null(survival_est)) "survival_est",
                if (!is.null(years_to_50)) "years_to_50",
                if (!is.null(at_max_timeline)) "at_max_timeline",
                if (!is.null(model1)) "model1",
                if (!is.null(model1_baseline)) "model1_baseline",
                if (!is.null(model2)) "model2")

  list(
    cohorts    = cohorts,
    life_table = life_table,
    exit_curve = exit_curve,
    exit_curve_by_initial_band = exit_curve_by_initial_band,

    survival_est               = survival_est,
    years_to_50                = years_to_50,
    at_max_timeline            = at_max_timeline,
    model1                     = model1,
    model1_baseline            = model1_baseline,
    model2                     = model2,

    # Experimental, and labeled so wherever it renders: untested against the
    # Indiana pipeline, and used mechanically nowhere.
    retention_share            = weights$retention_share,
    selection_strength         = weights$selection_strength,
    grade_weights              = weights$grade_weights,

    # [Type-gated: Type 3 only, and the SIM spec is Type 1]
    type3_covariate = type3_covariate,

    meta = list(
      stage                = "check1",
      spec_id              = spec$program$short_name %||% NA_character_,
      sample               = .check1_sample_text(cohorts, start_grade, panel_last,
                                                 spec),
      start_grade          = start_grade,
      cohort               = selected,
      panel                = c(min(roster$YEAR, na.rm = TRUE), panel_last),
      data_fields          = .check1_data_fields(data, spec, computed),
      note                 = .check1_note(cohorts, spec),
      band_note            = if (!is.na(selected) && !has_band) {
        paste0("No annual ", spec_headline_label(spec), " score is available ",
               "for this cohort, so the exit curve is not split by initial ",
               "ELP band.")
      } else NA_character_,
      model1_note          = if (is.na(selected)) NA_character_
                             else if (!is.null(unfittable1)) unfittable1
                             else if (!has_scale) paste0(
                               "The ", spec_headline_label(spec), " is ",
                               "reported as a level with no scale score ",
                               "behind it, so there is nothing to use as a ",
                               "predictor: the model with baseline ELP, and ",
                               "Model 2, are not fitted. The unconditional ",
                               "model, the life table, the exit curves and ",
                               "the survival estimate are unaffected.")
                             else NA_character_,
      model2_note          = if (is.na(selected) || !has_scale) NA_character_
                             else if (is.null(el_academic(data))) paste0(
                               "No academic table in this delivery, so Model 2 ",
                               "and the selection-strength half of the ",
                               "weighting index are not computed.")
                             else if (!is.null(unfittable2)) unfittable2
                             else NA_character_,
      pending              = character(),
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
    " ", .EM_DASH, " the longest follow-up.")
}

# Can the fitted models run at all? Both use the headline scale score as a
# covariate, and some instruments report the headline as a level or a
# determination with no scale behind it (the spec says which, through the
# component's `measures`). The spec is asked first and the delivery second:
# a spec that declares a scale but a delivery that carries none is the same
# dead end, and the guard exists so the slot degrades to NULL instead of the
# compute function stopping.
.check1_has_headline_scale <- function(data, spec) {
  headline <- spec_headline(spec)
  measures <- spec$elp$components[[headline]]$measures
  if (!is.null(measures) && !"scale" %in% measures) return(FALSE)
  h <- elp_headline(data, spec)
  nrow(h) > 0L && any(!is.na(h$SCALE_SCORE))
}

# Type 3 is "composite plus other source": the exit rule carries a term drawn
# from the academic table. Returns that subject when the spec declares Type 3,
# the term is there, and the delivery actually carries the subject; NA
# otherwise, which leaves the slot NULL.
.check1_type3_subject <- function(spec, data) {
  if (!isTRUE(spec_has_type(spec, 3))) return(NA_character_)
  terms <- unlist(lapply(spec$policy$exit$pathways, function(p) p$terms),
                  recursive = FALSE)
  academic <- Filter(function(tm) identical(tm$from, "academic"), terms)
  if (!length(academic)) return(NA_character_)

  subject <- academic[[1]]$component
  ac <- el_academic(data)
  if (is.null(ac) || !subject %in% ac$SUBJECT) return(NA_character_)
  subject
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
