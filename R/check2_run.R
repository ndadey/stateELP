# Check 2 -- assembly. run_<stage>() computes nothing itself: it calls the
# compute functions, degrades absent slots to NULL with a comment, and builds
# $meta (CLAUDE.md 8).

#' Run Check 2 -- on-time vs. prolonged exiters
#'
#' Takes the cohort Check 1 analyzes -- the selected row of
#' [compute_cohorts()], not a second selection -- and classifies each student
#' by exit outcome: `$flags` (the student-level indicators), `$crosstab` (the
#' slide-63/64 crosstab, Total block plus the characteristic blocks),
#' `$academic_summary` (achievement at comparable points in each group's
#' trajectory) and `$trajectories` (ELP level by year in status, conditioned on
#' exit group).
#'
#' With no qualifying cohort -- the same condition under which Check 1 selects
#' none -- every analysis slot stays `NULL` and `$meta$note` says why. Never a
#' crash, never an invented denominator. `$academic_summary` needs the academic
#' table and is `NULL` without it.
#'
#' `$blocked_groups` is the Type-gated Additional check and is `NULL` for a
#' Type 1 spec. `$meta$type_gate_applies` says whether the spec declares a
#' Type it would apply to, so the document can tell "does not apply" from
#' "applies, but this delivery yields no groups".
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param start_grade Entry grade defining the cohorts, e.g. `"K"`.
#' @param subject Academic subject for `$academic_summary`. Defaults to the
#'   spec's first.
#' @return A named list of results slots plus `$meta`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' res <- run_check2(bundle, spec)
#' res$meta$sample
run_check2 <- function(data, spec, start_grade = "K", subject = NULL) {
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

  # The characteristic blocks need the cohort's fields at entry. A delivery
  # carrying none of them still gets the Total block.
  profile <- if (is.null(flags)) NULL else {
    compute_cohort_profile(data, spec, flags)
  }
  crosstab <- if (is.null(flags)) NULL else {
    compute_crosstab(flags, spec, profile = profile)
  }

  academic <- if (is.null(flags)) NULL else {
    compute_academic_summary(data, spec, flags, subject = subject)
  }
  trajectories <- if (is.null(flags)) NULL else {
    compute_trajectories(data, spec, flags)
  }

  # [Type-gated: Types 2-6 only, and the SIM spec is Type 1.]
  blocked <- if (is.null(flags)) NULL else {
    compute_blocked_groups(data, spec, flags)
  }

  subjects <- spec_academic_subjects(spec)
  on_time_text <- if (is.null(flags)) NA_character_
                  else .check2_on_time_text(flags$ON_TIME_YEARS)
  provisional <- spec$policy$timelines$source %in% "provisional"
  blocks <- if (is.null(crosstab)) character()
            else setdiff(unique(crosstab$BLOCK), "Total")

  list(
    flags            = flags,
    crosstab         = crosstab,
    profile          = profile,
    academic_summary = academic,
    trajectories     = trajectories,
    blocked_groups   = blocked,

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
      blocks               = blocks,
      academic_subject     = if (is.null(academic)) NA_character_
                             else attr(academic, "subject"),
      proficient_level     = spec$policy$academic_benchmark$proficient_level,
      other_subjects       = setdiff(subjects, attr(academic, "subject")),
      type_gate_applies    = spec_has_type(spec, 2:6),
      blocked_note         = .check2_blocked_note(blocked, spec),
      band_clock_note      = .check2_band_clock_note(spec),
      note                 = if (is.na(selected)) .check2_no_cohort_note()
                             else NA_character_,
      sim_contract_version = data$meta$sim_contract_version,
      package_version      = as.character(utils::packageVersion("stateELP")),
      run_date             = Sys.Date()
    )
  )
}

# The Initial ELP block and the on-time rule can be the same fact twice.
#
# `policy$timelines$on_time_rule$by_initial_band` gives each starting band its
# own deadline, and the crosstab then breaks the cohort down BY starting band.
# Where both are true, the Exit_OnTime column down that block measures every
# band against a different clock: a band allowed seven years is mechanically
# likelier to be "on time" than one allowed three, whatever its students did.
# Two bands whose students behaved identically can read 100% and 0%.
#
# That is the policy restated, not a finding, and nothing in the table shows
# it -- so the note carries the deadlines themselves, and points at Exit_Max,
# which uses one fixed window for every band and stays comparable.
#
# `NA` when the rule is flat, which is the ordinary case and needs no caution.
# `by_start_grade` needs none either: Check 2 analyzes one entry grade, so the
# allowance is constant within the table.
#
# Added 2026-09-21 from the Check 1 session's circularity review; see
# internal_docs/NOTES-check2.md.
.check2_band_clock_note <- function(spec) {
  by_band <- spec$policy$timelines$on_time_rule$by_initial_band
  if (is.null(by_band) || !length(by_band)) return(NA_character_)

  years <- paste(sprintf("%s: %s years", names(by_band), as.integer(by_band)),
                 collapse = "; ")
  paste0(
    "**The on-time deadline is not the same for every band.** This state ",
    "sets it by starting ELP band (", years, "), and the rows below are ",
    "those same bands. The Exit_OnTime column therefore measures each band ",
    "against its own clock, so a band given more years will show a higher ",
    "on-time share even if its students exited no sooner. Read **Exit_Max** ",
    "down this block instead for a comparison across bands: it uses the ",
    "single ", spec_max_years(spec), "-year maximum timeline for everyone. ",
    "Exit_OnTime remains the right column everywhere else in this table, ",
    "where the rows are not what sets the deadline."
  )
}

# Why the Additional check has no table: the rule is Type 1 and it does not
# apply, or it applies and this delivery yields no blocked groups. The two are
# different facts and the document says which.
.check2_blocked_note <- function(blocked, spec) {
  if (!is.null(blocked)) return(NA_character_)
  if (!spec_has_type(spec, 2:6)) {
    return(paste0(
      "The exit rule is declared as Type 1 (a single overall composite), so ",
      "there are no blocked-by-one-criterion groups to compare and this check ",
      "does not apply. Declare the additional criteria in ",
      "`spec$policy$exit$pathways` and set `types` to activate it."))
  }
  paste0(
    "The exit rule declares more than one criterion, so this check applies, ",
    "but no student in this cohort is blocked by exactly one criterion in the ",
    "delivered data -- either the criteria never disagree near the cut, or the ",
    "components they name have no results to evaluate.")
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
