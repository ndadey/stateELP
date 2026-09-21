# Prep 2 -- assembly. run_<stage>() computes nothing itself: it calls the
# compute functions, degrades absent slots to NULL with a comment, and builds
# $meta (CLAUDE.md 8).

#' Run Prep 2 -- population analysis
#'
#' The shape of the EL population: how many, in which grades and districts,
#' entering when and at what level, and with what profile. It is the
#' interpretive backdrop every later stage is read against.
#'
#' Sample: the full statewide EL population -- current, and former where the
#' data carries them -- observed across the whole panel. **Not a single
#' cohort.** Never-ELs enter only as the enrollment denominator, and only
#' weighted.
#'
#' Slots (each `NULL`, never a crash, when its dependency is absent):
#' `$counts_by_year`, `$el_share`, `$by_grade_span`, `$by_grade`,
#' `$by_district`, `$district_bins`, `$entry_sources`, `$entry_grade`,
#' `$entry_by_year`, `$entry_cohorts`, `$time_in_status`, `$initial_elp`,
#' `$profile`, `$monitoring`, `$analytic_sample_memo`, and the Type-gated
#' `$criteria_crosswalk`, `$blocked_groups` and `$contested_zone`, which are
#' `NULL` for a Type 1 rule. `$entry_sources`, `$entry_by_year`,
#' `$entry_cohorts`, `$time_in_status` and `$blocked_groups` are additions to
#' the instruction file's slot list `[PROPOSED - CONFIRM]`.
#'
#' `$meta$notes` holds the QC lines the compute steps produced -- how entry was
#' dated and how the delivered fields agree with the panel, how the initial
#' level was resolved -- keyed by slot, so the document can show them.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @return A named list of results slots plus `$meta`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' res <- run_prep2(bundle, spec)
#' res$meta$sample
run_prep2 <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  .roster_cols(roster, ROSTER_REQUIRED)

  counts_by_year <- compute_counts_by_year(data, spec)
  el_share       <- compute_el_share(data, spec)         # [NULL: no never-EL rows]
  by_grade       <- compute_by_grade(data, spec)
  by_grade_span  <- compute_by_grade_span(data, spec)
  by_district    <- compute_by_district(data, spec)      # [NULL: no DISTRICT_ID]
  district_bins  <- compute_district_bins(data, spec)    # [NULL: no DISTRICT_ID / size bands]

  entry          <- compute_entry(data, spec)            # student-level: not returned
  entry_grade    <- compute_entry_grade(entry, spec)
  entry_by_year  <- compute_entry_by_year(entry, spec)
  entry_cohorts  <- compute_entry_cohorts(data, spec)    # [NULL: no cohort in this panel]
  time_in_status <- compute_time_in_status(data, spec, entry)  # [NULL: no lt_el_years]
  initial_elp    <- compute_initial_elp(data, spec, entry)
  profile        <- compute_profile(data, spec, entry)   # $HERITAGE stays NULL
  monitoring     <- compute_monitoring(data, spec)       # [NULL: no former rows / monitoring field]

  # [Type-gated: Types 2-6. NULL for the Type 1 sim.]
  crosswalk <- compute_criteria_crosswalk(data, spec)

  panel <- range(roster$YEAR, na.rm = TRUE)
  years <- sort(unique(roster$YEAR))
  meta <- list(
    stage       = "prep2",
    spec_id     = spec$program$short_name %||% NA_character_,
    sample      = .prep2_sample_text(roster),
    panel       = panel,
    panel_years = years,
    n_years     = length(years),
    latest_year = panel[[2]],
    short_panel = length(years) < 5L,
    has_never   = any(roster$EL_STATUS %in% "never"),
    has_former  = any(roster$EL_STATUS %in% "former"),
    min_cell    = spec_min_cell(spec),
    headline_label = spec_headline_label(spec),
    lt_el_years = spec$policy$timelines$lt_el_years,
    small_district_n = .p2_small_n(spec),
    years_in_status_first_value = if (is.null(entry)) NA_integer_ else entry$yis_first,
    notes = list(
      entry       = entry$notes,
      initial_elp = attr(initial_elp, "notes"),
      grade       = .prep2_grade_note(by_grade),
      crosswalk   = if (is.null(crosswalk) && spec_has_type(spec, 2:6)) paste0(
        "The spec declares Type(s) ", paste(spec_exit_types(spec), collapse = ", "),
        ", but no exit pathway has two or more ELP terms to set against each ",
        "other, so the crosswalk is not computed.")),
    sim_contract_version = data$meta$sim_contract_version,
    package_version      = as.character(utils::packageVersion("stateELP")),
    run_date             = Sys.Date()
  )

  slots <- list(
    counts_by_year = counts_by_year, el_share = el_share,
    by_grade_span = by_grade_span, by_grade = by_grade,
    by_district = by_district, district_bins = district_bins,
    entry_sources = entry$sources, entry_grade = entry_grade,
    entry_by_year = entry_by_year, entry_cohorts = entry_cohorts,
    time_in_status = time_in_status, initial_elp = initial_elp,
    profile = profile, monitoring = monitoring)

  c(slots,
    list(analytic_sample_memo = .prep2_memo(slots, meta),
         criteria_crosswalk = crosswalk$crosswalk,
         blocked_groups     = crosswalk$blocked_groups,
         contested_zone     = crosswalk$contested_zone,
         meta = meta))
}

# The Sample statement. Also `$meta$sample`, so the callout and the results
# object cannot disagree.
.prep2_sample_text <- function(roster) {
  years <- sort(unique(roster$YEAR))
  has_former <- any(roster$EL_STATUS %in% "former")
  txt <- sprintf(
    paste0("Full statewide EL population: every student identified as an English ",
           "learner (current%s), observed across %d years (%d%s%d). Not a single cohort."),
    if (has_former) " and former, within the monitoring window"
    else "; no former-EL rows were delivered",
    length(years), min(years), .P2_ENDASH, max(years))
  if (any(roster$EL_STATUS %in% "never")) {
    txt <- paste(txt, "Never-EL students enter only as the enrollment denominator.")
  }
  txt
}

.prep2_grade_note <- function(by_grade) {
  off <- attr(by_grade, "off_grid")
  if (is.null(off) || off == 0L) return(NULL)
  sprintf(paste0("%s current student-years carry a grade outside K-12 (pre-K or ",
                 "ungraded). They count in the year totals but not in any grade view."),
          format(off, big.mark = ","))
}
