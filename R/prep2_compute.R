# Prep 2 -- population analysis: the computations. Plain data out, no plotting,
# no I/O (CLAUDE.md 8). Ported from the standalone draft
# (stateData/MT/prep2_population_V2.qmd), where every block ran inline.
#
# Conventions this file leans on, all from CLAUDE.md:
#   * the roster is the counting frame; students are never counted from the base
#     or academic tables (4);
#   * statewide shares weight by SAMPLE_WEIGHT, within-EL statistics do not, and
#     no unweighted never-EL total leaves this file (4);
#   * a suppressed cell is NA, never a dropped row (4). Prep 2 suppresses a cell
#     when 0 < n < min_cell: a true zero discloses nothing and stays a zero,
#     which is what the standalone did. [PROPOSED - CONFIRM] -- apply_suppression()
#     also blanks zeros, which reads as "suppressed" for a year with no former
#     ELs at all;
#   * every column of every data frame is ALL CAPS (4).
#
# Scalars and QC lines a plot or the document needs travel as attributes of
# the tibble they describe (documented on each @return), so the slot stays a
# plain data frame and run_prep2() gathers the notes into $meta$notes.

.P2_GRADES <- c("K", as.character(1:12))
.P2_ENTRY_SOURCES <- c("LEP_PROGRAM_START_DATE", "COHORT_START_YEAR",
                       "YEAR_IN_STATUS", "ENTRY_GRADE")
.P2_DUR_UNKNOWN <- "unknown (entry before panel)"
.P2_ALT_LABEL <- "alternate form (excluded from bands)"

# Display glyphs from code points: R CMD check wants ASCII sources.
.P2_DASH <- intToUtf8(0x2014)
.P2_ENDASH <- intToUtf8(0x2013)

# ---- small helpers -----------------------------------------------------------

.p2_hit <- function(n, spec) !is.na(n) & n > 0 & n < spec_min_cell(spec)

.p2_suppress <- function(n, spec) {
  n[.p2_hit(n, spec)] <- NA
  n
}

# Shares of a total, NA throughout when the total is zero. (Not ifelse(): a
# length-1 test would return a length-1 result and recycle the first share.)
.p2_prop <- function(n) {
  if (sum(n) > 0) n / sum(n) else rep(NA_real_, length(n))
}

.p2_weights <- function(roster) {
  w <- if ("SAMPLE_WEIGHT" %in% names(roster)) roster$SAMPLE_WEIGHT
       else rep(1, nrow(roster))
  ifelse(is.na(w), 1, w)
}

.p2_current <- function(roster) roster[roster$EL_STATUS %in% "current", , drop = FALSE]

.p2_span <- function(g) ifelse(g %in% c("K", as.character(1:5)), "K-5", "6-12")

# "6+" -> 6L, " 3 " -> 3L, "" -> NA. A delivery may cap a count as "n+".
.p2_as_count <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & !nzchar(x)] <- NA
  suppressWarnings(as.integer(sub("\\+$", "", x)))
}

# Delivered grade codes through the spec's value map, then held to K-12.
.p2_norm_grade <- function(g, spec) {
  g <- .recode_grade(g, spec)
  g[!g %in% .P2_GRADES] <- NA_character_
  g
}

# One value per student: the first non-missing `x`, given rows ordered by
# student then year. Aligned to `ids`.
.p2_first_by <- function(id, x, ids) {
  keep <- !is.na(x)
  id <- id[keep]
  x <- x[keep]
  first <- !duplicated(id)
  x[first][match(ids, id[first])]
}

.p2_size_bands <- function(spec) {
  b <- spec$policy$district_size_bands
  if (is.null(b)) return(NULL)
  stopifnot(is.numeric(b$breaks), is.character(b$labels),
            length(b$breaks) == length(b$labels) + 1L)
  b
}

# Left-closed, like assign_elp_band(): a district with exactly the first
# interior break's count lands in the band above it.
.p2_size_band <- function(n, spec) {
  b <- .p2_size_bands(spec)
  if (is.null(b)) return(rep(NA_character_, length(n)))
  as.character(cut(n, breaks = b$breaks, labels = b$labels, right = FALSE,
                   include.lowest = TRUE))
}

# The count the "small district" reference line sits at: the first interior
# break of the spec's size bands (100 in the deck default).
.p2_small_n <- function(spec) {
  b <- .p2_size_bands(spec)
  if (is.null(b) || length(b$breaks) < 3L) return(NA_real_)
  b$breaks[[2]]
}

# ---- counts and shares by year -----------------------------------------------

#' Roster coverage by year
#'
#' Current ELs by year, split by what the ELP data offers for them, plus former
#' ELs and the weighted never-EL count. Counted from the roster -- the only
#' counting frame -- through [el_coverage_table()], never from the base table
#' (which has eight rows per WIDA student-year).
#'
#' `N_CURRENT = N_ANNUAL_HEADLINE + N_ALTERNATE + N_NO_HEADLINE`: a current EL
#' either has an annual-form headline row, took the alternate form (reported as
#' a group, never banded), or has no headline result that year.
#'
#' Never-ELs appear **only weighted** (`N_NEVER_WEIGHTED`, by `SAMPLE_WEIGHT`):
#' an unweighted never-EL total is never reported. Cells with `0 < n <
#' min_cell` are `NA`.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @return A tibble, one row per year: `YEAR`, `N_CURRENT`,
#'   `N_ANNUAL_HEADLINE`, `N_ALTERNATE`, `N_NO_HEADLINE`, `N_FORMER`,
#'   `N_NEVER_WEIGHTED`, `SUPPRESSED`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' compute_counts_by_year(bundle, spec)
compute_counts_by_year <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  cov <- el_coverage(data)
  # A bundle built without a spec cannot resolve the headline, so its coverage
  # table carries NA for the with-headline count; rebuild it with this spec.
  if (is.null(cov) || all(is.na(cov$N_CURRENT_ELP))) {
    cov <- el_coverage_table(roster, elp = el_base(data), spec = spec)
  }

  out <- cov |>
    dplyr::group_by(.data$YEAR) |>
    dplyr::summarise(
      N_CURRENT         = sum(.data$N_CURRENT),
      N_ANNUAL_HEADLINE = sum(.data$N_CURRENT_ELP),
      N_ALTERNATE       = sum(.data$N_ALTERNATE),
      N_FORMER          = sum(.data$N_FORMER),
      N_NEVER_WEIGHTED  = sum(.data$N_NEVER_WEIGHTED),
      .groups = "drop")
  out$N_NO_HEADLINE <- out$N_CURRENT - out$N_ANNUAL_HEADLINE - out$N_ALTERNATE
  out <- out[c("YEAR", "N_CURRENT", "N_ANNUAL_HEADLINE", "N_ALTERNATE",
               "N_NO_HEADLINE", "N_FORMER", "N_NEVER_WEIGHTED")]

  counts <- setdiff(names(out), "YEAR")
  out$SUPPRESSED <- Reduce(`|`, lapply(counts, function(cl) .p2_hit(out[[cl]], spec)))
  for (cl in counts) out[[cl]] <- .p2_suppress(out[[cl]], spec)
  out
}

#' Current ELs as a share of total enrollment
#'
#' **Weighted** by `SAMPLE_WEIGHT`: never-EL rows in a delivery may be a
#' sample, so an unweighted share reads several times too high. `NULL` when the
#' roster carries no never-EL rows, because total enrollment is then unknown.
#'
#' @inheritParams compute_counts_by_year
#' @return A tibble, one row per year: `YEAR`, `N_ENROLLMENT_WEIGHTED`,
#'   `N_EL_WEIGHTED`, `PCT_EL`; or `NULL`.
#' @export
compute_el_share <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  if (!any(roster$EL_STATUS %in% "never")) return(NULL)   # [no never-EL rows]

  d <- tibble::tibble(YEAR = roster$YEAR, W = .p2_weights(roster),
                      CUR = roster$EL_STATUS %in% "current")
  out <- d |>
    dplyr::group_by(.data$YEAR) |>
    dplyr::summarise(N_ENROLLMENT_WEIGHTED = sum(.data$W),
                     N_EL_WEIGHTED = sum(.data$W[.data$CUR]),
                     .groups = "drop")
  out$PCT_EL <- out$N_EL_WEIGHTED / out$N_ENROLLMENT_WEIGHTED
  hit <- .p2_hit(out$N_EL_WEIGHTED, spec)
  out$PCT_EL[hit] <- NA
  out$N_EL_WEIGHTED <- .p2_suppress(out$N_EL_WEIGHTED, spec)
  out
}

# ---- grade distribution ------------------------------------------------------

#' Current ELs by grade and year
#'
#' One row per year and grade (`K`, `1`-`12`), zero-filled so an empty grade is
#' a row rather than a gap. `PCT` is the grade's share of that year's current
#' ELs (column percentages: read down a year for concentration, across a grade
#' for drift). Student-years whose grade is outside K-12 (pre-K, ungraded) are
#' not in this view; their count is `attr(x, "off_grid")`.
#'
#' @inheritParams compute_counts_by_year
#' @return A tibble: `YEAR`, `GRADE`, `N`, `PCT`, `SUPPRESSED`, with attribute
#'   `off_grid`, the number of current student-years outside K-12.
#' @export
compute_by_grade <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  cur_all <- .p2_current(roster)
  cur <- cur_all[cur_all$GRADE %in% .P2_GRADES, , drop = FALSE]

  grid <- tibble::as_tibble(expand.grid(YEAR = sort(unique(roster$YEAR)),
                                        GRADE = .P2_GRADES,
                                        stringsAsFactors = FALSE))
  n <- dplyr::count(cur, .data$YEAR, .data$GRADE, name = "N")
  out <- dplyr::left_join(grid, n, by = c("YEAR", "GRADE"))
  out$N <- dplyr::coalesce(out$N, 0L)
  out <- out[order(out$YEAR, match(out$GRADE, .P2_GRADES)), ]
  out <- out |>
    dplyr::group_by(.data$YEAR) |>
    dplyr::mutate(PCT = .p2_prop(.data$N)) |>
    dplyr::ungroup()

  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out$PCT[out$SUPPRESSED] <- NA
  attr(out, "off_grid") <- sum(!cur_all$GRADE %in% .P2_GRADES)
  out
}

#' Current ELs by grade span and year
#'
#' K-5 against 6-12, per year (deck slide 35).
#'
#' @inheritParams compute_counts_by_year
#' @return A tibble: `YEAR`, `SPAN`, `N`, `PCT`, `SUPPRESSED`.
#' @export
compute_by_grade_span <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  cur <- .p2_current(roster)
  cur <- cur[cur$GRADE %in% .P2_GRADES, , drop = FALSE]
  cur$SPAN <- .p2_span(cur$GRADE)

  grid <- tibble::as_tibble(expand.grid(YEAR = sort(unique(roster$YEAR)),
                                        SPAN = c("K-5", "6-12"),
                                        stringsAsFactors = FALSE))
  n <- dplyr::count(cur, .data$YEAR, .data$SPAN, name = "N")
  out <- dplyr::left_join(grid, n, by = c("YEAR", "SPAN"))
  out$N <- dplyr::coalesce(out$N, 0L)
  out <- out[order(out$YEAR, match(out$SPAN, c("K-5", "6-12"))), ]
  out <- out |>
    dplyr::group_by(.data$YEAR) |>
    dplyr::mutate(PCT = .p2_prop(.data$N)) |>
    dplyr::ungroup()
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out$PCT[out$SUPPRESSED] <- NA
  out
}

# ---- districts ---------------------------------------------------------------

# Current ELs per district in the latest year, unsuppressed and ranked: the
# shared input of the ranked bars and the size bins.
.p2_district_counts <- function(roster) {
  latest <- max(roster$YEAR, na.rm = TRUE)
  cur <- .p2_current(roster)
  cur <- cur[cur$YEAR == latest & !is.na(cur$DISTRICT_ID), , drop = FALSE]
  d <- dplyr::count(cur, .data$DISTRICT_ID, name = "N")
  d <- d[order(-d$N, d$DISTRICT_ID), ]
  d$RANK <- seq_len(nrow(d))
  d$PCT_CUM <- if (nrow(d)) cumsum(d$N) / sum(d$N) else numeric()
  attr(d, "year") <- latest
  d
}

#' Current ELs by district, ranked
#'
#' Districts in the latest year, ranked by current-EL count (deck slides
#' 33-34). `PCT_CUM` is the cumulative share down the ranking and `TWO_THIRDS`
#' marks the districts that together first reach `share` of all current ELs --
#' the rank the chart's dashed line sits at. `SIZE_BAND` is the district's band
#' under `policy$district_size_bands`; **never** the delivery's own
#' `DISTRICT_SIZE_BAND` column, which is a tertile split of a different thing.
#'
#' `NULL` when the roster carries no `DISTRICT_ID`. A district with
#' `0 < N < min_cell` keeps its rank and its cumulative share -- both are
#' computed before suppression -- and shows `N = NA`.
#'
#' @inheritParams compute_counts_by_year
#' @param share Cumulative share that defines the reference rank.
#' @return A tibble: `RANK`, `DISTRICT_ID`, `N`, `PCT_CUM`, `TWO_THIRDS`,
#'   `SIZE_BAND`, `SUPPRESSED`; attributes `year` (the latest year) and
#'   `small_n` (the count the small-district line sits at, `NA` when the spec
#'   declares no size bands). `NULL` without `DISTRICT_ID`.
#' @export
compute_by_district <- function(data, spec, share = 2 / 3) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.numeric(share), length(share) == 1L, share > 0, share <= 1)
  roster <- el_roster(data)
  if (!"DISTRICT_ID" %in% names(roster)) return(NULL)   # [no DISTRICT_ID]
  d <- .p2_district_counts(roster)
  if (!nrow(d)) return(NULL)

  k <- which(d$PCT_CUM >= share)[1]
  out <- tibble::tibble(
    RANK       = d$RANK,
    DISTRICT_ID = d$DISTRICT_ID,
    N          = d$N,
    PCT_CUM    = d$PCT_CUM,
    TWO_THIRDS = d$RANK <= k,
    SIZE_BAND  = .p2_size_band(d$N, spec))
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  attr(out, "year") <- attr(d, "year")
  attr(out, "small_n") <- .p2_small_n(spec)
  out
}

#' Districts binned by EL enrollment
#'
#' Districts and current ELs per size band of `policy$district_size_bands`, in
#' the latest year. `NULL` when the roster carries no `DISTRICT_ID` or the spec
#' declares no bands.
#'
#' @inheritParams compute_counts_by_year
#' @return A tibble: `SIZE_BAND`, `N_DISTRICTS`, `N`, `PCT`, `SUPPRESSED`, one
#'   row per declared band (empty bands stay); or `NULL`.
#' @export
compute_district_bins <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  b <- .p2_size_bands(spec)
  if (is.null(b) || !"DISTRICT_ID" %in% names(roster)) return(NULL)
  d <- .p2_district_counts(roster)
  if (!nrow(d)) return(NULL)
  year <- attr(d, "year")

  d$SIZE_BAND <- .p2_size_band(d$N, spec)
  n_dist <- table(factor(d$SIZE_BAND, levels = b$labels))
  n_el <- tapply(d$N, factor(d$SIZE_BAND, levels = b$labels), sum)
  n_el[is.na(n_el)] <- 0L
  out <- tibble::tibble(SIZE_BAND = b$labels,
                        N_DISTRICTS = as.integer(n_dist),
                        N = as.integer(n_el))
  out$PCT <- out$N / sum(out$N)
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out$PCT[out$SUPPRESSED] <- NA
  attr(out, "year") <- year
  out
}

# ---- entry into EL status ----------------------------------------------------

# A delivered start date to the spring (end) year it belongs to. The default
# rule is the academic calendar: 2023-24 is 2024, so July or later starts the
# NEXT end-year. A delivery that stamps the calendar year of the spring label
# (the sim does) sets `spec$data$entry_date_rule = "calendar_year"`.
.p2_entry_year_from_date <- function(d, rule) {
  if (!inherits(d, "Date")) {
    d <- as.character(d)
    d[!is.na(d) & !nzchar(trimws(d))] <- NA
    d <- as.Date(d, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"),
                 optional = TRUE)
  }
  yr <- as.integer(format(d, "%Y"))
  mo <- as.integer(format(d, "%m"))
  if (identical(rule, "calendar_year")) yr else yr + as.integer(mo >= 7L)
}

#' Date each student's entry into EL status
#'
#' The entry year of every student ever observed as `current`, resolved from
#' the first available of the delivered entry fields in
#' `spec$data$entry_sources` (default `LEP_PROGRAM_START_DATE`,
#' `COHORT_START_YEAR`, `YEAR_IN_STATUS`, `ENTRY_GRADE`), capped at the first
#' year the panel observes them as `current`. With none delivered, the panel
#' itself dates entry when a student first turns `current` after the panel
#' opens (and a K student in the panel's first year is taken as a K entrant
#' that year). A student already EL when the panel opens with nothing to date
#' them has an **unknown** entry: their time in status is only a lower bound.
#'
#' `YEAR_IN_STATUS` counts a first year as 1 or as 0; which is detected from
#' in-panel entrants unless `spec$data$years_in_status_first_value` says
#' (`[DERIVED - VERIFY]`). A delivered start date maps to an end-year by
#' `spec$data$entry_date_rule`: `"academic_end_year"` (default) or
#' `"calendar_year"`.
#'
#' This is Prep 2's own derivation. The shared cohort machinery in `R/utils.R`
#' dates entry from panel transitions only; here the delivered fields are used
#' as *sources* for entry the panel cannot see, and every disagreement between
#' the two is reported in the returned notes. `[PROPOSED - CONFIRM]`
#'
#' @inheritParams compute_counts_by_year
#' @return A list, or `NULL` when the roster has no `current` rows:
#'   `students` (a tibble, one row per ever-current student: `STUDENT_ID`,
#'   `FIRST_CURRENT_YEAR`, `FIRST_CURRENT_GRADE`, `ENTRY_SOURCE`, `ENTRY_YEAR`,
#'   `ENTRY_GRADE`, `ENTRY_KNOWN`, `LEFT_TRUNCATED`, `ENTRY_STATUS`),
#'   `sources` (how many students each source dated), `notes` (QC lines: the
#'   detected `YEAR_IN_STATUS` convention, and how the delivered fields agree
#'   with the panel), `yis_first`, and `years` (the panel's first and last
#'   year). Student-level: [run_prep2()] keeps only the aggregates.
#' @export
compute_entry <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  first_year <- min(roster$YEAR, na.rm = TRUE)
  last_year <- max(roster$YEAR, na.rm = TRUE)
  cur <- .p2_current(roster)
  if (!nrow(cur)) return(NULL)                            # [no current rows]
  cur <- cur[order(cur$STUDENT_ID, cur$YEAR), , drop = FALSE]
  for (cl in .P2_ENTRY_SOURCES) if (!cl %in% names(cur)) cur[[cl]] <- NA

  notes <- character()
  min_cell <- spec_min_cell(spec)
  cross <- spec$data$columns$roster
  delivered_as <- function(nm) {
    if (!is.null(cross) && nm %in% names(cross) && !identical(cross[[nm]], nm)) {
      cross[[nm]]
    } else NA_character_
  }

  pref <- intersect(spec$data$entry_sources %||% .P2_ENTRY_SOURCES, .P2_ENTRY_SOURCES)
  if (!length(pref)) pref <- .P2_ENTRY_SOURCES
  date_rule <- spec$data$entry_date_rule %||% "academic_end_year"

  by <- cur$STUDENT_ID
  first_row <- !duplicated(by)
  ids <- by[first_row]
  first_cur_row <- cur$YEAR[first_row][match(by, ids)]

  # --- YEAR_IN_STATUS: does a first year code as 1 or as 0? -------------------
  yis_raw <- cur$YEAR_IN_STATUS
  yis_val <- .p2_as_count(yis_raw)
  yis_first <- spec$data$years_in_status_first_value
  if (any(!is.na(yis_val))) {
    capped <- sum(grepl("\\+$", trimws(as.character(yis_raw))))
    if (capped > 0) {
      notes <- c(notes, sprintf(paste0(
        "%s `YEAR_IN_STATUS` values are capped (\"n+\") and are read as their ",
        "floor, so durations above the cap are understated."),
        format(capped, big.mark = ",")))
    }
    probe <- yis_val[first_cur_row > first_year & cur$YEAR == first_cur_row]
    probe <- probe[!is.na(probe)]
    if (is.null(yis_first)) {
      if (length(probe) >= min_cell) {
        tab <- table(probe)
        modal <- as.integer(names(tab)[which.max(tab)])
        yis_first <- if (modal == 0L) 0L else 1L
        notes <- c(notes, sprintf(paste0(
          "On the first observed current year of in-panel entrants, ",
          "`YEAR_IN_STATUS` is %d for %s of them, so it is read as %s ",
          "[DERIVED - VERIFY]."),
          modal, .p2_fmt_pct(max(tab) / sum(tab)),
          if (modal == 0L) "completed years (first year = 0)"
          else "counting the current year (first year = 1)"))
        if (!modal %in% c(0L, 1L)) {
          notes <- c(notes, paste0(
            "That modal value is neither 0 nor 1: the count starts before ",
            "the observed status (pre-K identification, or years elsewhere), ",
            "so entry years taken from it precede the panel's first observed ",
            "current year. Read the validation lines below with that in mind."))
        }
      } else {
        yis_first <- 1L
        notes <- c(notes, paste0(
          "Too few in-panel entrants carry `YEAR_IN_STATUS` to detect its ",
          "convention; it is assumed to count the current year (first year = ",
          "1). Set `spec$data$years_in_status_first_value` to override."))
      }
    } else {
      notes <- c(notes, sprintf("`YEAR_IN_STATUS` is read per the spec as %s.",
        if (yis_first == 0) "completed years (first year = 0)"
        else "counting the current year (first year = 1)"))
    }
  }
  if (is.null(yis_first)) yis_first <- 1L

  # --- an entry year from each delivered source, per row ----------------------
  entry_grade_dl <- .p2_norm_grade(cur$ENTRY_GRADE, spec)
  yr_date <- .p2_entry_year_from_date(cur$LEP_PROGRAM_START_DATE, date_rule)
  yr_cohort <- .p2_as_count(cur$COHORT_START_YEAR)
  yr_yis <- as.integer(cur$YEAR - yis_val + as.integer(yis_first))
  yr_grade <- as.integer(cur$YEAR - (match(cur$GRADE, .P2_GRADES) -
                                       match(entry_grade_dl, .P2_GRADES)))

  fb <- function(x) .p2_first_by(by, x, ids)
  students <- tibble::tibble(
    STUDENT_ID          = ids,
    FIRST_CURRENT_YEAR  = cur$YEAR[first_row],
    FIRST_CURRENT_GRADE = cur$GRADE[first_row],
    YR_DATE   = fb(yr_date),
    YR_COHORT = fb(yr_cohort),
    YR_YIS    = fb(yr_yis),
    YR_GRADE  = fb(yr_grade),
    GRADE_DL  = fb(entry_grade_dl))

  # Does YEAR_IN_STATUS advance by one each year? (one implied entry year)
  ok <- !is.na(yr_yis)
  consistent <- if (any(ok)) {
    tibble::tibble(STUDENT_ID = by[ok], Y = yr_yis[ok]) |>
      dplyr::group_by(.data$STUDENT_ID) |>
      dplyr::summarise(CONSISTENT = min(.data$Y) == max(.data$Y), .groups = "drop")
  } else NULL
  students$YIS_CONSISTENT <- if (is.null(consistent)) NA
                             else consistent$CONSISTENT[match(students$STUDENT_ID,
                                                              consistent$STUDENT_ID)]

  # --- first available source per student, in preference order ----------------
  srcm <- cbind(LEP_PROGRAM_START_DATE = students$YR_DATE,
                COHORT_START_YEAR = students$YR_COHORT,
                YEAR_IN_STATUS = students$YR_YIS,
                ENTRY_GRADE = students$YR_GRADE)[, pref, drop = FALSE]
  avail <- !is.na(srcm)
  has_any <- rowSums(avail) > 0
  pick <- max.col(avail + 0, ties.method = "first")
  delivered_source <- ifelse(has_any, colnames(srcm)[pick], NA_character_)
  delivered_year <- as.integer(srcm[cbind(seq_len(nrow(srcm)), pick)])
  delivered_year[!has_any] <- NA_integer_

  src_label <- stats::setNames(pref, pref)
  if ("ENTRY_GRADE" %in% pref) {
    src_label["ENTRY_GRADE"] <- "ENTRY_GRADE (one grade per year assumed)"
  }
  for (nm in pref) {
    d <- delivered_as(nm)
    if (!is.na(d)) src_label[nm] <- sprintf("%s (delivered as %s)", nm, d)
  }

  src_unknown <- "unknown: already EL when the panel opened"
  entry_source <- dplyr::case_when(
    !is.na(delivered_source)                      ~ unname(src_label[delivered_source]),
    students$FIRST_CURRENT_YEAR > first_year      ~ "first current year (entry observed in panel)",
    students$FIRST_CURRENT_GRADE %in% "K"         ~ "K in the panel's first year",
    TRUE                                          ~ src_unknown)
  # A delivered year cannot postdate an observed current year.
  entry_year <- dplyr::case_when(
    !is.na(delivered_year)      ~ pmin(delivered_year, students$FIRST_CURRENT_YEAR),
    entry_source == src_unknown ~ NA_integer_,
    TRUE                        ~ students$FIRST_CURRENT_YEAR)
  entry_year <- as.integer(entry_year)

  g_at <- roster$GRADE[match(paste(students$STUDENT_ID, entry_year),
                             paste(roster$STUDENT_ID, roster$YEAR))]
  entry_grade <- dplyr::coalesce(g_at, students$GRADE_DL)
  entry_grade[!entry_grade %in% .P2_GRADES] <- NA_character_

  known <- !is.na(entry_year)
  truncated <- !known | entry_year < first_year
  out <- tibble::tibble(
    STUDENT_ID          = students$STUDENT_ID,
    FIRST_CURRENT_YEAR  = students$FIRST_CURRENT_YEAR,
    FIRST_CURRENT_GRADE = students$FIRST_CURRENT_GRADE,
    ENTRY_SOURCE        = entry_source,
    ENTRY_YEAR          = entry_year,
    ENTRY_GRADE         = entry_grade,
    ENTRY_KNOWN         = known,
    LEFT_TRUNCATED      = truncated,
    ENTRY_STATUS        = dplyr::case_when(!known ~ "unknown",
                                           truncated ~ "before panel",
                                           TRUE ~ "in panel"))

  # --- validation against the panel -------------------------------------------
  n_yis <- sum(!is.na(students$YR_YIS))
  if (n_yis > 0 && any(!students$YIS_CONSISTENT[!is.na(students$YR_YIS)], na.rm = TRUE)) {
    bad <- mean(!students$YIS_CONSISTENT[!is.na(students$YR_YIS)], na.rm = TRUE)
    notes <- c(notes, sprintf(paste0(
      "For %s of the %s students carrying `YEAR_IN_STATUS`, the field does not ",
      "advance by one each year (different rows imply different entry years); ",
      "the earliest current row's value is used."),
      .p2_fmt_pct(bad), format(n_yis, big.mark = ",")))
  }
  dl <- !is.na(delivered_year)
  if (any(dl)) {
    in_panel <- dl & students$FIRST_CURRENT_YEAR > first_year
    notes <- c(notes, sprintf(paste0(
      "Delivered entry fields cover %s of ever-current students. Where the ",
      "panel also observes the entry (%s students first current after %d), ",
      "the delivered year matches the first current year for %s; %s delivered ",
      "years fell after an observed current year and were pulled back to it."),
      .p2_fmt_pct(sum(dl) / nrow(students)),
      format(sum(in_panel), big.mark = ","), first_year,
      if (any(in_panel)) .p2_fmt_pct(mean(delivered_year[in_panel] ==
                                            students$FIRST_CURRENT_YEAR[in_panel]))
      else .P2_DASH,
      format(sum(delivered_year[dl] > students$FIRST_CURRENT_YEAR[dl]),
             big.mark = ",")))
  }
  if ("LEFT_TRUNCATED" %in% names(roster)) {
    flagged <- unique(roster$STUDENT_ID[roster$LEFT_TRUNCATED %in% TRUE])
    agree <- mean(out$LEFT_TRUNCATED == (out$STUDENT_ID %in% flagged))
    notes <- c(notes, sprintf(paste0("Left-truncation recomputed here agrees ",
      "with the delivered `LEFT_TRUNCATED` flag for %s of students."),
      .p2_fmt_pct(agree)))
  }

  sources <- dplyr::count(out, .data$ENTRY_SOURCE, name = "N")
  sources <- sources[order(-sources$N), ]
  sources$PCT <- sources$N / sum(sources$N)
  sources$SUPPRESSED <- .p2_hit(sources$N, spec)
  sources$N[sources$SUPPRESSED] <- NA
  sources$PCT[sources$SUPPRESSED] <- NA

  list(students = out, sources = sources, notes = notes,
       yis_first = as.integer(yis_first), years = c(first_year, last_year))
}

#' Entry grade of students entering EL status
#'
#' Grade at entry among students whose entry the panel observes -- not
#' left-truncated, grade known. Zero-filled over K-12.
#'
#' @param entry Output of [compute_entry()].
#' @param spec An `el_spec`.
#' @return A tibble: `ENTRY_GRADE`, `N`, `PCT`, `SUPPRESSED`; attribute
#'   `counts`, a named vector -- `OBSERVED` entrants in this view, `BEFORE_PANEL`
#'   students dated to an entry before the panel opened, `UNKNOWN` entry -- and
#'   attribute `years`, the panel's first and last year. `NULL` for a `NULL`
#'   `entry`.
#' @export
compute_entry_grade <- function(entry, spec) {
  stopifnot(inherits(spec, "el_spec"))
  if (is.null(entry)) return(NULL)
  s <- entry$students
  ent <- s[!s$LEFT_TRUNCATED & !is.na(s$ENTRY_GRADE), , drop = FALSE]

  n <- dplyr::count(ent, .data$ENTRY_GRADE, name = "N")
  out <- tibble::tibble(ENTRY_GRADE = .P2_GRADES)
  out$N <- dplyr::coalesce(n$N[match(out$ENTRY_GRADE, n$ENTRY_GRADE)], 0L)
  out$PCT <- if (sum(out$N) > 0) out$N / sum(out$N) else NA_real_
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out$PCT[out$SUPPRESSED] <- NA

  attr(out, "counts") <- .p2_suppress(c(
    OBSERVED     = nrow(ent),
    BEFORE_PANEL = sum(s$ENTRY_STATUS == "before panel"),
    UNKNOWN      = sum(s$ENTRY_STATUS == "unknown")), spec)
  attr(out, "years") <- entry$years
  out
}

#' Entrants by entry year and entry grade
#'
#' @inheritParams compute_entry_grade
#' @return A tibble, one row per entry year present and grade (K-12,
#'   zero-filled): `ENTRY_YEAR`, `ENTRY_GRADE`, `N`, `SUPPRESSED`; or `NULL`.
#' @export
compute_entry_by_year <- function(entry, spec) {
  stopifnot(inherits(spec, "el_spec"))
  if (is.null(entry)) return(NULL)
  s <- entry$students
  ent <- s[!s$LEFT_TRUNCATED & !is.na(s$ENTRY_GRADE), , drop = FALSE]
  if (!nrow(ent)) return(NULL)                            # [no observed entrants]

  grid <- tibble::as_tibble(expand.grid(ENTRY_YEAR = sort(unique(ent$ENTRY_YEAR)),
                                        ENTRY_GRADE = .P2_GRADES,
                                        stringsAsFactors = FALSE))
  n <- dplyr::count(ent, .data$ENTRY_YEAR, .data$ENTRY_GRADE, name = "N")
  out <- dplyr::left_join(grid, n, by = c("ENTRY_YEAR", "ENTRY_GRADE"))
  out$N <- dplyr::coalesce(out$N, 0L)
  out <- out[order(out$ENTRY_YEAR, match(out$ENTRY_GRADE, .P2_GRADES)), ]
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out
}

#' Candidate entry cohorts for Check 1
#'
#' Every entry year's cohort for one starting grade, with how long it is
#' followed. Membership comes from [build_cohort()] -- the same shared
#' derivation Check 1 selects from -- so this table and Check 1's `$cohorts`
#' cannot disagree about who is in a cohort. Unlike Check 1's candidates it
#' also lists the shorter cohorts that cannot cover the full exit window
#' (`FULL_WINDOW = FALSE`): usable, but censored at the window's edge.
#'
#' A cohort is a set of students who entered in the same grade in the same
#' year; left-truncated students are excluded by construction.
#'
#' @inheritParams compute_counts_by_year
#' @param start_grade Entry grade defining the cohorts.
#' @return A tibble: `COHORT_LABEL`, `COHORT_START_YEAR`, `N_ENTRY`,
#'   `FOLLOWUP_YEARS`, `FULL_WINDOW`, `SUPPRESSED`; or `NULL` when the panel
#'   supports no cohort.
#' @export
compute_entry_cohorts <- function(data, spec, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(start_grade), length(start_grade) == 1L)
  roster <- el_roster(data)
  panel_first <- min(roster$YEAR, na.rm = TRUE)
  panel_last <- max(roster$YEAR, na.rm = TRUE)

  members <- build_cohort(roster, spec, start_grade = start_grade,
                          entry_years = seq(panel_first, panel_last))
  if (!nrow(members)) return(NULL)                        # [no cohort in this panel]

  out <- members |>
    dplyr::count(.data$COHORT_LABEL, .data$COHORT_START_YEAR, name = "N_ENTRY") |>
    dplyr::arrange(.data$COHORT_START_YEAR)
  out$FOLLOWUP_YEARS <- as.integer(panel_last - out$COHORT_START_YEAR + 1L)
  # Seeing the whole exit window needs the maximum timeline plus the year
  # that observes an exit in its last year (first-former-year dating).
  out$FULL_WINDOW <- out$FOLLOWUP_YEARS >= spec_max_years(spec) + 1L
  apply_suppression(out, spec, n_col = "N_ENTRY", cols = character(0),
                    blank_n = TRUE)
}

# ---- time in status ----------------------------------------------------------

# Every current student-year with its time in status. A student whose entry is
# unknown has only a lower bound: the years observed since the panel opened.
.p2_current_duration <- function(roster, spec, entry) {
  lt <- spec$policy$timelines$lt_el_years
  cur <- .p2_current(roster)[c("STUDENT_ID", "YEAR", "GRADE")]
  s <- entry$students
  m <- match(cur$STUDENT_ID, s$STUDENT_ID)
  cur$ENTRY_YEAR <- s$ENTRY_YEAR[m]
  cur$ENTRY_KNOWN <- s$ENTRY_KNOWN[m]

  cur$YEARS_IN_STATUS <- ifelse(cur$ENTRY_KNOWN, cur$YEAR - cur$ENTRY_YEAR + 1L,
                                NA_integer_)
  cur$YEARS_OBSERVED <- cur$YEAR - entry$years[[1]] + 1L
  cur$IS_NEW <- cur$ENTRY_KNOWN & cur$YEARS_IN_STATUS == 1L
  # "At least": dated, or observed as current long enough. "At most" adds every
  # student whose unknown entry could still make them long-term.
  cur$LT_MIN <- (cur$ENTRY_KNOWN & cur$YEARS_IN_STATUS >= lt) |
    (!cur$ENTRY_KNOWN & cur$YEARS_OBSERVED >= lt)
  cur$LT_MAX <- cur$LT_MIN | !cur$ENTRY_KNOWN
  cur$DUR_BIN <- ifelse(cur$LT_MIN, paste0(lt, "+"),
                        ifelse(cur$ENTRY_KNOWN, as.character(cur$YEARS_IN_STATUS),
                               .P2_DUR_UNKNOWN))
  cur
}

.p2_dur_levels <- function(lt) {
  c(as.character(seq_len(lt - 1L)), paste0(lt, "+"), .P2_DUR_UNKNOWN)
}

#' New and long-term ELs
#'
#' Time in EL status among current ELs, counted from the entry year resolved by
#' [compute_entry()], not from a student's first appearance in the panel. A
#' student whose entry is unknown has only a lower bound, so the long-term
#' share is a **range**: `PCT_LT_MIN` counts students dated or observed as
#' `current` for `policy$timelines$lt_el_years`+ years; `PCT_LT_MAX` adds every
#' unknown-entry student who could still be long-term. The bounds meet once
#' every student's entry is dated.
#'
#' **No mean or average time in status is computed** (CLAUDE.md 9): a mean of
#' durations across students who entered at different grades and are censored
#' at different points describes no one. Distributions and shares only.
#'
#' `NULL` when the spec carries no structured long-term definition
#' (`lt_el_years`).
#'
#' @param data An `el_data` bundle.
#' @inheritParams compute_entry_grade
#' @return A list, or `NULL`: `by_year` (`YEAR`, `N`, `PCT_NEW`, `PCT_LT_MIN`,
#'   `PCT_LT_MAX`, `PCT_UNKNOWN`), `by_grade` (the same, by `GRADE` for the
#'   latest year, zero-filled K-12) and `distribution` (`GRADE`, `DUR_BIN`,
#'   `N`, `PCT` within grade, for the latest year).
#' @export
compute_time_in_status <- function(data, spec, entry) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  lt <- spec$policy$timelines$lt_el_years
  if (is.null(lt) || is.null(entry)) return(NULL)     # [no structured LTEL definition]
  roster <- el_roster(data)
  cur <- .p2_current_duration(roster, spec, entry)
  latest <- max(roster$YEAR, na.rm = TRUE)

  shares <- function(d, by) {
    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::summarise(N = dplyr::n(),
                       PCT_NEW = mean(.data$IS_NEW),
                       PCT_LT_MIN = mean(.data$LT_MIN),
                       PCT_LT_MAX = mean(.data$LT_MAX),
                       PCT_UNKNOWN = mean(!.data$ENTRY_KNOWN),
                       .groups = "drop")
  }
  blank <- function(x) {
    hit <- .p2_hit(x$N, spec) | x$N == 0L
    for (cl in grep("^PCT_", names(x), value = TRUE)) x[[cl]][hit] <- NA
    x$SUPPRESSED <- .p2_hit(x$N, spec)
    x$N[x$SUPPRESSED] <- NA
    x
  }

  by_year <- blank(shares(cur, "YEAR"))

  lat <- cur[cur$YEAR == latest & cur$GRADE %in% .P2_GRADES, , drop = FALSE]
  by_grade <- tibble::tibble(GRADE = .P2_GRADES)
  sg <- shares(lat, "GRADE")
  by_grade <- dplyr::left_join(by_grade, sg, by = "GRADE")
  by_grade$N <- dplyr::coalesce(by_grade$N, 0L)
  by_grade <- blank(by_grade)

  levels <- .p2_dur_levels(lt)
  grid <- tibble::as_tibble(expand.grid(GRADE = .P2_GRADES, DUR_BIN = levels,
                                        stringsAsFactors = FALSE))
  dn <- dplyr::count(lat, .data$GRADE, .data$DUR_BIN, name = "N")
  dist <- dplyr::left_join(grid, dn, by = c("GRADE", "DUR_BIN"))
  dist$N <- dplyr::coalesce(dist$N, 0L)
  dist <- dist[order(match(dist$GRADE, .P2_GRADES), match(dist$DUR_BIN, levels)), ]
  dist <- dist |>
    dplyr::group_by(.data$GRADE) |>
    dplyr::mutate(PCT = .p2_prop(.data$N)) |>
    dplyr::ungroup()
  dist$SUPPRESSED <- .p2_hit(dist$N, spec)
  dist$N[dist$SUPPRESSED] <- NA
  dist$PCT[dist$SUPPRESSED] <- NA

  list(by_year = by_year, by_grade = by_grade, distribution = dist)
}

# ---- initial ELP level -------------------------------------------------------

#' Initial ELP level of entrants
#'
#' The banded headline level at entry, among students whose entry the panel
#' observes. Read through [elp_headline()] and banded per `elp$bands`; which
#' year counts follows `policy$timelines$initial_elp_source`:
#' `"first_annual"` is the first annual headline at or after the entry year,
#' anything else (or nothing) the entry year alone.
#'
#' Alternate-form takers are counted as a group and **never banded**: the
#' alternate scale is a different measurement scale. Entrants with no annual
#' headline result are their own group, so the shares sum to the entrants.
#'
#' @inheritParams compute_time_in_status
#' @return A tibble, one row per band plus the two groups: `INITIAL`, `N`,
#'   `PCT`, `IN_BANDS`, `SUPPRESSED`; attribute `source` (the rule used) and
#'   `notes` (agreement with the delivered `INITIAL_ELP`, when present). `NULL`
#'   without observed entrants.
#' @export
compute_initial_elp <- function(data, spec, entry) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  if (is.null(entry)) return(NULL)
  roster <- el_roster(data)
  s <- entry$students
  ent <- s[!s$LEFT_TRUNCATED & !is.na(s$ENTRY_GRADE), c("STUDENT_ID", "ENTRY_YEAR")]
  if (!nrow(ent)) return(NULL)                            # [no observed entrants]

  source <- spec$policy$timelines$initial_elp_source %||% "entry_year"
  first_annual <- identical(source, "first_annual")

  h <- elp_headline(data, spec)
  hj <- dplyr::inner_join(ent, h[c("STUDENT_ID", "YEAR", "ELP_BAND")],
                          by = "STUDENT_ID")
  hj <- hj[if (first_annual) hj$YEAR >= hj$ENTRY_YEAR else hj$YEAR == hj$ENTRY_YEAR, ]
  hj <- hj[order(hj$STUDENT_ID, hj$YEAR), ]
  hj <- hj[!duplicated(hj$STUDENT_ID), ]
  band <- hj$ELP_BAND[match(ent$STUDENT_ID, hj$STUDENT_ID)]

  form <- if ("ELP_FORM" %in% names(roster)) {
    roster$ELP_FORM[match(paste(ent$STUDENT_ID, ent$ENTRY_YEAR),
                          paste(roster$STUDENT_ID, roster$YEAR))]
  } else rep(NA_character_, nrow(ent))

  ls <- spec$elp$level_scale
  band_labels <- spec_bands(spec)$labels %||%
    if (identical(ls$type, "categorical")) ls$levels
    else sort(unique(stats::na.omit(h$ELP_BAND)))
  # Axis tick and table cell: plain words, since the component is named in
  # the surrounding prose (CLAUDE.md 9).
  lab_none <- if (first_annual) "no annual score observed"
              else "no annual score in entry year"
  initial <- dplyr::case_when(form %in% "alternate" ~ .P2_ALT_LABEL,
                              is.na(band)           ~ lab_none,
                              TRUE                  ~ band)

  levels <- c(band_labels, .P2_ALT_LABEL, lab_none)
  n <- table(factor(initial, levels = levels))
  out <- tibble::tibble(INITIAL = levels, N = as.integer(n))
  out$PCT <- out$N / sum(out$N)
  out$IN_BANDS <- out$INITIAL %in% band_labels
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out$PCT[out$SUPPRESSED] <- NA

  notes <- character()
  if ("INITIAL_ELP" %in% names(roster)) {
    r <- roster[roster$STUDENT_ID %in% ent$STUDENT_ID & !is.na(roster$INITIAL_ELP), ]
    r <- r[order(r$STUDENT_ID, r$YEAR), ]
    delivered <- r$INITIAL_ELP[!duplicated(r$STUDENT_ID)][match(ent$STUDENT_ID,
                                                                r$STUDENT_ID[!duplicated(r$STUDENT_ID)])]
    both <- !is.na(delivered) & !is.na(band)
    if (any(both)) {
      notes <- sprintf(paste0(
        "The band recomputed here from the %s (%s) agrees with the ",
        "delivered `INITIAL_ELP` for %s of the %s entrants carrying both."),
        spec_headline_label(spec),
        if (first_annual) "first annual result at or after entry" else "entry year",
        .p2_fmt_pct(mean(delivered[both] == band[both])),
        format(sum(both), big.mark = ","))
    }
  }
  attr(out, "source") <- if (first_annual) "first_annual" else "entry_year"
  attr(out, "notes") <- notes
  out
}

# ---- profile -----------------------------------------------------------------

.P2_PROFILE_VARS <- c("HOME_LANGUAGE", "SERVICE_MODEL", "IEP",
                      "SPED_CLASSIFICATION", "GENDER", "FRL", "FRL_DETAIL",
                      "ELP_FORM", "NEWCOMER_FLAG", "SIFE_FLAG",
                      "IMMIGRANT_STATUS", "MIGRANT_STATUS")

.p2_profile_one <- function(x, spec, top, order = NULL) {
  v <- ifelse(is.na(x), "(missing)", as.character(x))
  out <- dplyr::count(tibble::tibble(VALUE = v), .data$VALUE, sort = TRUE)
  out$VALUE[seq_len(nrow(out)) > top] <- "(all other values)"
  out <- dplyr::count(out, .data$VALUE, wt = .data$n, name = "N")
  out$PCT <- out$N / sum(out$N)
  out$SUPPRESSED <- .p2_hit(out$N, spec) | out$N < spec_min_cell(spec)
  out$PCT[out$SUPPRESSED] <- NA
  out$N[out$SUPPRESSED] <- NA
  # By size, unless the values have a natural order (time in status).
  if (is.null(order)) out[order(-dplyr::coalesce(out$N, 0L)), ]
  else out[order(match(out$VALUE, order)), ]
}

#' Demographic profile of current ELs
#'
#' Within-EL shares (unweighted) in the latest year, one table per profile
#' variable, plus time in status. Long value lists collapse to the `top` most
#' common values and "(all other values)". A column the delivery does not carry
#' is simply absent from the list.
#'
#' @inheritParams compute_time_in_status
#' @param entry Output of [compute_entry()], or `NULL` to omit the time-in-status
#'   profile.
#' @param vars Roster columns to profile.
#' @param top Values kept per variable before collapsing.
#' @return A named list of tibbles (`VALUE`, `N`, `PCT`, `SUPPRESSED`), one per
#'   variable present, plus `TIME_IN_STATUS` when computable, and `HERITAGE`,
#'   which is `NULL` -- no delivered field carries it.
#' @export
compute_profile <- function(data, spec, entry = NULL, vars = .P2_PROFILE_VARS,
                            top = 12L) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  latest <- max(roster$YEAR, na.rm = TRUE)
  cur <- .p2_current(roster)
  cur <- cur[cur$YEAR == latest, , drop = FALSE]

  out <- list()
  for (v in intersect(vars, names(cur))) out[[v]] <- .p2_profile_one(cur[[v]], spec, top)

  lt <- spec$policy$timelines$lt_el_years
  if (!is.null(entry) && !is.null(lt) && nrow(cur)) {
    d <- .p2_current_duration(roster, spec, entry)
    d <- d[d$YEAR == latest, , drop = FALSE]
    lab <- c("1 year (new)", paste0("2-", lt - 1L, " years"),
             paste0(lt, "+ years (long-term)"), .P2_DUR_UNKNOWN)
    cat_ <- ifelse(d$IS_NEW, lab[1],
            ifelse(d$LT_MIN, lab[3],
            ifelse(!d$ENTRY_KNOWN, lab[4], lab[2])))
    out$TIME_IN_STATUS <- .p2_profile_one(cat_, spec, Inf, order = lab)
  }
  # [REQUIRES a heritage-language indicator -- NULL until one is delivered]
  out["HERITAGE"] <- list(NULL)
  out
}

# ---- monitoring --------------------------------------------------------------

#' Former ELs by monitoring year
#'
#' `[PROPOSED - CONFIRM]` view. Monitoring years are sub-states of `former`;
#' the sim's formers leave the panel after monitoring year 4, so this counts
#' former-within-the-monitoring-window. `NULL` when there are no former rows or
#' the delivery carries neither `EL_MONITOR_YEAR` nor `EL_STATUS_DETAIL`.
#'
#' @inheritParams compute_counts_by_year
#' @return A tibble, zero-filled: `YEAR`, `MONITOR`, `N`, `SUPPRESSED`; or
#'   `NULL`.
#' @export
compute_monitoring <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  roster <- el_roster(data)
  fmr <- roster[roster$EL_STATUS %in% "former", , drop = FALSE]
  if (!nrow(fmr) || !any(c("EL_MONITOR_YEAR", "EL_STATUS_DETAIL") %in% names(fmr))) {
    return(NULL)                                          # [no former rows / no monitoring field]
  }
  fmr$MONITOR <- if ("EL_MONITOR_YEAR" %in% names(fmr)) {
    paste("Monitoring year", dplyr::coalesce(as.character(fmr$EL_MONITOR_YEAR), "unknown"))
  } else {
    dplyr::coalesce(as.character(fmr$EL_STATUS_DETAIL), "unknown")
  }
  levels <- sort(unique(fmr$MONITOR))
  grid <- tibble::as_tibble(expand.grid(YEAR = sort(unique(roster$YEAR)),
                                        MONITOR = levels, stringsAsFactors = FALSE))
  n <- dplyr::count(fmr, .data$YEAR, .data$MONITOR, name = "N")
  out <- dplyr::left_join(grid, n, by = c("YEAR", "MONITOR"))
  out$N <- dplyr::coalesce(out$N, 0L)
  out <- out[order(out$YEAR, match(out$MONITOR, levels)), ]
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out
}

# ---- Additional check (Types 2-6): the criteria crosswalk -------------------

# The first pathway with two or more ELP terms: the criteria the crosswalk sets
# against each other. `from = "academic"` terms (Type 3) are not evaluated
# here -- they need the academic table and are a later pass.
.p2_criteria <- function(spec) {
  paths <- spec$policy$exit$pathways
  for (i in seq_along(paths)) {
    terms <- Filter(function(t) identical(t$from %||% "elp", "elp"), paths[[i]]$terms)
    if (length(terms) >= 2L) {
      return(list(pathway = paths[[i]]$name %||% paste0("pathway_", i),
                  combine = paths[[i]]$combine %||% "all",
                  terms = terms))
    }
  }
  NULL
}

#' Criteria crosswalk and contested zone
#'
#' **Type-gated** (Types 2-6; `NULL` for a Type 1 rule). Sets each exit
#' criterion of the first multi-term pathway against the others on current
#' annual student-years: which pass and fail together near the headline cut,
#' who is blocked by exactly one criterion, and the **contested zone** -- the
#' range of headline levels where the criteria disagree. It carries forward to
#' Checks 2 (the blocked groups) and 3 (the zone).
#'
#' Terms are read from `policy$exit$pathways` and only ever *evaluated* here,
#' as pass/fail per student-year; the spec validator never evaluates them. A
#' criterion on a component the student-year has no row for passes when the
#' term says `exempt_if_absent` and fails otherwise. Criteria are combined
#' conjunctively (all must pass) `[PROPOSED - CONFIRM]`. The near-cut window is
#' `spec$policy$exit$zone_half_width` (default 0.5) either side of the
#' headline term's value; a rule with no term on the headline has no cut, and
#' the crosswalk then covers every current annual student-year.
#'
#' @inheritParams compute_counts_by_year
#' @return `NULL` unless a Type 2-6 rule has a pathway with two or more ELP
#'   terms; otherwise a list: `crosswalk` (`criteria`, `pathway`, `combine`,
#'   `window`, `n_window`, `n_total`, `pattern`, `blocked`, `by_level`),
#'   `contested_zone` (`headline`, `cut`, `n_disagree`, `n_total`, `range`,
#'   `core_90`) and `blocked_groups` (student-years blocked by exactly one
#'   criterion: `STUDENT_ID`, `YEAR`, `HEADLINE_LEVEL`, `BLOCKED_BY`).
#' @export
compute_criteria_crosswalk <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  if (!spec_has_type(spec, 2:6)) return(NULL)             # [Type-gated: Types 2-6 only]
  crit <- .p2_criteria(spec)
  if (is.null(crit)) return(NULL)                         # [no pathway with two ELP terms]

  roster <- el_roster(data)
  cur_keys <- roster[roster$EL_STATUS %in% "current", c("STUDENT_ID", "YEAR")]
  ann <- el_base(data)
  ann <- ann[ann$ELP_FORM %in% "annual", , drop = FALSE]
  ann <- dplyr::semi_join(ann, cur_keys, by = c("STUDENT_ID", "YEAR"))
  spine <- dplyr::distinct(ann[c("STUDENT_ID", "YEAR")])
  if (!nrow(spine)) return(NULL)                          # [no annual ELP results]
  keys <- paste(spine$STUDENT_ID, spine$YEAR)

  headline <- spec_headline(spec)
  terms <- crit$terms
  names_ <- make.unique(toupper(vapply(terms, function(k) {
    paste(k$component, k$measure, sep = "_")
  }, "")), sep = "_")

  passm <- vapply(terms, function(k) {
    val_col <- if (identical(k$measure, "level")) "ACHIEVEMENT_LEVEL" else "SCALE_SCORE"
    sub <- ann[ann$COMPONENT == k$component, c("STUDENT_ID", "YEAR", val_col)]
    x <- sub[[val_col]][match(keys, paste(sub$STUDENT_ID, sub$YEAR))]
    pass <- do.call(k$op, list(x, k$value))
    pass[is.na(x)] <- isTRUE(k$exempt_if_absent)
    pass
  }, logical(nrow(spine)))
  if (!is.matrix(passm)) passm <- matrix(passm, nrow = nrow(spine))
  colnames(passm) <- names_

  h <- elp_headline(data, spec)
  level <- h$ACHIEVEMENT_LEVEL[match(keys, paste(h$STUDENT_ID, h$YEAR))]

  is_head <- vapply(terms, function(k) {
    identical(k$component, headline) && identical(k$measure, "level")
  }, TRUE)
  cut <- if (any(is_head)) as.numeric(terms[[which(is_head)[1]]]$value) else NA_real_
  half_w <- as.numeric(spec$policy$exit$zone_half_width %||% 0.5)

  fail <- !passm
  n_crit <- ncol(passm)
  n_fail <- rowSums(fail)
  failed <- apply(fail, 1, function(r) paste(names_[r], collapse = " + "))
  disagree <- n_fail > 0 & n_fail < n_crit
  blocked_group <- dplyr::case_when(
    n_fail == 0 ~ "passes every criterion",
    n_fail == 1 ~ paste0("blocked by ", failed, " only"),
    TRUE        ~ "blocked by more than one criterion")

  near <- if (is.na(cut)) rep(TRUE, nrow(spine))
          else !is.na(level) & abs(level - cut) <= half_w

  with_pct <- function(x) {
    x$PCT <- x$N / sum(x$N)
    x$SUPPRESSED <- .p2_hit(x$N, spec)
    x$PCT[x$SUPPRESSED] <- NA
    x$N[x$SUPPRESSED] <- NA
    x
  }
  pat <- as.data.frame(ifelse(passm[near, , drop = FALSE], "pass", "fail"),
                       stringsAsFactors = FALSE)
  pattern <- with_pct(dplyr::count(pat, !!!rlang::syms(names(pat)), name = "N"))
  blocked <- with_pct(dplyr::count(tibble::tibble(BLOCKED_GROUP = blocked_group[near]),
                                   .data$BLOCKED_GROUP, name = "N"))

  wide <- if (is.na(cut)) !is.na(level) else !is.na(level) & abs(level - cut) <= 2 * half_w
  by_level <- tibble::tibble(LEVEL_BIN = round(level[wide], 1),
                             DISAGREE = disagree[wide]) |>
    dplyr::group_by(.data$LEVEL_BIN) |>
    dplyr::summarise(N = dplyr::n(), PCT_DISAGREE = mean(.data$DISAGREE),
                     .groups = "drop")
  by_level$SUPPRESSED <- .p2_hit(by_level$N, spec) | by_level$N < spec_min_cell(spec)
  by_level$PCT_DISAGREE[by_level$SUPPRESSED] <- NA
  by_level$N[by_level$SUPPRESSED] <- NA

  dis_level <- level[disagree & !is.na(level)]
  contested <- list(
    headline   = headline,
    cut        = cut,
    n_disagree = sum(disagree),
    n_total    = nrow(spine),
    range      = if (length(dis_level)) range(dis_level) else c(NA_real_, NA_real_),
    core_90    = if (length(dis_level)) unname(stats::quantile(dis_level, c(.05, .95)))
                 else c(NA_real_, NA_real_))

  one <- near & n_fail == 1
  blocked_groups <- tibble::tibble(STUDENT_ID = spine$STUDENT_ID[one],
                                   YEAR = spine$YEAR[one],
                                   HEADLINE_LEVEL = level[one],
                                   BLOCKED_BY = failed[one])

  criteria <- tibble::tibble(
    CRITERION = names_,
    COMPONENT = vapply(terms, function(k) k$component, ""),
    MEASURE   = vapply(terms, function(k) k$measure, ""),
    OP        = vapply(terms, function(k) k$op, ""),
    VALUE     = vapply(terms, function(k) as.numeric(k$value), 0),
    EXEMPT_IF_ABSENT = vapply(terms, function(k) isTRUE(k$exempt_if_absent), TRUE))

  list(
    crosswalk = list(criteria = criteria, pathway = crit$pathway,
                     combine = crit$combine,
                     window = c(cut = cut, half_width = half_w),
                     n_window = sum(near), n_total = nrow(spine),
                     pattern = pattern, blocked = blocked, by_level = by_level),
    contested_zone = contested,
    blocked_groups = blocked_groups)
}
