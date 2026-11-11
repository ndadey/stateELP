# Prep 2 -- context views for a briefing deck (decks/context.qmd): the
# population picture of prep2_compute.R, cut for slides. Exit rates by grade
# and by years in status, districts by EL share and by EL enrollment, and a
# profile of each selected district.
#
# Added 2026-11-11 at the user's request (a context slide deck that runs on
# stateELP); none of it is in the instruction file [PROPOSED - CONFIRM]. run_prep2() is untouched: the deck calls
# run_prep2_context(), which reuses compute_by_grade(), compute_el_share() and
# compute_by_district() unchanged.
#
# Conventions, all from prep2_compute.R and CLAUDE.md 4: the roster is the
# counting frame; statewide and district shares of enrollment weight by
# SAMPLE_WEIGHT; a cell with 0 < n < min_cell is NA (primary suppression only,
# Prep 2's standard) and a true zero stays 0; every column is ALL CAPS.
#
# Exit rates are cross-sectional and say nothing about time in status. A
# current EL in year t is "exited" when recorded as a former EL in year t + 1
# -- the package's first-former-year dating -- and a student with no record
# in t + 1 is censored and leaves the denominator (censored is not a
# non-exit). Check 1 measures how long students take; these do not.

# ---- helpers -----------------------------------------------------------------

# End-year integer to the school-year label: 2025 -> "2024-25" (en dash).
.p2c_school_year <- function(y) {
  ifelse(is.na(y), NA_character_,
         paste0(y - 1L, .P2_ENDASH, sprintf("%02d", as.integer(y) %% 100L)))
}

# Current-EL student-years at risk of exit: the next year is in the panel, the
# student has not already exited (exit is absorbing), and -- for the tested
# population -- the student has an annual-form score on the state's reported
# component that year. NULL when nothing is at risk.
.p2c_at_risk <- function(data, spec, population) {
  roster <- el_roster(data)
  .roster_cols(roster, c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS"))
  years <- sort(unique(roster$YEAR))
  cur <- roster[roster$EL_STATUS %in% "current", , drop = FALSE]
  marks <- .status_marks(roster)
  ffy <- marks$FIRST_FORMER_YEAR[match(cur$STUDENT_ID, marks$STUDENT_ID)]
  cur <- cur[(is.na(ffy) | cur$YEAR < ffy) & (cur$YEAR + 1L) %in% years, ,
             drop = FALSE]
  if (identical(population, "tested") && nrow(cur)) {
    h <- elp_headline(data, spec)
    cur <- cur[paste(cur$STUDENT_ID, cur$YEAR) %in% paste(h$STUDENT_ID, h$YEAR), ,
               drop = FALSE]
  }
  if (!nrow(cur)) return(NULL)
  nxt <- roster$EL_STATUS[match(paste(cur$STUDENT_ID, cur$YEAR + 1L),
                                paste(roster$STUDENT_ID, roster$YEAR))]
  tibble::tibble(
    STUDENT_ID   = cur$STUDENT_ID,
    YEAR         = as.integer(cur$YEAR),
    GRADE        = .p2_norm_grade(cur$GRADE, spec),
    DISTRICT_ID  = if ("DISTRICT_ID" %in% names(cur)) as.character(cur$DISTRICT_ID)
                   else NA_character_,
    EXITED       = nxt %in% "former",
    NOT_OBSERVED = !nxt %in% c("current", "former"))
}

# One row per district in `year`: weighted enrollment, current and former ELs.
# Every district with any roster row that year is here, zero-EL districts
# included -- unlike .p2_district_counts(), which sees only districts with ELs.
.p2c_district_frame <- function(roster, year) {
  l <- roster[roster$YEAR == year & !is.na(roster$DISTRICT_ID), , drop = FALSE]
  tibble::tibble(DISTRICT_ID = as.character(l$DISTRICT_ID), W = .p2_weights(l),
                 CUR = l$EL_STATUS %in% "current",
                 FMR = l$EL_STATUS %in% "former") |>
    dplyr::group_by(.data$DISTRICT_ID) |>
    dplyr::summarise(N_ENROLLMENT_WEIGHTED = sum(.data$W),
                     N_EL = sum(.data$CUR),
                     N_FORMER = sum(.data$FMR),
                     .groups = "drop")
}

# "Less than 5%", "5-19%", "20-39%", "40% or more" from c(0, .05, .2, .4).
.p2c_share_labels <- function(breaks) {
  p <- round(100 * breaks)
  k <- length(p)
  mid <- if (k > 2L) paste0(p[2:(k - 1L)], .P2_ENDASH, p[3:k] - 1L, "%") else character()
  c(paste0("Less than ", p[2], "%"), mid, paste0(p[k], "% or more"))
}

# "None", "1-9", "10-99", ..., "10,000+" from c(0, 1, 10, ..., 10000, Inf).
.p2c_count_labels <- function(breaks) {
  f <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  lo <- utils::head(breaks, -1L)
  hi <- utils::tail(breaks, -1L)
  ifelse(lo == 0 & hi == 1, "None",
         ifelse(is.infinite(hi), paste0(f(lo), "+"),
                ifelse(hi - lo == 1, f(lo), paste0(f(lo), .P2_ENDASH, f(hi - 1)))))
}

# ---- exit rates --------------------------------------------------------------

#' Annual exit rates, by grade, years in status, district, or statewide
#'
#' The share of current English learners in a year who are recorded as former
#' ELs the next year -- the package's exit dating, where an exit belongs to the
#' first former year -- for every year whose next year is in the panel, and
#' pooled over the last `pooled_years` of them (total exits over total
#' followed, not an average of rates). It is the life table's annual rate
#' taken on a cross-section instead of a cohort, with one difference: a
#' student with no record the next year is censored and leaves the
#' denominator, because an unobserved outcome is not a non-exit. A group
#' nobody can be followed out of -- grade 12, which graduates -- therefore has
#' no rate (`PCT_EXITED` `NA` with `SUPPRESSED` `FALSE`).
#'
#' **A snapshot, not a duration.** A rate for one grade or one year says who
#' exits now, not how long anyone took; Check 1's life table answers that, and
#' a single-year rate must never be read as one (CLAUDE.md 9 of the parent
#' framework).
#'
#' `population = "tested"` (default) counts only current ELs with an
#' annual-form score on the state's reported component that year -- the
#' students who could meet the exit rule -- and `"all"` every current EL.
#' Exit is absorbing: a current row after a student's first former year is not
#' at risk. A student absent the next year is counted in `N_AT_RISK` and
#' `N_NOT_OBSERVED` (the life table's column C), and `PCT_EXITED` is
#' `N_EXITED / (N_AT_RISK - N_NOT_OBSERVED)`.
#'
#' Years in status come from Prep 2's own entry dating ([compute_entry()]):
#' the student's first year in status is year 1, and the rate in year *k* is
#' the share exiting at the end of year *k*. Student-years whose entry cannot
#' be dated are left out and counted in `attr(x, "n_unknown_entry")`; years at
#' or beyond `top` are pooled into one `"<top>+"` group.
#'
#' @inheritParams compute_counts_by_year
#' @param by `"grade"` (K-12), `"years_in_status"`, `"district"` or
#'   `"state"`.
#' @param pooled_years How many of the most recent at-risk years the pooled
#'   rate covers.
#' @param population `"tested"` or `"all"`; see Details.
#' @param top For `by = "years_in_status"`, the year from which on years are
#'   pooled. Defaults to the spec's maximum timeline plus two.
#' @return A tibble, one row per year and group plus one pooled row per group:
#'   `FIRST_YEAR`, `LAST_YEAR` (equal on a single-year row; the at-risk year,
#'   whose exits are observed the year after), `POOLED`, the group column
#'   (`GRADE`, `YEARS_IN_STATUS`, `DISTRICT_ID` or `STATE`), `N_AT_RISK`,
#'   `N_EXITED`, `N_NOT_OBSERVED`, `PCT_EXITED`, `SUPPRESSED`. Attributes:
#'   `by`, `population`, `latest` (the last at-risk year), `pooled` (its first
#'   and last year), `n_unknown_entry`, `top`. `NULL` when no current EL has a
#'   next year in the data, or `by = "district"` without `DISTRICT_ID`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' x <- compute_exit_rates(bundle, spec, by = "grade")
#' x[x$POOLED, c("GRADE", "N_AT_RISK", "PCT_EXITED")]
compute_exit_rates <- function(data, spec,
                               by = c("grade", "years_in_status", "district", "state"),
                               pooled_years = 5L,
                               population = c("tested", "all"),
                               top = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.numeric(pooled_years), length(pooled_years) == 1L, pooled_years >= 1,
            is.null(top) || (is.numeric(top) && length(top) == 1L && top >= 2))
  by <- match.arg(by)
  population <- match.arg(population)
  roster <- el_roster(data)
  if (by == "district" && !"DISTRICT_ID" %in% names(roster)) return(NULL)  # [no DISTRICT_ID]

  ar <- .p2c_at_risk(data, spec, population)
  if (is.null(ar)) return(NULL)               # [no current EL with a next year observed]

  n_unknown <- NA_integer_
  if (by == "grade") {
    ar <- ar[!is.na(ar$GRADE), , drop = FALSE]
    ar$GROUP <- ar$GRADE
    lv <- .P2_GRADES
  } else if (by == "years_in_status") {
    entry <- compute_entry(data, spec)
    if (is.null(entry)) return(NULL)          # [no entry to date]
    s <- entry$students
    i <- match(ar$STUDENT_ID, s$STUDENT_ID)
    yrs <- ar$YEAR - s$ENTRY_YEAR[i] + 1L
    known <- !is.na(i) & s$ENTRY_KNOWN[i] %in% TRUE & !is.na(yrs) & yrs >= 1L
    n_unknown <- sum(!known)
    ar <- ar[known, , drop = FALSE]
    yrs <- yrs[known]
    if (is.null(top)) top <- spec_max_years(spec) + 2L
    top <- as.integer(top)
    ar$GROUP <- ifelse(yrs >= top, paste0(top, "+"), as.character(yrs))
    lv <- c(as.character(seq_len(top - 1L)), paste0(top, "+"))
  } else if (by == "district") {
    ar <- ar[!is.na(ar$DISTRICT_ID), , drop = FALSE]
    ar$GROUP <- ar$DISTRICT_ID
    lv <- sort(unique(stats::na.omit(as.character(roster$DISTRICT_ID))))
  } else {
    ar$GROUP <- "All"
    lv <- "All"
  }
  if (!nrow(ar)) return(NULL)

  at_risk_years <- sort(unique(ar$YEAR))
  pool <- utils::tail(at_risk_years, as.integer(pooled_years))

  agg <- function(d, first, last, pooled) {
    g <- factor(d$GROUP, levels = lv)
    n_ex <- tapply(d$EXITED, g, sum)
    n_no <- tapply(d$NOT_OBSERVED, g, sum)
    tibble::tibble(
      FIRST_YEAR = as.integer(first), LAST_YEAR = as.integer(last), POOLED = pooled,
      GROUP = lv,
      N_AT_RISK = as.integer(table(g)),
      N_EXITED = as.integer(ifelse(is.na(n_ex), 0L, n_ex)),
      N_NOT_OBSERVED = as.integer(ifelse(is.na(n_no), 0L, n_no)))
  }
  out <- dplyr::bind_rows(c(
    lapply(at_risk_years, function(y) agg(ar[ar$YEAR == y, ], y, y, FALSE)),
    list(agg(ar[ar$YEAR %in% pool, ], min(pool), max(pool), TRUE))))

  # The denominator is the students whose next year was observed: one with no
  # record the next year is censored, and censored is not a non-exit. A group
  # nobody can be followed out of (grade 12, which graduates) has no rate.
  n_obs <- out$N_AT_RISK - out$N_NOT_OBSERVED
  out$PCT_EXITED <- ifelse(n_obs > 0, out$N_EXITED / n_obs, NA_real_)
  hit_r <- .p2_hit(out$N_AT_RISK, spec)
  hit_e <- .p2_hit(out$N_EXITED, spec)
  hit_n <- .p2_hit(out$N_NOT_OBSERVED, spec)
  hit_o <- .p2_hit(n_obs, spec)
  out$SUPPRESSED <- hit_r | hit_e | hit_n | hit_o
  out$PCT_EXITED[hit_r | hit_e | hit_o] <- NA
  out$N_AT_RISK[hit_r] <- NA
  out$N_EXITED[hit_e] <- NA
  out$N_NOT_OBSERVED[hit_n] <- NA

  grp <- c(grade = "GRADE", years_in_status = "YEARS_IN_STATUS",
           district = "DISTRICT_ID", state = "STATE")[[by]]
  names(out)[names(out) == "GROUP"] <- grp

  attr(out, "by") <- by
  attr(out, "population") <- population
  attr(out, "latest") <- max(at_risk_years)
  attr(out, "pooled") <- range(pool)
  attr(out, "n_unknown_entry") <- n_unknown
  attr(out, "top") <- if (by == "years_in_status") top else NULL
  out
}

# ---- districts by EL share -----------------------------------------------------

#' Districts by EL share of enrollment
#'
#' How many districts fall in each band of EL share -- current ELs over
#' **weighted** total enrollment, never-EL rows carrying their `SAMPLE_WEIGHT`
#' -- in the latest year. Bands are left-closed on `breaks`, so a district at
#' exactly 5% is in "5-19%". Districts are counted, not students, so no cell
#' is suppressed.
#'
#' @inheritParams compute_counts_by_year
#' @param breaks Lower edges of the share bands, as proportions, starting at 0.
#' @return A tibble: `SHARE_BAND`, `LOWER`, `N_DISTRICTS`, `PCT_DISTRICTS`;
#'   attributes `year`, `n_districts`, and `pct_state` (the statewide EL share
#'   that year, as [compute_el_share()] computes it). `NULL` without
#'   `DISTRICT_ID`, or without never-EL rows (enrollment is then unknown).
#' @export
compute_district_share <- function(data, spec, breaks = c(0, 0.05, 0.20, 0.40)) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.numeric(breaks), length(breaks) >= 2L, breaks[[1]] == 0,
            !is.unsorted(breaks, strictly = TRUE), all(breaks < 1))
  roster <- el_roster(data)
  if (!"DISTRICT_ID" %in% names(roster)) return(NULL)        # [no DISTRICT_ID]
  if (!any(roster$EL_STATUS %in% "never")) return(NULL)     # [no never-EL rows]
  year <- max(roster$YEAR, na.rm = TRUE)
  d <- .p2c_district_frame(roster, year)
  if (!nrow(d)) return(NULL)

  labels <- .p2c_share_labels(breaks)
  band <- cut(d$N_EL / d$N_ENROLLMENT_WEIGHTED, c(breaks, Inf), right = FALSE,
              labels = labels)
  out <- tibble::tibble(SHARE_BAND = labels, LOWER = breaks,
                        N_DISTRICTS = as.integer(table(band)))
  out$PCT_DISTRICTS <- out$N_DISTRICTS / sum(out$N_DISTRICTS)

  share <- compute_el_share(data, spec)
  attr(out, "year") <- year
  attr(out, "n_districts") <- nrow(d)
  attr(out, "pct_state") <- share$PCT_EL[share$YEAR == year][1]
  out
}

# ---- districts by EL enrollment ------------------------------------------------

#' Districts and EL students by district EL enrollment
#'
#' The share of districts against the share of current ELs in each band of
#' district EL enrollment, in the latest year -- the "few districts enroll
#' most ELs" view. Every district with a roster row that year counts,
#' zero-EL districts included, so the first band may be "None".
#'
#' `breaks` are lower edges starting at 0 (an upper `Inf` is added); `NULL`
#' takes the spec's `policy$district_size_bands` with its labels. With
#' `split_at_mean = TRUE`, the band holding the average district's EL count is
#' split there, so the table separates below-average from above-average
#' districts -- the "resist the urge to average" view (slide 38 of the source
#' deck).
#'
#' Counts with `0 < N < min_cell` are `NA`, and so is their share.
#'
#' @inheritParams compute_counts_by_year
#' @param breaks Lower edges of the EL-count bands, or `NULL` for the spec's.
#' @param split_at_mean Whether to split the bands at the average district.
#' @param share The cumulative EL share the `top` summary reports on.
#' @return A tibble: `SIZE_BAND`, `LOWER`, `UPPER` (exclusive), `N_DISTRICTS`,
#'   `PCT_DISTRICTS`, `N`, `PCT`, `ABOVE_MEAN`, `SUPPRESSED`. Attributes:
#'   `year`; `n_districts`; `n_el` (current ELs in a district); `mean_n` (the
#'   average district's EL count) and `mean_break` (where the split falls, `NA`
#'   unsplit); `split` (`PCT_DISTRICTS` and `PCT` below and above that break);
#'   `small` (the share of districts under the spec's first size break and
#'   their share of ELs); `top` (how many of the largest districts first reach
#'   `share` of ELs, and the smallest of them). `NULL` without `DISTRICT_ID`,
#'   or with neither `breaks` nor declared size bands.
#' @export
compute_district_concentration <- function(data, spec, breaks = NULL,
                                           split_at_mean = FALSE, share = 2 / 3) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.logical(split_at_mean), length(split_at_mean) == 1L,
            is.numeric(share), length(share) == 1L, share > 0, share <= 1)
  roster <- el_roster(data)
  if (!"DISTRICT_ID" %in% names(roster)) return(NULL)        # [no DISTRICT_ID]
  year <- max(roster$YEAR, na.rm = TRUE)
  d <- .p2c_district_frame(roster, year)
  if (!nrow(d)) return(NULL)

  labels <- NULL
  if (is.null(breaks)) {
    b <- .p2_size_bands(spec)
    if (is.null(b)) return(NULL)                             # [no size bands]
    breaks <- b$breaks
    labels <- b$labels
  }
  stopifnot(is.numeric(breaks), breaks[[1]] == 0,
            !is.unsorted(breaks, strictly = TRUE))
  if (is.finite(breaks[[length(breaks)]])) breaks <- c(breaks, Inf)

  mean_n <- mean(d$N_EL)
  mean_break <- NA_real_
  if (split_at_mean) {
    mean_break <- ceiling(mean_n)
    if (!mean_break %in% breaks) {
      breaks <- sort(c(breaks, mean_break))
      labels <- NULL
    }
  }
  if (is.null(labels)) labels <- .p2c_count_labels(breaks)

  band <- cut(d$N_EL, breaks, right = FALSE, labels = labels)
  n_el <- tapply(d$N_EL, band, sum)
  out <- tibble::tibble(
    SIZE_BAND = labels,
    LOWER = utils::head(breaks, -1L),
    UPPER = utils::tail(breaks, -1L),
    N_DISTRICTS = as.integer(table(band)),
    N = as.integer(ifelse(is.na(n_el), 0L, n_el)))
  total <- sum(d$N_EL)
  out$PCT_DISTRICTS <- out$N_DISTRICTS / nrow(d)
  out$PCT <- if (total > 0) out$N / total else NA_real_
  out$ABOVE_MEAN <- if (is.na(mean_break)) NA else out$LOWER >= mean_break
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out$PCT[out$SUPPRESSED] <- NA
  out <- out[c("SIZE_BAND", "LOWER", "UPPER", "N_DISTRICTS", "PCT_DISTRICTS",
               "N", "PCT", "ABOVE_MEAN", "SUPPRESSED")]

  # Group summaries: shares of districts are never suppressed (districts, not
  # students); a group's EL share is, when its EL count is small.
  grp <- function(in_grp) {
    n <- sum(d$N_EL[in_grp])
    list(PCT_DISTRICTS = mean(in_grp),
         PCT = if (.p2_hit(n, spec) || total == 0) NA_real_ else n / total)
  }
  small_n <- .p2_small_n(spec)
  ranked <- sort(d$N_EL, decreasing = TRUE)
  k <- if (total > 0) which(cumsum(ranked) / total >= share)[1] else NA_integer_

  attr(out, "year") <- year
  attr(out, "n_districts") <- nrow(d)
  attr(out, "n_el") <- total
  attr(out, "mean_n") <- mean_n
  attr(out, "mean_break") <- mean_break
  attr(out, "split") <- if (is.na(mean_break)) NULL else
    list(below = grp(d$N_EL < mean_break), above = grp(d$N_EL >= mean_break))
  attr(out, "small") <- if (is.na(small_n)) NULL else
    c(list(N = small_n), grp(d$N_EL < small_n))
  attr(out, "top") <- list(SHARE = share, N_DISTRICTS = k,
                           MIN_N = if (is.na(k)) NA_integer_ else as.integer(ranked[[k]]))
  out
}

# ---- district profiles ---------------------------------------------------------

# The default "settings" when no districts are named: the largest EL
# enrollment, the highest EL share, the median EL enrollment and the smallest
# EL enrollment, among districts with at least min_cell current ELs, each
# district used once. [PROPOSED - CONFIRM]
.p2c_pick_districts <- function(d, spec, n) {
  d <- d[d$N_EL >= spec_min_cell(spec), , drop = FALSE]
  if (!nrow(d)) return(character())
  d <- d[order(-d$N_EL, d$DISTRICT_ID), , drop = FALSE]
  picks <- character()
  take <- function(i, label) {
    left <- d[!d$DISTRICT_ID %in% picks, , drop = FALSE]
    if (!nrow(left)) return(invisible())
    picks[[label]] <<- left$DISTRICT_ID[[i(left)]]
  }
  take(function(x) 1L, "Largest EL enrollment")
  if (!all(is.na(d$PCT_EL))) {
    take(function(x) which.max(ifelse(is.na(x$PCT_EL), -Inf, x$PCT_EL)), "Highest EL share")
  }
  take(function(x) ceiling(nrow(x) / 2), "Median EL enrollment")
  take(function(x) nrow(x), "Smallest EL enrollment")
  utils::head(picks, n)
}

# Counts of one roster field among a district's current ELs.
.p2c_field_shares <- function(cur, field, spec) {
  if (!field %in% names(cur)) return(NULL)
  v <- as.character(cur[[field]])
  out <- tibble::tibble(DISTRICT_ID = as.character(cur$DISTRICT_ID), VALUE = v) |>
    dplyr::filter(!is.na(.data$VALUE)) |>
    dplyr::count(.data$DISTRICT_ID, .data$VALUE, name = "N") |>
    dplyr::group_by(.data$DISTRICT_ID) |>
    dplyr::mutate(PCT = .data$N / sum(.data$N)) |>
    dplyr::arrange(.data$DISTRICT_ID, dplyr::desc(.data$N), .data$VALUE) |>
    dplyr::mutate(RANK = dplyr::row_number()) |>
    dplyr::ungroup()
  out$SUPPRESSED <- .p2_hit(out$N, spec)
  out$N[out$SUPPRESSED] <- NA
  out$PCT[out$SUPPRESSED] <- NA
  names(out)[names(out) == "VALUE"] <- field
  out
}

#' Profiles of selected districts
#'
#' One profile per district, for the "look at each setting, not the average"
#' slides: EL share of enrollment and EL count in the first and latest panel
#' years, the district's share of the state's ELs, its annual exit rate (as
#' [compute_exit_rates()]) against the state's, former ELs, the spread of EL
#' share across its schools, and its ELs' home languages and service models.
#'
#' `districts` names the districts to profile -- a character vector of
#' `DISTRICT_ID`s, optionally named with the setting each one stands for
#' ("large suburban"). `NULL` picks up to `n` from the data: the largest EL
#' enrollment, the highest EL share, the median and the smallest EL
#' enrollment, among districts with at least `min_cell` ELs
#' `[PROPOSED - CONFIRM]`.
#'
#' Shares of enrollment weight by `SAMPLE_WEIGHT`; a school's share is shown
#' only when its EL count is reportable. Counts with `0 < N < min_cell` are
#' `NA`, and so is every share built on them.
#'
#' @inheritParams compute_exit_rates
#' @param districts District ids to profile, optionally named by setting, or
#'   `NULL` to pick them.
#' @param n How many districts to pick when `districts` is `NULL`.
#' @return A list, or `NULL` without `DISTRICT_ID`: `summary` (one row per
#'   district: `DISTRICT_ID`, `SETTING`, `FIRST_YEAR`, `YEAR`, `N_EL`,
#'   `N_EL_FIRST`, `PCT_EL`, `PCT_EL_FIRST`, `PCT_OF_STATE_EL`,
#'   `N_ENROLLMENT_WEIGHTED`, `ENROLLMENT_CHANGE`, `EL_CHANGE`, `N_FORMER`,
#'   `PCT_FORMER`, `N_FORMER_FIRST`, `PCT_FORMER_FIRST`, `EXIT_YEAR`,
#'   `PCT_EXITED`, `EXIT_FIRST_YEAR`, `PCT_EXITED_FIRST`, `STATE_PCT_EXITED`,
#'   `N_SCHOOLS`, `N_SCHOOLS_SHOWN`, `SCHOOL_PCT_MIN`, `SCHOOL_PCT_MID`,
#'   `SCHOOL_PCT_MAX`, `N_LANGUAGES`); `languages` and `service_models`
#'   (`DISTRICT_ID`, the field, `N`, `PCT`, `RANK`, `SUPPRESSED`; `NULL` when
#'   the roster lacks the field); `schools` (`DISTRICT_ID`, `SCHOOL_ID`, `N_EL`,
#'   `PCT_EL`, `SUPPRESSED`; `NULL` without `SCHOOL_ID`).
#' @export
compute_district_profiles <- function(data, spec, districts = NULL, n = 4L,
                                      population = c("tested", "all")) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.null(districts) || is.character(districts),
            is.numeric(n), length(n) == 1L, n >= 1)
  population <- match.arg(population)
  roster <- el_roster(data)
  if (!"DISTRICT_ID" %in% names(roster)) return(NULL)        # [no DISTRICT_ID]
  years <- range(roster$YEAR, na.rm = TRUE)
  has_never <- any(roster$EL_STATUS %in% "never")

  d_l <- .p2c_district_frame(roster, years[[2]])
  d_f <- .p2c_district_frame(roster, years[[1]])
  d_l$PCT_EL <- if (has_never) d_l$N_EL / d_l$N_ENROLLMENT_WEIGHTED else NA_real_

  if (is.null(districts)) {
    districts <- .p2c_pick_districts(d_l, spec, n)
  } else {
    bad <- setdiff(districts, d_l$DISTRICT_ID)
    if (length(bad)) {
      stop("Not in the latest year's roster: ", paste(bad, collapse = ", "),
           ". Districts there: ", paste(utils::head(d_l$DISTRICT_ID, 20), collapse = ", "),
           if (nrow(d_l) > 20) ", ..." else "", call. = FALSE)
    }
  }
  if (!length(districts)) return(NULL)                       # [no reportable district]
  ids <- unname(districts)
  setting <- if (is.null(names(districts))) rep(NA_character_, length(ids))
             else ifelse(nzchar(names(districts)), names(districts), NA_character_)

  er <- compute_exit_rates(data, spec, by = "district", pooled_years = 1L,
                           population = population)
  st <- compute_exit_rates(data, spec, by = "state", pooled_years = 1L,
                           population = population)
  ex_at <- function(id, y) {
    if (is.null(er) || is.na(y)) return(NA_real_)
    er$PCT_EXITED[!er$POOLED & er$DISTRICT_ID == id & er$FIRST_YEAR == y][1]
  }
  ex_last <- if (is.null(er)) NA_integer_ else attr(er, "latest")
  ex_first <- if (is.null(er)) NA_integer_ else min(er$FIRST_YEAR)

  lf <- d_f[match(ids, d_f$DISTRICT_ID), ]
  ll <- d_l[match(ids, d_l$DISTRICT_ID), ]
  state_el <- sum(d_l$N_EL)
  pct <- function(num, den) if (has_never) num / den else rep(NA_real_, length(num))
  s <- tibble::tibble(
    DISTRICT_ID = ids, SETTING = setting,
    FIRST_YEAR = years[[1]], YEAR = years[[2]],
    N_EL = ll$N_EL, N_EL_FIRST = lf$N_EL,
    PCT_EL = pct(ll$N_EL, ll$N_ENROLLMENT_WEIGHTED),
    PCT_EL_FIRST = pct(lf$N_EL, lf$N_ENROLLMENT_WEIGHTED),
    PCT_OF_STATE_EL = ll$N_EL / state_el,
    N_ENROLLMENT_WEIGHTED = ll$N_ENROLLMENT_WEIGHTED,
    ENROLLMENT_CHANGE = if (has_never)
      ll$N_ENROLLMENT_WEIGHTED / lf$N_ENROLLMENT_WEIGHTED - 1 else NA_real_,
    EL_CHANGE = ifelse(lf$N_EL > 0, ll$N_EL / lf$N_EL - 1, NA_real_),
    N_FORMER = ll$N_FORMER,
    PCT_FORMER = pct(ll$N_FORMER, ll$N_ENROLLMENT_WEIGHTED),
    N_FORMER_FIRST = lf$N_FORMER,
    PCT_FORMER_FIRST = pct(lf$N_FORMER, lf$N_ENROLLMENT_WEIGHTED),
    EXIT_YEAR = ex_last,
    PCT_EXITED = vapply(ids, ex_at, numeric(1), y = ex_last, USE.NAMES = FALSE),
    EXIT_FIRST_YEAR = ex_first,
    PCT_EXITED_FIRST = vapply(ids, ex_at, numeric(1), y = ex_first, USE.NAMES = FALSE),
    STATE_PCT_EXITED = if (is.null(st)) NA_real_
      else st$PCT_EXITED[!st$POOLED & st$FIRST_YEAR == ex_last][1])

  # Suppression: each count, and every share built on it.
  for (pair in list(c("N_EL", "PCT_EL", "PCT_OF_STATE_EL", "EL_CHANGE"),
                    c("N_EL_FIRST", "PCT_EL_FIRST", "EL_CHANGE"),
                    c("N_FORMER", "PCT_FORMER"),
                    c("N_FORMER_FIRST", "PCT_FORMER_FIRST"))) {
    hit <- .p2_hit(s[[pair[[1]]]], spec)
    for (cl in pair) s[[cl]][hit] <- NA
  }
  s$N_ENROLLMENT_WEIGHTED <- round(s$N_ENROLLMENT_WEIGHTED)

  latest <- roster[roster$YEAR == years[[2]] & roster$DISTRICT_ID %in% ids, , drop = FALSE]
  cur <- latest[latest$EL_STATUS %in% "current", , drop = FALSE]
  langs <- .p2c_field_shares(cur, "HOME_LANGUAGE", spec)
  models <- .p2c_field_shares(cur, "SERVICE_MODEL", spec)
  s$N_LANGUAGES <- if (is.null(langs)) NA_integer_ else
    as.integer(table(factor(langs$DISTRICT_ID, levels = ids)))

  schools <- NULL
  s$N_SCHOOLS <- NA_integer_
  s$N_SCHOOLS_SHOWN <- NA_integer_
  s$SCHOOL_PCT_MIN <- s$SCHOOL_PCT_MID <- s$SCHOOL_PCT_MAX <- NA_real_
  if ("SCHOOL_ID" %in% names(latest)) {
    schools <- tibble::tibble(DISTRICT_ID = as.character(latest$DISTRICT_ID),
                              SCHOOL_ID = as.character(latest$SCHOOL_ID),
                              W = .p2_weights(latest),
                              CUR = latest$EL_STATUS %in% "current") |>
      dplyr::filter(!is.na(.data$SCHOOL_ID)) |>
      dplyr::group_by(.data$DISTRICT_ID, .data$SCHOOL_ID) |>
      dplyr::summarise(N_EL = sum(.data$CUR), ENR = sum(.data$W), .groups = "drop")
    schools$PCT_EL <- if (has_never) schools$N_EL / schools$ENR else NA_real_
    schools$SUPPRESSED <- .p2_hit(schools$N_EL, spec)
    schools$N_EL[schools$SUPPRESSED] <- NA
    schools$PCT_EL[schools$SUPPRESSED] <- NA
    schools <- schools[c("DISTRICT_ID", "SCHOOL_ID", "N_EL", "PCT_EL", "SUPPRESSED")]
    for (j in seq_along(ids)) {
      sc <- schools[schools$DISTRICT_ID == ids[[j]], , drop = FALSE]
      shown <- sc$PCT_EL[!is.na(sc$PCT_EL)]
      s$N_SCHOOLS[j] <- nrow(sc)
      s$N_SCHOOLS_SHOWN[j] <- length(shown)
      if (length(shown)) {
        s$SCHOOL_PCT_MIN[j] <- min(shown)
        s$SCHOOL_PCT_MID[j] <- stats::median(shown)
        s$SCHOOL_PCT_MAX[j] <- max(shown)
      }
    }
  }

  list(summary = s, languages = langs, service_models = models, schools = schools)
}

# ---- assembly ------------------------------------------------------------------

#' Run Prep 2's context views -- the deck's results
#'
#' Everything the context deck (`decks/context.qmd`) shows, computed in one
#' call: Prep 2's own grade counts, statewide EL share and ranked districts,
#' plus the context views of [compute_exit_rates()],
#' [compute_district_share()], [compute_district_concentration()] and
#' [compute_district_profiles()]. Each slot is `NULL`, never a crash, when its
#' dependency is absent.
#'
#' Sample: the full statewide EL population, each year a cross-section -- not
#' a cohort. Exit rates cover current ELs whose next year is in the data.
#'
#' @inheritParams compute_exit_rates
#' @param share_breaks Passed to [compute_district_share()] as `breaks`.
#' @param size_breaks Passed to [compute_district_concentration()] as
#'   `breaks`; `NULL` uses the spec's size bands.
#' @param districts,n Passed to [compute_district_profiles()].
#' @return A named list: `exit_by_grade`, `exit_by_years`, `el_share`,
#'   `district_share`, `by_grade`, `by_district`, `concentration`,
#'   `concentration_by_mean`, `profiles`, and `$meta`.
#' @export
#' @examples
#' \donttest{
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' res <- run_prep2_context(bundle, spec)
#' res$meta$sample
#' }
run_prep2_context <- function(data, spec, pooled_years = 5L,
                              population = c("tested", "all"),
                              share_breaks = c(0, 0.05, 0.20, 0.40),
                              size_breaks = NULL, districts = NULL, n = 4L) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  population <- match.arg(population)
  roster <- el_roster(data)
  .roster_cols(roster, ROSTER_REQUIRED)

  exit_by_grade <- compute_exit_rates(data, spec, by = "grade",
                                      pooled_years = pooled_years, population = population)
  exit_by_years <- compute_exit_rates(data, spec, by = "years_in_status",
                                      pooled_years = pooled_years, population = population)
  slots <- list(
    exit_by_grade  = exit_by_grade,
    exit_by_years  = exit_by_years,                          # [NULL: no entry dating]
    el_share       = compute_el_share(data, spec),           # [NULL: no never-EL rows]
    district_share = compute_district_share(data, spec, share_breaks),
    by_grade       = compute_by_grade(data, spec),
    by_district    = compute_by_district(data, spec),        # [NULL: no DISTRICT_ID]
    concentration  = compute_district_concentration(data, spec, size_breaks),
    concentration_by_mean = compute_district_concentration(data, spec, size_breaks,
                                                           split_at_mean = TRUE),
    profiles       = compute_district_profiles(data, spec, districts, n, population))

  panel <- range(roster$YEAR, na.rm = TRUE)
  # The latest year's current ELs in every grade, and how many of them sit
  # outside K-12 (pre-K, ungraded) -- the by-grade slide's total. Both
  # suppressed like any count.
  # A small off-grid count is suppressed by reporting the K-12 total alone,
  # since the all-grade total minus the grade bars would give it away.
  cur_l <- roster$GRADE[roster$YEAR == panel[[2]] & roster$EL_STATUS %in% "current"]
  n_off <- sum(is.na(.p2_norm_grade(cur_l, spec)))
  n_cur <- length(cur_l)
  if (.p2_hit(n_off, spec)) {
    n_cur <- n_cur - n_off
    n_off <- NA_integer_
  }
  meta <- list(
    stage       = "prep2",
    view        = "context",
    spec_id     = spec$program$short_name %||% NA_character_,
    sample      = .p2c_sample_text(panel, population, spec),
    panel       = panel,
    latest_year = panel[[2]],
    n_current_latest   = if (.p2_hit(n_cur, spec)) NA_integer_ else n_cur,
    n_current_off_grid = n_off,   # NA: suppressed, and the total is K-12 only
    min_cell    = spec_min_cell(spec),
    population  = population,
    headline_label = spec_headline_label(spec),
    notes = list(
      exit = paste0(
        "A one-year snapshot of who exits, not how long exiting takes: Check 1's ",
        "life table measures time in status."),
      unknown_entry = if (!is.null(exit_by_years) &&
                          isTRUE(attr(exit_by_years, "n_unknown_entry") > 0)) sprintf(
        "%s at-risk student-years have no datable entry and are left out of the years-in-status view.",
        format(attr(exit_by_years, "n_unknown_entry"), big.mark = ","))),
    sim_contract_version = data$meta$sim_contract_version,
    package_version      = as.character(utils::packageVersion("stateELP")),
    run_date             = Sys.Date())
  c(slots, list(meta = meta))
}

.p2c_sample_text <- function(panel, population, spec) {
  paste0(
    "Full statewide EL population, ", panel[[1]], .P2_ENDASH, panel[[2]],
    ", read one year at a time (not a cohort). Exit rates cover current ELs ",
    if (population == "tested") paste0("with an annual ", spec_headline_label(spec),
                                       " score ") else "",
    "in a year whose next year is in the data; exited means recorded as a ",
    "former EL the next year, and a student with no record that year is left ",
    "out rather than counted as staying. Never-EL students enter only as the ",
    "enrollment denominator, weighted.")
}
