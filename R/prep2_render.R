# Prep 2 -- presentation. These take a computed results object and return a
# flextable or a sentence; they compute no analysis of their own and no
# student-level data is in scope (CLAUDE.md 8). Follows Check 1's ft_*()
# branch: Word is the primary format and every table in it is a flextable.
#
# Every display column is preformatted text ("1,234", "12.3%", an em dash for a
# suppressed or unavailable cell), so a table reads the same in Word and HTML
# and flextable never re-formats a year as "2,015".

# ---- formatting --------------------------------------------------------------

.p2_fmt_n <- function(x) {
  ifelse(is.na(x), .P2_DASH,
         format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

.p2_fmt_pct <- function(x, d = 1) {
  ifelse(is.na(x), .P2_DASH,
         paste0(formatC(100 * x, format = "f", digits = d), "%"))
}

# A bounded share: one figure when the bounds meet.
.p2_fmt_range <- function(lo, hi) {
  ifelse(is.na(lo) | is.na(hi), .P2_DASH,
         ifelse(abs(hi - lo) < 1e-9, .p2_fmt_pct(lo),
                paste0(.p2_fmt_pct(lo), .P2_ENDASH, .p2_fmt_pct(hi))))
}

# A long table (`row`, `col`, `val`) to a wide data frame, cells NA where the
# long table has none.
.p2_wide <- function(df, row, col, val, rows = unique(df[[row]]),
                     cols = unique(df[[col]])) {
  m <- matrix(NA, nrow = length(rows), ncol = length(cols),
              dimnames = list(NULL, as.character(cols)))
  m[cbind(match(df[[row]], rows), match(df[[col]], cols))] <- df[[val]]
  out <- data.frame(ROW = as.character(rows), m, check.names = FALSE,
                    stringsAsFactors = FALSE)
  out
}

.p2_flex <- function(df, labels = NULL, size = NULL) {
  df <- as.data.frame(lapply(df, as.character), check.names = FALSE,
                      stringsAsFactors = FALSE)
  ft <- flextable::flextable(df)
  if (!is.null(labels)) ft <- flextable::set_header_labels(ft, values = as.list(labels))
  ft <- flextable::bold(ft, part = "header")
  if (ncol(df) > 1L) {
    ft <- flextable::align(ft, j = seq(2L, ncol(df)), align = "right", part = "all")
  }
  if (!is.null(size)) ft <- flextable::fontsize(ft, size = size, part = "all")
  flextable::autofit(ft)
}

# ---- tables ------------------------------------------------------------------

#' Prep 2 tables
#'
#' Flextables built from [run_prep2()]'s results. The document calls these; it
#' builds no tables of its own. Each needs its slot to be non-`NULL` and says
#' so if it is not.
#'
#' @param results Output of [run_prep2()].
#' @return A `flextable`. `prep2_crosswalk_note()` and `prep2_contested_note()`
#'   return a sentence.
#' @name prep2_tables
NULL

.p2_need <- function(results, slot) {
  stopifnot(is.list(results))
  if (is.null(results[[slot]])) {
    stop("`", slot, "` is NULL in these results, so there is nothing to tabulate.",
         call. = FALSE)
  }
  results[[slot]]
}

# The component the state reports, named for the reader: documents say
# "Overall Composite", never this package's word "headline" (CLAUDE.md 9).
# Carried in $meta so a results object read back from RDS still knows it.
.p2_headline_label <- function(results) {
  lab <- results$meta$headline_label
  if (is.null(lab) || is.na(lab)) "ELP" else lab
}

#' @rdname prep2_tables
#' @export
ft_prep2_coverage <- function(results) {
  d <- .p2_need(results, "counts_by_year")
  out <- data.frame(
    YEAR = d$YEAR,
    N_CURRENT = .p2_fmt_n(d$N_CURRENT),
    N_ANNUAL_HEADLINE = .p2_fmt_n(d$N_ANNUAL_HEADLINE),
    N_ALTERNATE = .p2_fmt_n(d$N_ALTERNATE),
    N_NO_HEADLINE = .p2_fmt_n(d$N_NO_HEADLINE),
    N_FORMER = .p2_fmt_n(d$N_FORMER),
    check.names = FALSE)
  # Never-ELs are deliberately not a column here: they matter only as the
  # enrollment denominator, which ft_prep2_el_share() shows weighted. They stay
  # in the counts_by_year slot.
  lab <- .p2_headline_label(results)
  .p2_flex(out, stats::setNames(
    c("Year", "Current ELs", paste("Annual", lab), "Alternate form",
      "No annual score", "Former ELs"),
    c("YEAR", "N_CURRENT", "N_ANNUAL_HEADLINE", "N_ALTERNATE",
      "N_NO_HEADLINE", "N_FORMER")))
}

#' @rdname prep2_tables
#' @export
ft_prep2_el_share <- function(results) {
  d <- .p2_need(results, "el_share")
  out <- data.frame(YEAR = d$YEAR,
                    ENROLLMENT = .p2_fmt_n(d$N_ENROLLMENT_WEIGHTED),
                    EL = .p2_fmt_n(d$N_EL_WEIGHTED),
                    SHARE = .p2_fmt_pct(d$PCT_EL), check.names = FALSE)
  .p2_flex(out, c(YEAR = "Year", ENROLLMENT = "Enrollment (weighted)",
                  EL = "Current ELs", SHARE = "EL share"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_grade_shares <- function(results) {
  d <- .p2_need(results, "by_grade")
  w <- .p2_wide(d, "GRADE", "YEAR", "PCT", rows = .P2_GRADES,
                cols = sort(unique(d$YEAR)))
  w[-1] <- lapply(w[-1], .p2_fmt_pct)
  .p2_flex(w, c(ROW = "Grade"), size = if (ncol(w) > 8L) 8 else NULL)
}

#' @rdname prep2_tables
#' @export
ft_prep2_district_bins <- function(results) {
  d <- .p2_need(results, "district_bins")
  out <- data.frame(SIZE_BAND = d$SIZE_BAND,
                    N_DISTRICTS = .p2_fmt_n(d$N_DISTRICTS),
                    N = .p2_fmt_n(d$N), PCT = .p2_fmt_pct(d$PCT),
                    check.names = FALSE)
  .p2_flex(out, c(SIZE_BAND = "District EL count", N_DISTRICTS = "Districts",
                  N = "Current ELs", PCT = "Share of ELs"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_districts <- function(results) {
  d <- .p2_need(results, "by_district")
  d <- d[d$TWO_THIRDS, , drop = FALSE]
  out <- data.frame(RANK = d$RANK, DISTRICT_ID = d$DISTRICT_ID,
                    N = .p2_fmt_n(d$N), PCT_CUM = .p2_fmt_pct(d$PCT_CUM),
                    check.names = FALSE)
  .p2_flex(out, c(RANK = "Rank", DISTRICT_ID = "District", N = "Current ELs",
                  PCT_CUM = "Cumulative share"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_entry_sources <- function(results) {
  d <- .p2_need(results, "entry_sources")
  out <- data.frame(ENTRY_SOURCE = d$ENTRY_SOURCE, N = .p2_fmt_n(d$N),
                    PCT = .p2_fmt_pct(d$PCT), check.names = FALSE)
  .p2_flex(out, c(ENTRY_SOURCE = "Entry year dated from", N = "Students",
                  PCT = "Share"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_entry_by_year <- function(results) {
  d <- .p2_need(results, "entry_by_year")
  w <- .p2_wide(d, "ENTRY_YEAR", "ENTRY_GRADE", "N",
                cols = .P2_GRADES)
  w[-1] <- lapply(w[-1], .p2_fmt_n)
  .p2_flex(w, c(ROW = "Entry year"), size = 8)
}

#' @rdname prep2_tables
#' @export
ft_prep2_entry_cohorts <- function(results) {
  d <- .p2_need(results, "entry_cohorts")
  out <- data.frame(COHORT_START_YEAR = d$COHORT_START_YEAR,
                    N_ENTRY = .p2_fmt_n(d$N_ENTRY),
                    FOLLOWUP_YEARS = d$FOLLOWUP_YEARS,
                    FULL_WINDOW = ifelse(d$FULL_WINDOW, "yes", "no"),
                    check.names = FALSE)
  .p2_flex(out, c(COHORT_START_YEAR = "Entry year", N_ENTRY = "Entrants",
                  FOLLOWUP_YEARS = "Years observed",
                  FULL_WINDOW = "Covers full exit window"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_time_by_year <- function(results) {
  d <- .p2_need(results, "time_in_status")$by_year
  out <- data.frame(YEAR = d$YEAR, N = .p2_fmt_n(d$N),
                    NEW = .p2_fmt_pct(d$PCT_NEW),
                    LT_MIN = .p2_fmt_pct(d$PCT_LT_MIN),
                    LT_MAX = .p2_fmt_pct(d$PCT_LT_MAX),
                    UNKNOWN = .p2_fmt_pct(d$PCT_UNKNOWN), check.names = FALSE)
  .p2_flex(out, c(YEAR = "Year", N = "Current ELs", NEW = "First year in status",
                  LT_MIN = "Long-term EL, at least",
                  LT_MAX = "Long-term EL, at most", UNKNOWN = "Entry unknown"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_time_by_grade <- function(results) {
  d <- .p2_need(results, "time_in_status")$by_grade
  out <- data.frame(GRADE = d$GRADE, N = .p2_fmt_n(d$N),
                    NEW = .p2_fmt_pct(d$PCT_NEW),
                    LT = .p2_fmt_range(d$PCT_LT_MIN, d$PCT_LT_MAX),
                    UNKNOWN = .p2_fmt_pct(d$PCT_UNKNOWN), check.names = FALSE)
  .p2_flex(out, c(GRADE = "Grade", N = "Current ELs",
                  NEW = "First year in status", LT = "Long-term EL",
                  UNKNOWN = "Entry unknown"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_initial_elp <- function(results) {
  d <- .p2_need(results, "initial_elp")
  out <- data.frame(INITIAL = d$INITIAL, N = .p2_fmt_n(d$N),
                    PCT = .p2_fmt_pct(d$PCT), check.names = FALSE)
  .p2_flex(out, c(INITIAL = "Initial ELP", N = "N", PCT = "Share"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_profile <- function(results) {
  p <- .p2_need(results, "profile")
  p <- p[!vapply(p, is.null, TRUE)]
  if (!length(p)) stop("`profile` holds no tables.", call. = FALSE)
  out <- do.call(rbind, lapply(names(p), function(v) {
    data.frame(VARIABLE = v, VALUE = p[[v]]$VALUE, N = .p2_fmt_n(p[[v]]$N),
               PCT = .p2_fmt_pct(p[[v]]$PCT), check.names = FALSE)
  }))
  .p2_flex(out, c(VARIABLE = "Variable", VALUE = "Value", N = "N", PCT = "Share"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_monitoring <- function(results) {
  d <- .p2_need(results, "monitoring")
  w <- .p2_wide(d, "YEAR", "MONITOR", "N", rows = sort(unique(d$YEAR)),
                cols = sort(unique(d$MONITOR)))
  w[-1] <- lapply(w[-1], .p2_fmt_n)
  .p2_flex(w, c(ROW = "Year"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_crosswalk_pattern <- function(results) {
  cw <- .p2_need(results, "criteria_crosswalk")
  d <- cw$pattern
  crit <- setdiff(names(d), c("N", "PCT", "SUPPRESSED"))
  out <- d[crit]
  out$N <- .p2_fmt_n(d$N)
  out$PCT <- .p2_fmt_pct(d$PCT)
  .p2_flex(out, c(N = "Student-years", PCT = "Share"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_crosswalk_blocked <- function(results) {
  d <- .p2_need(results, "criteria_crosswalk")$blocked
  out <- data.frame(BLOCKED_GROUP = d$BLOCKED_GROUP, N = .p2_fmt_n(d$N),
                    PCT = .p2_fmt_pct(d$PCT), check.names = FALSE)
  .p2_flex(out, c(BLOCKED_GROUP = "Group", N = "Student-years", PCT = "Share"))
}

#' @rdname prep2_tables
#' @export
ft_prep2_crosswalk_by_level <- function(results) {
  d <- .p2_need(results, "criteria_crosswalk")$by_level
  out <- data.frame(LEVEL_BIN = d$LEVEL_BIN, N = .p2_fmt_n(d$N),
                    PCT_DISAGREE = .p2_fmt_pct(d$PCT_DISAGREE),
                    check.names = FALSE)
  .p2_flex(out, stats::setNames(
    c(paste(.p2_headline_label(results), "level"), "Student-years",
      "Criteria disagree"),
    c("LEVEL_BIN", "N", "PCT_DISAGREE")))
}

#' @rdname prep2_tables
#' @export
prep2_crosswalk_note <- function(results) {
  cw <- .p2_need(results, "criteria_crosswalk")
  k <- cw$criteria
  desc <- sprintf("`%s` (%s %s %s %s%s)", k$CRITERION, k$COMPONENT, k$MEASURE,
                  k$OP, k$VALUE, ifelse(k$EXEMPT_IF_ABSENT, ", exempt if absent", ""))
  win <- if (is.na(cw$window[["cut"]])) {
    sprintf(paste0("no criterion sits on the %s level, so every current ",
                   "annual student-year is in the window"),
            .p2_headline_label(results))
  } else {
    sprintf(paste0(.p2_headline_label(results), " level within %s%s of the cut %s"),
            intToUtf8(0x00B1), cw$window[["half_width"]], cw$window[["cut"]])
  }
  paste0("Criteria (", cw$pathway, " pathway): ", paste(desc, collapse = "; "),
         ". Combination assumed ", if (identical(cw$combine, "any")) "**any**" else "conjunctive",
         ", all must pass [PROPOSED - CONFIRM]. Window: ", win, "; ",
         format(cw$n_window, big.mark = ","), " of ",
         format(cw$n_total, big.mark = ","), " current annual student-years.")
}

#' @rdname prep2_tables
#' @export
prep2_contested_note <- function(results) {
  z <- .p2_need(results, "contested_zone")
  if (z$n_disagree == 0L) {
    return(paste0("**Contested zone.** Across all ", format(z$n_total, big.mark = ","),
                  " current annual student-years the criteria never disagree: ",
                  "every student-year passes all of them or fails all of them."))
  }
  paste0("**Contested zone.** Across all ", format(z$n_total, big.mark = ","),
         " current annual student-years, the criteria disagree for ",
         format(z$n_disagree, big.mark = ","), " (",
         .p2_fmt_pct(z$n_disagree / z$n_total), "). ",
         .p2_headline_label(results), " level of disagreeing student-years: ", z$range[1], .P2_ENDASH, z$range[2],
         " (central 90%: ", round(z$core_90[1], 1), .P2_ENDASH,
         round(z$core_90[2], 1), "). This range carries forward to Data Check 3.")
}

# ---- the analytic-sample memo ------------------------------------------------

# Bullets for the "Decide before moving on" block, built from the computed
# slots only: what the numbers say, never what to do about them. Each bullet
# drops out when the slot behind it is NULL.
.prep2_memo <- function(s, meta) {
  latest <- meta$latest_year
  cnt <- s$counts_by_year
  now <- cnt[cnt$YEAR == latest, , drop = FALSE]
  avg <- mean(cnt$N_CURRENT, na.rm = TRUE)

  panel <- sprintf("**Panel.** %d years (%d%s%d)%s.", meta$n_years, meta$panel[[1]],
                   .P2_ENDASH, meta$panel[[2]],
                   if (meta$short_panel) " -- fewer than five, so trends over time are thin" else "")

  size <- sprintf("**Size.** %s current ELs in %d (%d-year average %s).",
                  .p2_fmt_n(now$N_CURRENT), latest, meta$n_years, .p2_fmt_n(avg))

  grades <- NULL
  g <- s$by_grade[s$by_grade$YEAR == latest & !is.na(s$by_grade$N), , drop = FALSE]
  if (nrow(g) >= 3L) {
    g <- g[order(-g$N), ][1:3, ]
    sp <- s$by_grade_span[s$by_grade_span$YEAR == latest & s$by_grade_span$SPAN == "K-5", ]
    grades <- sprintf("**Grade concentration (%d).** Grades %s hold %s of current ELs; K-5 holds %s.",
                      latest, paste(g$GRADE, collapse = ", "), .p2_fmt_pct(sum(g$PCT)),
                      .p2_fmt_pct(sp$PCT))
  }

  districts <- if (!is.null(s$by_district)) {
    d <- s$by_district
    small <- attr(d, "small_n")
    sprintf("**Districts (%d).** %d of %d districts hold two-thirds of current ELs%s.",
            latest, sum(d$TWO_THIRDS), nrow(d),
            if (is.na(small)) ""
            else sprintf("; %d districts serve fewer than %s",
                         sum(is.na(d$N) | d$N < small), format(small, big.mark = ",")))
  } else "**Districts.** Not available (no `DISTRICT_ID`)."

  entry <- NULL
  if (!is.null(s$entry_grade)) {
    eg <- s$entry_grade
    k1 <- sum(eg$PCT[eg$ENTRY_GRADE %in% c("K", "1")], na.rm = TRUE)
    kc <- s$entry_cohorts
    full <- if (is.null(kc)) kc else kc[kc$FULL_WINDOW, , drop = FALSE]
    short <- if (is.null(kc)) 0L else sum(!kc$FULL_WINDOW)
    entry <- sprintf("**Entry.** %s of observed entrants enter in K or grade 1. K-start cohorts that cover the full exit window: %s%s.",
                     .p2_fmt_pct(k1),
                     if (is.null(full) || !nrow(full)) "none"
                     else paste(sprintf("%d (n = %s, %d years observed)", full$COHORT_START_YEAR,
                                        .p2_fmt_n(full$N_ENTRY), full$FOLLOWUP_YEARS),
                                collapse = "; "),
                     if (short > 0L) sprintf(" (plus %d shorter %s, censored at the window's edge)",
                                             short, if (short == 1L) "cohort" else "cohorts")
                     else "")
  }

  time <- NULL
  if (!is.null(s$time_in_status)) {
    d <- s$time_in_status$by_year
    d <- d[d$YEAR == latest, , drop = FALSE]
    time <- sprintf("**Time in status (%d).** %s of current ELs are in their first year; %s have %d+ years (long-term); entry is unknown for %s (already EL when the panel opened, no entry field delivered).",
                    latest, .p2_fmt_pct(d$PCT_NEW),
                    .p2_fmt_range(d$PCT_LT_MIN, d$PCT_LT_MAX), meta$lt_el_years,
                    .p2_fmt_pct(d$PCT_UNKNOWN))
  }

  alt <- sprintf("**Alternate-form ELP.** %s of current ELs in %d take the alternate form (reported as a group, never banded).",
                 .p2_fmt_pct(now$N_ALTERNATE / now$N_CURRENT), latest)

  c(panel, size, grades, districts, entry, time, alt)
}
