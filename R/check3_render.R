# Check 3 -- tables and the sentences that read them. Built from run_check3()'s
# results; the document calls these and builds no tables of its own.

#' Check 3 tables
#'
#' Flextables built from [run_check3()]'s results, and the sentences that say
#' what the numbers mean.
#'
#' @param results Output of [run_check3()].
#' @param subject,grade Which panel to show, defaulting to the focal one.
#' @param slot Which results slot to tabulate: the summary table, or the
#'   SES-filtered `fairness_boxes` in the same shape.
#' @return `ft_check3_*()` a `flextable`; the `check3_*_note()` functions a
#'   string, or `NA` when the slot they read is absent.
#' @name check3_tables
NULL

#' @rdname check3_tables
#' @export
ft_check3_summary <- function(results, subject = NULL, grade = NULL,
                              slot = "summary_table") {
  stopifnot(is.list(results), !is.null(results[[slot]]))
  s <- .c3_one_panel(results[[slot]], subject, grade)
  # `$fairness_boxes` is box-data shaped: same statistics, no IQR column,
  # because it is drawn as a figure rather than read as a table. Deriving it
  # here lets one table serve both rather than duplicating the formatting.
  if (!"Y_IQR" %in% names(s)) s$Y_IQR <- s$Y_P75 - s$Y_P25

  out <- data.frame(
    GROUP = s$GROUP, N = s$N, N_PLOTTED = s$N_PLOTTED,
    Y_MIN = s$Y_MIN, Y_P25 = s$Y_P25, Y_MEDIAN = s$Y_MEDIAN,
    Y_MEAN = s$Y_MEAN, Y_P75 = s$Y_P75, Y_MAX = s$Y_MAX, Y_IQR = s$Y_IQR,
    PCT = s$PCT_AT_CUT, check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft, GROUP = "Group", N = "n", N_PLOTTED = "n scored",
    Y_MIN = "Min", Y_P25 = "25th", Y_MEDIAN = "Median", Y_MEAN = "Mean",
    Y_P75 = "75th", Y_MAX = "Max", Y_IQR = "IQR", PCT = "At/above cut")
  ft <- flextable::colformat_num(ft, j = c("N", "N_PLOTTED"), big.mark = ",",
                                 na_str = .C3_EM_DASH)
  ft <- flextable::colformat_double(
    ft, j = c("Y_MIN", "Y_P25", "Y_MEDIAN", "Y_MEAN", "Y_P75", "Y_MAX",
              "Y_IQR"), digits = 0, na_str = .C3_EM_DASH)
  ft <- flextable::set_formatter(
    ft, PCT = function(x) ifelse(is.na(x), .C3_EM_DASH,
                                 paste0(round(100 * x), "%")))
  ft <- flextable::bold(ft, part = "header")
  flextable::autofit(ft)
}

#' @rdname check3_tables
#' @export
ft_check3_landing <- function(results) {
  stopifnot(is.list(results), !is.null(results$landing))
  l <- results$landing

  out <- data.frame(
    SUBJECT = l$SUBJECT, GRADE = l$GRADE,
    BAND = ifelse(is.na(l$LANDING_BAND), .C3_EM_DASH, l$LANDING_BAND),
    TOP = l$TOP_BAND_PCT,
    CROSSING = l$LANDING_CROSSING,
    CI = .check3_ci_text(l$CROSSING_LO, l$CROSSING_HI),
    IN_RANGE = ifelse(l$CROSSING_IN_RANGE %in% TRUE, "yes", "no"),
    check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft, SUBJECT = "Subject", GRADE = "Grade",
    BAND = "First band at/above the cut for half its students",
    TOP = "Highest band's share", CROSSING = "Logistic 50% crossing",
    CI = "95% CI", IN_RANGE = "Crossing within observed scores")
  ft <- flextable::set_formatter(
    ft, TOP = function(x) ifelse(is.na(x), .C3_EM_DASH,
                                 paste0(round(100 * x), "%")))
  ft <- flextable::colformat_double(ft, j = "CROSSING", digits = 0,
                                    na_str = .C3_EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  # The rows that rest on a population the exit rule constitutes.
  hit <- which(l$LANDING_AT_OR_ABOVE_EXIT %in% TRUE)
  if (length(hit)) ft <- flextable::bold(ft, i = hit, j = "BAND")
  flextable::autofit(ft)
}

#' @rdname check3_tables
#' @export
ft_check3_fairness <- function(results) {
  stopifnot(is.list(results), !is.null(results$fairness_logistic))
  f <- results$fairness_logistic
  l <- results$logistic

  at <- match(paste(f$SUBJECT, f$GRADE), paste(l$SUBJECT, l$GRADE))
  out <- data.frame(
    SUBJECT = f$SUBJECT, GRADE = f$GRADE,
    STATE_CUT = f$STATE_CUT, EO_BAR = f$THRESHOLD,
    STATE_CROSSING = l$CROSSING[at], EO_CROSSING = f$CROSSING,
    check.names = FALSE)
  out$GAP <- out$STATE_CROSSING - out$EO_CROSSING

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft, SUBJECT = "Subject", GRADE = "Grade",
    STATE_CUT = "State cut", EO_BAR = "English-only average",
    STATE_CROSSING = "ELP crossing, state cut",
    EO_CROSSING = "ELP crossing, English-only bar",
    GAP = "Difference")
  ft <- flextable::colformat_double(
    ft, j = c("STATE_CUT", "EO_BAR", "STATE_CROSSING", "EO_CROSSING", "GAP"),
    digits = 0, na_str = .C3_EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  flextable::autofit(ft)
}

#' @rdname check3_tables
#' @export
check3_landing_note <- function(results) {
  l <- results$landing
  if (is.null(l)) return(NA_character_)

  none <- sum(is.na(l$LANDING_BAND))
  at_exit <- sum(l$LANDING_AT_OR_ABOVE_EXIT %in% TRUE)
  landed <- nrow(l) - none
  out_of_range <- sum(!l$CROSSING_IN_RANGE %in% TRUE & !is.na(l$LANDING_CROSSING))

  parts <- c(sprintf(
    paste0("Of %d grade-and-subject panels, %d have a band where at least ",
           "half the students reach the cut; in %d no band does."),
    nrow(l), landed, none))

  if (landed > 0 && at_exit == landed) {
    parts <- c(parts, paste0(
      "**Every panel that lands, lands at or above the band the exit ",
      "threshold falls in.** Those bands hold students the exit rule is ",
      "about to remove, so the landing statistic there is describing a ",
      "population the rule itself constitutes rather than a level students ",
      "sit at and stay."))
  } else if (at_exit > 0) {
    parts <- c(parts, sprintf(
      paste0("%d of the %d landings sit at or above the exit band, where the ",
             "population is constituted by the exit rule rather than observed ",
             "at rest."), at_exit, landed))
  }

  if (out_of_range > 0) {
    parts <- c(parts, sprintf(
      paste0("The logistic crossing falls outside the ELP scores actually ",
             "observed in %d of %d panels: there the curve is being read past ",
             "its data, and the number is an extrapolation rather than a ",
             "level anyone was seen at."), out_of_range, nrow(l)))
  }
  paste(parts, collapse = " ")
}

#' @rdname check3_tables
#' @export
check3_fairness_note <- function(results) {
  f <- results$fairness_logistic
  l <- results$logistic
  if (is.null(f) || is.null(l)) return(NA_character_)

  at <- match(paste(f$SUBJECT, f$GRADE), paste(l$SUBJECT, l$GRADE))
  gap <- l$CROSSING[at] - f$CROSSING
  below <- sum(f$THRESHOLD < f$STATE_CUT, na.rm = TRUE)

  paste(
    sprintf(paste0("In %d of %d panels the English-only group's own average ",
                   "score sits BELOW the state's proficiency cut -- the bar ",
                   "being asked of English learners is one the average ",
                   "English-speaking peer does not clear."),
            below, nrow(f)),
    if (all(is.na(gap))) "" else sprintf(
      paste0("Swapping the bar moves the implied ELP crossing by a median of ",
             "%.0f scale points."), stats::median(gap, na.rm = TRUE)),
    "A persistent gap is a construct-validity signal, not a threshold",
    "recommendation: an ELP test measures English proficiency, and a content",
    "cut is not automatically a language-access cut. It goes to the Decision",
    "stage rather than being settled here.")
}

.C3_EM_DASH <- intToUtf8(0x2014)

# The crossing interval as one cell. Wide is informative here -- it is the
# same fact IN_RANGE reports, in the units of the statistic -- so it is never
# trimmed, only dashed when the fit could not supply one.
.check3_ci_text <- function(lo, hi) {
  ifelse(is.na(lo) | is.na(hi), .C3_EM_DASH,
         sprintf("%.0f%s%.0f", lo, .C3_EM_DASH, hi))
}
