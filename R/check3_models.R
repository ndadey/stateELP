# Check 3 -- the later passes: the summary table, the logistic and where it
# crosses, and the two fairness checks. Same prefix as check3_compute.R, split
# off because that file holds the panel and the box statistics the figure
# needs and this one holds what is read off them.

#' The slide-71 summary table for one panel
#'
#' The figure's numbers, written out: n, minimum, 25th, median, mean, 75th,
#' maximum and IQR for every group on the axis. Built from
#' [compute_box_data()] rather than recomputed, so the table and the figure
#' cannot disagree about the same panel.
#'
#' `N` counts everyone in the group, including students on an alternate
#' academic assessment; `N_PLOTTED` is how many scores the statistics rest on.
#' `N_WEIGHTED` matters for the English-only group, which is a 10% sample.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param panel Output of [compute_panel()], or `NULL` to derive it.
#' @param box_data Output of [compute_box_data()] for the same panel, when it
#'   has already been computed.
#' @return A tibble, one row per group, or `NULL` when the panel is empty.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_summary_table(b, spec)
compute_summary_table <- function(data, spec, panel = NULL, box_data = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  if (is.null(panel)) panel <- compute_panel(data, spec)
  if (is.null(box_data)) box_data <- compute_box_data(data, spec, panel)
  if (is.null(box_data)) return(NULL)

  out <- box_data
  out$Y_IQR <- out$Y_P75 - out$Y_P25
  out <- out[c("GROUP", "N", "N_PLOTTED", "N_ALTERNATE", "N_WEIGHTED",
               "Y_MIN", "Y_P25", "Y_MEDIAN", "Y_MEAN", "Y_P75", "Y_MAX",
               "Y_IQR", "PCT_AT_CUT", "SUPPRESSED")]
  attr(out, "panel") <- panel
  out
}

#' Probability of academic proficiency as a function of ELP
#'
#' A logistic regression of "did this student reach the academic proficiency
#' cut" on their headline ELP scale score, fitted on the **current ELs** in one
#' panel -- they are the only students with an ELP score to regress on. The
#' reported statistic is the scale score where the fitted probability crosses
#' 50%: the language level at which a student is as likely as not to clear the
#' content bar.
#'
#' **The threshold is swappable, and that is the second fairness check.**
#' Left at its default the outcome is the state's own proficiency cut. Passing
#' `threshold` re-runs the identical model against a different bar -- the
#' English-only group's average score, in [compute_fairness_logistic()] -- and
#' the gap between the two crossings is the construct-validity signal.
#'
#' **Alternate-assessment takers are excluded**: their scores are on another
#' scale, so "reached the cut" is not defined for them.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param panel Output of [compute_panel()], or `NULL` to derive it.
#' @param threshold Academic score defining the outcome. Defaults to the
#'   spec's proficiency cut.
#' @param threshold_kind Label for what the threshold is, carried into the
#'   result so a reader of the slot knows which bar was used.
#' @return A one-row tibble: the fitted coefficients, `CROSSING` (the 50%
#'   scale score, `NA` when the slope is not positive), `N`, `N_ABOVE`,
#'   `THRESHOLD`, `THRESHOLD_KIND`, `SUPPRESSED`; `NULL` when the panel
#'   cannot support a fit at all.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_logistic(b, spec)
compute_logistic <- function(data, spec, panel = NULL, threshold = NULL,
                             threshold_kind = "state proficiency cut") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  if (is.null(panel)) panel <- compute_panel(data, spec)
  if (is.na(panel$year)) return(NULL)

  if (is.null(threshold)) threshold <- .check3_cut(spec, panel$subject)
  if (is.na(threshold)) return(NULL)   # [no cut in the spec: nothing to model]

  rows <- .check3_panel_rows(data, spec, panel)
  fr <- rows[rows$EL_STATUS %in% "current" & !rows$ALTERNATE &
               !is.na(rows$ELP_SCALE) & !is.na(rows$SCALE_SCORE), ,
             drop = FALSE]
  if (!nrow(fr)) return(NULL)

  fr$ABOVE <- as.integer(fr$SCALE_SCORE >= threshold)
  out <- .check3_empty_logistic(panel, threshold, threshold_kind,
                                n = nrow(fr), n_above = sum(fr$ABOVE))

  # A panel where everybody, or nobody, cleared the bar has no curve to fit:
  # the outcome does not vary. That is a fact about the grade rather than an
  # error, so the row comes back with an empty crossing instead of a stop.
  if (length(unique(fr$ABOVE)) < 2L || nrow(fr) < spec_min_cell(spec)) {
    return(apply_suppression(out, spec, n_col = "N",
                             cols = c("INTERCEPT", "SLOPE", "CROSSING",
                                      "CROSSING_SE", "CROSSING_LO",
                                      "CROSSING_HI")))
  }

  fit <- stats::glm(ABOVE ~ ELP_SCALE, data = fr, family = stats::binomial())
  cf <- stats::coef(fit)
  out$INTERCEPT <- unname(cf[["(Intercept)"]])
  out$SLOPE <- unname(cf[["ELP_SCALE"]])
  ci <- .check3_crossing_ci(fit)
  out$CROSSING_SE <- ci[["se"]]
  out$CROSSING_LO <- ci[["lo"]]
  out$CROSSING_HI <- ci[["hi"]]
  # logit^-1(a + b x) = 0.5 at x = -a / b. A flat or negative slope means the
  # curve never rises through a half, so no crossing is reported rather than
  # a number read off the wrong side of it.
  out$CROSSING <- if (is.na(out$SLOPE) || out$SLOPE <= 0) NA_real_ else
    -out$INTERCEPT / out$SLOPE
  # Whether the crossing is inside the scores actually observed. Outside them
  # it is an extrapolation of the fitted curve, not a level anyone was seen at.
  out$CROSSING_IN_RANGE <- !is.na(out$CROSSING) &&
    out$CROSSING >= min(fr$ELP_SCALE) && out$CROSSING <= max(fr$ELP_SCALE)

  apply_suppression(out, spec, n_col = "N",
                    cols = c("INTERCEPT", "SLOPE", "CROSSING", "CROSSING_SE",
                             "CROSSING_LO", "CROSSING_HI"))
}

.check3_empty_logistic <- function(panel, threshold, kind, n, n_above) {
  tibble::tibble(
    SUBJECT = panel$subject, GRADE = panel$grade, YEAR = panel$year,
    INTERCEPT = NA_real_, SLOPE = NA_real_, CROSSING = NA_real_,
    CROSSING_SE = NA_real_, CROSSING_LO = NA_real_, CROSSING_HI = NA_real_,
    CROSSING_IN_RANGE = FALSE,
    N = as.integer(n), N_ABOVE = as.integer(n_above),
    THRESHOLD = as.numeric(threshold), THRESHOLD_KIND = kind)
}

#' Where the panel lands
#'
#' Two readings of the same question, reported side by side because agreement
#' between them is the evidence and disagreement is itself informative:
#'
#' * `LANDING_BAND` -- the first ELP band whose students reach the cut at
#'   least half the time. Equivalently the first band whose **median** clears
#'   the cut: the same statement, and the one that supersedes the earlier
#'   distance-to-cut operationalization.
#' * `LANDING_CROSSING` -- the ELP scale score at which the fitted logistic
#'   passes 50%.
#'
#' **`NA` is an answer here, not a failure.** A grade where no band reaches
#' half is a grade where language proficiency at every observed level is not
#' sufficient for the content bar, which is exactly the kind of finding this
#' stage exists to surface. `TOP_BAND_PCT` says how far the highest band did
#' get, so the empty answer is readable.
#'
#' `LANDING_AT_OR_ABOVE_EXIT` flags a landing band at or above the band the
#' state's own exit threshold falls in. Those bands are thin and peculiar --
#' a current EL observed at the exit level is usually in their final year --
#' so a landing statistic that falls there rests on a population the exit rule
#' itself constitutes. See `internal_docs/NOTES-check3.md`.
#'
#' @param box_data Output of [compute_box_data()].
#' @param logistic Output of [compute_logistic()], or `NULL`.
#' @param spec An `el_spec`.
#' @return A one-row tibble, or `NULL` without box data.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' bd <- compute_box_data(b, spec)
#' compute_landing(bd, compute_logistic(b, spec), spec)
compute_landing <- function(box_data, logistic, spec) {
  stopifnot(inherits(spec, "el_spec"))
  if (is.null(box_data)) return(NULL)
  panel <- attr(box_data, "panel")

  bands <- spec_bands(spec)$labels
  b <- box_data[match(bands, box_data$GROUP), , drop = FALSE]
  reached <- !is.na(b$PCT_AT_CUT) & b$PCT_AT_CUT >= 0.5
  first <- which(reached)[1]

  exit_band <- .check3_exit_band(spec)
  landing <- if (is.na(first)) NA_character_ else bands[[first]]
  top <- suppressWarnings(max(b$PCT_AT_CUT, na.rm = TRUE))

  tibble::tibble(
    SUBJECT = if (is.null(panel)) NA_character_ else panel$subject,
    GRADE   = if (is.null(panel)) NA_character_ else panel$grade,
    YEAR    = if (is.null(panel)) NA_integer_ else panel$year,
    LANDING_BAND = landing,
    LANDING_PCT = if (is.na(first)) NA_real_ else b$PCT_AT_CUT[[first]],
    LANDING_CROSSING = if (is.null(logistic)) NA_real_ else logistic$CROSSING,
    CROSSING_LO = if (is.null(logistic)) NA_real_ else logistic$CROSSING_LO,
    CROSSING_HI = if (is.null(logistic)) NA_real_ else logistic$CROSSING_HI,
    CROSSING_IN_RANGE = if (is.null(logistic)) NA else
      logistic$CROSSING_IN_RANGE,
    EXIT_BAND = exit_band,
    LANDING_AT_OR_ABOVE_EXIT = !is.na(landing) && !is.na(exit_band) &&
      match(landing, bands) >= match(exit_band, bands),
    TOP_BAND_PCT = if (is.finite(top)) top else NA_real_)
}

# Which band the state's own exit threshold falls in. Read from the exit
# rule's structured terms, never typed: the rule is declared on a level, and
# the bands are a banding of that same level scale.
.check3_exit_band <- function(spec) {
  terms <- unlist(lapply(spec$policy$exit$pathways, function(p) p$terms),
                  recursive = FALSE)
  lvl <- NULL
  for (tm in terms) {
    if (identical(tm$measure, "level") && is.null(tm$from) &&
        is.numeric(tm$value)) {
      lvl <- tm$value
      break
    }
  }
  if (is.null(lvl)) return(NA_character_)
  as.character(assign_elp_band(lvl, spec))
}

#' The fairness check: an economically comparable English-only group
#'
#' The same box plot with the English-only column restricted to students
#' economically comparable to the EL population, because a gap between ELs and
#' *all* English-only peers confounds language with poverty.
#'
#' The shell filter is crude by design, and the instruction file says so: this
#' delivery carries a single free-and-reduced-lunch flag and no parental
#' education, so the group is filtered rather than matched. A
#' propensity-matched version is a recorded later addition. The point of the
#' crude version is that it moves the comparison in the right direction and
#' shows how far that alone takes it.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param panel Output of [compute_panel()], or `NULL` to derive it.
#' @return A tibble in [compute_box_data()]'s shape with the reference group
#'   restricted; `NULL` when the delivery carries no SES flag.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_fairness_boxes(b, spec)
compute_fairness_boxes <- function(data, spec, panel = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  if (is.null(panel)) panel <- compute_panel(data, spec)
  if (is.na(panel$year)) return(NULL)
  roster <- el_roster(data)
  if (!"FRL" %in% names(roster)) return(NULL)   # [no SES flag to filter on]

  rows <- .check3_panel_rows(data, spec, panel)
  if (!nrow(rows)) return(NULL)
  keep <- !(rows$GROUP %in% .CHECK3_NEVER) |
    .check3_is_disadvantaged(rows$FRL)

  out <- .check3_box_from_rows(rows[keep, , drop = FALSE], spec, panel)
  if (is.null(out)) return(NULL)

  attr(out, "panel") <- panel
  attr(out, "cut") <- .check3_cut(spec, panel$subject)
  attr(out, "filter") <- paste0(
    "English-only students restricted to the economically disadvantaged. A ",
    "crude filter, not a match: this delivery carries one SES flag and no ",
    "parental education.")
  out
}

#' The fairness check: the bar the English-only group itself clears
#'
#' [compute_logistic()] re-run with one change -- the outcome is no longer
#' "reached the state's proficiency cut" but "reached the English-only group's
#' **average** score in this panel". If the ELP level implied by that bar sits
#' materially below the one implied by the state's cut, the state's cut is
#' asking of ELs something their English-speaking peers do not themselves
#' average.
#'
#' That gap is a construct-validity signal, not a threshold recommendation: an
#' ELP test measures English proficiency, and a content cut is not
#' automatically a language-access cut. It is surfaced to the Decision stage
#' and never resolved here.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param panel Output of [compute_panel()], or `NULL` to derive it.
#' @return [compute_logistic()]'s shape with the alternative threshold, plus
#'   the state cut beside it; `NULL` when the panel has too few English-only
#'   students to average.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_fairness_logistic(b, spec)
compute_fairness_logistic <- function(data, spec, panel = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  if (is.null(panel)) panel <- compute_panel(data, spec)
  if (is.na(panel$year)) return(NULL)

  rows <- .check3_panel_rows(data, spec, panel)
  eo <- rows[rows$GROUP %in% .CHECK3_NEVER & !rows$ALTERNATE &
               !is.na(rows$SCALE_SCORE), , drop = FALSE]
  if (nrow(eo) < spec_min_cell(spec)) return(NULL)

  # A constant weight leaves a mean unchanged, but weighting it is what makes
  # the number a population average rather than a sample one, and the contract
  # requires every never-EL figure to be weighted or labelled.
  bar <- stats::weighted.mean(eo$SCALE_SCORE, eo$SAMPLE_WEIGHT)

  out <- compute_logistic(data, spec, panel, threshold = bar,
                          threshold_kind = "English-only average")
  if (is.null(out)) return(NULL)
  out$STATE_CUT <- .check3_cut(spec, panel$subject)
  out$N_REFERENCE <- nrow(eo)
  out
}

# Economically disadvantaged, from whatever the delivery calls it. The sim
# uses Y/N; a crosswalk state may send 1/0 or TRUE/FALSE, and none of those
# should silently filter the group down to nobody.
.check3_is_disadvantaged <- function(x) {
  v <- toupper(trimws(as.character(x)))
  !is.na(v) & v %in% c("Y", "YES", "1", "TRUE", "T")
}

# A confidence interval for the 50% crossing, by the delta method.
#
# The crossing is x = -a/b, a ratio of two correlated estimates, so its
# variance is not read off either coefficient alone:
#   dx/da = -1/b,  dx/db = a/b^2,  Var(x) = g' V g
# with V the fitted covariance. That is the standard treatment for this
# quantity (Fieller's theorem is the exact alternative and behaves badly for
# exactly the same reason noted below, so it buys little here).
#
# The interval is wide, sometimes absurdly so, whenever the slope is small
# relative to its own error -- which is precisely when the crossing sits far
# outside the observed scores. That is not a defect of the interval: it is the
# interval reporting that the curve is being read past its data, the same fact
# CROSSING_IN_RANGE flags. Reported as computed, never trimmed to something
# comfortable.
.check3_crossing_ci <- function(fit, level = 0.95) {
  empty <- c(se = NA_real_, lo = NA_real_, hi = NA_real_)
  cf <- stats::coef(fit)
  if (length(cf) < 2L || any(is.na(cf))) return(empty)

  a <- unname(cf[[1L]])
  b <- unname(cf[[2L]])
  if (!is.finite(b) || b <= 0) return(empty)

  v <- try(stats::vcov(fit), silent = TRUE)
  if (inherits(v, "try-error") || any(!is.finite(v))) return(empty)

  g <- c(-1 / b, a / b^2)
  var_x <- drop(t(g) %*% v[1:2, 1:2] %*% g)
  if (!is.finite(var_x) || var_x < 0) return(empty)

  se <- sqrt(var_x)
  z <- stats::qnorm(1 - (1 - level) / 2)
  x <- -a / b
  c(se = se, lo = x - z * se, hi = x + z * se)
}
