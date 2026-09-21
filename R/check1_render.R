# Check 1 -- presentation. These take a computed results object and return a
# flextable or a sentence; they never compute anything themselves, and no
# student-level data is in scope (CLAUDE.md 8).
#
# `ft_*()` is a new naming branch for table builders, alongside plot_*() for
# ggplots: Word is the primary format and every table in it is a flextable.
# `[PROPOSED - CONFIRM]`
#
# Typography for the rendered tables, built from code points: R CMD check
# requires the sources themselves to be ASCII ("Portable packages must use
# only ASCII characters in their R code"), and these are display characters,
# not data.
.EM_DASH <- intToUtf8(0x2014)
.EN_DASH <- intToUtf8(0x2013)
.SIGMA <- intToUtf8(0x03A3)
.SUB_ONE <- intToUtf8(0x2081)

#' Check 1 tables
#'
#' Flextables built from [run_check1()]'s results, and the sentence that
#' closes the "Data fields used" box. The document calls these; it builds no
#' tables of its own.
#'
#' @param results Output of [run_check1()].
#' @return `ft_check1_*()` a `flextable`; `check1_derived_note()` a string,
#'   or `NA` when no derived field was used.
#' @name check1_tables
NULL

# What the box shows, as data. Split from the flextable so a test can read
# the rendered content without reaching into flextable's internals.
.check1_fields_display <- function(results) {
  fields <- results$meta$data_fields
  delivered <- fields[fields$TABLE %in% c("roster", "base", "academic"), ,
                      drop = FALSE]

  used_for <- ifelse(
    delivered$KIND == "optional" & !delivered$PRESENT,
    paste0("Not delivered ", .EM_DASH, " ", delivered$FALLBACK),
    ifelse(delivered$KIND == "optional",
           paste0(delivered$USED_FOR, " (optional)"),
           delivered$USED_FOR))

  # The same name can appear on two tables -- SCALE_SCORE is both the ELP
  # score and the content score -- so say which one when it does.
  field <- ifelse(duplicated(delivered$FIELD) |
                    duplicated(delivered$FIELD, fromLast = TRUE),
                  paste0(delivered$FIELD, " (", delivered$TABLE, ")"),
                  delivered$FIELD)

  # Your column earns a column only when the crosswalk actually renames
  # something; on an identity crosswalk it would repeat Field down the page.
  renamed <- any(delivered$YOUR_COLUMN != delivered$FIELD)
  if (renamed) {
    tibble::tibble(FIELD = field, YOUR_COLUMN = delivered$YOUR_COLUMN,
                   USED_FOR = used_for)
  } else {
    tibble::tibble(FIELD = field, USED_FOR = used_for)
  }
}

#' @rdname check1_tables
#' @export
ft_check1_data_fields <- function(results) {
  stopifnot(is.list(results), !is.null(results$meta$data_fields))
  out <- .check1_fields_display(results)

  ft <- flextable::flextable(as.data.frame(out))
  ft <- flextable::set_header_labels(ft, FIELD = "Field",
                                     YOUR_COLUMN = "Your column",
                                     USED_FOR = "Used for")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::bold(ft, part = "header")
  flextable::autofit(ft)
}

#' @rdname check1_tables
#' @export
check1_derived_note <- function(results) {
  stopifnot(is.list(results), !is.null(results$meta$data_fields))
  fields <- results$meta$data_fields
  derived <- fields$FIELD[fields$TABLE == "derived"]
  if (!length(derived)) return(NA_character_)

  label <- ifelse(derived == "A_TO_F",
                  paste0("the life table's A", .EN_DASH, "F"), derived)
  paste0("Derived by the package, not read from the delivery: ",
         .and_list(label),
         ". These are recomputed from EL_STATUS transitions every run, so a ",
         "delivered column of the same name is not used.")
}

.and_list <- function(x) {
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  paste0(paste(utils::head(x, -1L), collapse = ", "), " and ", utils::tail(x, 1L))
}

#' @rdname check1_tables
#' @export
ft_check1_life_table <- function(results) {
  stopifnot(is.list(results), !is.null(results$life_table))
  lt <- results$life_table

  row_label <- ifelse(lt$GRADE %in% "K", "K",
                      ifelse(is.na(lt$GRADE), "?", paste0("G", lt$GRADE)))
  out <- data.frame(
    ROW = paste0(row_label, " (Y", lt$YEAR_IN_STATUS, ")"),
    A = lt$A, B = lt$B, C = lt$C, D = lt$D, E = lt$E, F = lt$F,
    check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft,
    ROW = "Grade (year in status)",
    A = "A\nEnrolled at year's beginning",
    B = "B\nExited at year's end",
    C = "C\nDisappeared at year's end",
    D = "D\nAnnual exit rate (B/A)",
    E = "E\nAnnual censor rate (C/A)",
    F = paste0("F\nCumulative exit rate (", .SIGMA, "B/A", .SUB_ONE, ")"))
  ft <- flextable::colformat_num(ft, j = c("A", "B", "C"), big.mark = ",",
                                 na_str = .EM_DASH)
  ft <- flextable::colformat_double(ft, j = c("D", "E", "F"), digits = 2,
                                    na_str = .EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = c("A", "B", "C", "D", "E", "F"),
                         align = "right", part = "all")
  flextable::autofit(ft)
}

#' @rdname check1_tables
#' @export
ft_check1_cohorts <- function(results) {
  stopifnot(is.list(results), !is.null(results$cohorts))
  co <- results$cohorts
  out <- data.frame(
    COHORT_LABEL = co$COHORT_LABEL,
    N_ENTRY = co$N_ENTRY,
    FOLLOWUP_YEARS = co$FOLLOWUP_YEARS,
    SELECTED = ifelse(co$SELECTED, "Selected",
                      ifelse(co$SUPPRESSED, "Below minimum cell size", "")),
    check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(ft,
                                     COHORT_LABEL = "Cohort",
                                     N_ENTRY = "Students at entry",
                                     FOLLOWUP_YEARS = "Years of follow-up",
                                     SELECTED = "")
  ft <- flextable::colformat_num(ft, j = "N_ENTRY", big.mark = ",",
                                 na_str = .EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, i = which(co$SELECTED))
  flextable::autofit(ft)
}

# One column rather than two, because the pair is read as an interval and
# splitting it across columns invites reading a bound on its own. A row whose
# estimate is suppressed, or whose variance ran out with its risk set, shows a
# dash.
.check1_ci_text <- function(lo, hi) {
  ifelse(is.na(lo) | is.na(hi), .EM_DASH,
         sprintf("%.2f%s%.2f", lo, .EN_DASH, hi))
}

#' @rdname check1_tables
#' @export
ft_check1_survival <- function(results) {
  stopifnot(is.list(results), !is.null(results$survival_est))
  s <- results$survival_est
  out <- data.frame(
    ROW = paste0("Y", s$YEAR_IN_STATUS),
    A = s$A,
    HAZARD = s$HAZARD,
    CUM_EXIT_OBS = s$CUM_EXIT_OBS,
    CUM_EXIT_EST = s$CUM_EXIT_EST,
    CI = .check1_ci_text(s$CI_LO, s$CI_HI),
    check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft,
    ROW = "Year in status",
    A = "At risk at year's start",
    HAZARD = "Exit rate that year (D)",
    CUM_EXIT_OBS = "Exited, against the starting cohort (F)",
    CUM_EXIT_EST = "Exited, at-risk estimate",
    CI = "95% CI on the estimate")
  ft <- flextable::colformat_num(ft, j = "A", big.mark = ",",
                                 na_str = .EM_DASH)
  ft <- flextable::colformat_double(ft, j = c("HAZARD", "CUM_EXIT_OBS",
                                              "CUM_EXIT_EST"),
                                    digits = 2, na_str = .EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = c("A", "HAZARD", "CUM_EXIT_OBS",
                                   "CUM_EXIT_EST", "CI"),
                         align = "right", part = "all")
  flextable::autofit(ft)
}

#' @rdname check1_tables
#' @export
ft_check1_timeline <- function(results) {
  stopifnot(is.list(results), !is.null(results$at_max_timeline))
  d <- results$at_max_timeline
  out <- data.frame(OUTCOME = d$OUTCOME, N = d$N, PCT = d$PCT,
                    check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(ft, OUTCOME = "Outcome by the window's end",
                                     N = "Students", PCT = "Share of cohort")
  ft <- flextable::colformat_num(ft, j = "N", big.mark = ",",
                                 na_str = .EM_DASH)
  ft <- flextable::colformat_double(ft, j = "PCT", digits = 3,
                                    na_str = .EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  # The nested rows are a different kind of statement from the partition:
  # rule them off so nobody adds the column up.
  nested <- which(d$NESTED)
  if (length(nested)) {
    ft <- flextable::italic(ft, i = nested, j = "OUTCOME")
  }
  flextable::autofit(ft)
}

#' @rdname check1_tables
#' @export
ft_check1_model2 <- function(results) {
  stopifnot(is.list(results), !is.null(results$model2))
  m <- results$model2
  .check1_ft_model(m, .check1_term_labels(m))
}

#' @rdname check1_tables
#' @export
ft_check1_model2_dropped <- function(results) {
  stopifnot(is.list(results), !is.null(results$model2))
  .check1_ft_dropped(results$model2$dropped)
}

#' @rdname check1_tables
#' @export
ft_check1_model1 <- function(results) {
  stopifnot(is.list(results), !is.null(results$model1_baseline))
  m <- results$model1_baseline
  .check1_ft_model(m, .check1_term_labels(m))
}

#' @rdname check1_tables
#' @export
ft_check1_model1_dropped <- function(results) {
  stopifnot(is.list(results), !is.null(results$model1_baseline))
  .check1_ft_dropped(results$model1_baseline$dropped)
}

# All three fits render the same way, and they are meant to be read as a
# sequence, so they share the table rather than resembling each other.
.check1_term_labels <- function(m) {
  c(INITIAL_ELP_Z = "ELP score at entry",
    ACADEMIC_LAG_Z = paste("Prior-year", m$subject, "score"),
    CRITERION_Z = "Exit-rule criterion, same year")
}

.check1_ft_model <- function(m, labels) {
  cf <- m$coefficients[m$coefficients$FOCAL, , drop = FALSE]
  label <- unname(labels[cf$TERM])
  label[is.na(label)] <- cf$TERM[is.na(label)]

  out <- data.frame(
    TERM = label, RATIO = cf$RATIO, CI_LO = cf$CI_LO, CI_HI = cf$CI_HI,
    P_VALUE = cf$P_VALUE, check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft, TERM = "Covariate (standardised)", RATIO = "Ratio",
    CI_LO = "95% CI, low", CI_HI = "95% CI, high", P_VALUE = "p")
  ft <- flextable::colformat_double(ft, j = c("RATIO", "CI_LO", "CI_HI"),
                                    digits = 3, na_str = .EM_DASH)
  ft <- flextable::colformat_double(ft, j = "P_VALUE", digits = 3,
                                    na_str = .EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  flextable::autofit(ft)
}

.check1_ft_dropped <- function(d) {
  out <- data.frame(REASON = d$REASON, N = d$N, check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(ft, REASON = "Rows not used, and why",
                                     N = "Person-years")
  ft <- flextable::colformat_num(ft, j = "N", big.mark = ",",
                                 na_str = .EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  flextable::autofit(ft)
}

#' @rdname check1_tables
#' @export
ft_check1_grade_weights <- function(results) {
  stopifnot(is.list(results), !is.null(results$grade_weights))
  w <- results$grade_weights
  out <- data.frame(
    ROW = paste0("Y", w$YEAR_IN_STATUS),
    RETENTION_SHARE = w$RETENTION_SHARE,
    WEIGHT = w$WEIGHT,
    check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft, ROW = "Year in status",
    RETENTION_SHARE = "Retention share (still at risk)",
    WEIGHT = "Candidate weight (EXPERIMENTAL)")
  ft <- flextable::colformat_double(ft, j = c("RETENTION_SHARE", "WEIGHT"),
                                    digits = 2, na_str = .EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  flextable::autofit(ft)
}

#' @rdname check1_tables
#' @export
check1_model2_note <- function(results) {
  m <- results$model2
  if (is.null(m)) return(results$meta$model2_note)
  paste(c(.check1_fitted_on(m), m$note, .check1_separation_note(m)),
        collapse = " ")
}

#' @rdname check1_tables
#' @export
check1_model1_note <- function(results) {
  m <- results$model1
  if (is.null(m)) return(results$meta$model1_note)
  paste(c(.check1_fitted_on(m), m$note, .check1_separation_note(m)),
        collapse = " ")
}

#' @rdname check1_tables
#' @export
check1_model1_baseline_note <- function(results) {
  m <- results$model1_baseline
  if (is.null(m)) return(results$meta$model1_note)
  paste(c(.check1_fitted_on(m), m$note, .check1_separation_note(m)),
        collapse = " ")
}

# The student count leads: it is the number of people the estimate is about,
# and the rows are an artifact of writing one of them per year.
.check1_fitted_on <- function(m) {
  sprintf(paste0("Fitted on %s students, one row per year at risk (%s rows), ",
                 "carrying %s exits. Intervals are clustered on the student, ",
                 "so they count students rather than rows."),
          format(m$n_students, big.mark = ","),
          format(m$n_person_periods, big.mark = ","),
          format(m$n_events, big.mark = ","))
}

# Separation is no longer expected -- the covariates are all fixed at entry or
# lagged, so none of them is the exit decision restated -- but a delivery
# could still produce it, and silence would be the wrong way to find out.
.check1_separation_note <- function(m) {
  if (!isTRUE(m$quasi_separated)) return(NULL)
  paste0("The fitted probabilities reach 0 and 1, which means a covariate is ",
         "deciding the outcome almost perfectly. Its ratio is not ",
         "interpretable. Check whether that covariate is, or stands in for, ",
         "the rule being used to exit students.")
}

#' @rdname check1_tables
#' @export
check1_nested_note <- function(results) {
  m1 <- results$model1_baseline
  m2 <- results$model2
  if (is.null(m1) || is.null(m2)) return(NA_character_)

  elp1 <- m1$coefficients$RATIO[match("INITIAL_ELP_Z", m1$coefficients$TERM)]
  elp2 <- m2$coefficients$RATIO[match("INITIAL_ELP_Z", m2$coefficients$TERM)]
  if (is.na(elp1) || is.na(elp2)) return(NA_character_)

  # The two fits do not run on the same rows -- Model 2 needs a prior-year
  # content score and the model before it does not -- so this is a comparison
  # between two samples as much as between two models, and saying so is the
  # difference between a reading and a claim.
  out <- sprintf(paste0(
    "The entry-score ratio is %s on its own and %s once prior achievement ",
    "joins it. The two fits do not use the same rows: Model 2 keeps only the ",
    "%s rows that carry a prior-year score, against %s. Read the movement as ",
    "a direction, not a measurement."),
    .check1_ratio_text(elp1), .check1_ratio_text(elp2),
    format(m2$n_person_periods, big.mark = ","),
    format(m1$n_person_periods, big.mark = ","))

  if (isTRUE(m1$quasi_separated) || isTRUE(m2$quasi_separated)) {
    out <- paste(out, paste0(
      "One of the fits separates, so the ratio it reports is not a quantity ",
      "and neither is the movement between them."))
  }
  out
}

# A ratio that has run away from separation should not be dressed up in two
# decimal places as though it were measured.
.check1_ratio_text <- function(x) {
  if (!is.finite(x)) return("not estimable")
  if (abs(x) >= 1000) return(format(round(x), big.mark = ",", scientific = FALSE))
  sprintf("%.2f", x)
}
