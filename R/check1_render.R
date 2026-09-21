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
    check.names = FALSE)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft,
    ROW = "Year in status",
    A = "At risk at year's start",
    HAZARD = "Exit rate that year (D)",
    CUM_EXIT_OBS = "Exited, against the starting cohort (F)",
    CUM_EXIT_EST = "Exited, at-risk estimate")
  ft <- flextable::colformat_num(ft, j = "A", big.mark = ",",
                                 na_str = .EM_DASH)
  ft <- flextable::colformat_double(ft, j = c("HAZARD", "CUM_EXIT_OBS",
                                              "CUM_EXIT_EST"),
                                    digits = 2, na_str = .EM_DASH)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = c("A", "HAZARD", "CUM_EXIT_OBS",
                                   "CUM_EXIT_EST"),
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
  cf <- m$coefficients[m$coefficients$FOCAL, , drop = FALSE]

  label <- ifelse(cf$TERM == "ACADEMIC_LAG_Z",
                  paste("Prior-year", m$subject, "score"),
                  ifelse(cf$TERM == "ELP_Z", "ELP score that year",
                         "Exit-rule criterion, same year"))
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

#' @rdname check1_tables
#' @export
ft_check1_model2_dropped <- function(results) {
  stopifnot(is.list(results), !is.null(results$model2))
  d <- results$model2$dropped
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

  parts <- c(sprintf(paste0("Fitted on %s person-years from %s students, ",
                            "carrying %s exits."),
                     format(m$n_person_periods, big.mark = ","),
                     format(m$n_students, big.mark = ","),
                     format(m$n_events, big.mark = ",")),
             m$note)
  if (isTRUE(m$quasi_separated)) {
    parts <- c(parts, paste0(
      "The model's fitted probabilities reach 0 and 1, which happens when a ",
      "covariate almost perfectly decides the outcome. Here that is the ELP ",
      "score, because exit in this data is a rule applied to it; its ratio ",
      "is therefore not interpretable, while the prior-achievement ratio -- ",
      "the question this model exists to answer -- still is."))
  }
  paste(parts, collapse = " ")
}
