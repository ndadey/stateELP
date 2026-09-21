# Check 1 -- presentation. These take a computed results object and return a
# flextable or a sentence; they never compute anything themselves, and no
# student-level data is in scope (CLAUDE.md 8).
#
# `ft_*()` is a new naming branch for table builders, alongside plot_*() for
# ggplots: Word is the primary format and every table in it is a flextable.
# `[PROPOSED - CONFIRM]`

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
  delivered <- fields[fields$TABLE == "roster", , drop = FALSE]

  used_for <- ifelse(
    delivered$KIND == "optional" & !delivered$PRESENT,
    paste0("Not delivered — ", delivered$FALLBACK),
    ifelse(delivered$KIND == "optional",
           paste0(delivered$USED_FOR, " (optional)"),
           delivered$USED_FOR))

  # Your column earns a column only when the crosswalk actually renames
  # something; on an identity crosswalk it would repeat Field down the page.
  renamed <- any(delivered$YOUR_COLUMN != delivered$FIELD)
  if (renamed) {
    tibble::tibble(FIELD = delivered$FIELD,
                   YOUR_COLUMN = delivered$YOUR_COLUMN,
                   USED_FOR = used_for)
  } else {
    tibble::tibble(FIELD = delivered$FIELD, USED_FOR = used_for)
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

  label <- ifelse(derived == "A_TO_F", "the life table's A–F", derived)
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
    F = "F\nCumulative exit rate (ΣB/A₁)")
  ft <- flextable::colformat_num(ft, j = c("A", "B", "C"), big.mark = ",",
                                 na_str = "—")
  ft <- flextable::colformat_double(ft, j = c("D", "E", "F"), digits = 2,
                                    na_str = "—")
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
                                 na_str = "—")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, i = which(co$SELECTED))
  flextable::autofit(ft)
}
