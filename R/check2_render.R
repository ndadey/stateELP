# Check 2 -- presentation. These take a computed results object and return a
# flextable or a sentence; they never compute anything themselves, and no
# student-level data is in scope (CLAUDE.md 8).

#' Check 2 tables
#'
#' Flextables built from [run_check2()]'s results. The document calls these;
#' it builds no tables of its own.
#'
#' @param results Output of [run_check2()].
#' @return `ft_check2_crosstab()` a `flextable`.
#' @name check2_tables
NULL

# Header text: the deck's column names, each with the plain-language reading of
# the indicator underneath. "Exit_Ever" etc. are the deck's spellings and what
# a state's analysts will recognise; the second line says what they mean.
.check2_header_labels <- function() {
  c(ALL         = "All EL students",
    EXIT_EVER   = "Exit_Ever\nExited in the years observed",
    EXIT_MAX    = "Exit_Max\nExited within the maximum timeline",
    EXIT_ONTIME = "Exit_OnTime\nExited within the on-time timeline",
    NO_EXIT     = "No_Exit\nNot observed exiting by the window's end",
    DISAPPEARED = "Disappeared\nLeft the data before the window closed")
}

# What the table shows, as data. Split from the flextable so a test can read
# the rendered content without reaching into flextable's internals. A
# suppressed cell is an em dash, never a blank and never a zero.
.check2_crosstab_display <- function(results) {
  ct <- results$crosstab
  total <- ct[ct$BLOCK == "Total", , drop = FALSE]
  at <- match(.CHECK2_COLUMNS, total$COLUMN)

  n   <- total$N[at]
  pct <- total$PCT[at]
  n_txt   <- ifelse(is.na(n), .CHECK2_EM_DASH,
                    formatC(n, format = "d", big.mark = ","))
  pct_txt <- ifelse(is.na(pct), .CHECK2_EM_DASH, sprintf("%.0f%%", 100 * pct))

  out <- data.frame(ROW = c("Total students", "% of all EL students"),
                    stringsAsFactors = FALSE)
  for (i in seq_along(.CHECK2_COLUMNS)) {
    out[[.CHECK2_COLUMNS[[i]]]] <- c(n_txt[[i]], pct_txt[[i]])
  }
  out
}

#' @rdname check2_tables
#' @export
ft_check2_crosstab <- function(results) {
  stopifnot(is.list(results), !is.null(results$crosstab))
  out <- .check2_crosstab_display(results)

  ft <- flextable::flextable(out)
  ft <- do.call(flextable::set_header_labels,
                c(list(ft, ROW = ""), as.list(.check2_header_labels())))
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, j = "ROW", part = "body")
  ft <- flextable::valign(ft, valign = "top", part = "header")
  ft <- flextable::align(ft, j = .CHECK2_COLUMNS, align = "right", part = "all")
  flextable::autofit(ft)
}
