# Check 2 -- presentation. These take a computed results object and return a
# flextable or a sentence; they never compute anything themselves, and no
# student-level data is in scope (CLAUDE.md 8).

#' Check 2 tables
#'
#' Flextables built from [run_check2()]'s results, and the sentences that
#' caption them. The document calls these; it builds no tables of its own.
#'
#' @param results Output of [run_check2()].
#' @return `ft_check2_*()` a `flextable`; `check2_*_note()` a string.
#' @name check2_tables
NULL

# ---- display labels ----------------------------------------------------------
#
# Coded roster values (`spa`, `esl_pullout`) and this stage's own exit
# categories (`on_time`) are what the slots carry; the words a reader sees come
# from inst/lookups/Display_Labels.R, which exists for exactly this and names
# `exit_category` as Check 2's vocabulary. Labelling happens here, at the edge:
# the compute slots stay in the delivery's own codes.
#
# The lookup's rule is that a value absent from it renders as itself, so a
# delivery using its own codes loses nothing and no category is ever silently
# dropped. A missing or unreadable lookup file is not an error either -- every
# value then renders as itself.
.check2_labels <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file("lookups", "Display_Labels.R", package = "stateELP")
    cache <<- if (nzchar(path) && file.exists(path)) {
      # baseenv(), not emptyenv(): the file is R code calling list() and c(),
      # which an empty parent would not resolve.
      env <- new.env(parent = baseenv())
      ok <- tryCatch({ sys.source(path, envir = env); TRUE },
                     error = function(e) FALSE)
      if (ok && !is.null(env$Display_Labels)) env$Display_Labels else list()
    } else list()
    cache
  }
})

# `keys` names the lookups to try, in order: the first that holds the value
# wins. A row block is labelled by its own field; the exit groups by
# `exit_category`.
.check2_label <- function(x, keys) {
  labels <- .check2_labels()
  out <- as.character(x)
  for (k in keys) {
    lk <- labels[[k]]
    if (is.null(lk)) next
    hit <- !is.na(out) & out %in% names(lk)
    out[hit] <- unname(lk[out[hit]])
  }
  out
}

.check2_group_labels <- function(x) .check2_label(x, "exit_category")

# A distribution block's rows are values of one roster column, so that column's
# lookup labels them; the yes/no lookup covers an indicator field delivered as
# a code. The block's own field travels in the slot, so nothing is guessed from
# the row text.
.check2_row_labels <- function(x, field) {
  if (is.na(field)) return(as.character(x))
  .check2_label(x, c(field, "yes_no"))
}

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

.check2_fmt_n <- function(n) {
  ifelse(is.na(n), .CHECK2_EM_DASH, formatC(n, format = "d", big.mark = ","))
}

.check2_fmt_pct <- function(p, digits = 0) {
  ifelse(is.na(p), .CHECK2_EM_DASH,
         paste0(formatC(100 * p, format = "f", digits = digits), "%"))
}

# What the crosstab shows, as data: one row per printed line, a column per exit
# outcome. Split from the flextable so a test can read the rendered content
# without reaching into flextable's internals.
#
# The Total block prints counts and the share of the cohort; every
# characteristic block prints column percentages, which is how the deck's
# template reads. `IS_HEADER` marks the block title lines and `LEVEL` the
# indent, both for the flextable to style.
.check2_crosstab_display <- function(results) {
  ct <- results$crosstab
  wide <- function(rows, fmt) {
    out <- data.frame(ROW = rows$ROW, stringsAsFactors = FALSE)
    for (cl in .CHECK2_COLUMNS) {
      at <- match(rows$ROW, ct$ROW[ct$BLOCK == rows$BLOCK[[1]] &
                                     ct$COLUMN == cl])
      sub <- ct[ct$BLOCK == rows$BLOCK[[1]] & ct$COLUMN == cl, ]
      out[[cl]] <- fmt(sub, at)
    }
    out$IS_HEADER <- FALSE
    out$LEVEL <- 1L
    out
  }

  total <- ct[ct$BLOCK == "Total", , drop = FALSE]
  at <- match(.CHECK2_COLUMNS, total$COLUMN)
  parts <- list(data.frame(
    ROW = c("Total students", "% of all EL students"),
    stringsAsFactors = FALSE))
  for (cl in seq_along(.CHECK2_COLUMNS)) {
    parts[[1]][[.CHECK2_COLUMNS[[cl]]]] <-
      c(.check2_fmt_n(total$N[at][[cl]]), .check2_fmt_pct(total$PCT[at][[cl]]))
  }
  parts[[1]]$IS_HEADER <- FALSE
  parts[[1]]$LEVEL <- 0L

  for (b in setdiff(unique(ct$BLOCK), "Total")) {
    rows <- unique(ct[ct$BLOCK == b, c("BLOCK", "FIELD", "ROW")])
    head_row <- data.frame(ROW = b, stringsAsFactors = FALSE)
    for (cl in .CHECK2_COLUMNS) head_row[[cl]] <- ""
    head_row$IS_HEADER <- TRUE
    head_row$LEVEL <- 0L
    parts[[length(parts) + 1L]] <- head_row

    body <- wide(rows, function(sub, at) .check2_fmt_pct(sub$PCT[at]))
    # Coded values become words here, never in the slot.
    body$ROW <- vapply(seq_len(nrow(rows)), function(i) {
      .check2_row_labels(rows$ROW[[i]], rows$FIELD[[i]])
    }, "")
    parts[[length(parts) + 1L]] <- body
  }
  do.call(rbind, parts)
}

#' @rdname check2_tables
#' @export
ft_check2_crosstab <- function(results) {
  stopifnot(is.list(results), !is.null(results$crosstab))
  out <- .check2_crosstab_display(results)
  header <- out$IS_HEADER
  indent <- out$LEVEL

  ft <- flextable::flextable(out[setdiff(names(out), c("IS_HEADER", "LEVEL"))])
  ft <- do.call(flextable::set_header_labels,
                c(list(ft, ROW = ""), as.list(.check2_header_labels())))
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::valign(ft, valign = "top", part = "header")
  ft <- flextable::align(ft, j = .CHECK2_COLUMNS, align = "right", part = "all")
  if (any(header)) ft <- flextable::bold(ft, i = which(header), j = "ROW")
  if (any(indent > 0L)) {
    ft <- flextable::padding(ft, i = which(indent > 0L), j = "ROW",
                             padding.left = 18)
  }
  ft <- flextable::bold(ft, i = which(indent == 0L & !header), j = "ROW")
  flextable::autofit(ft)
}

#' @rdname check2_tables
#' @export
ft_check2_academic <- function(results) {
  stopifnot(is.list(results), !is.null(results$academic_summary))
  d <- results$academic_summary

  # One label per year, not per row. A group with nobody left to measure has no
  # modal grade, and labelling its own row from that would print "Y9" beside
  # "Y9 (grade 8)" and defeat the merge that makes the year read once.
  grade_of <- tapply(d$GRADE_MODAL, d$YEAR_IN_STATUS,
                     function(g) {
                       g <- g[!is.na(g)]
                       if (length(g)) g[[1]] else NA_character_
                     })
  grade <- grade_of[as.character(d$YEAR_IN_STATUS)]

  out <- data.frame(
    YEAR_IN_STATUS = paste0("Y", d$YEAR_IN_STATUS,
                            ifelse(is.na(grade), "",
                                   paste0(" (grade ", grade, ")"))),
    EXIT_GROUP     = .check2_group_labels(d$EXIT_GROUP),
    N_SCORED       = .check2_fmt_n(d$N_SCORED),
    PCT_PROFICIENT = .check2_fmt_pct(d$PCT_PROFICIENT),
    MEDIAN_SCALE   = .check2_fmt_n(round(d$MEDIAN_SCALE)),
    stringsAsFactors = FALSE)
  out <- out[order(d$YEAR_IN_STATUS,
                   match(d$EXIT_GROUP, .CHECK2_GROUPS)), , drop = FALSE]

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft,
    YEAR_IN_STATUS = "Year in status",
    EXIT_GROUP     = "Exit group",
    N_SCORED       = "Students scored",
    PCT_PROFICIENT = "At or above the proficiency cut",
    MEDIAN_SCALE   = "Median scale score")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = c("N_SCORED", "PCT_PROFICIENT",
                                  "MEDIAN_SCALE"),
                         align = "right", part = "all")
  ft <- flextable::merge_v(ft, j = "YEAR_IN_STATUS")
  ft <- flextable::valign(ft, j = "YEAR_IN_STATUS", valign = "top")
  flextable::autofit(ft)
}

#' @rdname check2_tables
#' @export
ft_check2_composition <- function(results) {
  stopifnot(is.list(results), !is.null(results$trajectories$composition))
  comp <- results$trajectories$composition
  years <- sort(unique(comp$YEAR_IN_STATUS))
  groups <- intersect(.CHECK2_GROUPS, unique(comp$EXIT_GROUP))

  out <- data.frame(EXIT_GROUP = .check2_group_labels(groups),
                    stringsAsFactors = FALSE)
  for (y in years) {
    sub <- comp[comp$YEAR_IN_STATUS == y, ]
    out[[paste0("Y", y)]] <-
      .check2_fmt_pct(sub$PCT_OF_MEASURED[match(groups, sub$EXIT_GROUP)])
  }

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(ft, EXIT_GROUP = "Exit group")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, j = "EXIT_GROUP", part = "body")
  ft <- flextable::align(ft, j = setdiff(names(out), "EXIT_GROUP"),
                         align = "right", part = "all")
  flextable::autofit(ft)
}

#' @rdname check2_tables
#' @export
ft_check2_blocked_groups <- function(results) {
  stopifnot(is.list(results), !is.null(results$blocked_groups))
  bg <- results$blocked_groups
  rows <- unique(bg$BLOCKED_BY)

  out <- data.frame(BLOCKED_BY = rows, stringsAsFactors = FALSE)
  for (cl in .CHECK2_COLUMNS) {
    sub <- bg[bg$COLUMN == cl, ]
    out[[cl]] <- .check2_fmt_pct(sub$PCT[match(rows, sub$BLOCKED_BY)])
  }

  ft <- flextable::flextable(out)
  ft <- do.call(flextable::set_header_labels,
                c(list(ft, BLOCKED_BY = "Blocked by"),
                  as.list(.check2_header_labels())))
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::valign(ft, valign = "top", part = "header")
  ft <- flextable::align(ft, j = .CHECK2_COLUMNS, align = "right", part = "all")
  flextable::autofit(ft)
}

#' @rdname check2_tables
#' @export
check2_academic_note <- function(results) {
  stopifnot(is.list(results))
  d <- results$academic_summary
  if (is.null(d)) {
    return(paste0("No academic summary: the delivery carries no academic ",
                  "table, so there is nothing to compare at comparable ",
                  "points in each group's trajectory."))
  }
  years <- sort(unique(d$YEAR_IN_STATUS))
  missing <- setdiff(seq(min(years), max(years)), years)
  paste0(
    "Subject: ", results$meta$academic_subject, ". Proficiency is level ",
    results$meta$proficient_level, " or above on the state's own scale. Rows ",
    "run from year in status ", min(years), " to ", max(years),
    ": the content assessment does not reach this cohort's earlier grades, so ",
    "those years have no rows at all",
    if (length(missing)) {
      paste0(", and year", if (length(missing) > 1) "s " else " ",
             .check2_and_list(as.character(missing)),
             " ", if (length(missing) > 1) "are" else "is",
             " absent because no assessment was administered that year")
    } else "",
    ". A group with no students left to measure shows a count of zero, not a ",
    "suppressed cell.")
}

.check2_and_list <- function(x) {
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  paste0(paste(utils::head(x, -1L), collapse = ", "), " and ",
         utils::tail(x, 1L))
}
