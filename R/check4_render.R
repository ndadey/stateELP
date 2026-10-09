# Check 4 -- tables and the sentences that read them. Built from run_check4()'s
# results; they never compute anything themselves, and no student-level data
# is in scope (CLAUDE.md 8).

# R CMD check requires the sources themselves to be ASCII, so the dash a
# suppressed cell renders as is built from its code point.
.CHECK4_EM_DASH <- intToUtf8(0x2014)

#' Check 4 tables
#'
#' Flextables built from [run_check4()]'s results, and the sentences that say
#' what the numbers mean.
#'
#' @param results Output of [run_check4()].
#' @param subject,grade Which panel to show, defaulting to the first.
#' @return `ft_check4_*()` a `flextable`; the `check4_*_note()` functions a
#'   string, or `NA` when the slot they read is absent.
#' @name check4_tables
NULL

#' @rdname check4_tables
#' @export
ft_check4_optimum <- function(results) {
  stopifnot(is.list(results), !is.null(results$j_optimal))
  out <- .check4_optimum_display(results$j_optimal)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft, SUBJECT = "Subject", GRADE = "Grade", N = "n",
    BASE_RATE = "Proficient", J_CUT = "J optimum", J = "J there",
    AGREE_CUT = "Highest reportable\nagreement", AGREE = "Agreement there",
    THRESHOLD = "Current rule", J_AT_THRESHOLD = "J at the rule")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::valign(ft, valign = "bottom", part = "header")
  ft <- flextable::align(ft, j = setdiff(names(out), c("SUBJECT", "GRADE")),
                         align = "right", part = "all")
  flextable::autofit(ft)
}

#' @rdname check4_tables
#' @export
ft_check4_curve <- function(results, subject = NULL, grade = NULL) {
  stopifnot(is.list(results), !is.null(results$youden_j))
  out <- .check4_curve_display(results$youden_j, results$j_optimal,
                               subject, grade)

  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(
    ft, CUT = "Cut (at or above)",
    TP = "Proficient,\nat/above", FP = "Not proficient,\nat/above",
    FN = "Proficient,\nbelow", TN = "Not proficient,\nbelow",
    AGREE = "Agreement", SENSITIVITY = "Sensitivity",
    SPECIFICITY = "Specificity", J = "J", MARK = "")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::valign(ft, valign = "bottom", part = "header")
  ft <- flextable::align(ft, j = setdiff(names(out), c("CUT", "MARK")),
                         align = "right", part = "all")
  # The row the stage reports.
  hit <- which(grepl("J optimum", out$MARK, fixed = TRUE))
  if (length(hit)) ft <- flextable::bold(ft, i = hit, part = "body")
  flextable::autofit(ft)
}

#' @rdname check4_tables
#' @export
check4_optimum_note <- function(results) {
  o <- results$j_optimal
  if (is.null(o) || !nrow(o)) return(NA_character_)
  o <- o[1, , drop = FALSE]
  where <- paste0("grade ", o$GRADE, " ", o$SUBJECT)
  reason <- o$NO_OPTIMUM

  if (identical(reason, "suppressed") && isTRUE(o$SUPPRESSED)) {
    return(paste0(
      "Every candidate cut in ", where, " would disclose a group smaller ",
      "than the suppression threshold, so no optimum is reported."))
  }
  share <- paste0(
    "In ", where, ", ", .check4_fmt_pct(o$BASE_RATE), " of the ",
    .check4_fmt_n(o$N), " current ELs reached proficiency.")
  if (identical(reason, "suppressed")) {
    return(paste(
      share, "Every cut that could separate proficient from not-proficient",
      "students would disclose a group smaller than the suppression",
      "threshold, so no optimum is reported -- which says nothing about",
      "whether the level separates them."))
  }
  if (identical(reason, "no_variation")) {
    return(paste(
      share, "With", if (o$BASE_RATE == 0) "no student" else "every student",
      paste0("proficient, the two groups J compares do not both exist, so J ",
             "is undefined at every cut and no optimum is reported.")))
  }
  if (identical(reason, "uninformative")) {
    return(paste(
      share, "No candidate cut separates proficient from not-proficient",
      "students -- J is zero or below at every cut -- so no optimum is",
      "reported."))
  }

  best <- paste0(
    "Youden's J is highest",
    if (isTRUE(o$AGREE_MAX_HIDDEN)) ", among the cuts that can be shown," else
      "",
    " at ", o$J_CUT_LABEL, " (J = ", .check4_fmt_stat(o$J), ")",
    if (!is.na(o$N_J_TIED) && o$N_J_TIED > 1L) {
      paste0(", tied with ", o$N_J_TIED - 1L, " other cut",
             if (o$N_J_TIED > 2L) "s" else "", " -- the lowest is reported")
    } else "", ".",
    if (isTRUE(o$J_MAX_HIDDEN)) {
      paste0(" Suppressed cuts sit right beside it, so the optimum is ",
             "located only to within ",
             if (!is.na(o$J_SHOWN_BELOW) && !is.na(o$J_SHOWN_ABOVE)) {
               paste0("the range from ", o$J_SHOWN_BELOW, " to ",
                      o$J_SHOWN_ABOVE, ", the cuts shown either side")
             } else if (!is.na(o$J_SHOWN_BELOW)) {
               paste0("the range above ", o$J_SHOWN_BELOW,
                      ", where no cut is shown")
             } else if (!is.na(o$J_SHOWN_ABOVE)) {
               paste0("the range below ", o$J_SHOWN_ABOVE,
                      ", where no cut is shown")
             } else {
               "the suppressed range around it"
             }, ".")
    } else "")

  # Classifying every student not ready agrees with the content test for every
  # non-proficient student. Agreement's peak is read against that floor, which
  # it can fail to clear at all.
  floor_text <- if (!is.na(o$AGREE_FLOOR) && !is.na(o$AGREE) &&
                    o$AGREE <= o$AGREE_FLOOR + .CHECK4_TOL) {
    paste0("no better than simply calling every student not ready, which ",
           "agrees ", .check4_fmt_pct(o$AGREE_FLOOR), " of the time")
  } else {
    paste0("against ", .check4_fmt_pct(o$AGREE_FLOOR), " for simply ",
           "calling every student not ready")
  }
  agree <- if (isTRUE(o$OPTIMA_DIVERGE)) {
    paste0(
      "Raw agreement's highest reportable point is elsewhere, at ",
      o$AGREE_CUT_LABEL, " (", .check4_fmt_pct(o$AGREE), "), ", floor_text,
      ". That is the base-rate pull, and it is why the J optimum, not the ",
      "agreement peak, is this stage's optimal-cut statistic.")
  } else {
    paste0("Raw agreement's highest reportable point is the same cut (",
           .check4_fmt_pct(o$AGREE), "), ", floor_text, ".")
  }
  if (isTRUE(o$AGREE_MAX_HIDDEN)) {
    agree <- paste(agree, "Some cuts are suppressed, so agreement's true",
                   "maximum may lie among them.")
  }

  paste(share, best, agree, .check4_rule_sentence(o, results$meta))
}

#' @rdname check4_tables
#' @export
check4_tolerance_note <- function(results) {
  o <- results$j_optimal
  has_o <- !is.null(o) && nrow(o) > 0L
  ce <- if (has_o) o$COSTLIER_ERROR[[1]] else results$meta$costlier_error
  if (is.null(ce)) return(NA_character_)
  if (is.na(ce)) ce <- "unknown"

  has_opt <- has_o && !is.na(o$J_CUT_LABEL[[1]])
  ratio <- if (has_o) o$IMPLIED_COST_RATIO[[1]] else NA_real_
  lean <- if (has_o) o$LEAN[[1]] else NA_character_
  who <- results$meta$error_tolerance
  # Who was asked is naturally plural; collapse before testing.
  flat <- function(x) paste(stats::na.omit(as.character(x)), collapse = ", ")
  asked_of <- flat(who$asked_of)
  asked_date <- flat(who$asked_date)
  asked <- if (nzchar(asked_of)) {
    paste0(" (asked of ", asked_of,
           if (nzchar(asked_date)) paste0(", ", asked_date) else "", ")")
  } else ""
  answer <- switch(
    ce,
    premature_exit = paste0("Stakeholders judged a premature exit the ",
                            "costlier error", asked, "."),
    prolonged_retention = paste0("Stakeholders judged prolonged retention the ",
                                 "costlier error", asked, "."),
    equal = paste0("Stakeholders judged the two errors equally costly per ",
                   "student", asked, "."),
    paste("Which error costs more is still an open question: Prep 3 has not",
          "recorded an answer."))

  # What J weights, said once in every branch: equal error *rates*, which per
  # student is a hold priced at `ratio` exits. Prep 3 asks per student.
  rates <- paste(
    "Youden's J weights the two errors equally as rates -- the share of",
    "proficient students held back and the share of not-proficient students",
    "exited -- however common each group is.")

  # No optimum in this panel (or no base rate to price it by): record the
  # answer, and say there is nothing to place it against.
  if (!has_opt || is.na(ratio)) {
    return(paste(
      answer, rates, "No J optimum is reported for this panel, so there is",
      if (identical(ce, "unknown")) {
        "nothing here to place an answer against once Prep 3 records one."
      } else {
        "nothing here to place that answer against."
      }))
  }

  # Near an even trade, one decimal would round the ratio to 1.0 and hide
  # which side of it the panel sits.
  near_even <- abs(ratio - 1) < 0.1
  r_txt <- formatC(ratio, format = "f", digits = if (near_even) 2 else 1)
  per_student <- paste0(
    "Per student that is ",
    if (abs(ratio - 1) < 0.05) "close to an even trade" else
      "not an even trade",
    ": with ", .check4_fmt_pct(o$BASE_RATE[[1]]), " of these students ",
    "proficient, the J optimum of ", o$J_CUT_LABEL[[1]], " weighs holding ",
    "back one proficient student as heavily as exiting ", r_txt, " who are ",
    "not.")
  values <- "-- a values input, not a departure from the evidence."
  land <- function(side, other) {
    paste("That answer is a documented reason to land the threshold", side,
          "the J optimum rather than", other, "it", values)
  }

  verdict <- switch(
    ce,
    premature_exit = if (identical(lean, "above")) land("above", "below") else
      paste("With most of these students proficient, J already prices a",
            "premature exit above a prolonged hold, so which side of the J",
            "optimum the answer points to depends on how much costlier it is",
            "judged."),
    prolonged_retention = if (identical(lean, "below")) {
      land("below", "above")
    } else {
      paste0(
        "So the answer points below the J optimum only if a prolonged hold ",
        "is judged more than about ", r_txt, " times as costly as a ",
        "premature exit, and above it otherwise; Prep 3 would need to record ",
        "how much costlier to say which.")
    },
    equal = switch(
      lean %||% "none",
      above = paste(
        land("above", "below"),
        if (!isTRUE(o$OPTIMA_DIVERGE[[1]]) && !is.na(o$AGREE[[1]]) &&
            o$AGREE[[1]] <= o$AGREE_FLOOR[[1]] + .CHECK4_TOL) {
          paste("Equal per-student costs point toward classifying nobody",
                "ready, which agrees with the content test more often than",
                "any cut.")
        } else {
          paste("Equal per-student costs point toward the cut where raw",
                "agreement is highest.")
        }),
      below = land("below", "above"),
      at = paste("Here equal per-student costs land on the J optimum itself:",
                 "it is also the cut where raw agreement is highest."),
      ""),
    paste("If stakeholders price the two errors differently from that, it is",
          "a documented reason to land the threshold deliberately above or",
          "below the J optimum", values))

  paste(answer, rates, per_student, verdict)
}

# The sentence placing the current rule against the J optimum, by the cut the
# rule classifies as rather than the value typed in the spec.
.check4_rule_sentence <- function(o, meta) {
  if (is.na(o$THRESHOLD)) {
    return("No current threshold sits on this axis to compare.")
  }
  rule <- paste(.check4_op_text(o$THRESHOLD_OP), o$THRESHOLD_LABEL)
  name <- if (isTRUE(o$RULE_IS_TERM)) {
    paste0("The current rule's ", meta$component_label %||% "ELP",
           " term (", rule, ")")
  } else if (isTRUE(o$RULE_IS_ROUTE)) {
    paste0("The current rule's ", meta$component_label %||% "ELP",
           " route (", rule, ")")
  } else {
    paste0("The current rule (", rule, ")")
  }
  where <- switch(
    o$THRESHOLD_SIDE %||% "none",
    above = "sits above the J optimum",
    below = "sits below the J optimum",
    within = paste("falls inside the suppressed range around the J optimum,",
                   "so which side of it the rule sits cannot be said"),
    same = if (!is.na(o$J_CUT) &&
               abs(o$THRESHOLD - o$J_CUT) < .CHECK4_TOL &&
               identical(o$THRESHOLD_OP, ">=")) {
      "sits at the J optimum"
    } else {
      paste("classifies exactly as the J optimum: no observed level lies",
            "between them")
    },
    "cannot be compared with an optimum")
  j_txt <- if (o$THRESHOLD_SIDE %in% c("same", "within")) {
    "."
  } else if (!is.na(o$THRESHOLD_CUT) && is.na(o$J_AT_THRESHOLD)) {
    paste(", and the cut it classifies as is suppressed, so J there is not",
          "reported.")
  } else {
    paste0(", where J = ", .check4_fmt_stat(o$J_AT_THRESHOLD), ".")
  }
  tail <- if (isTRUE(o$RULE_IS_TERM)) {
    paste(" It is one criterion of several; how the rule performs as a",
          "whole is the Additional check below.")
  } else if (isTRUE(o$RULE_IS_ROUTE)) {
    paste(" It is one route of several -- students below it can also exit",
          "another way; how the rule performs as a whole is the Additional",
          "check below.")
  } else ""
  paste0(name, " ", where, j_txt, tail)
}

# ---- display helpers ---------------------------------------------------------

# Display data, split from the flextable so a test can read what renders
# without reaching into flextable's internals.
.check4_optimum_display <- function(o) {
  data.frame(
    SUBJECT        = o$SUBJECT,
    GRADE          = as.character(o$GRADE),
    N              = .check4_fmt_n(o$N),
    BASE_RATE      = .check4_fmt_pct(o$BASE_RATE),
    J_CUT          = .check4_dash(o$J_CUT_LABEL),
    J              = .check4_fmt_stat(o$J),
    AGREE_CUT      = .check4_dash(o$AGREE_CUT_LABEL),
    AGREE          = .check4_fmt_pct(o$AGREE),
    THRESHOLD      = .check4_dash(ifelse(
      is.na(o$THRESHOLD_LABEL), NA_character_,
      paste(.check4_op_text(o$THRESHOLD_OP), o$THRESHOLD_LABEL))),
    J_AT_THRESHOLD = .check4_fmt_stat(o$J_AT_THRESHOLD),
    check.names = FALSE, stringsAsFactors = FALSE)
}

.check4_curve_display <- function(y, o, subject = NULL, grade = NULL) {
  y <- .c4_one_panel(y, subject, grade)
  opt <- .c4_panel_optimum(o, y)
  mark <- rep("", nrow(y))
  add <- function(mark, i, text) {
    if (length(i) && !is.na(i)) {
      mark[i] <- ifelse(nzchar(mark[i]), paste(mark[i], text, sep = "; "),
                        text)
    }
    mark
  }
  if (!is.null(opt)) {
    mark <- add(mark, match(opt$J_CUT, y$CUT), "J optimum")
    mark <- add(mark, match(opt$AGREE_CUT, y$CUT),
                "highest reportable agreement")
    mark <- add(mark, match(opt$THRESHOLD_CUT, y$CUT), "current rule")
  }
  data.frame(
    CUT         = .check4_dash(y$CUT_LABEL),
    TP          = .check4_fmt_n(y$TP),
    FP          = .check4_fmt_n(y$FP),
    FN          = .check4_fmt_n(y$FN),
    TN          = .check4_fmt_n(y$TN),
    AGREE       = .check4_fmt_pct(y$AGREE),
    SENSITIVITY = .check4_fmt_stat(y$SENSITIVITY),
    SPECIFICITY = .check4_fmt_stat(y$SPECIFICITY),
    J           = .check4_fmt_stat(y$J),
    MARK        = mark,
    check.names = FALSE, stringsAsFactors = FALSE)
}

# Formatted as text, so a table reads the same in Word and HTML.
.check4_fmt_n <- function(n) {
  ifelse(is.na(n), .CHECK4_EM_DASH,
         formatC(as.numeric(n), format = "d", big.mark = ","))
}
.check4_fmt_pct <- function(p) {
  ifelse(is.na(p), .CHECK4_EM_DASH,
         paste0(formatC(100 * p, format = "f", digits = 0), "%"))
}
.check4_fmt_stat <- function(x, digits = 2) {
  ifelse(is.na(x), .CHECK4_EM_DASH, formatC(x, format = "f", digits = digits))
}
.check4_dash <- function(x) ifelse(is.na(x), .CHECK4_EM_DASH, x)

# The rule's comparison as a reader writes it.
.check4_op_text <- function(op) {
  ifelse(op %in% ">=", intToUtf8(0x2265), ifelse(op %in% ">", ">", ""))
}
