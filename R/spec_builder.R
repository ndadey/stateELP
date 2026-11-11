# The spec builder: the intake form's questions, asked one at a time at the R
# console, written out as a spec file and validated.
#
# Adapted from a draft builder and intake form written during a state test
# run (October 2026). The question ids match inst/spec_builder/
# spec_intake_form.md, so a completed form can be typed in beside it.
# Changes from the draft: it lives in the package (no source() step), it asks
# for cut scores grade by grade when they differ, and it can be driven by a
# vector of scripted answers, which is how it is tested.
#
# Nothing here is analysis: it writes a file and calls read_el_spec() on it.

# ---- session state -----------------------------------------------------------

.spec_builder <- new.env(parent = emptyenv())

.sb_reset <- function(file, sections, input) {
  .spec_builder$answers  <- list()
  .spec_builder$file     <- file
  .spec_builder$sections <- sections
  .spec_builder$input    <- input
  .spec_builder$pos      <- 0L
  .spec_builder$stdin    <- NULL
}

# One line of input: scripted answers first (tests, reproducible reruns), then
# readline() at the console, then stdin under Rscript.
.sb_read_line <- function(prompt) {
  if (!is.null(.spec_builder$input)) {
    .spec_builder$pos <- .spec_builder$pos + 1L
    if (.spec_builder$pos > length(.spec_builder$input)) {
      stop("The scripted answers ran out before the questions did (at prompt ",
           .spec_builder$pos, ").", call. = FALSE)
    }
    x <- .spec_builder$input[[.spec_builder$pos]]
    cat(prompt, x, "\n", sep = "")
    return(x)
  }
  if (interactive()) return(readline(prompt))
  if (is.null(.spec_builder$stdin)) .spec_builder$stdin <- file("stdin", open = "r")
  cat(prompt)
  x <- readLines(.spec_builder$stdin, n = 1)
  if (!length(x)) stop("Input ended before the questions did.", call. = FALSE)
  cat(x, "\n", sep = "")
  x
}

.sb_save <- function() saveRDS(.spec_builder$answers, .spec_builder$file)

.sb_blank <- function(x) is.null(x) || length(x) == 0 || all(is.na(x)) ||
  (is.character(x) && all(!nzchar(x)))

# ---- asking a question -------------------------------------------------------

# type: text, int, num, yesno, choice, list (comma-separated), numlist.
# choices: a named character vector, value = label.
.sb_ask <- function(id, question, example = NULL, help = NULL, default = NULL,
                    type = "text", choices = NULL, required = FALSE) {
  saved <- .spec_builder$answers[[id]]
  has_saved <- id %in% names(.spec_builder$answers)
  if (has_saved && !substr(id, 1, 1) %in% .spec_builder$sections) return(saved)
  if (has_saved) default <- saved
  # A saved answer of the wrong shape for a choice (from an older answers
  # file) is not a default.
  if (identical(type, "choice") && !is.null(default) &&
      !(is.character(default) && length(default) == 1L &&
        default %in% names(choices))) {
    default <- NULL
  }

  cat("\n[", id, "] ", question, "\n", sep = "")
  if (!is.null(help)) cat(strwrap(help, indent = 2, exdent = 2), sep = "\n")
  if (!is.null(choices)) {
    cat(sprintf("  %d. %s\n", seq_along(choices), choices), sep = "")
  }
  if (!is.null(example)) cat("  e.g. ", example, "\n", sep = "")

  shown <- .sb_show(default, type, choices)
  prompt <- if (is.null(shown)) "> " else paste0("[Enter = ", shown, "] > ")

  repeat {
    raw <- trimws(.sb_read_line(prompt))
    if (identical(tolower(raw), "q")) {
      .sb_save()
      stop("Stopped. Answers saved to ", .spec_builder$file,
           "; run build_el_spec() again to continue.", call. = FALSE)
    }
    if (raw == "" && !is.null(default) && !(required && .sb_blank(default))) {
      val <- default
    } else if (raw == "" || raw == "-") {
      if (required) {
        cat("  An answer is needed here.\n")
        if (!is.null(.spec_builder$input)) {
          stop("Scripted answer for [", id, "] is blank, but one is required.",
               call. = FALSE)
        }
        next
      }
      val <- NA
    } else {
      val <- .sb_parse(raw, type, choices)
      if (is.null(val)) {
        if (!is.null(.spec_builder$input)) {
          stop("Scripted answer \"", raw, "\" for [", id, "] is not valid.",
               call. = FALSE)
        }
        next
      }
    }
    break
  }
  .spec_builder$answers[[id]] <- val
  .sb_save()
  val
}

.sb_show <- function(x, type, choices) {
  if (is.null(x)) return(NULL)
  if (.sb_blank(x)) return("blank")
  switch(type,
    yesno  = if (isTRUE(x)) "yes" else "no",
    choice = paste0(match(x, names(choices)), ". ", choices[[x]]),
    paste(x, collapse = ", "))
}

.sb_parse <- function(raw, type, choices) {
  bad <- function(msg) {
    cat("  ", msg, "\n", sep = "")
    NULL
  }
  switch(type,
    text = raw,
    int = if (grepl("^[0-9]+$", raw)) as.integer(raw) else bad("Please enter a whole number."),
    num = {
      v <- suppressWarnings(as.numeric(raw))
      if (is.na(v)) bad("Please enter a number.") else v
    },
    yesno = {
      r <- tolower(raw)
      if (r %in% c("y", "yes")) TRUE
      else if (r %in% c("n", "no")) FALSE
      else bad("Please answer yes or no.")
    },
    choice = {
      i <- suppressWarnings(as.integer(raw))
      if (!is.na(i) && i >= 1 && i <= length(choices)) names(choices)[i]
      else if (raw %in% names(choices)) raw
      else bad(paste0("Please enter a number from 1 to ", length(choices), "."))
    },
    list = trimws(strsplit(raw, ",")[[1]]),
    numlist = {
      v <- suppressWarnings(as.numeric(trimws(strsplit(raw, ",")[[1]])))
      if (anyNA(v)) bad("Please enter numbers separated by commas.") else v
    })
}

.sb_heading <- function(letter, title, who) {
  if (!letter %in% .spec_builder$sections) return(invisible())
  cat("\n", strrep("=", 70), "\n", "Part ", letter, ". ", title,
      "\n(usually answered by: ", who, ")\n", strrep("=", 70), "\n", sep = "")
}

# A value that is already known: shown, not asked.
.sb_filled <- function(id, what, value, shown = paste(value, collapse = ", "),
                       why = "same for every state using this test") {
  if (substr(id, 1, 1) %in% .spec_builder$sections) {
    cat("\n[", id, "] ", what, ": ", shown, "  (", why, ")\n", sep = "")
  }
  value
}

# A citation if one was given, otherwise "provisional" ([DERIVED - VERIFY]).
.sb_source <- function(x) if (.sb_blank(x)) "provisional" else x
.sb_or_na  <- function(x) if (.sb_blank(x)) NA_character_ else x

# ---- WIDA ACCESS and ELPA21 presets ------------------------------------------
# The component keys are the package vocabulary; spec.R is the only file that
# types them as literals (a test greps for any other), so the presets index
# into it: 1-4 the four domains, 5 oral, 6 written, 7 comprehension,
# 9 overall, 10 determination. A test checks the presets' keys.

.sb_wida_components <- function() {
  v <- el_component_vocabulary()
  dom <- function() list(kind = "domain", measures = c("scale", "level"))
  comp <- function(from, name) list(kind = "composite",
                                    measures = c("scale", "level"),
                                    from = from, native_name = name)
  out <- list(dom(), dom(), dom(), dom(),
              comp(stats::setNames(c(0.50, 0.50), v[1:2]),
                   "Oral (Listening + Speaking)"),
              comp(stats::setNames(c(0.50, 0.50), v[3:4]),
                   "Literacy (Reading + Writing)"),
              comp(stats::setNames(c(0.30, 0.70), v[c(1, 3)]),
                   "Comprehension (Listening + Reading)"),
              comp(stats::setNames(c(0.15, 0.15, 0.35, 0.35), v[1:4]),
                   "Overall Composite"))
  stats::setNames(out, v[c(1:4, 5, 6, 7, 9)])
}

.sb_elpa21_components <- function() {
  v <- el_component_vocabulary()
  dom <- function() list(kind = "domain", measures = c("scale", "level"))
  w <- stats::setNames(rep(0.25, 4), v[1:4])
  stats::setNames(
    list(dom(), dom(), dom(), dom(),
         list(kind = "composite", measures = "scale", from = w,
              native_name = "Overall scale score"),
         list(kind = "composite", measures = "level", from = w,
              native_name = "Overall Performance Determination")),
    v[c(1:4, 9, 10)])
}

.SB_WIDA_LABELS <- c("1 Entering", "2 Emerging", "3 Developing",
                     "4 Expanding", "5 Bridging", "6 Reaching")
.SB_WIDA_BREAKS <- c(1, 2, 3, 4, 4.5, 5, 7)

.sb_presets <- function() {
  v <- el_component_vocabulary()
  list(
    wida = list(
      name = "ACCESS for ELLs", vendor = "WIDA / DRC", window = "Jan-Mar",
      components = .sb_wida_components(), headline = v[[9]],
      level_scale = list(type = "numeric", range = c(1, 6),
                         labels = .SB_WIDA_LABELS),
      level_text = "1.0 to 6.0, to one decimal (1 Entering, 2 Emerging, 3 Developing, 4 Expanding, 5 Bridging, 6 Reaching)",
      alternate = "WIDA Alternate ACCESS",
      # The cut differs by state, so no value is pre-filled.
      rule = list(list(component = v[[9]], measure = "level", op = ">=",
                       value = NULL, exempt_if_absent = FALSE)),
      rule_text = "An Overall Composite proficiency level of 4.5 or higher"
    ),
    elpa21 = list(
      name = "ELPA21 Summative", vendor = "ELPA21", window = NULL,
      components = .sb_elpa21_components(), headline = v[[10]],
      level_scale = list(type = "categorical",
                         levels = c("Emerging", "Progressing", "Proficient")),
      level_text = "Emerging, Progressing, Proficient (each domain a level from 1 to 5)",
      alternate = NULL,
      # The consortium's Proficient determination: level 4 or 5 in every
      # domain. A waived domain is skipped rather than failed.
      rule = lapply(v[1:4], function(d)
        list(component = d, measure = "level", op = ">=", value = 4,
             exempt_if_absent = TRUE)),
      rule_text = "Proficient: level 4 or 5 in each of listening, speaking, reading and writing"
    )
  )
}

# Keep only the chosen components; drop `from` weights that point at a
# component no longer in the list (the validator rejects them).
.sb_subset_components <- function(comps, keep) {
  comps <- comps[keep]
  lapply(comps, function(cp) {
    if (!is.null(cp$from) && !all(names(cp$from) %in% names(comps))) cp$from <- NULL
    cp
  })
}

.sb_component_label <- function(comps, key) comps[[key]]$native_name %||% key

# "1.0-1.9", ..., "5.0+" from the breaks; the last break is the open top.
.sb_band_labels <- function(breaks) {
  f <- function(v) formatC(v, format = "f", digits = 1)
  n <- length(breaks) - 1
  c(paste0(f(breaks[seq_len(n - 1)]), "-", f(breaks[2:n] - 0.1)),
    paste0(f(breaks[n]), "+"))
}

# "3-8", "K-2", "3, 4, 5" -> c("3", "4", ...)
.sb_expand_grades <- function(x) {
  if (.sb_blank(x)) return(NULL)
  num <- function(g) if (toupper(g) == "K") 0L else as.integer(g)
  out <- unlist(lapply(trimws(strsplit(x, ",")[[1]]), function(p) {
    ends <- trimws(strsplit(p, "-")[[1]])
    if (length(ends) == 2) seq(num(ends[1]), num(ends[2])) else num(ends)
  }))
  ifelse(out == 0L, "K", as.character(out))
}

# ---- parts A-J ---------------------------------------------------------------

.sb_part_a <- function() {
  .sb_heading("A", "About the program", "anyone")
  a <- list(
    state  = .sb_ask("A1", "Which state is this for? (two-letter code)", "XX",
                     required = TRUE),
    agency = .sb_ask("A2", "What is the name of the state education agency?",
                     "State Department of Education", required = TRUE),
    label  = .sb_ask("A3", "What short label should reports use for this setup?",
                     "XX_ACCESS",
                     help = "Don't use SIM: the package reserves it for its simulated data.",
                     required = TRUE),
    test_key = .sb_ask("A4", "Which ELP test does your state use?",
                       type = "choice", required = TRUE, default = "wida",
                       help = "WIDA ACCESS and ELPA21 fill in most of Part B for you.",
                       choices = c(wida = "WIDA ACCESS for ELLs",
                                   elpa21 = "ELPA21", other = "Another test"))
  )
  p <- .sb_presets()[[a$test_key]]
  c(a, list(
    test   = if (is.null(p)) .sb_ask("A4b", "The test's name?", "LAS Links",
                                     required = TRUE)
             else p$name,
    vendor = if (is.null(p)) .sb_ask("A5", "Who publishes or scores the test?",
                                     "WIDA / DRC", required = TRUE)
             else .sb_filled("A5", "Publisher", p$vendor),
    year   = .sb_ask("A6", "Most recent year the test was given (spring year)?",
                     "2025", type = "int", required = TRUE),
    window = .sb_ask("A7", "When during the year is the test given?",
                     "Jan-Mar", default = p$window)
  ))
}

.sb_part_b <- function(test_key) {
  .sb_heading("B", "The ELP test", "assessment office")
  p <- .sb_presets()[[test_key]]
  vocab <- el_component_vocabulary()

  comps <- if (!is.null(p)) {
    all <- p$components
    labels <- vapply(names(all), function(k) .sb_component_label(all, k), "")
    repeat {
      picked <- .sb_ask("B1", "Which of these scores are in your student data file? (numbers, comma-separated)",
                        type = "numlist", default = seq_along(all),
                        required = TRUE, choices = labels)
      if (all(picked %in% seq_along(all))) break
      cat("  Please use numbers from 1 to ", length(all), ".\n", sep = "")
      .spec_builder$answers[["B1"]] <- NULL
      if (!is.null(.spec_builder$input)) {
        stop("Scripted answer for [B1] names a score that is not listed.",
             call. = FALSE)
      }
    }
    .sb_subset_components(all, names(all)[sort(unique(picked))])
  } else {
    keys <- .sb_ask("B1.list", "List the scores, using these package names:",
                    help = paste("Allowed:", paste(vocab, collapse = ", "),
                                 ". Map your own name onto one of these; you give your name next."),
                    example = "listening, speaking, reading, writing, overall",
                    type = "list", required = TRUE)
    bad <- setdiff(keys, vocab)
    if (length(bad)) stop("Not package score names: ", paste(bad, collapse = ", "),
                          ". Rerun with sections = \"B\".", call. = FALSE)
    stats::setNames(lapply(keys, function(k) {
      kind <- .sb_ask(paste0("B1.", k, ".kind"),
                      paste0("Is '", k, "' a single domain or a combined score?"),
                      type = "choice",
                      choices = c(domain = "A single domain",
                                  composite = "A combined score"),
                      default = if (k %in% vocab[1:4]) "domain" else "composite")
      meas <- .sb_ask(paste0("B1.", k, ".measures"),
                      paste0("What does '", k, "' report?"),
                      type = "choice", default = "both",
                      choices = c(both = "Scale score and proficiency level",
                                  scale = "Scale score only",
                                  level = "Proficiency level only"))
      name <- .sb_ask(paste0("B1.", k, ".name"),
                      paste0("What does your state call '", k, "'?"),
                      example = "Overall Composite")
      out <- list(kind = kind,
                  measures = if (meas == "both") c("scale", "level") else meas)
      if (!.sb_blank(name)) out$native_name <- name
      out
    }), keys)
  }

  # Every score in the file is offered as the main one, the test's usual main
  # score first. With only one score in the file there is nothing to ask.
  usual <- if (!is.null(p) && p$headline %in% names(comps)) p$headline else
    if (vocab[[9]] %in% names(comps)) vocab[[9]] else names(comps)[1]
  ord <- c(usual, setdiff(names(comps), usual))
  choices <- stats::setNames(vapply(ord, function(k) .sb_component_label(comps, k), ""),
                             ord)
  headline <- if (length(comps) == 1) {
    .sb_filled("B2", "Main score", names(comps),
               .sb_component_label(comps, names(comps)),
               why = "the only score in the file")
  } else {
    .sb_ask("B2", "Which score is the main one in your exit rule?",
            help = "If the rule has more than one requirement, pick the main one; the others come in C2.",
            type = "choice", choices = choices, required = TRUE, default = usual)
  }

  # Known for WIDA ACCESS and ELPA21; for another test, the known scales are
  # offered as options before a scale is entered by hand.
  presets <- .sb_presets()
  scale_kind <- if (!is.null(p)) "preset" else
    .sb_ask("B3", "How are proficiency levels reported?", type = "choice",
            default = "like_wida", required = TRUE,
            choices = c(like_wida   = paste("Like WIDA ACCESS: 1.0-6.0,",
                                            paste(.SB_WIDA_LABELS, collapse = ", ")),
                        like_elpa21 = "Like ELPA21: Emerging, Progressing, Proficient",
                        numeric     = "Another number scale",
                        categorical = "Other named categories"))

  if (scale_kind == "preset") {
    level_scale <- .sb_filled("B3", "Proficiency levels", p$level_scale, p$level_text)
  } else if (scale_kind == "like_wida") {
    level_scale <- presets$wida$level_scale
  } else if (scale_kind == "like_elpa21") {
    level_scale <- presets$elpa21$level_scale
  } else if (scale_kind == "numeric") {
    range  <- .sb_ask("B3.range", "Lowest and highest level?", "1, 6",
                      type = "numlist", default = c(1, 6), required = TRUE)
    n_levels <- floor(range[2]) - ceiling(range[1]) + 1
    repeat {
      labels <- .sb_ask("B3.labels", "Names of the whole levels, lowest first?",
                        paste(.SB_WIDA_LABELS, collapse = ", "), type = "list",
                        default = if (identical(range, c(1, 6))) .SB_WIDA_LABELS else NULL)
      if (.sb_blank(labels) || length(labels) == n_levels) break
      cat("  That is ", length(labels), " name(s) for ", n_levels,
          " levels. Please give one name per level, or \"-\" for none.\n", sep = "")
      .spec_builder$answers[["B3.labels"]] <- NULL
      if (!is.null(.spec_builder$input)) {
        stop("Scripted answer for [B3.labels] has the wrong number of names.",
             call. = FALSE)
      }
    }
    level_scale <- list(type = "numeric", range = range,
                        labels = if (.sb_blank(labels)) NULL else labels)
  } else {
    levels <- .sb_ask("B3.levels", "List the categories, lowest first.",
                      "Emerging, Progressing, Proficient", type = "list",
                      required = TRUE)
    level_scale <- list(type = "categorical", levels = levels)
  }

  alt <- .sb_ask("B4", "Does your state also give an alternate ELP test?",
                 type = "yesno",
                 default = if (identical(test_key, "wida")) TRUE else NULL,
                 required = TRUE)
  alternate <- if (isTRUE(alt)) list(
    instrument    = if (!is.null(p$alternate)) .sb_filled("B4a", "Alternate test", p$alternate)
                    else .sb_ask("B4a", "Its name?", "Alt ELPA", required = TRUE),
    grades        = c("K", "1", "2-3", "4-5", "6-8", "9-12"),
    eligibility   = .sb_or_na(.sb_ask("B4b", "Who takes it?",
                                      "Students with an IEP who meet the alternate assessment participation criteria")),
    exit_criteria = .sb_or_na(.sb_ask("B4c", "How do students taking it exit EL status?",
                                      "Not yet set by the state")),
    level_scale   = NULL
  )

  list(test_key = test_key, comps = comps, headline = headline,
       level_scale = level_scale, alternate = alternate)
}

.sb_part_c <- function(b) {
  .sb_heading("C", "The exit rule", "EL program office")
  comps <- b$comps
  p <- .sb_presets()[[b$test_key]]

  # The preset's usual rule is offered as the defaults, only when every score
  # it uses is in the file. Each value is still shown for confirmation.
  usual <- p$rule
  if (!is.null(usual) &&
      !all(vapply(usual, function(t) t$component %in% names(comps), TRUE))) usual <- NULL
  if (!is.null(usual)) {
    cat("\nThe usual ", p$name, " rule is filled in below: ", p$rule_text,
        ".\nConfirm each part, or type your state's own values.\n", sep = "")
  }

  # The wording is pre-filled only when the usual rule is complete; a WIDA cut
  # differs by state, so WIDA gets an example instead.
  complete <- !is.null(usual) &&
    all(vapply(usual, function(t) !is.null(t$value), TRUE))
  rule <- .sb_ask("C1", "In one sentence, what does a student need to exit EL status?",
                  p$rule_text %||% "An Overall Composite proficiency level of 4.5 or higher",
                  default = if (complete) p$rule_text else NULL, required = TRUE)

  n <- .sb_ask("C2", "How many separate requirements does the rule have?", "1",
               type = "int", default = if (is.null(usual)) 1L else length(usual),
               required = TRUE)
  score_choices <- stats::setNames(
    vapply(names(comps), function(k) .sb_component_label(comps, k), ""),
    names(comps))
  terms <- lapply(seq_len(n), function(i) {
    cat("\n  Requirement ", i, " of ", n, sep = "")
    u <- if (i <= length(usual)) usual[[i]] else NULL
    score <- .sb_ask(paste0("C2.", i, ".score"), "Which score?", type = "choice",
                     choices = score_choices, required = TRUE,
                     default = u$component %||% (if (i == 1) b$headline else NULL))
    m <- comps[[score]]$measures
    measure <- if (length(m) == 1) m else
      .sb_ask(paste0("C2.", i, ".measure"), "Proficiency level or scale score?",
              type = "choice",
              choices = c(level = "Proficiency level", scale = "Scale score"),
              default = u$measure %||% "level")
    op <- .sb_ask(paste0("C2.", i, ".op"), "Comparison?", type = "choice",
                  default = u$op %||% ">=",
                  choices = c(">=" = "at least (>=)", ">" = "more than (>)",
                              "<=" = "at most (<=)", "<" = "less than (<)",
                              "==" = "exactly (==)"))
    value <- .sb_ask(paste0("C2.", i, ".value"), "Minimum value?", "4.5",
                     type = "num", default = u$value, required = TRUE,
                     help = if (identical(b$level_scale$type, "categorical") &&
                                score == b$headline)
                       paste0("For a category, give its position: ",
                              paste0(seq_along(b$level_scale$levels), " = ",
                                     b$level_scale$levels, collapse = ", "),
                              ".") else NULL)
    term <- list(component = score, measure = measure, op = op, value = value)
    if (identical(comps[[score]]$kind, "domain")) {
      term$exempt_if_absent <- .sb_ask(
        paste0("C2.", i, ".exempt"),
        "If this domain wasn't tested (e.g. waived), is the student judged on the other requirements alone?",
        type = "yesno", default = u$exempt_if_absent %||% FALSE)
    }
    term
  })
  # The main score's term goes first. Check 3 no longer depends on the order,
  # but it keeps the rule reading the way a state writes it.
  first <- vapply(terms, function(t) t$component == b$headline &&
                    t$measure == "level", TRUE)
  terms <- c(terms[first], terms[!first])

  combine <- if (n > 1) .sb_ask("C3", "Must a student meet all of them, or any one?",
                                type = "choice", default = "all",
                                choices = c(all = "All of them", any = "Any one"))
             else "all"

  # A suggestion from the rule's shape; the validator checks the declaration.
  kinds <- vapply(terms, function(t) comps[[t$component]]$kind, "")
  suggested <- if (n == 1 && terms[[1]]$component == b$headline) 1L else
    if (n >= 2 && all(kinds == "domain")) 6L else 2L
  types <- .sb_ask("C4", "Which of these best describes the rule? (numbers, comma-separated)",
                   type = "numlist", default = suggested, required = TRUE,
                   help = paste0(paste0(1:6, ". ", el_exit_typology()),
                                 collapse = "; "))
  types <- as.integer(types)

  authority <- .sb_ask("C5", "Who makes the exit decision?",
                       "Automatic once scores are reported; no committee")
  discretionary <- .sb_ask("C6", "Can a committee or educator override the scores?",
                           type = "yesno", default = FALSE)
  varies <- .sb_ask("C7", "Does the rule differ by grade?", type = "yesno",
                    default = FALSE)
  cite <- .sb_ask("C8", "Where is the exit rule written down?",
                  help = "A regulation, guidance document or memo. Leave blank if unknown; it will be marked to verify.",
                  example = "State regulation [number]; or a guidance document's title and date")
  set_date <- .sb_ask("C8b", "When was it adopted? (optional)", "2017")

  # Bands, with the option to split at a cut that falls inside one.
  bands <- NULL
  if (b$level_scale$type == "numeric") {
    breaks <- if (identical(b$level_scale$range, c(1, 6))) .SB_WIDA_BREAKS else
      .sb_ask("C9.breaks", "Band edges, lowest first (the last is the open top)?",
              "1, 2, 3, 4, 5, 7", type = "numlist", required = TRUE)
    cut <- terms[[1]]$value
    if (terms[[1]]$measure == "level" && !cut %in% breaks &&
        cut > min(breaks) && cut < max(breaks)) {
      split <- .sb_ask("C9", paste0("Your exit score (", cut, ") falls inside a band. Keep the standard bands, or split at the cut?"),
                       type = "choice", default = "split",
                       choices = c(keep = "Keep the standard bands",
                                   split = "Split the band at the cut"))
      if (split == "split") breaks <- sort(c(breaks, cut))
    }
    bands <- list(breaks = breaks, labels = .sb_band_labels(breaks))
  }

  pathway <- list(
    name          = "main_rule",
    rule          = rule,
    terms         = terms,
    combine       = combine,
    authority     = if (.sb_blank(authority)) "TODO: confirm who makes the exit decision." else authority,
    discretionary = discretionary
  )
  list(
    exit = list(
      pathways        = list(pathway),
      combination     = "any_pathway",
      types           = types,
      varies_by_grade = varies,
      set_date        = .sb_or_na(if (.sb_blank(set_date)) NA else as.character(set_date)),
      set_by          = if (.sb_blank(cite)) "TODO: the document that sets the rule" else cite,
      source          = .sb_source(cite)
    ),
    bands = bands
  )
}

.sb_part_d <- function() {
  .sb_heading("D", "Timelines", "EL program office")
  max_years <- .sb_ask("D1", "At most, how many years should it take a student to exit EL status?",
                       "6", type = "int", required = TRUE,
                       help = "Required by the package. If your state has not set one, enter 6 and leave D5 blank so it is marked to verify.")
  essa <- .sb_ask("D2", "Years your ESSA plan sets to reach English proficiency? (optional)",
                  "5", type = "int")
  on_time <- .sb_ask("D3", "A student who exits within how many years counts as on time? (optional)",
                     "5", type = "int", default = if (.sb_blank(essa)) NULL else essa)
  lt_years <- .sb_ask("D4", "After how many years in status is a student long-term EL? (optional)",
                      "6", type = "int")
  lt_def <- .sb_ask("D4b", "The full long-term EL definition (optional)",
                    "A student identified as an EL for six or more years without exiting")
  cite <- .sb_ask("D5", "Where are these timelines written down?",
                  example = "The state's ESSA Consolidated State Plan, with the section number")

  tl <- list(max_years_to_exit = max_years)
  if (!.sb_blank(essa)) tl$essa_timeline_years <- essa
  if (!.sb_blank(on_time)) tl$on_time_rule <- list(default_years = on_time,
                                                   by_start_grade = NULL,
                                                   by_initial_band = NULL)
  tl$lt_el_definition <- if (.sb_blank(lt_def)) "TODO: the state long-term EL definition." else lt_def
  if (!.sb_blank(lt_years)) tl$lt_el_years <- lt_years
  tl$exit_dating        <- "first_former_year"
  tl$initial_elp_source <- "first_annual"
  tl$source             <- .sb_source(cite)
  tl
}

.sb_part_e <- function() {
  .sb_heading("E", "Growth targets (optional)", "EL program office or accountability")
  target <- .sb_ask("E1", "Expected growth per year, in proficiency levels? (optional)",
                    "0.4", type = "num")
  how    <- .sb_ask("E2", "How are growth targets set? (optional)",
                    "Annual targets based on the student's starting level, set in the ESSA plan")
  cite   <- .sb_ask("E3", "Where are the growth targets written down? (optional)")
  list(annual_target_levels = if (.sb_blank(target)) NA_real_ else target,
       met_indicator        = NA_character_,
       how_set              = if (.sb_blank(how)) "TODO: how annual growth targets are set." else how,
       source               = .sb_source(cite))
}

.sb_part_f <- function() {
  .sb_heading("F", "Reporting rules", "data or accountability office")
  min_cell <- .sb_ask("F1", "Smallest group size you are allowed to report?", "10",
                      type = "int", default = 10L, required = TRUE)
  cite <- .sb_ask("F2", "Where is that rule written down?",
                  "The department's data suppression policy")
  keep <- .sb_ask("F3", "Keep the district groups (under 100, 101-999, 1,000+)?",
                  type = "yesno", default = TRUE)
  dsb <- if (isTRUE(keep)) {
    list(breaks = c(0, 100, 1000, Inf), labels = c("<100", "101-999", "1,000+"))
  } else {
    br <- .sb_ask("F3.breaks", "Group edges, lowest first?", "0, 100, 1000",
                  type = "numlist", required = TRUE)
    list(breaks = c(br, Inf),
         labels = .sb_ask("F3.labels", "Group names?", "<100, 101-999, 1,000+",
                          type = "list", required = TRUE))
  }
  list(suppression = list(min_cell = min_cell, source = .sb_source(cite)),
       district_size_bands = c(dsb, list(source = .sb_source(cite))))
}

# Cut scores for one subject: the same in every grade, different by grade
# (asked grade by grade), or not available now. Each answer is checked
# against the number of levels, because a short list would shift every level.
.sb_subject_cuts <- function(i, grades, levels) {
  n_cut <- length(levels) - 1L
  saved_same <- .spec_builder$answers[[paste0("G1.", i, ".d")]]
  kind <- .sb_ask(
    paste0("G1.", i, ".cuts"), "Are the cut scores the same in every grade?",
    type = "choice",
    default = if (!.sb_blank(saved_same)) "same" else
      if (length(grades) > 1L) "by_grade" else "same",
    choices = c(same = "The same in every grade (or an end-of-course test)",
                by_grade = "Different by grade (most state tests)",
                none = "Not available now"),
    help = paste0("Without cut scores, Checks 3 and 4 read proficiency from ",
                  "the academic file's own performance level instead, and ",
                  "draw no cut line."))
  one <- function(id, question) {
    repeat {
      v <- .sb_ask(id, question,
                   paste(seq(500, by = 25, length.out = max(n_cut, 1L)),
                         collapse = ", "),
                   type = "numlist", required = TRUE,
                   help = paste0("The lowest scale score of each level above ",
                                 levels[[1]], ": ", n_cut, " number(s), for ",
                                 paste(levels[-1], collapse = ", "), "."))
      if (length(v) == n_cut && !is.unsorted(v, strictly = TRUE)) return(v)
      cat("  Please give ", n_cut, " increasing number(s), one per level above ",
          levels[[1]], ".\n", sep = "")
      .spec_builder$answers[[id]] <- NULL
      if (!is.null(.spec_builder$input)) {
        stop("Scripted answer for [", id, "] needs ", n_cut,
             " increasing cut scores.", call. = FALSE)
      }
    }
  }
  if (identical(kind, "none")) return(NULL)
  if (identical(kind, "same") || !length(grades)) {
    return(one(paste0("G1.", i, ".d"),
               "Minimum score for each level above the lowest?"))
  }
  stats::setNames(lapply(grades, function(g) {
    one(paste0("G1.", i, ".d.", g),
        paste0("Grade ", g, ": minimum score for each level above the lowest?"))
  }), grades)
}

.sb_part_g <- function() {
  .sb_heading("G", "State academic tests", "assessment office")
  if ("G" %in% .spec_builder$sections) {
    cat("These are the state's general academic assessments that all students take\n",
        "(usually the state summative reading and math tests), not the ELP test.\n", sep = "")
  }
  n <- .sb_ask("G1", "How many academic test subjects should be included?", "2",
               help = "Usually 2: reading (or ELA) and math. Count only subjects in the academic data file.",
               type = "int", default = 2L, required = TRUE)
  subjects <- lapply(seq_len(n), function(i) {
    if ("G" %in% .spec_builder$sections) cat("\n  Subject ", i, " of ", n, sep = "")
    key <- .sb_ask(paste0("G1.", i, ".key"),
                   "Short name for this subject (used in the data and reports)?",
                   default = if (i <= 2) c("ELA", "Math")[i] else NULL,
                   required = TRUE)
    test <- .sb_ask(paste0("G1.", i, ".a"), "The test's name?",
                    "State Summative Assessment, Reading", required = TRUE)
    grades <- .sb_ask(paste0("G1.", i, ".b"), "Grades tested?", "3-8",
                      help = "A range (3-8) or a list (3, 5, 8). Leave blank for an end-of-course test.")
    levels <- .sb_ask(paste0("G1.", i, ".c"),
                      "Performance levels, lowest first, exactly as in the data file?",
                      "BB, B, P, A", type = "list", required = TRUE)
    grade_vec <- .sb_expand_grades(grades)
    cuts <- .sb_subject_cuts(i, grade_vec, levels)
    list(key = key, spec = list(
      instrument   = test,
      grades       = grade_vec,
      cuts         = cuts,
      op           = ">=",
      level_labels = levels
    ))
  })
  levels <- subjects[[1]]$spec$level_labels
  prof <- .sb_ask("G2", "Which performance level counts as proficient? (its position, lowest = 1)",
                  help = paste0("Levels: ", paste0(seq_along(levels), " = ",
                                                   levels, collapse = ", ")),
                  example = "3", type = "int", required = TRUE)
  cite <- .sb_ask("G3", "Where are the academic test's performance levels and cut scores published?",
                  "The academic test's technical report")

  academic <- stats::setNames(
    lapply(subjects, function(s) c(s$spec, list(source = .sb_source(cite)))),
    vapply(subjects, function(s) s$key, ""))
  list(academic = academic,
       benchmark = list(proficient_level = prof,
                        description = paste0(levels[prof], " or above (level ",
                                             prof, " of ", length(levels), ")."),
                        source = .sb_source(cite)))
}

.sb_part_h <- function() {
  .sb_heading("H", "Programs and priorities", "EL program office")
  models <- .sb_ask("H1", "What kinds of EL services does your state offer?",
                    "Push-in support, pull-out ESL, sheltered content classes")
  course <- .sb_ask("H2", "Does EL status affect which courses a student can take?",
                    "Newcomers take a sheltered English course in place of English I")
  cred   <- .sb_ask("H3", "Are there credentials tied to language?",
                    "Seal of Biliteracy")
  gated  <- .sb_ask("H4", "What changes for a student the moment they exit?",
                    "ESL services and accommodations end; monitoring begins for four years")
  worse  <- .sb_ask("H5", "Which mistake is worse?", type = "choice",
                    default = "unknown",
                    choices = c(premature_exit      = "Exiting a student before they're ready",
                                prolonged_retention = "Keeping a student in EL status too long",
                                equal               = "Both are equally bad",
                                unknown             = "Not sure / not discussed"))
  who    <- .sb_ask("H6a", "Who answered H5?", "EL program director")
  when   <- .sb_ask("H6b", "When?", "October 2026")
  why    <- .sb_ask("H6c", "Why?",
                    "Exiting too early removes supports a student still needs")
  cite   <- .sb_ask("H7", "Where are these program details written down? (optional)")
  list(
    program = list(models = .sb_or_na(models), course_access = .sb_or_na(course),
                   credentialing = .sb_or_na(cred),
                   gated_by_el_status = .sb_or_na(gated),
                   source = .sb_source(cite)),
    error_tolerance = list(costlier_error = worse, asked_of = .sb_or_na(who),
                           asked_date = .sb_or_na(when),
                           rationale = .sb_or_na(why),
                           source = .sb_source(cite))
  )
}

.sb_part_i <- function(subject_keys) {
  .sb_heading("I", "Data files", "data team")
  if ("I" %in% .spec_builder$sections) {
    cat("Give the column name for each item. \"-\" if the file doesn't have it.\n",
        "Allowed values for each column: el_data_format().\n", sep = "")
  }
  col <- function(id, what, default) {
    .sb_ask(id, paste0("Column for ", what, "?"), default = default)
  }
  pick <- function(x) {
    x <- unlist(x)
    x[!is.na(x) & nzchar(x)]
  }

  if ("I" %in% .spec_builder$sections) {
    cat("\n  I1. Student and year file (one row per student per year)")
  }
  roster <- pick(list(
    STUDENT_ID     = col("I1a", "student ID", "STUDENT_ID"),
    YEAR           = col("I1b", "school year", "YEAR"),
    GRADE          = col("I1c", "grade", "GRADE"),
    EL_STATUS      = col("I1d", "EL status (current / former / never)", "EL_STATUS"),
    ELP_FORM       = col("I1e", "ELP test form (values must be annual / alternate)", "ELP_FORM"),
    DISTRICT_ID    = col("I1f", "district", "DISTRICT_ID"),
    SCHOOL_ID      = col("I1g", "school", "SCHOOL_ID"),
    IEP            = col("I1h", "special education / IEP (values Y / N)", "IEP"),
    GENDER         = col("I1i", "gender", "GENDER"),
    RACE_ETHNICITY = col("I1j", "race / ethnicity", "RACE_ETHNICITY")
  ))
  k_code <- .sb_ask("I2", "How is kindergarten coded in the grade column?", "0",
                    default = "K")

  if ("I" %in% .spec_builder$sections) {
    cat("\n  I3. ELP score file (one row per student, year and score)")
  }
  same <- function(k) if (k %in% names(roster)) roster[[k]] else k
  base <- pick(list(
    STUDENT_ID        = col("I3a", "student ID", same("STUDENT_ID")),
    YEAR              = col("I3b", "school year", same("YEAR")),
    SCALE_SCORE       = col("I3c", "scale score", "SCALE_SCORE"),
    ACHIEVEMENT_LEVEL = col("I3d", "proficiency level", "ACHIEVEMENT_LEVEL")
  ))

  if ("I" %in% .spec_builder$sections) {
    cat("\n  I4. Academic test file (the state academic test from Part G; one row per student, year and subject)")
  }
  academic <- pick(list(
    STUDENT_ID        = col("I4a", "student ID", "STUDENT_ID"),
    YEAR              = col("I4b", "school year", "YEAR"),
    GRADE             = col("I4c", "grade", "GRADE"),
    SUBJECT           = col("I4d", "subject", "SUBJECT"),
    SCALE_SCORE       = col("I4e", "scale score", "SCALE_SCORE"),
    ACHIEVEMENT_LEVEL = col("I4f", "performance level", "ACHIEVEMENT_LEVEL")
  ))

  codes <- vapply(subject_keys, function(k) {
    as.character(.sb_ask(paste0("I5.", k),
                         paste0("How is '", k, "' coded in the subject column?"),
                         example = if (k == "ELA") "READING" else "MATHEMATICS",
                         default = k))
  }, "")

  value_maps <- list()
  if (any(codes != subject_keys)) {
    value_maps$CONTENT_AREA <- list(academic = stats::setNames(subject_keys, codes))
  }
  if (!.sb_blank(k_code) && k_code != "K") {
    value_maps$GRADE <- stats::setNames("K", k_code)
  }

  out <- list(roster = roster, base = base, academic = academic)
  if (length(value_maps)) out$value_maps <- value_maps
  list(columns = out)
}

.sb_part_j <- function() {
  .sb_heading("J", "Sign-off", "whoever reviews the finished form")
  status <- .sb_ask("J1", "How complete is this information?", type = "choice",
                    default = "provisional",
                    choices = c(provisional = "Draft: some answers still need checking",
                                unverified  = "Complete but not yet reviewed",
                                verified    = "Reviewed and confirmed"))
  list(status        = status,
       verified_by   = .sb_or_na(.sb_ask("J2", "Who reviewed it?")),
       verified_date = .sb_or_na(.sb_ask("J3", "Date reviewed?", "2026-10-22")),
       method        = "Built with stateELP::build_el_spec() from the spec intake form.",
       notes         = NA_character_)
}

# ---- writing the spec file ---------------------------------------------------

.sb_r_name <- function(n) if (make.names(n) == n) n else encodeString(n, quote = '"')

.sb_r_atoms <- function(x) {
  if (is.character(x)) return(ifelse(is.na(x), "NA_character_", encodeString(x, quote = '"')))
  if (is.logical(x))   return(ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE")))
  if (is.integer(x))   return(ifelse(is.na(x), "NA_integer_", paste0(x, "L")))
  ifelse(is.na(x), "NA_real_", as.character(x))
}

.sb_emit <- function(x, indent = 0) {
  pad <- strrep(" ", indent)
  if (is.null(x)) return("NULL")
  if (is.list(x)) {
    if (!length(x)) return("list()")
    nms <- names(x)
    items <- vapply(seq_along(x), function(i) {
      val <- .sb_emit(x[[i]], indent + 2)
      if (!is.null(nms) && nzchar(nms[i])) paste0(.sb_r_name(nms[i]), " = ", val) else val
    }, "")
    return(paste0("list(\n", paste0(strrep(" ", indent + 2), items, collapse = ",\n"),
                  "\n", pad, ")"))
  }
  vals <- .sb_r_atoms(x)
  if (!is.null(names(x))) vals <- paste0(vapply(names(x), .sb_r_name, ""), " = ", vals)
  if (length(x) == 1 && is.null(names(x))) return(vals)
  one_line <- paste0("c(", paste(vals, collapse = ", "), ")")
  if (nchar(one_line) + indent <= 76) return(one_line)
  paste0("c(\n", paste0(strrep(" ", indent + 2), vals, collapse = ",\n"), "\n", pad, ")")
}

.sb_write_spec <- function(args, path, object_name) {
  body <- vapply(names(args), function(n) paste0("  ", n, " = ", .sb_emit(args[[n]], 2)), "")
  header <- c(
    "# =============================================================================",
    paste0("# ", basename(path), "  --  built with stateELP::build_el_spec() on ",
           Sys.Date(), "."),
    "# -----------------------------------------------------------------------------",
    "# Generated from the answers in:",
    paste0("#   ", basename(.spec_builder$file)),
    "# To change an answer, rerun build_el_spec() (Enter keeps every saved",
    "# answer) rather than editing this file, or edit here and stop using the",
    "# builder.",
    "#",
    "# Any policy entry with source = \"provisional\" renders as [DERIVED - VERIFY].",
    "# ============================================================================="
  )
  writeLines(c(header, "", paste0(object_name, " <- stateELP::el_spec("), "",
               paste(body, collapse = ",\n\n"), ")"), path)
}

# ---- build -------------------------------------------------------------------

#' Build a spec file by answering questions
#'
#' Walks through the questions of the spec intake form ([spec_intake_form()])
#' at the R console, one prompt per question with the same ids, and writes a
#' spec file from the answers. The file is then read with [read_el_spec()],
#' which validates it; the result is printed, including its
#' `[DERIVED - VERIFY]` line of policy entries that still have no written
#' source.
#'
#' At any prompt, **Enter** keeps the value shown, **`-`** leaves an optional
#' answer blank, and **`q`** saves and stops. Answers are saved after every
#' question to `answers_path`; on a rerun every saved answer is the default,
#' so only what changed needs retyping. To redo one part after a validation
#' error, pass `sections`.
#'
#' **Presets.** Choosing WIDA ACCESS or ELPA21 fills in what is the same for
#' every state using the test -- the publisher, the score structure, the level
#' scale, the alternate test's name -- and offers the test's usual exit rule
#' as defaults to confirm. The WIDA exit cut is never pre-filled: it differs
#' by state. Any other test is entered score by score.
#'
#' **Academic cut scores** are asked grade by grade when they differ by grade
#' (most state tests), once when they do not, or not at all when they are not
#' available yet -- Checks 3 and 4 then read proficiency from the academic
#' file's own performance level.
#'
#' What it does not cover: a rule with more than one route to exit (Type 5)
#' needs the extra pathways added to the file by hand.
#'
#' @param out_path Where to write the spec file (`.R`). Keep it outside the
#'   package's own `inst/specs/`.
#' @param answers_path Where answers are saved between runs.
#' @param sections Which parts to ask (`"A"` to `"J"`). Parts not listed keep
#'   their saved answers.
#' @param input Optional character vector of answers, one per prompt in
#'   order, used instead of the console (for scripted or repeatable runs).
#' @return The validated `el_spec`, invisibly; `NULL` if the written spec did
#'   not validate (the reason is printed).
#' @export
#' @examples
#' \dontrun{
#' build_el_spec("specs/XX_EL_Spec.R")
#' # Fix one part after a validation error, keeping the other answers:
#' build_el_spec("specs/XX_EL_Spec.R", sections = "G")
#' }
build_el_spec <- function(out_path,
                          answers_path = sub("\\.R$", "_answers.rds", out_path),
                          sections = LETTERS[1:10], input = NULL) {
  stopifnot(is.character(out_path), length(out_path) == 1L,
            grepl("\\.R$", out_path),
            is.null(input) || is.character(input))
  .sb_reset(answers_path, toupper(sections), input)
  if (file.exists(answers_path)) {
    .spec_builder$answers <- readRDS(answers_path)
    if (length(.spec_builder$answers)) {
      cat("Loaded", length(.spec_builder$answers), "saved answers from",
          answers_path, "\nPress Enter at any prompt to keep the saved answer.\n")
    }
  }

  a  <- .sb_part_a()
  b  <- .sb_part_b(a$test_key)
  c_ <- .sb_part_c(b)
  d  <- .sb_part_d()
  e  <- .sb_part_e()
  f  <- .sb_part_f()
  g  <- .sb_part_g()
  h  <- .sb_part_h()
  i  <- .sb_part_i(names(g$academic))
  j  <- .sb_part_j()

  cites <- unique(unlist(lapply(list(c_$exit, d, e, f$suppression, g$benchmark,
                                     h$program), function(x) x$source)))
  cites <- setdiff(cites, "provisional")

  args <- list(
    schema_version = "1.0.0",
    program = list(name = a$test, short_name = a$label, state = a$state,
                   department = a$agency, assessment_type = "elp",
                   administration_year = a$year, vendor = a$vendor),
    elp = list(instrument = a$test, administration_window = .sb_or_na(a$window),
               components = b$comps, headline = b$headline,
               level_scale = b$level_scale, bands = c_$bands,
               grade_clusters = c("K", "1", "2-3", "4-5", "6-8", "9-12"),
               cuts = NULL, alternate = b$alternate),
    policy = list(exit = c_$exit, timelines = d, growth = e,
                  suppression = f$suppression, academic_benchmark = g$benchmark,
                  district_size_bands = f$district_size_bands,
                  program = h$program, error_tolerance = h$error_tolerance),
    academic = g$academic,
    data = i,
    verification = j,
    source_documents = lapply(cites, function(x) list(title = x, url = NA_character_))
  )

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  .sb_write_spec(args, out_path,
                 paste0(gsub("[^A-Za-z0-9]", "_", a$state), "_EL_Spec"))
  cat("\n", strrep("=", 70), "\nWrote ", out_path, "\n", sep = "")

  spec <- tryCatch(read_el_spec(out_path), error = function(err) err)
  if (inherits(spec, "error")) {
    cat("\nThe spec did not pass validation:\n", conditionMessage(spec),
        "\n\nRerun build_el_spec() with sections = the part(s) to fix; other answers are kept.\n")
    return(invisible(NULL))
  }
  cat("\nThe spec passed validation.\n\n")
  print(spec)
  invisible(spec)
}

#' The spec intake form
#'
#' A plain-language form, in Markdown, that collects everything a spec needs
#' from the people who know it -- the EL program office, the assessment
#' office, the data team -- with no R required. Each question has an id that
#' matches a [build_el_spec()] prompt, so the analyst can type the completed
#' form in beside it.
#'
#' @param path Where to write a copy of the form. `NULL` returns the path of
#'   the installed form instead.
#' @return The path written (or found), invisibly.
#' @export
#' @examples
#' spec_intake_form()
spec_intake_form <- function(path = NULL) {
  src <- system.file("spec_builder", "spec_intake_form.md", package = "stateELP")
  if (!nzchar(src)) stop("The intake form is not installed.", call. = FALSE)
  if (is.null(path)) return(src)
  file.copy(src, path, overwrite = TRUE)
  invisible(path)
}
