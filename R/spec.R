# el_spec: the define-once parameter object.
#
# This file is the ONE place component names may appear as string literals: it
# defines the controlled vocabulary the validator checks a spec against. Every
# other file iterates `names(spec$elp$components)` and filters on `kind`. A
# guardrail test greps R/ for component literals and exempts this file only.
#
# Block boundary (CLAUDE.md 5): vendor-changes-the-test -> `elp`;
# agency-changes-a-rule -> `policy`; state-changes-file-delivery -> `data`.
# The validator enforces that the fixed policy field list appears under
# `policy` and nowhere else.

# ---- controlled vocabularies -------------------------------------------------

#' The controlled component vocabulary
#'
#' `component` is a row key, not a set of instrument-named columns. Every
#' instrument maps its native names onto this vocabulary in its spec, via each
#' component's `native_name` (WIDA "Literacy" is `written`). Never extend this
#' to add an instrument's own word for something already here.
#'
#' @return A character vector of permitted component names.
#' @export
#' @examples
#' el_component_vocabulary()
el_component_vocabulary <- function() {
  c("listening", "speaking", "reading", "writing",
    "oral", "written", "comprehension", "expressive",
    "overall", "determination")
}

#' The fixed policy field list
#'
#' The validator requires every one of these to live under `policy` and rejects
#' any of them appearing under `elp`, so that no `compute_*()` can read a policy
#' value out of the measurement block.
#'
#' @return A character vector of permitted `policy` field names.
#' @export
el_policy_fields <- function() {
  c("exit", "timelines", "growth", "accountability_indicator",
    "suppression", "academic_benchmark", "district_size_bands",
    "program", "error_tolerance")
}

#' Exit-criteria typology
#'
#' The Process Overview's six structures. `policy$exit$types` declares which
#' apply; a state can be several. Every "Additional check" in the stage
#' documents is gated on a Type, so a Type 1 state skips all of them.
#'
#' @return A named character vector, names are the Type numbers.
#' @export
el_exit_typology <- function() {
  c("1" = "Single overall composite score",
    "2" = "Composite-plus-floor",
    "3" = "Composite-plus-other-source",
    "4" = "Compensatory combination",
    "5" = "Tiered/discretionary",
    "6" = "Domain profile")
}

# ---- small internal predicates -----------------------------------------------

.is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
.is_count <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && x == as.integer(x)
}
.has <- function(x, nm) !is.null(x[[nm]])

# ---- constructor -------------------------------------------------------------
#
# A person writes the SHORT form (el_spec()): the ELP score and its level
# range, the exit cut, the timelines, the academic proficiency cuts, the
# suppression rule. el_spec() checks it and expands it into the FULL internal
# list (.el_spec_full()) that every stage reads through the accessors below,
# so no stage code depends on which form was written. Everything the short
# form leaves out is a package convention (>=, the level measure, every
# criterion must be met, first-former-year exit dating, first-annual initial
# ELP) or a documented default.

.SPEC_STATUSES <- c("verified", "unverified", "provisional", "synthetic")

# Fields `sources` may name: the short form's own policy fields.
.SPEC_SOURCE_FIELDS <- c("exit", "timelines", "academic", "min_cell",
                         "district_sizes", "costlier_error")

# Fields `delivery` may carry: how a state's files differ from the canonical
# tables. Read by as_el_data() (the crosswalks and value maps) and Prep 2
# (entry dating).
.SPEC_DELIVERY_FIELDS <- c("roster", "base", "academic", "value_maps",
                           "entry_date_rule", "entry_sources",
                           "years_in_status_first_value")

# Every vocabulary component's kind. Fixed by what the component is, so the
# short form never asks for it.
.component_kinds <- function() {
  v <- el_component_vocabulary()
  stats::setNames(ifelse(v %in% c("listening", "speaking", "reading", "writing"),
                         "domain", "composite"), v)
}

#' Construct an `el_spec`
#'
#' The define-once parameter object: everything a state has to tell the
#' package, and nothing else. Write it in this short form; `el_spec()` checks
#' it, fills in the package's conventions and returns the object every stage
#' reads.
#'
#' What is **not** a field, because it is the same for every state: the exit
#' rule compares each score's *level* with `>=`, and a student must meet
#' every criterion; exit is dated to the first year observed as former; a
#' student's initial ELP is their first annual score.
#'
#' @param id Short name carried by every results object as `$meta$spec_id`,
#'   e.g. `"MA"`.
#' @param status How far the values can be trusted: `"verified"`,
#'   `"unverified"`, `"provisional"` or `"synthetic"`.
#' @param elp The ELP test, a list:
#'   * `test` -- its name, e.g. `"WIDA ACCESS"`;
#'   * `score` -- the score every stage runs on, a key of
#'     [el_component_vocabulary()] (WIDA's Overall Composite is `"overall"`,
#'     its Literacy score `"written"`);
#'   * `levels` -- the proficiency-level range, `c(lowest, highest)`, or the
#'     level names in order, lowest first, for a test that reports categories
#'     (a categorical score is taken to report no scale score);
#'   * `label` (optional) -- what the reports call the score, e.g.
#'     `"Overall Composite"`; or names keyed by component, to name an exit
#'     criterion too: `c(overall = "Overall Composite", written = "Literacy")`;
#'   * `bands` (optional) -- the lower edge of each reporting band,
#'     `c(1, 2, 3, 4, 4.5, 5)` gives `1.0-1.9` ... `4.5-4.9`, `5.0+`. Omitted:
#'     one band per level;
#'   * `level_labels` (optional) -- names of the whole levels of a numeric
#'     range, needed only when a delivery reports levels as names.
#' @param exit The exit rule: the minimum level on each score, named by
#'   component, e.g. `c(overall = 4.5)` or `c(overall = 4.2, written = 3.9)`.
#'   A single criterion must be on `elp$score`. On a categorical scale a
#'   level is its position (`3` = the third name).
#' @param timelines Years in EL status, a list: `max` (the maximum timeline to
#'   exit, required), `on_time` (the on-time or ESSA timeline; defaults to
#'   `max`), `lt_el` (the year from which a current EL is long-term; omitted,
#'   long-term status is not computed).
#' @param name Display name, e.g. `"Massachusetts"`. Defaults to `id`.
#' @param academic Optional. The state academic tests, a list named by
#'   subject; each a list of `grades` (the tested grades; `NULL` for an
#'   end-of-course test) and `proficient_cut` (the lowest proficient scale
#'   score: one number for every grade, or numbers named by grade,
#'   `c("3" = 450, "4" = 455)`). Optional per subject: `proficient_level`
#'   (the position of the lowest proficient performance level, lowest = 1:
#'   Check 2's academic summary reads proficiency from the delivered level
#'   against it, and Checks 3 and 4 do so for a grade with no cut; the same
#'   in every subject) and `level_labels` (the performance-level names,
#'   lowest first, needed only when a delivery reports them as names).
#'   Omitted: the academic analyses return `NULL` with a note.
#' @param min_cell Suppress any count below this. Default 10.
#' @param district_sizes Optional lower edges of the district-size groups,
#'   starting at 0, e.g. `c(0, 100, 1000)`. Omitted: the deck's groups
#'   (`<100`, `101-999`, `1,000+`).
#' @param costlier_error Which classification error stakeholders judge
#'   costlier: `"premature_exit"`, `"prolonged_retention"`, `"equal"` or
#'   `"unknown"` (the default). Read by Check 4.
#' @param sources Optional named character vector: for each sourced field
#'   (`exit`, `timelines`, `academic`, `min_cell`, `district_sizes`,
#'   `costlier_error`), where it is written down. A field not named reads
#'   `[DERIVED - VERIFY]`.
#' @param delivery Optional, how the state's files differ from the canonical
#'   tables: `roster`, `base` and `academic` crosswalks (named character
#'   vectors, ALL-CAPS canonical name = delivered name); `value_maps`
#'   (`CONTENT_AREA$base`, `CONTENT_AREA$academic`, `GRADE`, mapping delivered
#'   values to the package's); and Prep 2's entry dating (`entry_date_rule`,
#'   `entry_sources`, `years_in_status_first_value`).
#' @param ... Not used: anything here is an error that names the argument,
#'   so a spec written in the old long form says so instead of failing
#'   obscurely.
#'
#' @return An object of class `el_spec`.
#' @export
#' @examples
#' spec <- el_spec(
#'   id = "XX", status = "provisional",
#'   elp = list(test = "WIDA ACCESS", score = "overall",
#'              label = "Overall Composite", levels = c(1, 6),
#'              bands = c(1, 2, 3, 4, 4.5, 5)),
#'   exit = c(overall = 4.5),
#'   timelines = list(max = 6, on_time = 5),
#'   academic = list(ELA = list(grades = 3:8, proficient_cut = 750))
#' )
#' spec
el_spec <- function(id = NULL, status = NULL, elp = NULL, exit = NULL,
                    timelines = NULL, name = id, academic = NULL,
                    min_cell = 10L, district_sizes = NULL,
                    costlier_error = "unknown", sources = NULL,
                    delivery = NULL, ...) {
  extra <- names(list(...))
  if (length(list(...))) {
    stop("el_spec() does not take: ",
         paste(if (is.null(extra)) "unnamed argument(s)" else extra,
               collapse = ", "),
         ". It takes the short form -- id, status, elp, exit, timelines, ",
         "and optionally name, academic, min_cell, district_sizes, ",
         "costlier_error, sources, delivery. See ?el_spec.", call. = FALSE)
  }
  if (is.list(sources)) sources <- unlist(sources)
  short <- list(id = id, name = name, status = status, elp = elp,
                exit = exit, timelines = timelines, academic = academic,
                min_cell = min_cell, district_sizes = district_sizes,
                costlier_error = costlier_error, sources = sources,
                delivery = delivery)
  .validate_short_spec(short)
  .expand_el_spec(short)
}

# Every problem in a short-form spec, reported together.
.validate_short_spec <- function(s) {
  e <- character()
  add <- function(...) e <<- c(e, paste0(...))
  vocab <- el_component_vocabulary()
  fields <- function(x, allowed, where) {
    bad <- setdiff(names(x), allowed)
    if (length(bad)) {
      add("`", where, "` does not take: ", paste(bad, collapse = ", "),
          ". Its fields are ", paste(allowed, collapse = ", "), ".")
    }
  }

  if (!.is_string(s$id)) add("`id` must be a string, e.g. \"MA\".")
  if (!.is_string(s$name)) add("`name` must be a string.")
  if (!.is_string(s$status) || !s$status %in% .SPEC_STATUSES) {
    add("`status` must be one of ", paste(.SPEC_STATUSES, collapse = ", "), ".")
  }

  # --- the ELP test -----------------------------------------------------------
  elp <- s$elp
  lv <- NULL
  numeric_lv <- categorical_lv <- FALSE
  if (!is.list(elp)) {
    add("`elp` must be a list: test, score, levels (and optionally label, ",
        "bands, level_labels).")
    elp <- list()
  } else {
    fields(elp, c("test", "score", "label", "levels", "bands", "level_labels"),
           "elp")
    if (!.is_string(elp$test)) add("`elp$test` must be the test's name.")
    if (!.is_string(elp$score) || !elp$score %in% vocab) {
      add("`elp$score` must be one of: ", paste(vocab, collapse = ", "),
          ". Map the test's own name for it with `elp$label`.")
    }
    lab <- elp$label
    if (!is.null(lab)) {
      one <- .is_string(lab) && is.null(names(lab))
      keyed <- is.character(lab) && length(lab) >= 1L && !anyNA(lab) &&
        !is.null(names(lab)) && all(names(lab) %in% vocab) &&
        !anyDuplicated(names(lab))
      if (!one && !keyed) {
        add("`elp$label` must be the score's own name, \"Overall Composite\", ",
            "or names keyed by component: c(overall = \"Overall Composite\", ",
            "written = \"Literacy\").")
      }
    }
    lv <- elp$levels
    numeric_lv <- is.numeric(lv) && length(lv) == 2L && !anyNA(lv) &&
      lv[1] < lv[2]
    categorical_lv <- is.character(lv) && length(lv) >= 2L && !anyNA(lv) &&
      !anyDuplicated(lv)
    if (!numeric_lv && !categorical_lv) {
      add("`elp$levels` must be the level range, c(lowest, highest), or the ",
          "level names in order, lowest first.")
    }
    if (.has(elp, "bands")) {
      b <- elp$bands
      if (!numeric_lv) {
        add("`elp$bands` needs a numeric `elp$levels` range; a test that ",
            "reports categories gets one band per category.")
      } else if (!is.numeric(b) || !length(b) || anyNA(b) ||
                 is.unsorted(b, strictly = TRUE) || b[1] < lv[1] ||
                 b[length(b)] > lv[2]) {
        add("`elp$bands` must be the lower edge of each band, strictly ",
            "increasing and within `elp$levels` (", lv[1], "-", lv[2], ").")
      }
    }
    if (.has(elp, "level_labels")) {
      if (!numeric_lv) {
        add("`elp$level_labels` is for a numeric `elp$levels` range; ",
            "categorical levels are already names.")
      } else if (!is.character(elp$level_labels)) {
        add("`elp$level_labels` must be character.")
      }
    }
  }

  # --- the exit rule ----------------------------------------------------------
  ex <- s$exit
  if (!is.numeric(ex) || !length(ex) || is.null(names(ex)) || anyNA(ex) ||
      any(!nzchar(names(ex))) || anyDuplicated(names(ex))) {
    add("`exit` must be a named numeric vector, the minimum level on each ",
        "score: c(overall = 4.5), or c(overall = 4.2, written = 3.9).")
  } else {
    bad <- setdiff(names(ex), vocab)
    if (length(bad)) {
      add("`exit` names score(s) that are not components: ",
          paste(bad, collapse = ", "), ". Use the vocabulary keys: ",
          paste(vocab, collapse = ", "), ".")
    }
    if (length(ex) == 1L && .is_string(elp$score) &&
        !identical(names(ex), elp$score)) {
      add("A single exit criterion must be on `elp$score` (\"", elp$score,
          "\"); `exit` names \"", names(ex), "\".")
    }
    if (numeric_lv && any(ex < lv[1] | ex > lv[2])) {
      add("`exit` levels must lie within `elp$levels` (", lv[1], "-", lv[2],
          ").")
    }
    if (categorical_lv && any(ex != round(ex) | ex < 1 | ex > length(lv))) {
      add("On a categorical scale an `exit` level is a position, 1 to ",
          length(lv), " (", paste0(seq_along(lv), " = ", lv, collapse = ", "),
          ").")
    }
  }

  # --- timelines --------------------------------------------------------------
  tl <- s$timelines
  if (!is.list(tl)) {
    add("`timelines` must be a list: max (required), on_time, lt_el.")
  } else {
    fields(tl, c("max", "on_time", "lt_el"), "timelines")
    if (!.is_count(tl$max) || tl$max < 1) {
      add("`timelines$max` (the maximum years to exit) must be a whole ",
          "number of 1 or more.")
    }
    for (f in c("on_time", "lt_el")) {
      if (.has(tl, f) && (!.is_count(tl[[f]]) || tl[[f]] < 1)) {
        add("`timelines$", f, "` must be a whole number of 1 or more.")
      }
    }
  }

  # --- academic tests ---------------------------------------------------------
  ac <- s$academic
  if (!is.null(ac)) {
    if (!is.list(ac) || !length(ac) || is.null(names(ac)) ||
        any(!nzchar(names(ac)))) {
      add("`academic` must be a list named by subject, e.g. ",
          "list(ELA = list(grades = 3:8, proficient_cut = 750)).")
    } else {
      for (sb in names(ac)) {
        a <- ac[[sb]]
        tag <- paste0("`academic$", sb)
        if (!is.list(a)) {
          add(tag, "` must be a list: grades, proficient_cut (and optionally ",
              "proficient_level, level_labels).")
          next
        }
        fields(a, c("grades", "proficient_cut", "proficient_level",
                    "level_labels"), paste0("academic$", sb))
        g <- a$grades
        if (!is.null(g) && (!is.atomic(g) || !length(g) || anyNA(g) ||
                            anyDuplicated(as.character(g)))) {
          add(tag, "$grades` must be the tested grades, e.g. 3:8, or NULL ",
              "for an end-of-course test.")
        }
        pc <- a$proficient_cut
        if (!is.null(pc)) {
          if (!is.numeric(pc) || !length(pc) || anyNA(pc)) {
            add(tag, "$proficient_cut` must be a scale score, or scale ",
                "scores named by grade.")
          } else if (is.null(names(pc))) {
            if (length(pc) != 1L) {
              add(tag, "$proficient_cut` is one number for every grade, or ",
                  "one per grade named by grade: c(\"3\" = 450, \"4\" = 455).")
            }
          } else {
            pg <- names(pc)
            if (any(!nzchar(pg)) || anyDuplicated(pg)) {
              add(tag, "$proficient_cut` by grade needs each value named ",
                  "by a different grade.")
            } else if (!is.null(g)) {
              extra <- setdiff(pg, as.character(g))
              if (length(extra)) {
                add(tag, "$proficient_cut` names grade(s) not in `academic$",
                    sb, "$grades`: ", paste(extra, collapse = ", "), ".")
              }
            }
          }
        }
        if (.has(a, "proficient_level") &&
            (!.is_count(a$proficient_level) || a$proficient_level < 2)) {
          add(tag, "$proficient_level` must be a whole number of 2 or more ",
              "(lowest level = 1).")
        }
        if (.has(a, "level_labels") &&
            (!is.character(a$level_labels) || length(a$level_labels) < 2L)) {
          add(tag, "$level_labels` must name the performance levels, ",
              "lowest first.")
        }
      }
      lvls <- unique(unlist(lapply(ac, function(a) {
        if (is.list(a)) a$proficient_level
      })))
      if (length(lvls) > 1L) {
        add("`proficient_level` must be the same in every subject; got ",
            paste(lvls, collapse = ", "), ".")
      }
    }
  }

  # --- the rest ---------------------------------------------------------------
  if (!.is_count(s$min_cell) || s$min_cell < 1) {
    add("`min_cell` must be a whole number of 1 or more.")
  }
  ds <- s$district_sizes
  if (!is.null(ds) && (!is.numeric(ds) || length(ds) < 2L || anyNA(ds) ||
                       is.unsorted(ds, strictly = TRUE) || ds[1] != 0)) {
    add("`district_sizes` must be the lower edge of each district-size ",
        "group, starting at 0 and increasing, e.g. c(0, 100, 1000).")
  }
  ce <- c("premature_exit", "prolonged_retention", "equal", "unknown")
  if (!.is_string(s$costlier_error) || !s$costlier_error %in% ce) {
    add("`costlier_error` must be one of ", paste(ce, collapse = ", "), ".")
  }
  src <- s$sources
  if (!is.null(src)) {
    if (!is.character(src) || is.null(names(src)) || anyNA(src) ||
        any(!nzchar(names(src))) || anyDuplicated(names(src))) {
      add("`sources` must be a named character vector: field = where it is ",
          "written down.")
    } else {
      bad <- setdiff(names(src), .SPEC_SOURCE_FIELDS)
      if (length(bad)) {
        add("`sources` names field(s) it cannot source: ",
            paste(bad, collapse = ", "), ". It takes ",
            paste(.SPEC_SOURCE_FIELDS, collapse = ", "), ".")
      }
    }
  }
  dl <- s$delivery
  if (!is.null(dl)) {
    if (!is.list(dl)) {
      add("`delivery` must be a list.")
    } else {
      fields(dl, .SPEC_DELIVERY_FIELDS, "delivery")
    }
  }

  if (length(e)) {
    stop("Invalid el_spec:\n", paste0("  - ", e, collapse = "\n"), call. = FALSE)
  }
  invisible(s)
}

# Band labels from lower edges, to the edges' own precision: c(4, 4.5, 5)
# gives "4.0-4.4", "4.5-4.9", "5.0+". The top break sits one level above the
# range so the highest level falls in the last band (bands are left-closed).
.bands_from_edges <- function(edges, top) {
  d <- 1L
  while (d < 4L && any(abs(edges * 10^d - round(edges * 10^d)) > 1e-8)) {
    d <- d + 1L
  }
  step <- 10^-d
  n <- length(edges)
  fmt <- function(x) formatC(x, format = "f", digits = d)
  labels <- c(if (n > 1L) paste0(fmt(edges[-n]), "-", fmt(edges[-1] - step)),
              paste0(fmt(edges[n]), "+"))
  list(breaks = c(as.numeric(edges), top + 1), labels = labels)
}

# District-size groups from lower edges. Omitted, the deck's own groups,
# labels as the deck prints them (CLAUDE.md 3 records that "101-999" sits on
# a left-closed break at 100).
.district_bands <- function(edges) {
  if (is.null(edges)) {
    return(list(breaks = c(0, 100, 1000, Inf),
                labels = c("<100", "101-999", "1,000+")))
  }
  n <- length(edges)
  num <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  mid <- if (n > 2L) paste0(num(edges[2:(n - 1)]), "-", num(edges[3:n] - 1))
  list(breaks = c(as.numeric(edges), Inf),
       labels = c(paste0("<", num(edges[2])), mid, paste0(num(edges[n]), "+")))
}

# The Process Overview Type the rule's shape implies. Every stage gate reads
# Types 2-6 alike (more than one criterion), so only 1 vs more matters to the
# analysis; 6 is kept for a rule that is a profile of domains alone.
.derive_exit_types <- function(comps, score) {
  if (length(comps) == 1L) return(1L)
  kinds <- .component_kinds()[comps]
  if (!score %in% comps && sum(kinds == "domain") >= 2L) return(6L)
  2L
}

# Short form -> the full internal list, then the full validator.
.expand_el_spec <- function(s) {
  src <- function(field) {
    if (!is.null(s$sources) && field %in% names(s$sources)) {
      unname(s$sources[[field]])
    } else "provisional"
  }
  elp <- s$elp
  lv <- elp$levels
  categorical <- is.character(lv)
  score <- elp$score

  vocab <- el_component_vocabulary()
  kinds <- .component_kinds()
  # One unnamed label names the score; a keyed one names any component.
  labels <- elp$label
  if (!is.null(labels) && is.null(names(labels))) {
    labels <- stats::setNames(labels, score)
  }
  components <- stats::setNames(lapply(vocab, function(k) {
    cp <- list(kind = kinds[[k]],
               measures = if (identical(k, score) && categorical) "level"
                          else c("scale", "level"))
    if (k %in% names(labels)) cp$native_name <- unname(labels[[k]])
    cp
  }), vocab)

  level_scale <- if (categorical) {
    list(type = "categorical", levels = lv)
  } else {
    c(list(type = "numeric", range = as.numeric(lv)),
      if (!is.null(elp$level_labels)) list(labels = elp$level_labels))
  }
  bands <- if (!is.null(elp$bands)) .bands_from_edges(elp$bands, lv[2])

  # The score's own criterion first: that is how a state writes the rule.
  ex <- s$exit
  ex <- ex[c(which(names(ex) == score), which(names(ex) != score))]
  label_of <- function(k) {
    if (k %in% names(labels)) unname(labels[[k]]) else paste(k, kinds[[k]])
  }
  terms <- lapply(names(ex), function(k) {
    list(component = k, measure = "level", op = ">=", value = unname(ex[[k]]))
  })
  rule <- paste(vapply(names(ex), function(k) {
    paste0(label_of(k), " level >= ", format(ex[[k]]))
  }, ""), collapse = " and ")

  tl <- s$timelines
  timelines <- list(max_years_to_exit = as.integer(tl$max))
  if (!is.null(tl$on_time)) {
    timelines$essa_timeline_years <- as.integer(tl$on_time)
    timelines$on_time_rule <- list(default_years = as.integer(tl$on_time))
  }
  if (!is.null(tl$lt_el)) timelines$lt_el_years <- as.integer(tl$lt_el)
  timelines$exit_dating <- "first_former_year"
  timelines$initial_elp_source <- "first_annual"
  timelines$source <- src("timelines")

  policy <- list(
    exit = list(
      pathways = list(list(name = "exit", rule = rule, terms = terms,
                           combine = "all", discretionary = FALSE)),
      combination = "any_pathway",
      types = .derive_exit_types(names(ex), score),
      source = src("exit")),
    timelines = timelines,
    suppression = list(min_cell = as.integer(s$min_cell),
                       source = src("min_cell")),
    district_size_bands = c(.district_bands(s$district_sizes),
                            list(source = src("district_sizes"))),
    error_tolerance = list(costlier_error = s$costlier_error,
                           source = src("costlier_error"))
  )
  lvl <- unique(unlist(lapply(s$academic, function(a) a$proficient_level)))
  if (length(lvl)) {
    policy$academic_benchmark <- list(proficient_level = as.integer(lvl),
                                      source = src("academic"))
  }

  academic <- lapply(s$academic, function(a) {
    pc <- a$proficient_cut
    if (!is.null(pc)) {
      pc <- stats::setNames(as.numeric(pc), names(pc))
    }
    c(list(grades = if (!is.null(a$grades)) as.character(a$grades)),
      list(proficient_cut = pc, op = ">=", source = src("academic")),
      if (!is.null(a$level_labels)) list(level_labels = a$level_labels))
  })
  academic <- lapply(academic, function(a) Filter(Negate(is.null), a))

  dl <- s$delivery
  data <- list()
  cols <- dl[intersect(names(dl), c("roster", "base", "academic", "value_maps"))]
  if (length(cols)) data$columns <- cols
  for (f in c("entry_date_rule", "entry_sources", "years_in_status_first_value")) {
    if (!is.null(dl[[f]])) data[[f]] <- dl[[f]]
  }

  .el_spec_full(
    schema_version = "2.0.0",
    program = list(name = s$name, short_name = s$id, state = s$name),
    elp = c(list(instrument = elp$test, components = components,
                 headline = score, level_scale = level_scale),
            if (!is.null(bands)) list(bands = bands)),
    policy = policy,
    academic = if (length(academic)) academic else list(),
    data = data,
    verification = list(status = s$status),
    source_documents = list()
  )
}

# The full internal constructor: the shape every stage reads. Not written by
# hand any more -- el_spec() builds it -- but tests use it to build a
# deliberately unusual spec (a Type 3 or Type 5 rule, which the short form
# cannot express) and to exercise validate_el_spec() field by field.
#
# Blocks, as validate_el_spec() checks them:
#   program       name, short_name ($meta$spec_id), state.
#   elp           instrument, components (each kind, measures, optional
#                 native_name), headline, level_scale, bands.
#   policy        restricted to el_policy_fields(); every field carries a
#                 `source`.
#   academic      one entry per subject: grades, proficient_cut (or a full
#                 `cuts` table of level minimums, one vector or a list by
#                 grade), op, level_labels.
#   data          columns (roster / base / academic crosswalks, value_maps)
#                 and Prep 2's entry-dating fields.
#   verification  status.
# `validate = FALSE` lets a test build a deliberately invalid spec to check
# the validator's own messages.
.el_spec_full <- function(schema_version = "1.0.0",
                          program = list(),
                          elp = list(),
                          policy = list(),
                          academic = list(),
                          data = list(),
                          verification = list(),
                          source_documents = list(),
                          validate = TRUE) {
  spec <- list(
    schema_version   = schema_version,
    program          = program,
    elp              = elp,
    policy           = policy,
    academic         = academic,
    data             = data,
    verification     = verification,
    source_documents = source_documents
  )
  class(spec) <- "el_spec"
  if (validate) validate_el_spec(spec)
  spec
}

# ---- validator ---------------------------------------------------------------

#' Validate an `el_spec`
#'
#' Collects every problem and reports them together, rather than failing on the
#' first, so that a spec under construction can be fixed in one pass.
#'
#' Checks, in order: block presence; the component model; the headline;
#' the level scale and bands; the fixed policy field list and its
#' `source` requirement; the policy/measurement boundary; exit-rule terms
#' against the component model (validated, **never evaluated**); the
#' `policy$exit$types` declaration against the shape of the pathways; the
#' academic block; and the verification status.
#'
#' @param spec An `el_spec`.
#' @return `spec`, invisibly, if valid; otherwise an error listing every problem.
#' @export
validate_el_spec <- function(spec) {
  e <- character()
  add <- function(...) e <<- c(e, paste0(...))

  if (!inherits(spec, "el_spec")) add("spec must have class 'el_spec'.")

  for (blk in c("program", "elp", "policy", "academic", "data", "verification")) {
    if (!is.list(spec[[blk]])) add("Block `", blk, "` must be a list.")
  }
  if (!.is_string(spec$schema_version)) add("`schema_version` must be a string.")

  # --- component model --------------------------------------------------------
  comps <- spec$elp$components
  vocab <- el_component_vocabulary()
  if (!is.list(comps) || length(comps) == 0L || is.null(names(comps))) {
    add("`elp$components` must be a non-empty named list.")
    comps <- list()
  }
  for (nm in names(comps)) {
    cp <- comps[[nm]]
    if (!nm %in% vocab) {
      add("Component `", nm, "` is not in the controlled vocabulary. ",
          "Map an instrument's own name with `native_name` instead of ",
          "extending the vocabulary.")
    }
    if (!.is_string(cp$kind) || !cp$kind %in% c("domain", "composite")) {
      add("Component `", nm, "`: `kind` must be \"domain\" or \"composite\".")
    }
    if (!is.character(cp$measures) || !length(cp$measures) ||
        !all(cp$measures %in% c("scale", "level"))) {
      add("Component `", nm, "`: `measures` must be a non-empty subset of ",
          "c(\"scale\", \"level\").")
    }
    if (.has(cp, "from")) {
      if (!is.numeric(cp$from) || is.null(names(cp$from))) {
        add("Component `", nm, "`: `from` must be a named numeric vector.")
      } else {
        miss <- setdiff(names(cp$from), names(comps))
        if (length(miss)) {
          add("Component `", nm, "`: `from` references unknown component(s): ",
              paste(miss, collapse = ", "), ".")
        }
      }
      if (identical(cp$kind, "domain")) {
        add("Component `", nm, "`: a domain cannot have `from` weights.")
      }
    }
    if (.has(cp, "native_name") && !.is_string(cp$native_name)) {
      add("Component `", nm, "`: `native_name` must be a string.")
    }
  }

  # --- headline ---------------------------------------------------------------
  headline <- spec$elp$headline
  if (!.is_string(headline)) {
    add("`elp$headline` must be a string.")
  } else if (!headline %in% names(comps)) {
    add("`elp$headline` (\"", headline, "\") is not one of `elp$components`.")
  }

  # --- level scale and bands --------------------------------------------------
  ls <- spec$elp$level_scale
  if (!is.list(ls) || !.is_string(ls$type) ||
      !ls$type %in% c("numeric", "categorical")) {
    add("`elp$level_scale$type` must be \"numeric\" or \"categorical\".")
  } else if (identical(ls$type, "numeric")) {
    if (!is.numeric(ls$range) || length(ls$range) != 2L || ls$range[1] >= ls$range[2]) {
      add("`elp$level_scale$range` must be an increasing numeric pair.")
    } else if (!is.null(ls$labels)) {
      # One label per whole level in the range: a WIDA-shaped 1-6 scale names
      # six levels. A short list would leave some levels unnamed, and a
      # labeled delivery parsed against it would map onto the wrong level.
      n_whole <- floor(ls$range[2]) - ceiling(ls$range[1]) + 1
      if (!is.character(ls$labels) || length(ls$labels) != n_whole) {
        add("`elp$level_scale$labels` must name every whole level in the ",
            "range ", ls$range[1], "-", ls$range[2], ": expected ", n_whole,
            " label(s), got ", length(ls$labels), ".")
      }
    }
  } else if (!is.character(ls$levels) || !length(ls$levels)) {
    add("`elp$level_scale$levels` must be a non-empty character vector.")
  }

  bands <- spec$elp$bands
  if (!is.null(bands)) {
    if (!is.numeric(bands$breaks) || !is.character(bands$labels)) {
      add("`elp$bands` must carry numeric `breaks` and character `labels`.")
    } else {
      if (length(bands$breaks) != length(bands$labels) + 1L) {
        add("`elp$bands`: length(breaks) must be length(labels) + 1 ",
            "(got ", length(bands$breaks), " and ", length(bands$labels), ").")
      }
      if (is.unsorted(bands$breaks, strictly = TRUE)) {
        add("`elp$bands$breaks` must be strictly increasing.")
      }
    }
  }

  # --- policy: fixed field list, sources, and the block boundary --------------
  pol <- spec$policy
  allowed <- el_policy_fields()
  extra <- setdiff(names(pol), allowed)
  if (length(extra)) {
    add("`policy` has field(s) outside the fixed list: ",
        paste(extra, collapse = ", "), ".")
  }
  for (req in c("exit", "timelines", "suppression")) {
    if (!.has(pol, req)) add("`policy$", req, "` is required.")
  }
  for (nm in intersect(names(pol), allowed)) {
    if (!is.list(pol[[nm]]) || !.is_string(pol[[nm]]$source)) {
      add("`policy$", nm, "` must carry a `source` string. ",
          "Use source = \"provisional\" for a value that is not yet verified; ",
          "it renders as [DERIVED - VERIFY].")
    }
  }
  leaked <- intersect(names(spec$elp), allowed)
  if (length(leaked)) {
    add("Policy field(s) found under `elp`: ", paste(leaked, collapse = ", "),
        ". A rule an agency can change belongs under `policy`.")
  }

  # --- exit rule: validated, never evaluated ----------------------------------
  exit <- pol$exit
  paths <- exit$pathways
  if (!is.null(exit)) {
    if (!is.list(paths) || !length(paths)) {
      add("`policy$exit$pathways` must be a non-empty list.")
      paths <- list()
    }
    if (.has(exit, "combination") &&
        !exit$combination %in% c("any_pathway", "all_pathways")) {
      add("`policy$exit$combination` must be \"any_pathway\" or \"all_pathways\".")
    }
    for (i in seq_along(paths)) {
      p <- paths[[i]]
      tag <- paste0("`policy$exit$pathways[[", i, "]]`")
      if (!.is_string(p$rule)) add(tag, ": `rule` (the display string) is required.")
      if (.has(p, "combine") && !p$combine %in% c("all", "any")) {
        add(tag, ": `combine` must be \"all\" or \"any\".")
      }
      if (!is.list(p$terms) || !length(p$terms)) {
        add(tag, ": `terms` must be a non-empty list.")
        next
      }
      for (j in seq_along(p$terms)) {
        tm <- p$terms[[j]]
        ttag <- paste0(tag, " term ", j)
        from <- if (.has(tm, "from")) tm$from else "elp"
        if (!from %in% c("elp", "academic")) {
          add(ttag, ": `from` must be \"elp\" or \"academic\".")
          next
        }
        if (identical(from, "elp")) {
          if (!.is_string(tm$component) || !tm$component %in% names(comps)) {
            add(ttag, ": `component` must name one of `elp$components`.")
          } else if (!.is_string(tm$measure) ||
                     !tm$measure %in% comps[[tm$component]]$measures) {
            add(ttag, ": `measure` (\"", tm$measure %||% "", "\") is not ",
                "reported by component `", tm$component, "`.")
          }
        } else {
          if (!.is_string(tm$component) ||
              !tm$component %in% .academic_subjects(spec)) {
            add(ttag, ": `component` must name a subject in `academic` ",
                "when from = \"academic\".")
          }
        }
        if (!.is_string(tm$op) || !tm$op %in% c(">=", ">", "<=", "<", "==")) {
          add(ttag, ": `op` must be one of >=, >, <=, <, ==.")
        }
        if (length(tm$value) != 1L) add(ttag, ": `value` must be a scalar.")
        if (.has(tm, "exempt_if_absent") && !is.logical(tm$exempt_if_absent)) {
          add(ttag, ": `exempt_if_absent` must be TRUE/FALSE.")
        }
      }
    }
    e <- c(e, .validate_exit_types(exit, paths, comps, headline))
  }

  # --- timelines --------------------------------------------------------------
  tl <- pol$timelines
  if (!is.null(tl)) {
    if (!.is_count(tl$max_years_to_exit)) {
      add("`policy$timelines$max_years_to_exit` must be a whole number.")
    }
    if (.has(tl, "essa_timeline_years") && !.is_count(tl$essa_timeline_years)) {
      add("`policy$timelines$essa_timeline_years` must be a whole number.")
    }
    if (.has(tl, "exit_dating") && !identical(tl$exit_dating, "first_former_year")) {
      add("`policy$timelines$exit_dating` must be \"first_former_year\". ",
          "The package dates exit to the first year observed as former ",
          "(the deck's A-J convention); a delivery using another convention ",
          "is translated at the boundary, not here.")
    }
    if (.has(tl, "lt_el_years") && !.is_count(tl$lt_el_years)) {
      add("`policy$timelines$lt_el_years` must be a whole number: the year in ",
          "status from which a current EL counts as long-term.")
    }
    otr <- tl$on_time_rule
    if (!is.null(otr)) {
      if (!is.list(otr)) {
        add("`policy$timelines$on_time_rule` must be a list.")
      } else {
        if (.has(otr, "default_years") && !.is_count(otr$default_years)) {
          add("`policy$timelines$on_time_rule$default_years` must be a whole number.")
        }
        for (key in c("by_start_grade", "by_initial_band")) {
          v <- otr[[key]]
          if (!is.null(v) && (!is.numeric(v) || is.null(names(v)) ||
                              anyNA(v) || any(v != round(v)))) {
            add("`policy$timelines$on_time_rule$", key, "` must be a named ",
                "vector of whole years (names are the ",
                if (key == "by_start_grade") "start grades" else "initial bands",
                ").")
          }
        }
        if (is.numeric(otr$by_initial_band) && !is.null(bands$labels)) {
          miss <- setdiff(names(otr$by_initial_band), bands$labels)
          if (length(miss)) {
            add("`policy$timelines$on_time_rule$by_initial_band` names band(s) ",
                "not in `elp$bands$labels`: ", paste(miss, collapse = ", "), ".")
          }
        }
      }
    }
  }

  # --- suppression ------------------------------------------------------------
  if (!is.null(pol$suppression) && !.is_count(pol$suppression$min_cell)) {
    add("`policy$suppression$min_cell` must be a whole number.")
  }

  # --- error tolerance --------------------------------------------------------
  et <- pol$error_tolerance
  if (!is.null(et) && !is.null(et$costlier_error) &&
      !et$costlier_error %in% c("premature_exit", "prolonged_retention",
                                "equal", "unknown")) {
    add("`policy$error_tolerance$costlier_error` must be one of ",
        "premature_exit, prolonged_retention, equal, unknown.")
  }

  # --- academic ---------------------------------------------------------------
  subjects_declared <- .academic_subjects(spec)
  for (sub in subjects_declared) {
    a <- spec$academic[[sub]]
    if (!is.null(a$grades) && !is.character(a$grades)) {
      add("`academic$", sub, "$grades` must be character (\"3\", \"4\", ...) ",
          "or NULL for an end-of-course subject.")
    }
    e <- c(e, .validate_academic_cuts(a, sub))
    pc <- a$proficient_cut
    if (!is.null(pc) && (!is.numeric(pc) || anyNA(pc) ||
                         (is.null(names(pc)) && length(pc) != 1L))) {
      add("`academic$", sub, "$proficient_cut` must be one scale score, or ",
          "scale scores named by grade.")
    }
    if (.has(a, "op") && !identical(a$op, ">=")) {
      add("`academic$", sub, "$op` must be \">=\": cuts are level minimums, ",
          "so a score exactly at a cut belongs to the upper level.")
    }
  }

  # The proficient level only matters once a subject is declared: it is what
  # turns a cut table, or a delivered performance level, into "proficient".
  # An NA there used to surface as an `if (NA)` error deep inside Check 3.
  lvl <- pol$academic_benchmark$proficient_level
  if (length(subjects_declared) && !is.null(lvl)) {
    if (!.is_count(lvl) || lvl < 2) {
      add("`policy$academic_benchmark$proficient_level` must be a whole ",
          "number of 2 or more (the position of the lowest proficient level, ",
          "lowest level = 1) once `academic` declares a subject; got ",
          if (length(lvl) == 1L && is.na(lvl)) "NA" else
            paste(format(lvl), collapse = ", "), ".")
    } else {
      for (sub in subjects_declared) {
        n_lv <- length(spec$academic[[sub]]$level_labels)
        if (n_lv > 0L && lvl > n_lv) {
          add("`policy$academic_benchmark$proficient_level` (", lvl, ") is ",
              "above the ", n_lv, " levels `academic$", sub,
              "$level_labels` declares.")
        }
      }
    }
  }

  # --- delivery crosswalks ----------------------------------------------------
  # Canonical names are the ALL-CAPS column names of the three tables
  # (sim_contract.md 0.4.0); each entry maps canonical = delivered.
  cols <- spec$data$columns
  if (!is.null(cols)) {
    for (tbl in intersect(c("roster", "base", "academic"), names(cols))) {
      m <- cols[[tbl]]
      if (!is.character(m) || is.null(names(m))) {
        add("`data$columns$", tbl, "` must be a named character vector: ",
            "canonical name = delivered name.")
      } else if (any(names(m) != toupper(names(m)))) {
        add("`data$columns$", tbl, "`: canonical names are ALL CAPS; got ",
            paste(names(m)[names(m) != toupper(names(m))], collapse = ", "), ".")
      }
    }
    if (!is.null(cols$components)) {
      miss <- setdiff(names(cols$components), names(comps))
      if (length(miss)) {
        add("`data$columns$components` has entries for unknown component(s): ",
            paste(miss, collapse = ", "), ".")
      }
    }
    ca <- cols$value_maps$CONTENT_AREA
    if (!is.null(ca$base)) {
      miss <- setdiff(unname(ca$base), names(comps))
      if (length(miss)) {
        add("`data$columns$value_maps$CONTENT_AREA$base` maps to unknown ",
            "component(s): ", paste(miss, collapse = ", "), ".")
      }
    }
    subjects <- .academic_subjects(spec)
    if (!is.null(ca$academic) && length(subjects)) {
      miss <- setdiff(unname(ca$academic), subjects)
      if (length(miss)) {
        add("`data$columns$value_maps$CONTENT_AREA$academic` maps to unknown ",
            "subject(s): ", paste(miss, collapse = ", "), ".")
      }
    }
  }

  # --- verification -----------------------------------------------------------
  vs <- spec$verification$status
  if (!.is_string(vs) ||
      !vs %in% c("verified", "unverified", "provisional", "synthetic")) {
    add("`verification$status` must be one of verified, unverified, ",
        "provisional, synthetic.")
  }

  if (length(e)) {
    stop("Invalid el_spec:\n", paste0("  - ", e, collapse = "\n"), call. = FALSE)
  }
  invisible(spec)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.academic_subjects <- function(spec) {
  # Always character: no academic block is character(0), never NULL.
  as.character(setdiff(names(spec$academic), c("spec_id", "source")))
}

# A subject's `cuts` take one of two shapes:
#   * a numeric vector -- the minimum scale score of levels 2..n, the same in
#     every tested grade;
#   * a named list, one such vector per grade, names the grades ("3", "4", ...).
# Most state tests set their cuts grade by grade, so the
# second shape is the common one; the first is kept for a vertical scale or an
# end-of-course test.
.validate_academic_cuts <- function(a, sub) {
  e <- character()
  cuts <- a$cuts
  if (is.null(cuts)) return(e)
  tag <- paste0("`academic$", sub, "$cuts`")
  one <- function(v, where) {
    if (!is.numeric(v) || !length(v) || anyNA(v) ||
        is.unsorted(v, strictly = TRUE)) {
      return(paste0(where, " must be strictly increasing level minimums ",
                    "(the lowest scale score of each level above the first)."))
    }
    n_lv <- length(a$level_labels)
    if (n_lv > 0L && length(v) != n_lv - 1L) {
      return(paste0(where, " has ", length(v), " cut(s), but `level_labels` ",
                    "names ", n_lv, " levels: expected ", n_lv - 1L,
                    " (one per level above the lowest)."))
    }
    character()
  }
  if (is.list(cuts)) {
    g <- names(cuts)
    if (is.null(g) || any(!nzchar(g)) || anyDuplicated(g)) {
      return(paste0(tag, " given by grade must be a list named by grade, ",
                    "e.g. list(\"3\" = c(...), \"4\" = c(...))."))
    }
    if (!is.null(a$grades)) {
      extra <- setdiff(g, a$grades)
      if (length(extra)) {
        e <- c(e, paste0(tag, " names grade(s) not in `academic$", sub,
                         "$grades`: ", paste(extra, collapse = ", "), "."))
      }
    }
    for (gr in g) e <- c(e, one(cuts[[gr]], paste0(tag, "[[\"", gr, "\"]]")))
    return(e)
  }
  c(e, one(cuts, tag))
}

# Cross-check the declared Type(s) against the shape of the pathways.
# Types 1, 2, 3, 5 and 6 are structurally checkable; Type 4 (compensatory) is
# a schedule the shell does not yet represent, so it is accepted as declared.
# [PROPOSED - CONFIRM]
.validate_exit_types <- function(exit, paths, comps, headline) {
  e <- character()
  types <- exit$types
  if (is.null(types)) {
    return("`policy$exit$types` is required: it gates every Type-specific check.")
  }
  if (!is.numeric(types) || !length(types) || !all(types %in% 1:6)) {
    return("`policy$exit$types` must be a non-empty integer vector within 1:6.")
  }

  all_terms <- unlist(lapply(paths, function(p) p$terms), recursive = FALSE)
  n_terms <- length(all_terms)
  term_comp <- vapply(all_terms, function(t) t$component %||% NA_character_, "")
  term_from <- vapply(all_terms, function(t) t$from %||% "elp", "")
  elp_comps <- term_comp[term_from == "elp"]
  kinds <- vapply(elp_comps, function(k) comps[[k]]$kind %||% NA_character_,
                  "", USE.NAMES = FALSE)

  if (1 %in% types) {
    if (length(paths) != 1L || n_terms != 1L) {
      e <- c(e, paste0("Type 1 declared (single overall composite), but the ",
                       "rule has ", length(paths), " pathway(s) and ", n_terms,
                       " term(s). Declare the Type(s) the rule actually has."))
    } else if (!identical(term_comp[[1]], headline)) {
      e <- c(e, paste0("Type 1 declared, but the single term is on `",
                       term_comp[[1]], "`, not the headline `", headline, "`."))
    }
  }
  if (2 %in% types && !any(vapply(paths, function(p) length(p$terms) >= 2L, TRUE))) {
    e <- c(e, paste0("Type 2 declared (composite-plus-floor), but no pathway ",
                     "carries two or more terms."))
  }
  if (3 %in% types && !any(term_from == "academic")) {
    e <- c(e, paste0("Type 3 declared (composite-plus-other-source), but no ",
                     "term draws on a non-ELP source (`from = \"academic\"`)."))
  }
  if (5 %in% types) {
    discretionary <- any(vapply(paths, function(p) isTRUE(p$discretionary), TRUE))
    if (length(paths) < 2L && !discretionary) {
      e <- c(e, paste0("Type 5 declared (tiered/discretionary), but there is a ",
                       "single pathway and none is flagged `discretionary`."))
    }
  }
  if (6 %in% types) {
    if (n_terms == 1L && identical(term_comp[[1]], headline)) {
      e <- c(e, paste0("Type 6 declared (domain profile), but the rule is a ",
                       "single term on the headline `", headline, "`."))
    } else if (sum(kinds == "domain") < 2L) {
      e <- c(e, paste0("Type 6 declared (domain profile), but fewer than two ",
                       "terms reference components of kind \"domain\"."))
    }
  }
  e
}

# ---- accessors ---------------------------------------------------------------
# Every stage reads the spec through these, so that a shape change lands in one
# place. None of them names a component.

#' Spec accessors
#'
#' Read-only helpers over an `el_spec`. Stage code uses these rather than
#' reaching into the list, so that a spec shape change lands in one file.
#'
#' @param spec An `el_spec`.
#' @param kind Optionally `"domain"` or `"composite"` to filter components.
#' @param subject An academic subject name.
#' @param type An exit-criteria Type number (1-6).
#' Reader-facing text never says "headline" -- that is this package's word
#' for the abstraction, not a state's word for its score.
#' `spec_headline_label()` is what a rendered document names instead: the
#' state's own name for the component, so the same sentence reads correctly
#' for a WIDA composite and for an ELPA21 determination.
#'
#' @name el_spec_accessors
#' @return `spec_components()` a character vector; `spec_headline()` a string
#'   (the vocabulary key) and `spec_headline_label()` its display name;
#'   `spec_bands()` a list or `NULL`; `spec_min_cell()`, `spec_max_years()` and
#'   `spec_essa_years()` whole numbers; `spec_exit_types()` a numeric vector;
#'   `spec_has_type()` a flag; `spec_academic_subjects()` a character vector;
#'   `spec_academic_grades()` a character vector or `NULL`.
NULL

#' @rdname el_spec_accessors
#' @export
spec_components <- function(spec, kind = NULL) {
  nms <- names(spec$elp$components)
  if (is.null(kind)) return(nms)
  keep <- vapply(spec$elp$components, function(cp) identical(cp$kind, kind), TRUE)
  nms[keep]
}

#' @rdname el_spec_accessors
#' @export
spec_headline <- function(spec) spec$elp$headline

#' @rdname el_spec_accessors
#' @export
spec_headline_label <- function(spec) {
  headline <- spec_headline(spec)
  if (is.null(headline)) return(NA_character_)
  cp <- spec$elp$components[[headline]]
  # The state's own name for it when the spec gives one -- "Overall
  # Composite", "Overall Performance Determination". Otherwise the
  # vocabulary key and its kind, which reads as "overall composite": both
  # halves come from the spec, so no component name is typed here.
  if (!is.null(cp$native_name)) return(cp$native_name)
  if (is.null(cp$kind)) headline else paste(headline, cp$kind)
}

#' @rdname el_spec_accessors
#' @export
spec_bands <- function(spec) spec$elp$bands

#' @rdname el_spec_accessors
#' @export
spec_min_cell <- function(spec) as.integer(spec$policy$suppression$min_cell)

#' @rdname el_spec_accessors
#' @export
spec_max_years <- function(spec) as.integer(spec$policy$timelines$max_years_to_exit)

#' @rdname el_spec_accessors
#' @export
spec_essa_years <- function(spec) {
  v <- spec$policy$timelines$essa_timeline_years
  if (is.null(v)) NA_integer_ else as.integer(v)
}

#' @rdname el_spec_accessors
#' @export
spec_exit_types <- function(spec) spec$policy$exit$types

#' @rdname el_spec_accessors
#' @export
spec_has_type <- function(spec, type) {
  any(spec_exit_types(spec) %in% type)
}

#' @rdname el_spec_accessors
#' @export
spec_academic_subjects <- function(spec) .academic_subjects(spec)

#' @rdname el_spec_accessors
#' @export
spec_academic_grades <- function(spec, subject) spec$academic[[subject]]$grades

#' Academic cut scores and the proficiency cut
#'
#' A spec gives each subject's proficient cut, `academic$<subject>$
#' proficient_cut`: one scale score for every grade, or scores named by
#' grade. These accessors hide which: every stage asks for a subject *and a
#' grade* and gets that grade's cut.
#'
#' * `spec_proficient_cut()` returns the lowest proficient scale score for
#'   one subject and grade (`NA` when the spec gives none for that grade). It
#'   is the red line in Check 3's box plots and the content classification in
#'   Check 4.
#' * `spec_proficient_level()` returns the position of the lowest proficient
#'   performance level (`NA` when the spec does not set it), used only for a
#'   grade with no cut.
#' * `spec_academic_cuts()` returns a full table of level minimums for one
#'   subject and grade. A spec written in the short form carries only the
#'   proficient cut, so this is `NULL` for it; it serves a full internal spec
#'   that carries `academic$<subject>$cuts`.
#'
#' When the spec has no cut for a grade, Checks 3 and 4 fall back to the
#' academic table's own `ACHIEVEMENT_LEVEL`, compared with the proficient
#' level.
#'
#' @param spec An `el_spec`.
#' @param subject An academic subject name.
#' @param grade A tested grade, as character. Ignored when the subject's cuts
#'   are the same in every grade; required to pick one when they differ.
#' @return See above.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' spec_academic_cuts(spec, "ELA", "5")
#' spec_proficient_cut(spec, "ELA", "5")
spec_academic_cuts <- function(spec, subject, grade = NULL) {
  cuts <- spec$academic[[subject]]$cuts
  if (is.null(cuts)) return(NULL)
  if (!is.list(cuts)) return(as.numeric(cuts))
  if (is.null(grade) || length(grade) != 1L || is.na(grade)) return(NULL)
  v <- cuts[[as.character(grade)]]
  if (is.null(v)) NULL else as.numeric(v)
}

#' @rdname spec_academic_cuts
#' @export
spec_proficient_level <- function(spec) {
  lvl <- spec$policy$academic_benchmark$proficient_level
  if (is.null(lvl) || length(lvl) != 1L || is.na(lvl)) NA_integer_
  else as.integer(lvl)
}

#' @rdname spec_academic_cuts
#' @export
spec_proficient_cut <- function(spec, subject, grade = NULL) {
  # The short form stores the proficient cut itself.
  pc <- spec$academic[[subject]]$proficient_cut
  if (!is.null(pc)) {
    if (is.null(names(pc))) return(as.numeric(pc[[1]]))
    if (is.null(grade) || length(grade) != 1L || is.na(grade)) return(NA_real_)
    i <- match(as.character(grade), names(pc))
    return(if (is.na(i)) NA_real_ else as.numeric(pc[[i]]))
  }
  lvl <- spec_proficient_level(spec)
  cuts <- spec_academic_cuts(spec, subject, grade)
  # `cuts` are the minima of levels 2..n, so level k is cuts[k - 1].
  if (is.na(lvl) || is.null(cuts) || lvl < 2L || lvl - 1L > length(cuts)) {
    return(NA_real_)
  }
  as.numeric(cuts[[lvl - 1L]])
}

# ---- loading -----------------------------------------------------------------

#' Locate and load a shipped spec
#'
#' Spec instantiations are plain R files under `inst/specs/`; each builds a list
#' and closes with a call to [el_spec()], so sourcing one validates it.
#'
#' @param id A spec id (the file name without `.R`, e.g. `"00_SIM_EL_Spec"`),
#'   or a path to a spec file.
#' @return `el_spec_path()` a file path; `el_spec_files()` the available ids;
#'   `read_el_spec()` a validated `el_spec`.
#' @export
#' @examples
#' el_spec_files()
#' sim <- read_el_spec("00_SIM_EL_Spec")
#' spec_headline(sim)
el_spec_path <- function(id) {
  if (file.exists(id)) return(id)
  p <- system.file("specs", paste0(id, ".R"), package = "stateELP")
  if (!nzchar(p)) {
    stop("No spec named '", id, "'. Available: ",
         paste(el_spec_files(), collapse = ", "), call. = FALSE)
  }
  p
}

#' @rdname el_spec_path
#' @export
el_spec_files <- function() {
  d <- system.file("specs", package = "stateELP")
  if (!nzchar(d)) return(character())
  sub("\\.R$", "", list.files(d, pattern = "\\.R$"))
}

#' @rdname el_spec_path
#' @export
read_el_spec <- function(id) {
  env <- new.env(parent = parent.env(globalenv()))
  sys.source(el_spec_path(id), envir = env)
  objs <- mget(ls(env), envir = env)
  specs <- Filter(function(o) inherits(o, "el_spec"), objs)
  if (length(specs) != 1L) {
    stop("Spec file '", id, "' must define exactly one el_spec object; found ",
         length(specs), ".", call. = FALSE)
  }
  specs[[1]]
}

# ---- display -----------------------------------------------------------------

# Grades as runs: c("3", ..., "8", "11") prints "3-8, 11", never "3-11".
.grade_runs <- function(g) {
  n <- suppressWarnings(as.integer(g))
  if (anyNA(n) || is.unsorted(n, strictly = TRUE)) return(paste(g, collapse = ", "))
  ends <- c(0L, which(diff(n) != 1L), length(n))
  paste(vapply(seq_len(length(ends) - 1L), function(i) {
    r <- n[(ends[i] + 1L):ends[i + 1L]]
    if (length(r) > 2L) paste0(r[1], "-", r[length(r)]) else paste(r, collapse = ", ")
  }, ""), collapse = ", ")
}

#' @export
print.el_spec <- function(x, ...) {
  id <- x$program$short_name %||% "??"
  nm <- x$program$name %||% ""
  cat("<el_spec>", id, if (!identical(nm, id)) paste0("(", nm, ")"), "\n")

  ls <- x$elp$level_scale
  levels <- if (identical(ls$type, "categorical")) {
    paste(ls$levels, collapse = " < ")
  } else paste(ls$range, collapse = "-")
  nb <- length(spec_bands(x)$labels)
  cat("  ELP test   :", x$elp$instrument %||% "-", "--",
      spec_headline_label(x), paste0("(", spec_headline(x), "),"),
      "levels", levels,
      if (nb) paste0("in ", nb, " bands") else "", "\n")

  ty <- spec_exit_types(x)
  rules <- vapply(x$policy$exit$pathways, function(p) p$rule %||% "-", "")
  cat("  exit       :", paste(rules, collapse = "; or "),
      paste0("[Type ", paste(ty, collapse = ", "), "]"), "\n")

  tl <- x$policy$timelines
  cat("  timelines  : max", tl$max_years_to_exit %||% "-", "yrs; on time",
      tl$on_time_rule$default_years %||% tl$essa_timeline_years %||%
        tl$max_years_to_exit %||% "-",
      "yrs; long-term EL",
      if (is.null(tl$lt_el_years)) "not set" else paste("at", tl$lt_el_years, "yrs"),
      "\n")

  subs <- spec_academic_subjects(x)
  if (length(subs)) {
    desc <- vapply(subs, function(sb) {
      a <- x$academic[[sb]]
      g <- a$grades
      gr <- if (is.null(g)) "end of course" else
        paste(if (length(g) > 1L) "grades" else "grade", .grade_runs(g))
      pc <- a$proficient_cut
      cut <- if (is.null(pc) && is.null(a$cuts)) "no cut" else
        if (!is.null(pc) && is.null(names(pc))) paste("cut", pc) else
          "cuts by grade"
      paste0(sb, " (", gr, ", ", cut, ")")
    }, "")
    cat("  academic   :", paste(desc, collapse = "; "), "\n")
  } else {
    cat("  academic   : none\n")
  }
  cat("  suppression: min_cell", x$policy$suppression$min_cell %||% "-", "\n")
  cat("  status     :", x$verification$status %||% "-", "\n")
  # Named as the short form names them, which is what a reader edits.
  prov <- names(x$policy)[vapply(x$policy, function(p)
    identical(p$source, "provisional"), TRUE)]
  short <- c(suppression = "min_cell", district_size_bands = "district_sizes",
             error_tolerance = "costlier_error", academic_benchmark = "academic")
  prov <- ifelse(prov %in% names(short), short[prov], prov)
  if (any(vapply(subs, function(sb) identical(x$academic[[sb]]$source,
                                              "provisional"), TRUE))) {
    prov <- c(prov, "academic")
  }
  prov <- unique(prov)
  if (length(prov)) {
    cat("  [DERIVED - VERIFY]:", paste(prov, collapse = ", "), "\n")
  }
  invisible(x)
}
