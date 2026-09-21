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

#' Construct an `el_spec`
#'
#' The define-once parameter object. Every state-specific value the package
#' reads lives here; no `compute_*()` may carry a state value of its own.
#'
#' @param schema_version Character scalar, e.g. `"1.0.0"`.
#' @param program Assessment-program metadata: `name`, `state`, `vendor`,
#'   `administration_year`. Note this is the *assessment* program; Prep 3's
#'   inventory of what EL status gates is `policy$program`.
#' @param elp Measurement block: `instrument`, `administration_window`,
#'   `components`, `headline`, `level_scale`, `bands`, `grade_clusters`,
#'   `cuts`, `alternate`.
#' @param policy Policy block, restricted to [el_policy_fields()]. Every field
#'   carries a `source`.
#' @param academic Academic-assessment block, one entry per subject (plus an
#'   optional `spec_id` pointer). Check 3 drives its grade loop from
#'   `spec$academic$<subject>$grades`.
#' @param data Delivery block. `columns` holds one crosswalk per table --
#'   `roster`, `base`, `academic`, each a named character vector of
#'   ALL-CAPS canonical name = delivered name -- plus optional per-component
#'   entries (`components`) and `value_maps`: `CONTENT_AREA$base` and
#'   `CONTENT_AREA$academic` map delivered `CONTENT_AREA` *values* to
#'   component and subject keys, and `GRADE` maps delivered grade values
#'   (e.g. `"0"` to `"K"`). Values map by value, never by label.
#' @param verification `status`, `verified_by`, `verified_date`, `method`,
#'   `notes`.
#' @param source_documents List of `list(title=, url=)` entries.
#' @param validate Whether to validate on construction. Keep `TRUE`; `FALSE`
#'   exists so tests can build a deliberately invalid spec to check the
#'   validator's own messages.
#'
#' @return An object of class `el_spec`.
#' @export
el_spec <- function(schema_version = "1.0.0",
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
  for (sub in .academic_subjects(spec)) {
    a <- spec$academic[[sub]]
    if (!is.null(a$grades) && !is.character(a$grades)) {
      add("`academic$", sub, "$grades` must be character (\"3\", \"4\", ...) ",
          "or NULL for an end-of-course subject.")
    }
    if (!is.null(a$cuts) && (!is.numeric(a$cuts) ||
                             is.unsorted(a$cuts, strictly = TRUE))) {
      add("`academic$", sub, "$cuts` must be strictly increasing level minimums.")
    }
    if (.has(a, "op") && !identical(a$op, ">=")) {
      add("`academic$", sub, "$op` must be \">=\": cuts are level minimums, ",
          "so a score exactly at a cut belongs to the upper level.")
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
  setdiff(names(spec$academic), c("spec_id", "source"))
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

#' @export
print.el_spec <- function(x, ...) {
  cat("<el_spec>", x$program$state %||% "??",
      x$program$name %||% "", "\n")
  cat("  instrument :", x$elp$instrument %||% "-", "\n")
  cat("  components :", length(x$elp$components),
      paste0("(headline: ", x$elp$headline %||% "-", ")"), "\n")
  ty <- spec_exit_types(x)
  cat("  exit types :", paste(ty, collapse = ", "),
      paste0("- ", paste(el_exit_typology()[as.character(ty)], collapse = "; ")), "\n")
  cat("  timelines  : max", x$policy$timelines$max_years_to_exit %||% "-",
      "yrs; ESSA", x$policy$timelines$essa_timeline_years %||% "-", "yrs\n")
  cat("  suppression: min_cell", x$policy$suppression$min_cell %||% "-", "\n")
  cat("  verification:", x$verification$status %||% "-", "\n")
  prov <- names(x$policy)[vapply(x$policy, function(p)
    identical(p$source, "provisional"), TRUE)]
  if (length(prov)) {
    cat("  [DERIVED - VERIFY]:", paste(prov, collapse = ", "), "\n")
  }
  invisible(x)
}
