# The delivery boundary.
#
# The three long tables are the analysis model (sim_contract.md 0.4.0); the
# delivered form is whatever a state hands over. as_el_data() is the adapter,
# and it is the only place that knows about delivered shapes. Every
# data-taking run_<stage>() takes the validated bundle, never raw tables, so no
# stage has to ask what form the data arrived in.
#
#   base      one row per STUDENT_ID x YEAR x COMPONENT -- every ELP
#             measurement, and the only source of it
#   roster    one row per STUDENT_ID x YEAR -- the counting frame; no
#             measurement columns
#   academic  one row per STUDENT_ID x YEAR x SUBJECT
#
# Column names are ALL CAPS per ReportR's student_results_long conventions.
# Nothing in this file names a component: the headline is read from the spec.

ROSTER_REQUIRED   <- c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS")
BASE_COLUMNS      <- c("STUDENT_ID", "YEAR", "COMPONENT", "ELP_FORM",
                       "SCALE_SCORE", "ACHIEVEMENT_LEVEL")
ACADEMIC_REQUIRED <- c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT")
ACADEMIC_COLUMNS  <- c(ACADEMIC_REQUIRED, "SCALE_SCORE", "ACHIEVEMENT_LEVEL",
                       "ACHIEVEMENT_LEVEL_ALT")
EL_STATUS_VALUES  <- c("current", "former", "never")
KEY               <- c("STUDENT_ID", "YEAR")

#' Build a validated EL data bundle
#'
#' Two delivery forms land here directly:
#'
#' 1. **Separate objects** -- the sim's own call,
#'    `as_el_data(el_long_sim, roster = el_roster_sim, academic = academic_long_sim)`.
#' 2. **A fully merged single long file** -- roster attributes repeated on
#'    every component row, SGP LONG_DATA style -- passed as `elp` with
#'    `roster = NULL`. It is split on intake: roster attributes become the
#'    roster (one row per student-year, including rows whose `COMPONENT` is
#'    `NA`, which is how an untested or former student-year is carried), and
#'    measurement columns become the base table.
#'
#' SGP LONG_DATA files go through [as_el_data_sgp()] first, which recodes them
#' into the merged form.
#'
#' @section Validation:
#' Roster unique on `STUDENT_ID x YEAR`, with `EL_STATUS` in
#' `current / former / never`. Base table unique on `STUDENT_ID x YEAR x
#' COMPONENT`, `COMPONENT` character and (with a spec) inside the spec's
#' component model, every row with a roster row, and `ELP_FORM` equal to the
#' roster's. Academic table keyed on `SUBJECT`, `GRADE` present -- no
#' fill-from-roster fallback -- and equal to the roster's, every row with a
#' roster row. The two key names are never swapped.
#'
#' @param elp The base table: one row per student-year-component. Or, with
#'   `roster = NULL`, a merged single long file to be split.
#' @param roster The roster: one row per student-year, and the only object
#'   students may be counted from. `NULL` to split a merged `elp`.
#' @param academic Optional academic table, one row per student-year-subject.
#' @param spec Optional `el_spec`. When supplied: delivered column names are
#'   renamed through `spec$data$columns`, `GRADE` values are recoded through
#'   its value map, `COMPONENT` values are checked against the component
#'   model, the headline coverage count is filled in, and the spec is carried
#'   in the bundle so [elp_headline()] can resolve without it being passed
#'   again.
#'
#' @return An `el_data` object: `$elp` (base), `$roster`, `$academic`,
#'   `$coverage`, `$spec` and `$meta`.
#' @export
#' @examples
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                      academic = academic_long_sim,
#'                      spec = read_el_spec("00_SIM_EL_Spec"))
#' bundle
#' head(elp_headline(bundle))
as_el_data <- function(elp, roster = NULL, academic = NULL, spec = NULL) {
  stopifnot(is.data.frame(elp),
            is.null(roster) || is.data.frame(roster),
            is.null(academic) || is.data.frame(academic),
            is.null(spec) || inherits(spec, "el_spec"))

  version <- .contract_version(elp, roster, academic)
  cols <- if (is.null(spec)) NULL else spec$data$columns

  elp <- tibble::as_tibble(elp)
  if (is.null(roster)) {
    elp <- .apply_crosswalk(.apply_crosswalk(elp, cols$roster), cols$base)
    parts <- .split_merged(elp)
    elp <- parts$base
    roster <- parts$roster
  } else {
    elp <- .apply_crosswalk(elp, cols$base)
    roster <- .apply_crosswalk(tibble::as_tibble(roster), cols$roster)
  }

  roster <- .validate_roster(roster, spec)
  elp <- .validate_base(elp, roster, spec)
  if (!is.null(academic)) {
    academic <- .apply_crosswalk(tibble::as_tibble(academic), cols$academic)
    academic <- .validate_academic(academic, roster, spec)
  }

  out <- list(
    elp      = elp,
    roster   = roster,
    academic = academic,
    coverage = el_coverage_table(roster, elp = elp, academic = academic,
                                 spec = spec),
    spec     = spec,
    meta = list(
      sim_contract_version = version,
      spec_id    = if (is.null(spec)) NA_character_
                   else spec$program$short_name %||% NA_character_,
      n_students = length(unique(roster$STUDENT_ID)),
      years      = range(roster$YEAR, na.rm = TRUE),
      built      = Sys.time()
    )
  )
  class(out) <- "el_data"
  out
}

# ---- intake helpers ----------------------------------------------------------

.contract_version <- function(...) {
  for (x in list(...)) {
    v <- attr(x, "sim_contract_version")
    if (!is.null(v)) return(v)
  }
  NA_character_
}

# canonical = delivered. Renames only where the delivered name is present and
# the canonical one is not, so an identity crosswalk is a no-op.
.apply_crosswalk <- function(x, map) {
  if (is.null(map)) return(x)
  for (canon in names(map)) {
    delivered <- map[[canon]]
    if (!identical(canon, delivered) && delivered %in% names(x) &&
        !canon %in% names(x)) {
      names(x)[names(x) == delivered] <- canon
    }
  }
  x
}

.as_year <- function(y, what) {
  if (is.character(y) || is.factor(y)) {
    y <- as.character(y)
    # SGP "YYYY_YYYY" -> the spring (end) year.
    span <- grepl("^[0-9]{4}_[0-9]{4}$", y)
    y[span] <- substr(y[span], 6L, 9L)
    y <- suppressWarnings(as.numeric(y))
  }
  if (!is.numeric(y) || any(!is.na(y) & y != round(y))) {
    stop("`YEAR` on the ", what, " must be a whole spring year ",
         "(or SGP \"YYYY_YYYY\").", call. = FALSE)
  }
  as.integer(y)
}

.recode_grade <- function(g, spec) {
  g <- as.character(g)
  map <- if (is.null(spec)) NULL else spec$data$columns$value_maps$GRADE
  if (!is.null(map)) {
    hit <- !is.na(g) & g %in% names(map)
    g[hit] <- unname(map[g[hit]])
  }
  g
}

.require <- function(x, need, what, hint = NULL) {
  miss <- setdiff(need, names(x))
  if (length(miss)) {
    stop(what, " is missing required column(s): ",
         paste(miss, collapse = ", "), ".", hint, call. = FALSE)
  }
}

.split_merged <- function(merged) {
  .require(merged, c(KEY, "COMPONENT"), "A merged single long file",
           paste0("\nPass `roster =` for the separate-objects form; a merged ",
                  "file needs COMPONENT on every measurement row."))
  meas <- intersect(c("COMPONENT", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"),
                    names(merged))
  # ELP_FORM is functionally dependent on the student-year, so it belongs to
  # both tables (sim_contract.md 4.1).
  roster <- dplyr::distinct(merged[setdiff(names(merged), meas)])
  dup <- duplicated(roster[KEY])
  if (any(dup)) {
    stop(sum(dup), " student-year(s) in the merged file carry roster ",
         "attributes that vary across their component rows. A roster ",
         "attribute is one value per student-year.", call. = FALSE)
  }
  # A base row is a measurement: a component with a scale score, a level, or
  # both (sim_contract.md 4.2). A row with neither -- COMPONENT NA, or both
  # measures NA -- is how a merged file carries an untested or former
  # student-year, and it lands on the roster only.
  measured <- !is.na(merged$COMPONENT) &
    (!is.na(merged$SCALE_SCORE %||% NA) | !is.na(merged$ACHIEVEMENT_LEVEL %||% NA))
  base <- merged[measured, intersect(BASE_COLUMNS, names(merged)), drop = FALSE]
  list(roster = roster, base = base)
}

.validate_roster <- function(roster, spec) {
  .require(roster, ROSTER_REQUIRED, "The roster",
           paste0("\nThese four plus base-table rows for the headline ",
                  "component run Checks 1 and 2 in full."))
  roster$STUDENT_ID <- as.character(roster$STUDENT_ID)
  roster$YEAR <- .as_year(roster$YEAR, "roster")
  roster$GRADE <- .recode_grade(roster$GRADE, spec)
  if (anyDuplicated(roster[KEY]) != 0L) {
    stop("The roster is not unique on STUDENT_ID x YEAR. Every count in ",
         "Prep 2 and both Check 1 denominators assume that grain.",
         call. = FALSE)
  }
  bad <- setdiff(unique(stats::na.omit(roster$EL_STATUS)), EL_STATUS_VALUES)
  if (length(bad)) {
    stop("`EL_STATUS` must be one of ", paste(EL_STATUS_VALUES, collapse = "/"),
         "; found: ", paste(bad, collapse = ", "),
         ".\nMonitoring years are sub-states of `former`, not a fourth value.",
         call. = FALSE)
  }
  roster[c(ROSTER_REQUIRED, setdiff(names(roster), ROSTER_REQUIRED))]
}

.validate_base <- function(base, roster, spec) {
  # The swapped-key check comes first on purpose: a table keyed on SUBJECT is
  # also missing COMPONENT, and "missing: COMPONENT" would send someone off to
  # add a column rather than to rename the one they have.
  if ("SUBJECT" %in% names(base)) {
    stop("The base table must key on `COMPONENT`, not `SUBJECT`. A reading ",
         "domain and an academic Reading subject conflate in a join if both ",
         "key columns share a name.", call. = FALSE)
  }
  .require(base, c(KEY, "COMPONENT"), "The base table")

  extra <- setdiff(names(base), BASE_COLUMNS)
  if (length(extra)) {
    stop("The base table carries measurement only; found non-base column(s): ",
         paste(extra, collapse = ", "), ".\nRoster attributes belong on the ",
         "roster -- or deliver one merged file with `roster = NULL` and it is ",
         "split on intake.", call. = FALSE)
  }
  if (!"SCALE_SCORE" %in% names(base) && !"ACHIEVEMENT_LEVEL" %in% names(base)) {
    stop("The base table needs SCALE_SCORE, ACHIEVEMENT_LEVEL, or both.",
         call. = FALSE)
  }
  # The table's shape never changes: a component may lack a scale or a level,
  # but the column is always there.
  if (!"SCALE_SCORE" %in% names(base)) base$SCALE_SCORE <- NA_real_
  if (!"ACHIEVEMENT_LEVEL" %in% names(base)) base$ACHIEVEMENT_LEVEL <- NA_real_

  if (!"ELP_FORM" %in% names(base)) {
    if (!"ELP_FORM" %in% names(roster)) {
      stop("`ELP_FORM` is required on the base table or the roster: it gates ",
           "the headline. For an instrument with no alternate form, set it to ",
           "\"annual\".", call. = FALSE)
    }
    # Redundant with the roster's on purpose; carried across, never invented.
    base <- dplyr::left_join(base, roster[c(KEY, "ELP_FORM")], by = KEY)
  }

  base$STUDENT_ID <- as.character(base$STUDENT_ID)
  base$YEAR <- .as_year(base$YEAR, "base table")
  if (!is.character(base$COMPONENT)) {
    stop("`COMPONENT` must be character, never a factor: the spec supplies ",
         "display order, and a factor coerces on bind_rows() with a real ",
         "delivery.", call. = FALSE)
  }
  if (!is.numeric(base$ACHIEVEMENT_LEVEL)) {
    stop("`ACHIEVEMENT_LEVEL` must be numeric -- the instrument's value, or an ",
         "ordinal index for a categorical scale, with labels in the spec. ",
         "Labeled deliveries are parsed by as_el_data_sgp().", call. = FALSE)
  }
  if (anyDuplicated(base[c(KEY, "COMPONENT")]) != 0L) {
    stop("The base table is not unique on STUDENT_ID x YEAR x COMPONENT.",
         call. = FALSE)
  }
  if (!is.null(spec)) {
    unknown <- setdiff(unique(base$COMPONENT), spec_components(spec))
    if (length(unknown)) {
      stop("COMPONENT value(s) not in the spec's component model: ",
           paste(unknown, collapse = ", "),
           ".\nMap an instrument's own name with `native_name` in the spec.",
           call. = FALSE)
    }
  }

  chk <- dplyr::left_join(
    base[c(KEY, "ELP_FORM")],
    dplyr::mutate(roster[KEY], .ON_ROSTER = TRUE,
                  .ROSTER_FORM = if ("ELP_FORM" %in% names(roster))
                    roster$ELP_FORM else NA_character_),
    by = KEY)
  orphan <- is.na(chk$.ON_ROSTER)
  if (any(orphan)) {
    stop(sum(orphan), " base-table row(s) have no roster row. The roster is ",
         "the counting frame; a measurement without a student-year cannot be ",
         "counted or given a status.", call. = FALSE)
  }
  disagree <- !is.na(chk$.ROSTER_FORM) & chk$ELP_FORM != chk$.ROSTER_FORM
  if (any(disagree)) {
    stop(sum(disagree), " base-table row(s) carry an `ELP_FORM` that ",
         "disagrees with the roster's. A domain-level analysis would average ",
         "alternate ordinals with annual decimals.", call. = FALSE)
  }
  base[BASE_COLUMNS]
}

.validate_academic <- function(academic, roster, spec) {
  if ("COMPONENT" %in% names(academic)) {
    stop("The academic table must key on `SUBJECT`, not `COMPONENT`.",
         call. = FALSE)
  }
  .require(academic, ACADEMIC_REQUIRED, "The academic table",
           paste0("\n`GRADE` is required on the academic row, with no ",
                  "fill-from-roster fallback: an EOC delivery reports a real ",
                  "tested grade there, and a validator cannot check agreement ",
                  "on a column that is not present."))
  academic$STUDENT_ID <- as.character(academic$STUDENT_ID)
  academic$YEAR <- .as_year(academic$YEAR, "academic table")
  academic$GRADE <- .recode_grade(academic$GRADE, spec)
  academic$SUBJECT <- as.character(academic$SUBJECT)

  if (anyDuplicated(academic[c(KEY, "SUBJECT")]) != 0L) {
    stop("The academic table is not unique on STUDENT_ID x YEAR x SUBJECT.",
         call. = FALSE)
  }
  chk <- dplyr::left_join(
    academic[c(KEY, "GRADE")],
    dplyr::rename(roster[c(KEY, "GRADE")], .ROSTER_GRADE = "GRADE"),
    by = KEY)
  orphan <- is.na(chk$.ROSTER_GRADE)
  if (any(orphan)) {
    stop(sum(orphan), " academic row(s) have no roster row. The roster is the ",
         "counting frame; an academic row without one cannot be counted or ",
         "given a status.", call. = FALSE)
  }
  disagree <- chk$GRADE != chk$.ROSTER_GRADE
  if (any(disagree)) {
    stop(sum(disagree), " academic row(s) disagree with the roster on `GRADE`.",
         call. = FALSE)
  }
  academic[c(intersect(ACADEMIC_COLUMNS, names(academic)),
             setdiff(names(academic), ACADEMIC_COLUMNS))]
}

# ---- the headline ------------------------------------------------------------

#' Resolve the ELP headline
#'
#' **The only route any stage takes to the headline.** The base table filtered
#' to `spec$elp$headline` on the annual form, joined to the roster keys, and
#' banded per `spec$elp$bands` -- one filter, one join, nothing pivots,
#' nothing cached (sim_contract.md 4.3).
#'
#' Alternate-form student-years never resolve here: the alternate form
#' reports on a different scale, and banding it on the annual scheme silently
#' mixes two measurement systems. Former and never-EL student-years have no
#' base-table rows and so never resolve either.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`. Defaults to the spec the bundle was built with.
#' @return A tibble: `STUDENT_ID`, `YEAR`, `ACHIEVEMENT_LEVEL`, `SCALE_SCORE`,
#'   `ELP_BAND`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' table(elp_headline(b)$ELP_BAND)
elp_headline <- function(data, spec = NULL) {
  stopifnot(inherits(data, "el_data"))
  if (is.null(spec)) spec <- data$spec
  if (is.null(spec)) {
    stop("elp_headline() needs a spec to know which component is the ",
         "headline: pass `spec`, or build the bundle with ",
         "as_el_data(..., spec = ).", call. = FALSE)
  }
  stopifnot(inherits(spec, "el_spec"))
  .resolve_headline(data$elp, data$roster, spec)
}

.resolve_headline <- function(base, roster, spec) {
  h <- base[base$COMPONENT == spec_headline(spec) & base$ELP_FORM %in% "annual",
            c(KEY, "ACHIEVEMENT_LEVEL", "SCALE_SCORE"), drop = FALSE]
  h <- dplyr::semi_join(h, roster[KEY], by = KEY)
  h$ELP_BAND <- assign_elp_band(h$ACHIEVEMENT_LEVEL, spec)
  h
}

# ---- coverage ----------------------------------------------------------------

#' Coverage of the academic-by-ELP pairing
#'
#' Check 3's data-considerations block. Per year and grade, counted from the
#' roster -- the only counting frame -- how many current ELs there are, how
#' many have a same-year headline, how many took the alternate form, how large
#' the former and never-EL reference groups are, and how many student-years
#' are academically scored. No filter anywhere decides the pairing silently;
#' this table is where the overlap is visible.
#'
#' Never-EL counts appear only weighted (`N_NEVER_WEIGHTED`, by
#' `SAMPLE_WEIGHT`, which is taken as 1 when the roster carries none) or
#' labeled as sample rows (`N_NEVER_SAMPLE`): an unweighted never-EL total is
#' never reported.
#'
#' @param roster A roster data frame.
#' @param elp Optional base table; with `spec`, fills `N_CURRENT_ELP`.
#' @param academic Optional academic table; fills `N_ACADEMIC_SCORED`.
#' @param spec Optional `el_spec`, needed to resolve the headline.
#' @return A tibble by `YEAR` and `GRADE`: `N_CURRENT`, `N_CURRENT_ELP`,
#'   `N_ALTERNATE`, `N_FORMER`, `N_NEVER_SAMPLE`, `N_NEVER_WEIGHTED`,
#'   `N_ACADEMIC_SCORED`. Unavailable counts are `NA`, never `0`.
#' @export
el_coverage_table <- function(roster, elp = NULL, academic = NULL,
                              spec = NULL) {
  stopifnot(is.data.frame(roster), all(ROSTER_REQUIRED %in% names(roster)))

  r <- roster[c(KEY, "GRADE", "EL_STATUS")]
  r$.W <- if ("SAMPLE_WEIGHT" %in% names(roster)) roster$SAMPLE_WEIGHT else 1
  r$.ALT <- if ("ELP_FORM" %in% names(roster)) {
    roster$ELP_FORM %in% "alternate"
  } else NA
  r$.ELP <- NA
  if (!is.null(elp) && !is.null(spec)) {
    h <- .resolve_headline(elp, roster, spec)
    r$.ELP <- paste(r$STUDENT_ID, r$YEAR) %in% paste(h$STUDENT_ID, h$YEAR)
  }
  r$.ACAD <- NA
  if (!is.null(academic)) {
    scored <- academic[!is.na(academic$SCALE_SCORE %||% NA), KEY, drop = FALSE]
    r$.ACAD <- paste(r$STUDENT_ID, r$YEAR) %in%
      paste(scored$STUDENT_ID, scored$YEAR)
  }

  r |>
    dplyr::mutate(.CUR = .data$EL_STATUS %in% "current",
                  .NEV = .data$EL_STATUS %in% "never") |>
    dplyr::group_by(.data$YEAR, .data$GRADE) |>
    dplyr::summarise(
      N_CURRENT         = sum(.data$.CUR),
      N_CURRENT_ELP     = sum(.data$.CUR & .data$.ELP),
      N_ALTERNATE       = sum(.data$.CUR & .data$.ALT),
      N_FORMER          = sum(.data$EL_STATUS %in% "former"),
      N_NEVER_SAMPLE    = sum(.data$.NEV),
      N_NEVER_WEIGHTED  = sum(.data$.W[.data$.NEV]),
      N_ACADEMIC_SCORED = sum(.data$.ACAD),
      .groups = "drop"
    )
}

# ---- accessors ---------------------------------------------------------------

#' Bundle accessors
#'
#' `el_base()` is the ELP base table, `el_roster()` the counting frame. Stage
#' code reads the headline through [elp_headline()], never by filtering
#' `el_base()` itself.
#'
#' @param x An `el_data` bundle.
#' @param ... Unused.
#' @return The requested table, or `NULL` when the bundle does not carry it.
#' @name el_data_accessors
NULL

#' @rdname el_data_accessors
#' @export
el_base <- function(x) { stopifnot(inherits(x, "el_data")); x$elp }

#' @rdname el_data_accessors
#' @export
el_roster <- function(x) { stopifnot(inherits(x, "el_data")); x$roster }

#' @rdname el_data_accessors
#' @export
el_academic <- function(x) { stopifnot(inherits(x, "el_data")); x$academic }

#' @rdname el_data_accessors
#' @export
el_coverage <- function(x) { stopifnot(inherits(x, "el_data")); x$coverage }

#' @rdname el_data_accessors
#' @export
print.el_data <- function(x, ...) {
  cat("<el_data>\n")
  cat("  base     :", format(nrow(x$elp), big.mark = ","), "rows,",
      length(unique(x$elp$COMPONENT)), "components\n")
  cat("  roster   :", format(nrow(x$roster), big.mark = ","), "rows,",
      format(x$meta$n_students, big.mark = ","), "students,",
      paste(x$meta$years, collapse = "-"), "\n")
  cat("  academic :",
      if (is.null(x$academic)) "- (absent: Check 1 Model 2, Check 3 and Check 4 degrade to NULL)"
      else paste(format(nrow(x$academic), big.mark = ","), "rows"), "\n")
  cat("  spec     :",
      if (is.null(x$spec)) "- (none: pass one to elp_headline())"
      else paste0(x$meta$spec_id, " (headline: ", spec_headline(x$spec), ")"),
      "\n")
  if (!is.na(x$meta$sim_contract_version)) {
    cat("  sim contract:", x$meta$sim_contract_version, "\n")
  }
  invisible(x)
}

# ---- delivery form: SGP LONG_DATA -------------------------------------------

#' Adapt an SGP LONG_DATA delivery
#'
#' Recodes a LONG_DATA file into the merged single-long-file form and hands it
#' to [as_el_data()], which splits it. `CONTENT_AREA` **values** map to
#' components through `spec$data$columns$value_maps$CONTENT_AREA$base`, never
#' by label: `READING` is the overall composite in one state's WIDA file and
#' the reading test in another's, so a label-based guess silently produces a
#' different analysis.
#'
#' `ID` becomes `STUDENT_ID`; `YEAR` `"YYYY_YYYY"` becomes the spring year;
#' `GRADE` recodes through the spec's value map (`"0"` to `"K"`);
#' `ACHIEVEMENT_LEVEL` parses against the spec's level scale, labels included;
#' `VALID_CASE` is honored. When every delivered level is a whole number but
#' the spec bands on a fractional break, a warning says the decimal from the
#' raw score file is needed. `SGP` and growth-target columns are out of scope.
#'
#' @param long_data A LONG_DATA-shaped data frame: `VALID_CASE`,
#'   `CONTENT_AREA`, `YEAR`, `ID`, `GRADE`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`,
#'   plus per-row roster attributes, including `EL_STATUS` and `ELP_FORM`.
#' @param spec An `el_spec` supplying the value maps and the level scale.
#' @param academic Optional academic table, in any form [as_academic_long()]
#'   accepts.
#' @return An `el_data` bundle identical to what the separate-objects form
#'   produces from the same records.
#' @export
as_el_data_sgp <- function(long_data, spec, academic = NULL) {
  stopifnot(is.data.frame(long_data), inherits(spec, "el_spec"))
  d <- tibble::as_tibble(long_data)
  version <- attr(long_data, "sim_contract_version")

  if ("VALID_CASE" %in% names(d)) {
    d <- d[d$VALID_CASE %in% "VALID_CASE", , drop = FALSE]
    d$VALID_CASE <- NULL
  }
  d <- .rename_alias(d, "ID", "STUDENT_ID")
  .require(d, c("CONTENT_AREA", KEY, "GRADE"), "LONG_DATA")

  ca_map <- spec$data$columns$value_maps$CONTENT_AREA$base
  if (is.null(ca_map)) {
    stop("`spec$data$columns$value_maps$CONTENT_AREA$base` is required to ",
         "read a LONG_DATA delivery: CONTENT_AREA values map to components ",
         "through the spec, never by label.", call. = FALSE)
  }
  unknown <- setdiff(unique(as.character(d$CONTENT_AREA)), names(ca_map))
  if (length(unknown)) {
    stop("CONTENT_AREA value(s) absent from the spec's value map: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  d$COMPONENT <- unname(ca_map[as.character(d$CONTENT_AREA)])
  d$CONTENT_AREA <- NULL
  if ("ACHIEVEMENT_LEVEL" %in% names(d)) {
    d$ACHIEVEMENT_LEVEL <- .parse_achievement_level(d$ACHIEVEMENT_LEVEL, spec)
  }
  if ("SCALE_SCORE" %in% names(d)) d$SCALE_SCORE <- as.numeric(d$SCALE_SCORE)

  if (!is.null(academic)) academic <- as_academic_long(academic, spec)
  attr(d, "sim_contract_version") <- version
  as_el_data(d, roster = NULL, academic = academic, spec = spec)
}

.rename_alias <- function(d, alias, canon) {
  if (alias %in% names(d)) {
    if (canon %in% names(d)) {
      stop("Both `", alias, "` and `", canon, "` are present; `", alias,
           "` is an alias of `", canon, "`, so the delivery is ambiguous.",
           call. = FALSE)
    }
    names(d)[names(d) == alias] <- canon
  }
  d
}

# ACHIEVEMENT_LEVEL may arrive as the vendor's labels or as numbers; it is
# stored numeric always (an ordinal index for a categorical scale). Vendor
# levels only: nothing here derives a level from a scale score.
.parse_achievement_level <- function(x, spec) {
  ls <- spec$elp$level_scale
  num <- if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x)))
  labeled <- !is.na(x) & is.na(num)
  if (any(labeled)) {
    lab <- if (identical(ls$type, "categorical")) ls$levels else ls$labels
    if (is.null(lab)) {
      stop("ACHIEVEMENT_LEVEL carries labels, and the spec's level scale ",
           "declares none to match them against.", call. = FALSE)
    }
    idx <- match(as.character(x[labeled]), lab)
    if (anyNA(idx)) {
      stop("ACHIEVEMENT_LEVEL label(s) not in the spec's level scale: ",
           paste(unique(as.character(x[labeled])[is.na(idx)]), collapse = ", "),
           ".", call. = FALSE)
    }
    num[labeled] <- idx
  }
  # The 4.0-4.4 / 4.5-4.9 split needs the decimal from the raw score file. If
  # the spec bands on a fractional break but every delivered level is whole,
  # the delivery has been rounded and those bands can never be populated.
  bands <- spec_bands(spec)
  if (!is.null(bands) && any(bands$breaks %% 1 != 0) &&
      length(stats::na.omit(num)) && all(stats::na.omit(num) %% 1 == 0)) {
    warning("Every delivered ACHIEVEMENT_LEVEL is a whole number, but the ",
            "spec bands on a fractional break. Band(s) split at a decimal ",
            "cannot be populated from this delivery -- the raw file's ",
            "decimal level is needed.", call. = FALSE)
  }
  as.numeric(num)
}

# ---- delivery form: academic ------------------------------------------------

#' Adapt an academic results table
#'
#' The `standardize_student_results()` equivalent. For ReportR-shaped input
#' -- `student_results_long` plus `ACHIEVEMENT_LEVEL_ALT` -- it is a
#' near-identity: types are coerced and nothing is renamed. For SGP-long input
#' it applies the alias map: `ID` to `STUDENT_ID`, `CONTENT_AREA` to
#' `SUBJECT`, `"YYYY_YYYY"` to the spring year, `VALID_CASE` filtered. With a
#' spec, `CONTENT_AREA` values recode to subject keys through
#' `spec$data$columns$value_maps$CONTENT_AREA$academic`, and labeled
#' `ACHIEVEMENT_LEVEL`s map to integers through each subject's
#' `level_labels`.
#'
#' `stateELP` does not import `stateSummative` or ReportR; a handoff between
#' them is a rename at the boundary -- data crosses, code does not.
#'
#' @param x A data frame of academic results.
#' @param spec Optional `el_spec`.
#' @return A tibble with canonical ALL-CAPS names, canonical columns first.
#' @export
as_academic_long <- function(x, spec = NULL) {
  stopifnot(is.data.frame(x), is.null(spec) || inherits(spec, "el_spec"))
  d <- tibble::as_tibble(x)

  if ("VALID_CASE" %in% names(d)) {
    d <- d[d$VALID_CASE %in% "VALID_CASE", , drop = FALSE]
    d$VALID_CASE <- NULL
  }
  d <- .rename_alias(d, "ID", "STUDENT_ID")
  d <- .rename_alias(d, "CONTENT_AREA", "SUBJECT")

  if ("STUDENT_ID" %in% names(d)) d$STUDENT_ID <- as.character(d$STUDENT_ID)
  if ("YEAR" %in% names(d)) d$YEAR <- .as_year(d$YEAR, "academic table")
  if ("GRADE" %in% names(d)) d$GRADE <- .recode_grade(d$GRADE, spec)
  if ("SUBJECT" %in% names(d)) {
    d$SUBJECT <- as.character(d$SUBJECT)
    map <- if (is.null(spec)) NULL
           else spec$data$columns$value_maps$CONTENT_AREA$academic
    if (!is.null(map)) {
      hit <- d$SUBJECT %in% names(map)
      d$SUBJECT[hit] <- unname(map[d$SUBJECT[hit]])
      unknown <- setdiff(unique(d$SUBJECT), spec_academic_subjects(spec))
      if (length(unknown) && length(spec_academic_subjects(spec))) {
        stop("Academic subject value(s) neither in the spec's value map nor ",
             "spec subject keys: ", paste(unknown, collapse = ", "), ".",
             call. = FALSE)
      }
    }
  }
  if ("SCALE_SCORE" %in% names(d)) d$SCALE_SCORE <- as.numeric(d$SCALE_SCORE)
  for (lv in intersect(c("ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_ALT"),
                       names(d))) {
    d[[lv]] <- .parse_academic_level(d[[lv]], d$SUBJECT, spec, lv)
  }
  d[c(intersect(ACADEMIC_COLUMNS, names(d)), setdiff(names(d), ACADEMIC_COLUMNS))]
}

.parse_academic_level <- function(x, subject, spec, what) {
  num <- if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x)))
  labeled <- !is.na(x) & is.na(num)
  if (any(labeled)) {
    if (is.null(spec) || is.null(subject)) {
      stop("`", what, "` carries labels; pass the spec so they can be mapped ",
           "through each subject's `level_labels`.", call. = FALSE)
    }
    for (i in which(labeled)) {
      lab <- spec$academic[[subject[i]]]$level_labels
      num[i] <- match(as.character(x[i]), lab)
    }
    if (anyNA(num[labeled])) {
      stop("`", what, "` label(s) not in the spec's `level_labels`: ",
           paste(unique(as.character(x[labeled][is.na(num[labeled])])),
                 collapse = ", "), ".", call. = FALSE)
    }
  }
  if (any(!is.na(num) & num != round(num))) {
    stop("`", what, "` must be a whole level.", call. = FALSE)
  }
  as.integer(num)
}
