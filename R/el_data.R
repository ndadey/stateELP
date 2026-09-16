# The delivery boundary.
#
# The three long tables are the analysis model; the delivered form is whatever
# a state hands over. as_el_data() is the adapter, and it is the only place
# that knows about delivered shapes. Every data-taking run_<stage>() takes the
# validated bundle, never raw tables, so no stage has to ask what form the
# data arrived in.

SPINE_REQUIRED <- c("student_id", "school_year", "grade", "el_status")
EL_STATUS_VALUES <- c("current", "former", "never")

#' Build a validated EL data bundle
#'
#' @param spine One row per student-year: the denominator for anything counted
#'   per student-year, and the only object students may be counted from.
#' @param components Optional, one row per student-year-component.
#' @param academic Optional, one row per student-year-subject. `grade` is
#'   required on it, with no fill-from-spine fallback, so that the
#'   spine-agreement check always has something to check.
#' @param spec Optional `el_spec`. When supplied, component names are checked
#'   against the spec's component model.
#'
#' @return An `el_data` object: `$spine`, `$components`, `$academic`,
#'   `$coverage` and `$meta`.
#' @export
#' @examples
#' bundle <- as_el_data(el_long_sim,
#'                      components = elp_components_sim,
#'                      academic = academic_long_sim)
#' bundle
as_el_data <- function(spine, components = NULL, academic = NULL,
                       spec = NULL) {
  stopifnot(is.data.frame(spine))
  spine <- tibble::as_tibble(spine)

  miss <- setdiff(SPINE_REQUIRED, names(spine))
  if (length(miss)) {
    stop("Spine is missing required column(s): ", paste(miss, collapse = ", "),
         ".\nThese five run Checks 1 and 2 in full: ",
         paste(c(SPINE_REQUIRED, "elp_level"), collapse = ", "), ".",
         call. = FALSE)
  }
  if (anyDuplicated(spine[c("student_id", "school_year")]) != 0L) {
    stop("Spine is not unique on student_id x school_year. Every count in ",
         "Prep 2 and both Check 1 denominators assume that grain.",
         call. = FALSE)
  }
  bad <- setdiff(unique(stats::na.omit(spine$el_status)), EL_STATUS_VALUES)
  if (length(bad)) {
    stop("`el_status` must be one of ", paste(EL_STATUS_VALUES, collapse = "/"),
         "; found: ", paste(bad, collapse = ", "),
         ".\nMonitoring years are sub-states of `former`, not a fourth value.",
         call. = FALSE)
  }

  if (!is.null(components)) {
    components <- .validate_components(tibble::as_tibble(components), spine, spec)
  }
  if (!is.null(academic)) {
    academic <- .validate_academic(tibble::as_tibble(academic), spine)
  }

  out <- list(
    spine      = spine,
    components = components,
    academic   = academic,
    coverage   = el_coverage_table(spine, academic),
    meta = list(
      sim_contract_version = attr(spine, "sim_contract_version"),
      spec_id  = if (is.null(spec)) NA_character_ else spec$program$short_name,
      n_students = length(unique(spine$student_id)),
      years = range(spine$school_year, na.rm = TRUE),
      built = Sys.time()
    )
  )
  class(out) <- "el_data"
  out
}

.validate_components <- function(components, spine, spec) {
  # The swapped-key check comes first on purpose: a table keyed on `subject`
  # is also missing `component`, and "missing: component" would send someone
  # off to add a column rather than to rename the one they have.
  if ("subject" %in% names(components)) {
    stop("Components table must key on `component`, not `subject`. ",
         "A `reading` domain and an academic Reading subject conflate in a ",
         "join if both key columns share a name.", call. = FALSE)
  }
  need <- c("student_id", "school_year", "component")
  miss <- setdiff(need, names(components))
  if (length(miss)) {
    stop("Components table is missing: ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  }
  if (anyDuplicated(components[c("student_id", "school_year", "component")]) != 0L) {
    stop("Components table is not unique on student_id x school_year x component.",
         call. = FALSE)
  }
  if (!is.character(components$component)) {
    stop("`component` must be character, never a factor: the spec supplies ",
         "display order, and a factor coerces on bind_rows() with a real ",
         "delivery.", call. = FALSE)
  }
  if (!is.null(spec)) {
    unknown <- setdiff(unique(components$component), spec_components(spec))
    if (length(unknown)) {
      stop("Component(s) not in the spec's component model: ",
           paste(unknown, collapse = ", "),
           ".\nMap an instrument's own name with `native_name` in the spec.",
           call. = FALSE)
    }
  }
  # `form` is redundant with the spine's elp_form on purpose; assert it agrees,
  # or a domain-level analysis that forgot to join the spine averages alternate
  # ordinals with annual decimals.
  if ("form" %in% names(components) && "elp_form" %in% names(spine)) {
    chk <- dplyr::left_join(
      components[c("student_id", "school_year", "form")],
      spine[c("student_id", "school_year", "elp_form")],
      by = c("student_id", "school_year"))
    disagree <- !is.na(chk$elp_form) & chk$form != chk$elp_form
    if (any(disagree)) {
      stop(sum(disagree), " component row(s) carry a `form` that disagrees ",
           "with the spine's `elp_form`.", call. = FALSE)
    }
  }
  components
}

.validate_academic <- function(academic, spine) {
  if ("component" %in% names(academic)) {
    stop("Academic table must key on `subject`, not `component`.",
         call. = FALSE)
  }
  need <- c("student_id", "school_year", "grade", "subject")
  miss <- setdiff(need, names(academic))
  if (length(miss)) {
    stop("Academic table is missing: ", paste(miss, collapse = ", "),
         ".\n`grade` is required on the academic row, with no fill-from-spine ",
         "fallback: an EOC delivery reports a real tested grade there, and a ",
         "validator cannot check agreement on a column that is not present.",
         call. = FALSE)
  }
  if (anyDuplicated(academic[c("student_id", "school_year", "subject")]) != 0L) {
    stop("Academic table is not unique on student_id x school_year x subject.",
         call. = FALSE)
  }
  chk <- dplyr::left_join(
    academic[c("student_id", "school_year", "grade")],
    dplyr::rename(spine[c("student_id", "school_year", "grade")],
                  spine_grade = "grade"),
    by = c("student_id", "school_year"))
  orphan <- is.na(chk$spine_grade)
  if (any(orphan)) {
    stop(sum(orphan), " academic row(s) have no spine row. The spine is the ",
         "roster; an academic row without one cannot be counted or given a ",
         "status.", call. = FALSE)
  }
  disagree <- chk$grade != chk$spine_grade
  if (any(disagree)) {
    stop(sum(disagree), " academic row(s) disagree with the spine on `grade`.",
         call. = FALSE)
  }
  academic
}

#' Coverage of the academic-by-ELP pairing
#'
#' Check 3's data-considerations block. Reports, per year and grade, how many
#' students are academically scored, how many current ELs have a same-year ELP
#' headline, and how large the former and never-EL reference groups are. No
#' filter anywhere decides the pairing silently; this table is where the
#' overlap is visible.
#'
#' @param spine A spine data frame.
#' @param academic An academic data frame, or `NULL`.
#' @return A tibble by `school_year` and `grade`.
#' @export
el_coverage_table <- function(spine, academic = NULL) {
  has_headline <- "elp_level" %in% names(spine)
  has_form <- "elp_form" %in% names(spine)

  base <- spine |>
    dplyr::group_by(.data$school_year, .data$grade) |>
    dplyr::summarise(
      n_spine     = dplyr::n(),
      n_current   = sum(.data$el_status == "current", na.rm = TRUE),
      n_former    = sum(.data$el_status == "former", na.rm = TRUE),
      n_never     = sum(.data$el_status == "never", na.rm = TRUE),
      n_elp       = if (has_headline) {
        sum(.data$el_status == "current" & !is.na(.data$elp_level))
      } else NA_integer_,
      n_alternate = if (has_form) {
        sum(.data$elp_form %in% "alternate")
      } else NA_integer_,
      .groups = "drop"
    )

  if (is.null(academic)) {
    return(dplyr::mutate(base, n_academic = NA_integer_))
  }
  acad <- academic |>
    dplyr::distinct(.data$student_id, .data$school_year, .data$grade) |>
    dplyr::count(.data$school_year, .data$grade, name = "n_academic")

  base |>
    dplyr::left_join(acad, by = c("school_year", "grade")) |>
    dplyr::mutate(n_academic = dplyr::coalesce(.data$n_academic, 0L))
}

# ---- accessors ---------------------------------------------------------------

#' Bundle accessors
#'
#' @param x An `el_data` bundle.
#' @param ... Unused.
#' @return The requested table, or `NULL` when the bundle does not carry it.
#' @name el_data_accessors
NULL

#' @rdname el_data_accessors
#' @export
el_spine <- function(x) { stopifnot(inherits(x, "el_data")); x$spine }

#' @rdname el_data_accessors
#' @export
el_components <- function(x) { stopifnot(inherits(x, "el_data")); x$components }

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
  cat("  spine     :", format(nrow(x$spine), big.mark = ","), "rows,",
      format(x$meta$n_students, big.mark = ","), "students,",
      paste(x$meta$years, collapse = "-"), "\n")
  cat("  components:",
      if (is.null(x$components)) "- (absent)"
      else paste(format(nrow(x$components), big.mark = ","), "rows"), "\n")
  cat("  academic  :",
      if (is.null(x$academic)) "- (absent: Check 1 Model 2, Check 3 and Check 4 degrade to NULL)"
      else paste(format(nrow(x$academic), big.mark = ","), "rows"), "\n")
  if (!is.na(x$meta$sim_contract_version)) {
    cat("  sim contract:", x$meta$sim_contract_version, "\n")
  }
  invisible(x)
}

# ---- delivery form: SGP LONG_DATA -------------------------------------------

#' Adapt an SGP LONG_DATA delivery
#'
#' `CONTENT_AREA` **values** map to components through the spec's
#' `data$content_area_map`, never by label: `READING` is the overall composite
#' in one state's WIDA file and the reading test in another's, so a label-based
#' guess silently produces a different analysis.
#'
#' @param long_data A LONG_DATA-shaped data frame: `VALID_CASE`,
#'   `CONTENT_AREA`, `YEAR`, `ID`, `GRADE`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`,
#'   plus any per-row demographics, including `EL_STATUS`.
#' @param spec An `el_spec` supplying the content-area map, the grade recode
#'   and the level scale.
#' @param academic Optional academic table, already in canonical form or
#'   acceptable to [as_academic_long()].
#' @return An `el_data` bundle identical to what the separate-objects form
#'   would produce from the same records.
#' @export
as_el_data_sgp <- function(long_data, spec, academic = NULL) {
  stopifnot(is.data.frame(long_data), inherits(spec, "el_spec"))
  d <- tibble::as_tibble(long_data)

  if ("VALID_CASE" %in% names(d)) {
    d <- d[d$VALID_CASE == "VALID_CASE", , drop = FALSE]
  }
  need <- c("CONTENT_AREA", "YEAR", "ID", "GRADE")
  miss <- setdiff(need, names(d))
  if (length(miss)) {
    stop("LONG_DATA is missing: ", paste(miss, collapse = ", "), ".",
         call. = FALSE)
  }

  ca_map <- spec$data$content_area_map
  if (is.null(ca_map)) {
    stop("`spec$data$content_area_map` is required to read a LONG_DATA ",
         "delivery: CONTENT_AREA values map to components through the spec, ",
         "never by label.", call. = FALSE)
  }
  unknown <- setdiff(unique(d$CONTENT_AREA), names(ca_map))
  if (length(unknown)) {
    stop("CONTENT_AREA value(s) absent from the spec's content_area_map: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }

  grade_recode <- spec$data$grade_recode
  recode_grade <- function(g) {
    g <- as.character(g)
    if (!is.null(grade_recode)) {
      hit <- g %in% names(grade_recode)
      g[hit] <- unname(grade_recode[g[hit]])
    }
    g
  }

  level <- .parse_achievement_level(d[["ACHIEVEMENT_LEVEL"]], spec)

  components <- tibble::tibble(
    student_id  = as.character(d$ID),
    school_year = as.integer(d$YEAR),
    component   = unname(ca_map[as.character(d$CONTENT_AREA)]),
    scale_score = if ("SCALE_SCORE" %in% names(d)) as.numeric(d$SCALE_SCORE)
                  else NA_real_,
    level       = level
  )
  if ("ELP_FORM" %in% names(d)) components$form <- as.character(d$ELP_FORM)

  headline <- spec_headline(spec)
  head_rows <- components[components$component == headline, ]

  carry <- setdiff(names(d), c("CONTENT_AREA", "SCALE_SCORE",
                               "ACHIEVEMENT_LEVEL", "VALID_CASE"))
  spine <- d[carry] |>
    dplyr::distinct() |>
    dplyr::rename(student_id = "ID", school_year = "YEAR", grade = "GRADE")
  names(spine) <- tolower(names(spine))
  spine$student_id <- as.character(spine$student_id)
  spine$school_year <- as.integer(spine$school_year)
  spine$grade <- recode_grade(spine$grade)

  spine <- spine |>
    dplyr::left_join(
      dplyr::rename(head_rows[c("student_id", "school_year",
                                "scale_score", "level")],
                    elp_scale = "scale_score", elp_level = "level"),
      by = c("student_id", "school_year")) |>
    dplyr::mutate(elp_band = assign_elp_band(.data$elp_level, spec))

  if (!is.null(academic)) academic <- as_academic_long(academic)
  as_el_data(spine, components = components, academic = academic, spec = spec)
}

# ACHIEVEMENT_LEVEL may arrive as the vendor's labels or as numbers. Vendor
# levels only: nothing here derives a level from a scale score.
.parse_achievement_level <- function(x, spec) {
  if (is.null(x)) return(NA_real_)
  ls <- spec$elp$level_scale

  if (identical(ls$type, "categorical")) return(as.character(x))

  num <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.na(num)) && !all(is.na(x))) {
    lab <- ls$labels
    if (is.null(lab)) {
      stop("ACHIEVEMENT_LEVEL is not numeric and the spec's level scale ",
           "declares no labels to match it against.", call. = FALSE)
    }
    num <- match(as.character(x), lab)
  }
  # The 4.0-4.4 / 4.5-4.9 split needs the decimal from the raw score file. If
  # the spec bands on a fractional break but every delivered level is whole,
  # the delivery has been rounded and those bands can never be populated.
  bands <- spec_bands(spec)
  if (!is.null(bands) && any(bands$breaks %% 1 != 0) &&
      all(stats::na.omit(num) %% 1 == 0)) {
    warning("Every delivered ACHIEVEMENT_LEVEL is a whole number, but the ",
            "spec bands on a fractional break. Band(s) split at a decimal ",
            "cannot be populated from this delivery -- the raw file's ",
            "decimal level is needed.", call. = FALSE)
  }
  num
}

# ---- delivery form: academic ------------------------------------------------

#' Adapt an academic results table
#'
#' Accepts a stateSummative-shaped or SGP-long academic table and renames it to
#' this package's lowercase canonical names. `stateELP` does not import
#' `stateSummative`; a handoff between the two is a rename at the boundary --
#' data crosses, code does not.
#'
#' @param x A data frame of academic results.
#' @return A tibble with canonical names.
#' @export
as_academic_long <- function(x) {
  stopifnot(is.data.frame(x))
  d <- tibble::as_tibble(x)

  canon <- c(
    ID = "student_id", STUDENT_ID = "student_id",
    YEAR = "school_year", SCHOOL_YEAR = "school_year",
    GRADE = "grade",
    CONTENT_AREA = "subject", SUBJECT = "subject",
    SCALE_SCORE = "scale_score",
    ACHIEVEMENT_LEVEL = "perf_level", PERF_LEVEL = "perf_level",
    ALT_PERF_LEVEL = "alt_perf_level"
  )
  if ("VALID_CASE" %in% names(d)) {
    d <- d[d$VALID_CASE == "VALID_CASE", , drop = FALSE]
    d$VALID_CASE <- NULL
  }
  hit <- names(d) %in% names(canon)
  names(d)[hit] <- unname(canon[names(d)[hit]])

  if ("student_id" %in% names(d)) d$student_id <- as.character(d$student_id)
  if ("school_year" %in% names(d)) d$school_year <- as.integer(d$school_year)
  if ("grade" %in% names(d)) d$grade <- as.character(d$grade)
  if ("subject" %in% names(d)) d$subject <- as.character(d$subject)
  d
}
