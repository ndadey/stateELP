# The delivery format, written down once, and the check that holds a delivery
# against it.
#
# el_data_format() is the single source for what each of the three tables must
# carry. The walkthrough guide prints it and check_el_delivery() reads it, so
# the documentation and the check cannot drift apart -- the same pattern as
# Check 1's "Data fields used" box.
#
# check_el_delivery() runs every check as_el_data() makes, and the ones it
# does not (a value the stages silently read as "no", a grade code that hides
# the kindergarten cohort, a subject the spec cannot see), and reports them
# all at once instead of stopping at the first. It changes nothing.

#' The data format the stages expect
#'
#' One row per column a stage reads, for each of the three tables: whether it
#' is required, its type, its allowed values, an example, and which stages
#' read it. This is the reference to clean a delivery against;
#' [check_el_delivery()] checks a delivery against it.
#'
#' Column names are the package's canonical ALL-CAPS names. A delivery that
#' uses other names maps them in the spec's `data$columns` crosswalk, and
#' codes that differ (a kindergarten coded `"0"`, a subject coded `READING`)
#' map in `data$columns$value_maps`.
#'
#' @param table Optionally one of `"roster"`, `"base"`, `"academic"`.
#' @return A tibble: `TABLE`, `COLUMN`, `NEED` (`required`, `recommended`,
#'   `optional`), `TYPE`, `VALUES`, `EXAMPLE`, `USED_BY`, `NOTE`.
#' @export
#' @examples
#' el_data_format("roster")
el_data_format <- function(table = NULL) {
  stopifnot(is.null(table) || all(table %in% c("roster", "base", "academic")))
  f <- tibble::tribble(
    ~TABLE, ~COLUMN, ~NEED, ~TYPE, ~VALUES, ~EXAMPLE, ~USED_BY, ~NOTE,

    # --- roster: one row per student per year --------------------------------
    "roster", "STUDENT_ID", "required", "character",
    "A stable id, the same in every table and year", "S000123",
    "All stages",
    "Students are followed across years by this id.",

    "roster", "YEAR", "required", "integer",
    "Spring year of the school year (2024-25 is 2025), or \"2024_2025\"",
    "2025", "All stages", "",

    "roster", "GRADE", "required", "character",
    "\"K\", \"1\" to \"12\"", "K",
    "All stages",
    "Kindergarten must be \"K\": map another code in value_maps$GRADE (c(\"0\" = \"K\")), or Check 1 finds no kindergarten cohort.",

    "roster", "EL_STATUS", "required", "character",
    "current, former, never (NA allowed)", "current",
    "All stages",
    "current = classified as an EL that year; former = exited (monitoring years included); never = never classified. The package dates an exit to the first former year. NA means no status is known for that year: the row stays on the roster but is in no group, and an NA year between two years in status ends the student's observed run (read as leaving the data).",

    "roster", "ELP_FORM", "required", "character",
    "annual, alternate (NA where no ELP test was taken)", "annual",
    "All stages that read the ELP score",
    "Required on the roster or the ELP score table. Only \"annual\" rows give the main ELP score; recode a vendor label such as \"general\" to \"annual\". Alternate-test rows: mark_alternate_elp().",

    "roster", "LEFT_TRUNCATED", "recommended", "logical",
    "TRUE, FALSE", "FALSE", "Checks 1, 2",
    "TRUE for students already in EL status when the data begin. Without it, Check 1 treats every student first seen as an EL in the first year as left-truncated.",

    "roster", "SAMPLE_WEIGHT", "optional", "numeric",
    "Greater than 0; 1 when absent", "10", "Prep 2, Check 3",
    "When never-EL rows are a sample, the number of students each row stands for.",

    "roster", "DISTRICT_ID", "recommended", "character",
    "Any district code", "D042", "Prep 2, Check 2", "",

    "roster", "HOME_LANGUAGE", "recommended", "character",
    "Any language code", "spa", "Prep 2, Check 2", "",

    "roster", "SERVICE_MODEL", "recommended", "character",
    "Any service code", "esl_pullout", "Prep 2, Check 2", "",

    "roster", "GENDER", "recommended", "character",
    "M, F (others kept as given)", "F", "Prep 2, Check 2",
    "Check 2's Male row counts M or MALE.",

    "roster", "FRL", "recommended", "character",
    "Y, N (YES/NO, 1/0, TRUE/FALSE also read)", "Y", "Prep 2, Checks 2, 3",
    "Economically disadvantaged. Any other value is read as \"no\".",

    "roster", "IEP", "recommended", "character",
    "Y, N (YES/NO, 1/0, TRUE/FALSE also read)", "N", "Prep 2, Check 2",
    "Individualized education program. Any other value is read as \"no\"; Y/N displays as Yes/No in Prep 2's profile.",

    "roster", "NEWCOMER_FLAG", "optional", "character",
    "Y, N", "N", "Prep 2", "",

    "roster", "EL_MONITOR_YEAR", "optional", "integer",
    "1, 2, 3, 4 on former rows", "2", "Prep 2", "Monitoring year after exit.",

    "roster", "LEP_PROGRAM_START_DATE", "optional", "date",
    "A date", "2016-09-01", "Prep 2",
    "Dates entry for students already EL when the data begin. COHORT_START_YEAR, YEAR_IN_STATUS and ENTRY_GRADE are read the same way.",

    # --- base: one row per student, year and ELP score -----------------------
    "base", "STUDENT_ID", "required", "character", "As on the roster", "S000123",
    "All stages that read the ELP score", "Every row must have a roster row.",

    "base", "YEAR", "required", "integer", "As on the roster", "2025",
    "All stages that read the ELP score", "",

    "base", "COMPONENT", "required", "character",
    "A component key in the spec (el_component_vocabulary())",
    "overall (WIDA's Overall Composite)",
    "All stages that read the ELP score",
    "One row per score: the component is a row key, not a column. Map a file's own codes in value_maps$CONTENT_AREA$base.",

    "base", "ELP_FORM", "required", "character",
    "annual, alternate", "annual", "All stages that read the ELP score",
    "Required here or on the roster; where both carry it they must agree.",

    "base", "SCALE_SCORE", "recommended", "numeric", "The vendor's scale score",
    "362", "Check 1 models, Check 3 logistic", "",

    "base", "ACHIEVEMENT_LEVEL", "required", "numeric",
    "The vendor's level, with its decimal (4.5, not 4)", "4.5",
    "All stages that read the ELP score",
    "Numeric only. Alternate ACCESS codes such as \"P2\" are a different scale: mark_alternate_elp() sets them to NA and the rows to \"alternate\". No other column is allowed on this table.",

    # --- academic: one row per student, year and subject ---------------------
    "academic", "STUDENT_ID", "required", "character", "As on the roster",
    "S000123", "Checks 1-4", "Every row must have a roster row.",

    "academic", "YEAR", "required", "integer", "As on the roster", "2025",
    "Checks 1-4", "",

    "academic", "GRADE", "required", "character", "\"3\" to \"12\"", "5",
    "Checks 1-4", "Must equal the roster's grade for the same year.",

    "academic", "SUBJECT", "required", "character",
    "A subject key in the spec's academic block", "ELA", "Checks 1-4",
    "Map a file's own codes (READING, MATHEMATICS) in value_maps$CONTENT_AREA$academic.",

    "academic", "SCALE_SCORE", "recommended", "numeric", "The scale score",
    "512", "Checks 1-4",
    "Read against the grade's cut scores in the spec.",

    "academic", "ACHIEVEMENT_LEVEL", "recommended", "integer or label",
    "1 = lowest; or a label in the subject's level_labels (BB, B, P, A)", "3",
    "Checks 2-4",
    "Labels are translated through the spec. Checks 3 and 4 read proficiency from it when the spec gives no cut for a grade.",

    "academic", "ACHIEVEMENT_LEVEL_ALT", "optional", "integer",
    "Non-missing for an alternate academic test", "NA", "Check 3",
    "Alternate-test takers are counted but their scores are not plotted."
  )
  if (is.null(table)) f else f[f$TABLE %in% table, , drop = FALSE]
}

# ---- the check ---------------------------------------------------------------

# Tokens read as yes / no in a flag column (Check 2's .check2_is_yes() reads
# the first set as yes and everything else as no).
.YES_TOKENS <- c("Y", "YES", "TRUE", "T", "1")
.NO_TOKENS  <- c("N", "NO", "FALSE", "F", "0")

#' Check a delivery against the format the stages expect
#'
#' Runs before [as_el_data()] and lists **every** problem at once: the
#' column, what is wrong, how many rows, examples of the values found, and
#' how to fix it. It applies the spec's crosswalk and value maps first, as
#' `as_el_data()` would, so it reports what the bundle would actually hold.
#' Nothing is changed. Clean the files, rerun the check, and build the bundle
#' once it reports no errors.
#'
#' Severity:
#' * `error` -- `as_el_data()` stops, or every stage reading the column gets
#'   no students.
#' * `warning` -- results are affected: rows dropped, read as "no", or placed
#'   in no group.
#' * `note` -- worth knowing; nothing breaks.
#'
#' The format itself is [el_data_format()].
#'
#' @param elp The ELP score table (the base table), or a merged single long
#'   file when `roster = NULL`.
#' @param roster The roster. `NULL` when `elp` is a merged file.
#' @param academic Optional academic table.
#' @param spec The state's `el_spec`. Without it the crosswalk, the value
#'   maps and the component and subject checks are skipped.
#' @return An `el_delivery_check` tibble (`SEVERITY`, `TABLE`, `COLUMN`,
#'   `PROBLEM`, `N_ROWS`, `EXAMPLES`, `FIX`), printed as a report. Its `"ok"`
#'   attribute is `TRUE` when there are no errors.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' check_el_delivery(el_long_sim, roster = el_roster_sim,
#'                   academic = academic_long_sim, spec = spec)
check_el_delivery <- function(elp = NULL, roster = NULL, academic = NULL,
                              spec = NULL) {
  stopifnot(is.null(elp) || is.data.frame(elp),
            is.null(roster) || is.data.frame(roster),
            is.null(academic) || is.data.frame(academic),
            is.null(spec) || inherits(spec, "el_spec"))
  p <- .check_collector()
  if (is.null(spec)) {
    p$add("note", "spec", "", paste0(
      "No spec given: the column crosswalk, the value maps, and the ",
      "component and subject checks are skipped."),
      fix = "Pass `spec =` to check the delivery as as_el_data() will read it.")
  }
  cols <- if (is.null(spec)) NULL else spec$data$columns

  # The merged single-file form: split the way as_el_data() would.
  if (!is.null(elp) && is.null(roster)) {
    merged <- .apply_crosswalk(.apply_crosswalk(tibble::as_tibble(elp),
                                                cols$roster), cols$base)
    parts <- tryCatch(.split_merged(merged), error = function(e) e)
    if (inherits(parts, "error")) {
      p$add("error", "merged", "", conditionMessage(parts),
            fix = "Pass the roster separately with `roster =`.")
      return(p$result())
    }
    elp <- parts$base
    roster <- parts$roster
  } else {
    if (!is.null(roster)) {
      roster <- .apply_crosswalk(tibble::as_tibble(roster), cols$roster)
    }
    if (!is.null(elp)) elp <- .apply_crosswalk(tibble::as_tibble(elp), cols$base)
  }

  if (!is.null(roster)) roster <- .check_roster_table(roster, spec, p)
  if (!is.null(elp)) .check_base_table(elp, roster, spec, p)
  if (!is.null(academic)) {
    academic <- .apply_crosswalk(tibble::as_tibble(academic), cols$academic)
    .check_academic_table(academic, roster, spec, p)
  }
  p$result()
}

# A small accumulator, so each check is one line.
.check_collector <- function() {
  rows <- list()
  add <- function(severity, table, column, problem, n = NA_integer_,
                  examples = character(), fix = "") {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      SEVERITY = severity, TABLE = table, COLUMN = column, PROBLEM = problem,
      N_ROWS = as.integer(n),
      EXAMPLES = if (length(examples)) {
        paste0("\"", utils::head(unique(as.character(examples)), 5), "\"",
               collapse = ", ")
      } else "",
      FIX = fix)
  }
  result <- function() {
    out <- if (length(rows)) dplyr::bind_rows(rows) else tibble::tibble(
      SEVERITY = character(), TABLE = character(), COLUMN = character(),
      PROBLEM = character(), N_ROWS = integer(), EXAMPLES = character(),
      FIX = character())
    ord <- order(match(out$SEVERITY, c("error", "warning", "note")),
                 match(out$TABLE, c("spec", "merged", "roster", "base",
                                    "academic")))
    out <- out[ord, , drop = FALSE]
    attr(out, "ok") <- !any(out$SEVERITY == "error")
    class(out) <- c("el_delivery_check", class(out))
    out
  }
  list(add = add, result = result)
}

.fmt_count <- function(n) format(n, big.mark = ",")

# Years as as_el_data() reads them, with NA where a value does not parse.
.parse_years_quiet <- function(y) {
  if (is.character(y) || is.factor(y)) {
    y <- as.character(y)
    span <- grepl("^[0-9]{4}_[0-9]{4}$", y)
    y[span] <- substr(y[span], 6L, 9L)
    y <- suppressWarnings(as.numeric(y))
  }
  if (!is.numeric(y)) return(rep(NA_real_, length(y)))
  y[!is.na(y) & y != round(y)] <- NA
  y
}

.check_roster_table <- function(roster, spec, p) {
  miss <- setdiff(ROSTER_REQUIRED, names(roster))
  for (m in miss) {
    p$add("error", "roster", m, "Required column is missing.",
          fix = if (m == "EL_STATUS") paste0(
            "Build it: current, former or never for every student-year. ",
            "See el_data_format(\"roster\").")
          else "Add it, or map your column name in data$columns$roster.")
  }

  if ("YEAR" %in% names(roster)) {
    yr <- .parse_years_quiet(roster$YEAR)
    bad <- !is.na(roster$YEAR) & is.na(yr)
    if (any(bad)) {
      p$add("error", "roster", "YEAR", "Not a whole spring year.",
            sum(bad), roster$YEAR[bad],
            fix = "Use the spring year (2025 for 2024-25) or \"2024_2025\".")
    }
    roster$YEAR <- yr
  }
  if (all(c("STUDENT_ID", "YEAR") %in% names(roster))) {
    roster$STUDENT_ID <- as.character(roster$STUDENT_ID)
    dup <- duplicated(roster[KEY]) | duplicated(roster[KEY], fromLast = TRUE)
    if (any(dup)) {
      p$add("error", "roster", "STUDENT_ID x YEAR",
            "More than one row for the same student and year.", sum(dup),
            paste(roster$STUDENT_ID[dup], roster$YEAR[dup]),
            fix = "Keep one row per student per year.")
    }
  }

  if ("GRADE" %in% names(roster)) {
    roster$GRADE <- .recode_grade(roster$GRADE, spec)
    .check_grade_values(roster$GRADE, "roster", p)
  }

  if ("EL_STATUS" %in% names(roster)) {
    st <- roster$EL_STATUS
    bad <- !is.na(st) & !st %in% EL_STATUS_VALUES
    if (any(bad)) {
      p$add("error", "roster", "EL_STATUS",
            "Values outside current / former / never.", sum(bad), st[bad],
            fix = paste0("Recode to current, former or never. Monitoring ",
                         "years are former."))
    }
    na <- is.na(st)
    if (any(na)) {
      p$add("note", "roster", "EL_STATUS", paste0(
        "Student-years with no status (NA). They stay on the roster, so ",
        "their academic rows join, but they are counted in no status group."),
        sum(na),
        fix = "Fine if no status applies (e.g. a year before the first ELP test).")
      gap <- .na_status_inside_run(roster)
      if (gap > 0L) {
        p$add("warning", "roster", "EL_STATUS", paste0(
          "Students with an NA year between two years in EL status. The ",
          "package reads the NA year as leaving the data, so their observed ",
          "run ends there and they count as disappeared."), gap,
          fix = paste0("Give those years their status (usually current), or ",
                       "accept that the run ends there."))
      }
    }
  }

  if ("ELP_FORM" %in% names(roster)) {
    .check_form_values(roster$ELP_FORM, "roster", p)
  }

  for (fl in intersect(c("IEP", "FRL", "NEWCOMER_FLAG", "SIFE_FLAG"),
                       names(roster))) {
    .check_flag_values(roster[[fl]], fl, p)
  }

  if ("LEFT_TRUNCATED" %in% names(roster)) {
    lt <- roster$LEFT_TRUNCATED
    if (!is.logical(lt)) {
      p$add("error", "roster", "LEFT_TRUNCATED", paste0(
        "Not logical. The cohort checks keep a student out only when the ",
        "flag is TRUE, so a ", class(lt)[[1]], " column marks nobody."),
        sum(!is.na(lt)), lt,
        fix = "Convert to TRUE/FALSE, e.g. roster$LEFT_TRUNCATED <- roster$LEFT_TRUNCATED == \"Y\".")
    }
  } else if ("EL_STATUS" %in% names(roster)) {
    p$add("note", "roster", "LEFT_TRUNCATED", paste0(
      "Not delivered. Checks 1 and 2 will treat every student first seen as ",
      "an EL in the first year of the data as already in status, and leave ",
      "them out of the entry cohorts."),
      fix = "Deliver it if you know which students entered before the data begin.")
  }

  if ("SAMPLE_WEIGHT" %in% names(roster)) {
    w <- roster$SAMPLE_WEIGHT
    bad <- !is.numeric(w) | (is.numeric(w) & !is.na(w) & w <= 0)
    if (any(bad)) {
      p$add("warning", "roster", "SAMPLE_WEIGHT",
            "Not a positive number.", sum(bad), w[bad],
            fix = "Weights are positive numbers; use 1 when a row stands for itself.")
    }
  }

  absent <- setdiff(el_data_format("roster")$COLUMN[
    el_data_format("roster")$NEED == "recommended"], names(roster))
  absent <- setdiff(absent, "LEFT_TRUNCATED")   # its own note above
  if (length(absent)) {
    p$add("note", "roster", paste(absent, collapse = ", "), paste0(
      "Recommended column(s) not delivered: the views that break results ",
      "out by them are left out."),
      fix = "See el_data_format(\"roster\") for what each unlocks.")
  }
  roster
}

# Students with an NA-status year strictly between their first and last
# year in status -- the gap build_student_spans() reads as an absence.
.na_status_inside_run <- function(roster) {
  if (!all(c("STUDENT_ID", "YEAR", "EL_STATUS") %in% names(roster))) return(0L)
  inst <- roster[roster$EL_STATUS %in% c("current", "former") &
                   !is.na(roster$YEAR), c("STUDENT_ID", "YEAR")]
  if (!nrow(inst)) return(0L)
  rng <- dplyr::summarise(dplyr::group_by(inst, .data$STUDENT_ID),
                          LO = min(.data$YEAR), HI = max(.data$YEAR),
                          .groups = "drop")
  na <- roster[is.na(roster$EL_STATUS) & !is.na(roster$YEAR),
               c("STUDENT_ID", "YEAR")]
  if (!nrow(na)) return(0L)
  na <- dplyr::inner_join(na, rng, by = "STUDENT_ID")
  length(unique(na$STUDENT_ID[na$YEAR > na$LO & na$YEAR < na$HI]))
}

.check_grade_values <- function(g, table, p) {
  g <- as.character(g)
  if (anyNA(g)) {
    p$add("warning", table, "GRADE", "Missing grade.", sum(is.na(g)),
          fix = "Every row needs a grade.")
  }
  odd <- !is.na(g) & !g %in% GRADE_VALUES
  if (!any(odd)) return(invisible())
  kcodes <- c("0", "00", "KG", "KF", "KH", "k")
  if (any(g[odd] %in% kcodes)) {
    p$add("error", table, "GRADE", paste0(
      "Kindergarten coded as something other than \"K\": Check 1 will find ",
      "no kindergarten cohort."), sum(g %in% kcodes),
      g[g %in% kcodes],
      fix = "Add the code to data$columns$value_maps$GRADE, e.g. c(\"0\" = \"K\").")
  }
  other <- odd & !g %in% kcodes
  if (any(other)) {
    p$add("warning", table, "GRADE", paste0(
      "Grades outside K and 1-12. The rows are kept, but no grade-keyed ",
      "result places them."), sum(other), g[other],
      fix = "Drop them, or map them in data$columns$value_maps$GRADE.")
  }
}

.check_form_values <- function(x, table, p) {
  x <- as.character(x)
  bad <- !is.na(x) & !x %in% ELP_FORM_VALUES
  if (any(bad)) {
    p$add("error", table, "ELP_FORM", paste0(
      "Values other than annual / alternate. Only \"annual\" rows give the ",
      "main ELP score, so these rows would leave every stage that reads it ",
      "with no students."), sum(bad), x[bad],
      fix = paste0("Recode the annual test's label to \"annual\" ",
                   "(mark_alternate_elp(x, annual = \"general\") does it), ",
                   "and the alternate test's to \"alternate\"."))
  }
}

.check_flag_values <- function(x, field, p) {
  v <- toupper(trimws(as.character(x)))
  v <- v[!is.na(v)]
  if (!length(v)) return(invisible())
  unknown <- !v %in% c(.YES_TOKENS, .NO_TOKENS)
  if (any(unknown)) {
    p$add("warning", "roster", field, paste0(
      "Values read as neither yes nor no; Check 2 counts them as \"no\"."),
      sum(unknown), v[unknown],
      fix = "Recode to Y / N.")
  } else if (all(v %in% c("1", "0"))) {
    p$add("note", "roster", field, paste0(
      "Coded 1 / 0. Read correctly as yes / no, but Prep 2's profile prints ",
      "the codes as delivered."), length(v),
      fix = "Recode to Y / N for labelled tables.")
  }
}

.check_base_table <- function(base, roster, spec, p) {
  if ("SUBJECT" %in% names(base)) {
    p$add("error", "base", "SUBJECT", paste0(
      "The ELP score table keys on COMPONENT, not SUBJECT (a reading domain ",
      "and a Reading subject would mix in a join)."),
      fix = "Rename SUBJECT to COMPONENT, or map it in data$columns$base.")
  }
  for (m in setdiff(c(KEY, "COMPONENT", "ACHIEVEMENT_LEVEL"), names(base))) {
    if (m == "ACHIEVEMENT_LEVEL" && "SCALE_SCORE" %in% names(base)) {
      p$add("warning", "base", m, paste0(
        "Not delivered. Bands, the exit threshold and Check 4 all read the ",
        "level."), fix = "Deliver the vendor's level with its decimal.")
      next
    }
    p$add("error", "base", m, "Required column is missing.",
          fix = "Add it, or map your column name in data$columns$base.")
  }
  extra <- setdiff(names(base), BASE_COLUMNS)
  if (length(extra)) {
    p$add("error", "base", paste(extra, collapse = ", "), paste0(
      "The ELP score table carries scores only; as_el_data() stops on any ",
      "other column."),
      fix = "Drop them (roster attributes belong on the roster).")
  }
  if (!"ELP_FORM" %in% names(base) &&
      (is.null(roster) || !"ELP_FORM" %in% names(roster))) {
    p$add("error", "base", "ELP_FORM", paste0(
      "Missing from both the ELP score table and the roster: it gates the ",
      "main ELP score."),
      fix = "Add ELP_FORM = \"annual\" (and \"alternate\" for the alternate test).")
  } else if ("ELP_FORM" %in% names(base)) {
    .check_form_values(base$ELP_FORM, "base", p)
  }

  if ("YEAR" %in% names(base)) base$YEAR <- .parse_years_quiet(base$YEAR)
  if ("STUDENT_ID" %in% names(base)) base$STUDENT_ID <- as.character(base$STUDENT_ID)

  if ("COMPONENT" %in% names(base)) {
    if (is.factor(base$COMPONENT)) {
      p$add("error", "base", "COMPONENT", "A factor; it must be character.",
            fix = "base$COMPONENT <- as.character(base$COMPONENT)")
    }
    comp <- as.character(base$COMPONENT)
    if (!is.null(spec)) {
      unknown <- !is.na(comp) & !comp %in% spec_components(spec)
      if (any(unknown)) {
        p$add("error", "base", "COMPONENT", paste0(
          "Values that are not components in the spec (",
          paste(spec_components(spec), collapse = ", "), ")."),
          sum(unknown), comp[unknown],
          fix = "Recode to the spec's component keys, e.g. \"Overall\" to \"overall\".")
      }
    }
    if (all(KEY %in% names(base))) {
      dup <- duplicated(base[c(KEY, "COMPONENT")]) |
        duplicated(base[c(KEY, "COMPONENT")], fromLast = TRUE)
      if (any(dup)) {
        p$add("error", "base", "STUDENT_ID x YEAR x COMPONENT",
              "More than one row for the same student, year and score.",
              sum(dup), paste(base$STUDENT_ID[dup], base$YEAR[dup],
                              comp[dup]),
              fix = "Keep one row per student, year and score.")
      }
    }
  }

  lv <- base$ACHIEVEMENT_LEVEL
  if (!is.null(lv) && !is.numeric(lv)) {
    num <- suppressWarnings(as.numeric(as.character(lv)))
    txt <- !is.na(lv) & is.na(num)
    p$add("error", "base", "ACHIEVEMENT_LEVEL", paste0(
      "Text values where a number is needed. Codes like these usually come ",
      "from the alternate test, whose levels are a different scale."),
      sum(txt), lv[txt],
      fix = "mark_alternate_elp() sets them to NA and marks the rows \"alternate\".")
    lv <- num
  }

  if (!is.null(roster) && all(KEY %in% names(base)) &&
      all(KEY %in% names(roster))) {
    rk <- paste(roster$STUDENT_ID, roster$YEAR)
    bk <- paste(base$STUDENT_ID, base$YEAR)
    orphan <- !bk %in% rk
    if (any(orphan)) {
      p$add("error", "base", "STUDENT_ID x YEAR",
            "Score rows with no roster row for that student and year.",
            sum(orphan), bk[orphan],
            fix = "Every scored student-year needs a roster row.")
    }
    if ("ELP_FORM" %in% names(base) && "ELP_FORM" %in% names(roster)) {
      rf <- roster$ELP_FORM[match(bk, rk)]
      dis <- !is.na(rf) & !is.na(base$ELP_FORM) & rf != base$ELP_FORM
      if (any(dis)) {
        p$add("error", "base", "ELP_FORM",
              "Disagrees with the roster's ELP_FORM for the same student-year.",
              sum(dis), paste(bk[dis], base$ELP_FORM[dis]),
              fix = "Make the two agree, or drop ELP_FORM from the roster.")
      }
    }
  }

  if (!is.null(spec) && "COMPONENT" %in% names(base)) {
    is_head <- as.character(base$COMPONENT) %in% spec_headline(spec)
    form <- if ("ELP_FORM" %in% names(base)) base$ELP_FORM
            else if (!is.null(roster) && "ELP_FORM" %in% names(roster))
              roster$ELP_FORM[match(paste(base$STUDENT_ID, base$YEAR),
                                    paste(roster$STUDENT_ID, roster$YEAR))]
            else rep(NA_character_, nrow(base))
    annual <- is_head & form %in% "annual"
    if (!any(annual)) {
      p$add("error", "base", "COMPONENT / ELP_FORM", paste0(
        "No annual-form rows for the main ELP score (\"",
        spec_headline(spec), "\", the ", spec_headline_label(spec),
        "). Every stage that reads it would have no students."),
        sum(is_head),
        fix = "Check the COMPONENT codes and ELP_FORM values (\"annual\").")
    } else {
      .check_level_scale(lv[annual], spec, p)
      if (!"SCALE_SCORE" %in% names(base) ||
          all(is.na(base$SCALE_SCORE[annual]))) {
        p$add("note", "base", "SCALE_SCORE", paste0(
          "No scale score on the main ELP score: Check 1's models with an ",
          "ELP term and Check 3's logistic are not fitted."))
      }
    }
  }
}

.check_level_scale <- function(lv, spec, p) {
  lv <- lv[!is.na(lv)]
  if (!length(lv)) {
    p$add("warning", "base", "ACHIEVEMENT_LEVEL",
          "The main ELP score has no levels: bands and Check 4 are empty.")
    return(invisible())
  }
  ls <- spec$elp$level_scale
  if (identical(ls$type, "numeric")) {
    out <- lv < ls$range[[1]] | lv > ls$range[[2]]
    if (any(out)) {
      p$add("warning", "base", "ACHIEVEMENT_LEVEL", paste0(
        "Levels outside the spec's scale (", ls$range[[1]], "-",
        ls$range[[2]], ") on the main ELP score; Check 4 leaves them out."),
        sum(out), lv[out],
        fix = "Check for missing-value codes (e.g. 9, 99) and set them to NA.")
    }
  }
  bands <- spec_bands(spec)
  if (!is.null(bands) && any(bands$breaks %% 1 != 0) && all(lv %% 1 == 0)) {
    p$add("warning", "base", "ACHIEVEMENT_LEVEL", paste0(
      "Every level is a whole number, but the spec's bands split at a ",
      "decimal: bands such as 4.5-4.9 can never fill."), length(lv),
      fix = "Use the decimal level from the vendor's score file.")
  }
}

.check_academic_table <- function(ac, roster, spec, p) {
  ac <- .rename_alias_quiet(ac, "ID", "STUDENT_ID")
  ac <- .rename_alias_quiet(ac, "CONTENT_AREA", "SUBJECT")
  if ("COMPONENT" %in% names(ac)) {
    p$add("error", "academic", "COMPONENT",
          "The academic table keys on SUBJECT, not COMPONENT.",
          fix = "Rename COMPONENT to SUBJECT.")
  }
  for (m in setdiff(ACADEMIC_REQUIRED, names(ac))) {
    p$add("error", "academic", m, "Required column is missing.",
          fix = if (m == "GRADE") "GRADE is required on the academic row; it is not filled from the roster."
                else "Add it, or map your column name in data$columns$academic.")
  }
  if (!all(c(KEY, "SUBJECT") %in% names(ac))) return(invisible())

  ac$STUDENT_ID <- as.character(ac$STUDENT_ID)
  ac$YEAR <- .parse_years_quiet(ac$YEAR)
  if ("GRADE" %in% names(ac)) {
    ac$GRADE <- .recode_grade(ac$GRADE, spec)
    .check_grade_values(ac$GRADE, "academic", p)
  }

  subj <- as.character(ac$SUBJECT)
  subjects <- if (is.null(spec)) character() else spec_academic_subjects(spec)
  map <- if (is.null(spec)) NULL else spec$data$columns$value_maps$CONTENT_AREA$academic
  if (!is.null(map)) {
    hit <- subj %in% names(map)
    subj[hit] <- unname(map[subj[hit]])
  }
  if (!is.null(spec) && !length(subjects)) {
    p$add("error", "academic", "SUBJECT", paste0(
      "The spec declares no academic subject (its academic block is a ",
      "pointer), so no stage can read this table; Checks 2 and 3 stop."),
      fix = "Write the academic block inline: subjects, grades, level_labels, cuts.")
  } else if (length(subjects)) {
    unknown <- !is.na(subj) & !subj %in% subjects
    if (any(unknown)) {
      none <- !any(subj %in% subjects)
      p$add(if (none || !is.null(map)) "error" else "warning", "academic",
            "SUBJECT", paste0(
              if (none) "No row matches a subject in the spec (" else
                "Values that match no subject in the spec (",
              paste(subjects, collapse = ", "), ")",
              if (!is.null(map)) "; as_el_data() stops on an unmapped code."
              else ": no stage reads those rows."),
            sum(unknown), ac$SUBJECT[unknown],
            fix = paste0("Map them in data$columns$value_maps$CONTENT_AREA$",
                         "academic (e.g. c(READING = \"ELA\")), or drop the ",
                         "subjects you are not analysing."))
    }
  }

  dup <- duplicated(ac[c(KEY, "SUBJECT")]) |
    duplicated(ac[c(KEY, "SUBJECT")], fromLast = TRUE)
  if (any(dup)) {
    p$add("error", "academic", "STUDENT_ID x YEAR x SUBJECT",
          "More than one row for the same student, year and subject.",
          sum(dup), paste(ac$STUDENT_ID[dup], ac$YEAR[dup], subj[dup]),
          fix = "Keep one row per student, year and subject.")
  }

  if ("ACHIEVEMENT_LEVEL" %in% names(ac)) {
    lv <- ac$ACHIEVEMENT_LEVEL
    num <- if (is.numeric(lv)) lv else suppressWarnings(as.numeric(as.character(lv)))
    txt <- !is.na(lv) & is.na(num)
    if (any(txt)) {
      # By subject, not by row: a statewide file is all labels.
      bad <- rep(FALSE, length(lv))
      for (sb in unique(subj[txt])) {
        at <- txt & subj %in% sb
        lab <- if (is.null(spec) || is.na(sb)) NULL
               else spec$academic[[sb]]$level_labels
        bad[at] <- is.null(lab) | !as.character(lv[at]) %in% lab
      }
      if (any(bad)) {
        p$add("error", "academic", "ACHIEVEMENT_LEVEL", paste0(
          "Labels not in the subject's level_labels in the spec."),
          sum(bad), lv[bad],
          fix = "List the labels, lowest first, in academic$<subject>$level_labels (e.g. c(\"N\", \"A\", \"P\", \"D\")).")
      }
    }
    frac <- !is.na(num) & num != round(num)
    if (any(frac)) {
      p$add("error", "academic", "ACHIEVEMENT_LEVEL",
            "Not a whole level.", sum(frac), lv[frac],
            fix = "Academic levels are whole numbers, 1 = lowest.")
    }
  }
  if (!"SCALE_SCORE" %in% names(ac) || all(is.na(ac$SCALE_SCORE))) {
    p$add("note", "academic", "SCALE_SCORE", paste0(
      "No academic scale scores: Check 3's box plots and the cut-score ",
      "comparisons are empty."))
  }

  if (!is.null(roster) && all(KEY %in% names(roster))) {
    rk <- paste(roster$STUDENT_ID, roster$YEAR)
    ak <- paste(ac$STUDENT_ID, ac$YEAR)
    at <- match(ak, rk)
    if (any(is.na(at))) {
      p$add("error", "academic", "STUDENT_ID x YEAR",
            "Academic rows with no roster row for that student and year.",
            sum(is.na(at)), ak[is.na(at)],
            fix = paste0("Every tested student-year needs a roster row, with ",
                         "EL_STATUS (NA if unknown) -- including years before ",
                         "a student's first ELP test."))
    }
    if ("GRADE" %in% names(ac) && "GRADE" %in% names(roster)) {
      rg <- .recode_grade(roster$GRADE, spec)[at]
      dis <- !is.na(at) & !is.na(rg) & !is.na(ac$GRADE) & rg != ac$GRADE
      if (any(dis)) {
        p$add("error", "academic", "GRADE",
              "Disagrees with the roster's grade for the same student-year.",
              sum(dis), paste(ak[dis], ac$GRADE[dis]),
              fix = "Make the two agree.")
      }
    }
  }

  if (length(subjects) && "GRADE" %in% names(ac)) {
    .check_cut_coverage(ac, subj, spec, p)
  }
}

# For every declared subject and grade the data carries: is there a cut score,
# and if not, can proficiency be read off the delivered level?
.check_cut_coverage <- function(ac, subj, spec, p) {
  lvl <- spec_proficient_level(spec)
  has_level <- "ACHIEVEMENT_LEVEL" %in% names(ac) &&
    any(!is.na(ac$ACHIEVEMENT_LEVEL))
  no_cut <- character()
  for (sb in spec_academic_subjects(spec)) {
    grades <- sort(unique(as.character(ac$GRADE[subj %in% sb])))
    declared <- spec_academic_grades(spec, sb)
    if (!is.null(declared)) {
      extra <- setdiff(grades, declared)
      if (length(extra)) {
        p$add("note", "academic", "GRADE", paste0(
          sb, " rows in grades the spec does not declare tested; Check 3 ",
          "skips them."), sum(subj %in% sb & ac$GRADE %in% extra), extra,
          fix = paste0("Add them to academic$", sb, "$grades if they should be analysed."))
      }
      grades <- intersect(grades, declared)
    }
    for (g in grades) {
      if (is.na(spec_proficient_cut(spec, sb, g))) no_cut <- c(no_cut, paste(sb, g))
    }
  }
  if (!length(no_cut)) return(invisible())
  if (is.na(lvl) || !has_level) {
    p$add("warning", "academic", "SCALE_SCORE / ACHIEVEMENT_LEVEL", paste0(
      "No proficiency cut in the spec for these subject-grades, and ",
      if (is.na(lvl)) "no proficient level in the spec"
      else "no delivered performance level",
      " to fall back on: Checks 3 and 4 cannot tell who is proficient."),
      length(no_cut), no_cut,
      fix = "Enter each grade's cut scores in academic$<subject>$cuts (a list by grade).")
  } else {
    p$add("note", "academic", "SCALE_SCORE / ACHIEVEMENT_LEVEL", paste0(
      "No cut score in the spec for these subject-grades. Checks 3 and 4 ",
      "will read proficiency from the delivered performance level (level ",
      lvl, " or above) instead, and draw no cut line."),
      length(no_cut), no_cut,
      fix = "Enter each grade's cut scores in academic$<subject>$cuts (a list by grade).")
  }
}

.rename_alias_quiet <- function(d, alias, canon) {
  if (alias %in% names(d) && !canon %in% names(d)) {
    names(d)[names(d) == alias] <- canon
  }
  d
}

#' @export
print.el_delivery_check <- function(x, ...) {
  n <- table(factor(x$SEVERITY, levels = c("error", "warning", "note")))
  cat("<el_delivery_check> ", n[["error"]], " error(s), ", n[["warning"]],
      " warning(s), ", n[["note"]], " note(s)\n", sep = "")
  if (!nrow(x)) {
    cat("No problems found: build the bundle with as_el_data().\n")
    return(invisible(x))
  }
  heads <- c(
    error   = "ERRORS: as_el_data() stops, or the stages that read these get no students",
    warning = "WARNINGS: results are affected",
    note    = "NOTES")
  for (sv in c("error", "warning", "note")) {
    d <- x[x$SEVERITY == sv, , drop = FALSE]
    if (!nrow(d)) next
    cat("\n", heads[[sv]], "\n", sep = "")
    for (i in seq_len(nrow(d))) {
      col <- if (nzchar(d$COLUMN[[i]])) paste0(" ", d$COLUMN[[i]]) else ""
      cnt <- if (!is.na(d$N_ROWS[[i]])) paste0(" (", .fmt_count(d$N_ROWS[[i]]), ")") else ""
      cat(strwrap(paste0("[", d$TABLE[[i]], "]", col, ": ", d$PROBLEM[[i]], cnt),
                  width = 78, exdent = 4), sep = "\n")
      if (nzchar(d$EXAMPLES[[i]])) {
        cat(strwrap(paste0("found: ", d$EXAMPLES[[i]]), width = 78,
                    indent = 4, exdent = 6), sep = "\n")
      }
      if (nzchar(d$FIX[[i]])) {
        cat(strwrap(paste0("fix: ", d$FIX[[i]]), width = 78, indent = 4,
                    exdent = 6), sep = "\n")
      }
    }
  }
  if (isTRUE(attr(x, "ok"))) {
    cat("\nNo errors: the bundle will build. Read the warnings before going on.\n")
  }
  invisible(x)
}

# ---- alternate-test rows -----------------------------------------------------

#' Mark alternate ELP test rows
#'
#' Alternate ACCESS -- and other alternate ELP tests -- report levels on a
#' different scale, often as codes such as `"A2"` or `"P2"`. The package keeps
#' those students out of the main ELP score and the bands, and reports them as
#' a group, through `ELP_FORM = "alternate"`; their level is not a number on
#' the annual scale. This helper prepares a score table for that:
#'
#' * rows whose level does not read as a number (or the rows `is_alternate`
#'   names) get `ACHIEVEMENT_LEVEL = NA` and `ELP_FORM = "alternate"`, and so
#'   does every other row of the same student and year, so a student-year has
#'   one form;
#' * the level column becomes numeric;
#' * values listed in `annual` (a vendor's label for the annual test, such as
#'   `"general"`) become `"annual"`.
#'
#' Run it on the ELP score table before [as_el_data()]. If your roster also
#' carries `ELP_FORM`, give the same student-years `"alternate"` there too --
#' `as_el_data()` stops when the two disagree -- or drop it from the roster
#' and let the score table carry it.
#'
#' @param x The ELP score table (or a merged long file).
#' @param is_alternate Optional logical vector, one per row, marking
#'   alternate-test rows explicitly (for instance from a test-name column).
#'   `NULL` marks rows whose level is text that does not read as a number.
#' @param annual Values of the form column to recode to `"annual"`, e.g.
#'   `"general"`.
#' @param level_col,form_col The level and form columns.
#' @return `x`, changed as above, with a `"n_alternate"` attribute (the
#'   number of student-years marked).
#' @export
#' @examples
#' x <- data.frame(STUDENT_ID = c("a", "a", "b"), YEAR = 2025,
#'                 COMPONENT = c("overall", "written", "overall"),
#'                 ELP_FORM = "general",
#'                 ACHIEVEMENT_LEVEL = c("P2", "P1", "4.5"))
#' mark_alternate_elp(x, annual = "general")
mark_alternate_elp <- function(x, is_alternate = NULL, annual = NULL,
                               level_col = "ACHIEVEMENT_LEVEL",
                               form_col = "ELP_FORM") {
  stopifnot(is.data.frame(x), level_col %in% names(x),
            is.null(is_alternate) ||
              (is.logical(is_alternate) && length(is_alternate) == nrow(x)))
  lv <- x[[level_col]]
  num <- if (is.numeric(lv)) lv else suppressWarnings(as.numeric(as.character(lv)))
  alt <- if (is.null(is_alternate)) !is.na(lv) & is.na(num)
         else is_alternate %in% TRUE

  if (!form_col %in% names(x)) x[[form_col]] <- NA_character_
  form <- as.character(x[[form_col]])
  if (!is.null(annual)) form[form %in% annual] <- "annual"

  # One form per student-year: a student who took the alternate test took it
  # for every domain, and as_el_data() checks the form per student-year.
  if (all(c("STUDENT_ID", "YEAR") %in% names(x))) {
    key <- paste(x$STUDENT_ID, x$YEAR)
    alt <- key %in% key[alt]
    n_alt <- length(unique(key[alt]))
  } else {
    n_alt <- sum(alt)
  }
  form[alt] <- "alternate"
  num[alt] <- NA_real_
  x[[form_col]] <- form
  x[[level_col]] <- as.numeric(num)
  attr(x, "n_alternate") <- n_alt
  x
}
