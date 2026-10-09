# Disclosure control at the render boundary.
#
# THE RULE (decided 2026-10-09): student-level data may live on disk in the
# saved results objects -- the analyst's machine already holds the delivery
# those rows came from, so an RDS beside it discloses nothing new -- but it
# must never reach a .docx, .html or .xlsx, because those are the artifacts
# that get emailed, posted and forwarded.
#
# Two of Check 2's slots are legitimately student-level (`$flags` and
# `$profile`, one row per student keyed by STUDENT_ID; the crosstab is built
# from them), so this is not a hypothetical.
#
# WHY A POST-RENDER SCAN RATHER THAN ONLY A CLASSIFICATION. Labelling each
# slot "safe to export" is necessary but it rots: a slot added in a year and
# not classified, an htmlwidget that serialises its whole data frame into the
# page as JSON, a debugging chunk left with `echo: true`. Each of those buries
# rows in an output without anyone deciding to. Scanning the produced file for
# identifiers that actually appear in the delivery catches all of them,
# including the ones nobody anticipated, and cannot fall out of date -- it
# tests the artifact rather than our intentions about it.
#
# It is a backstop, not a substitute: a check that fires has already done the
# work of finding a leak, and the fix belongs upstream in whatever rendered
# the rows.

#' Scan rendered output for student identifiers
#'
#' Reads a produced `.docx`, `.xlsx`, `.html`, `.md` or `.csv` and reports any
#' of `ids` that appear in it. Word and Excel files are zip containers, so
#' their XML parts are extracted and searched; everything else is read as
#' text.
#'
#' **The search runs output-first, not id-first**, and that is the whole
#' design. Asking "does each id appear in this file" costs one scan per id,
#' which forces sampling on a delivery with tens of thousands of students --
#' and sampling cannot see a small leak. Five rows left by a stray
#' `head(flags, 5)` out of 88,279 students is caught by a 200-id sample
#' roughly one time in ninety.
#'
#' So the file is tokenised once and the tokens are looked up in the id set.
#' Every id is tested, a single leaked row is caught, and the cost is one
#' pass over the output rather than N passes.
#'
#' Matching is on whole tokens, so an id that is a substring of an unrelated
#' number does not register, and ids are never treated as patterns -- the
#' token alphabet is derived from the ids themselves, so an id containing
#' `.` or `(` works.
#'
#' @param path A rendered file.
#' @param ids Character vector of identifiers to look for.
#' @return A character vector of the identifiers found, empty when none are.
#' @export
#' @examples
#' f <- tempfile(fileext = ".html")
#' writeLines("<p>cohort of 2,373 students</p>", f)
#' scan_output_for_ids(f, c("S123", "2373"))
scan_output_for_ids <- function(path, ids) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  ids <- unique(stats::na.omit(as.character(ids)))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) return(character())

  text <- .disclosure_text(path)
  if (!length(text)) return(character())

  tokens <- .disclosure_tokens(text, ids)
  if (!length(tokens)) return(character())
  ids[ids %in% tokens]
}

# Tokenise the output, then compare whole tokens. Equality on a token is the
# safe stand-in for a word boundary, and it avoids ever compiling an id into
# a pattern.
#
# The alphabet is GENEROUS -- every alphanumeric, plus any further character
# the ids themselves use. It must not be narrowed to just the ids' own
# characters, which was the first attempt: with ids = "23456" that gives the
# class [23456]+, which splits "1234567" and hands back "23456" as a token
# that was never there. A wide alphabet keeps "1234567" whole, so it matches
# nothing; adding the ids' extras keeps "S.1" and "A-2" whole too.
.disclosure_tokens <- function(text, ids) {
  extra <- setdiff(unique(strsplit(paste(ids, collapse = ""), "")[[1]]),
                   c(LETTERS, letters, 0:9))
  extra <- extra[nzchar(extra)]
  cls <- paste0("[A-Za-z0-9",
                paste(gsub("([]\\\\^-])", "\\\\\\1", extra), collapse = ""),
                "]+")
  hits <- regmatches(text, gregexpr(cls, text, perl = TRUE))
  unique(unlist(hits, use.names = FALSE))
}

# Everything readable as text. A .docx or .xlsx is a zip of XML parts, and the
# cell values live in those parts, so unzipping is the only way to see what a
# workbook actually contains -- reading the container bytes would miss it,
# since the parts are deflated.
.disclosure_text <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("docx", "xlsx", "pptx")) {
    dir <- file.path(tempdir(), paste0("disclosure_", basename(path)))
    unlink(dir, recursive = TRUE, force = TRUE)
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
    ok <- tryCatch({ utils::unzip(path, exdir = dir); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) return(character())
    parts <- list.files(dir, recursive = TRUE, full.names = TRUE,
                        pattern = "\\.(xml|rels|txt)$")
    return(unlist(lapply(parts, function(p) {
      tryCatch(readLines(p, warn = FALSE, encoding = "UTF-8"),
               error = function(e) character())
    })))
  }
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
           error = function(e) character())
}


#' Check rendered output carries no student identifiers
#'
#' The guardrail behind the rule in this file: given the files a render
#' produced and the bundle it was built from, confirm none of the delivery's
#' `STUDENT_ID` values appears in any of them.
#'
#' **Every identifier is tested, not a sample.** An earlier version of this
#' sampled, on the reasoning that a leak is never one row; that reasoning was
#' wrong, and the end-to-end test that planted a five-row leak caught it
#' passing. [scan_output_for_ids()] now searches output-first, which makes
#' testing all of them cheaper than sampling was.
#'
#' @param files Paths to rendered output.
#' @param data An `el_data` bundle, or any data frame with a `STUDENT_ID`
#'   column.
#' @param action `"error"` to stop, `"warn"`, or `"report"` to return quietly.
#' @return Invisibly, a tibble of `FILE` and `ID` for every hit; zero rows
#'   when clean.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' f <- tempfile(fileext = ".html")
#' writeLines("<p>no identifiers here</p>", f)
#' check_output_disclosure(f, b)
check_output_disclosure <- function(files, data,
                                    action = c("error", "warn", "report")) {
  action <- match.arg(action)
  files <- files[file.exists(files)]
  ids <- .disclosure_ids(data)
  empty <- tibble::tibble(FILE = character(), ID = character())
  if (!length(files) || !length(ids)) return(invisible(empty))

  hits <- lapply(files, function(f) {
    got <- scan_output_for_ids(f, ids)
    if (!length(got)) return(NULL)
    tibble::tibble(FILE = basename(f), ID = got)
  })
  hits <- dplyr::bind_rows(hits)
  if (!nrow(hits)) return(invisible(empty))

  msg <- paste0(
    "Student identifiers reached rendered output. ",
    nrow(hits), " of the ", length(ids), " ids in this delivery appear in: ",
    paste(unique(hits$FILE), collapse = ", "), ". ",
    "Student-level data may be saved to disk but must never reach a ",
    "document or workbook (R/disclosure.R). Find what rendered those rows ",
    "-- a slot passed to a table, an htmlwidget serialising its data, a ",
    "chunk with echo: true -- and fix it there rather than suppressing ",
    "this check.")
  if (identical(action, "error")) stop(msg, call. = FALSE)
  if (identical(action, "warn")) warning(msg, call. = FALSE)
  invisible(hits)
}

# The identifiers to look for. A bundle's roster is the counting frame and
# carries every student; a bare data frame is taken as given.
.disclosure_ids <- function(data) {
  df <- if (inherits(data, "el_data")) el_roster(data) else data
  if (!is.data.frame(df) || !"STUDENT_ID" %in% names(df)) {
    stop("check_output_disclosure() needs an el_data bundle or a data frame ",
         "with STUDENT_ID.", call. = FALSE)
  }
  unique(as.character(df$STUDENT_ID))
}
