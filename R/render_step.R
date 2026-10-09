# The fifty-times-a-day loop.
#
# Renders one stage document. The copy-in / render / remove-copy dance exists
# so that Quarto's `<name>_files/` directory and its intermediates land in the
# output directory and never in the source tree -- rendering in place leaves
# build litter inside inst/templates/ that then ships in the package.

#' Render one stage document
#'
#' Resolves a stage id to its `.qmd`, copies it to the output directory,
#' renders it there in each requested format, optionally saves the stage's
#' results object beside the output, and removes the copy.
#'
#' Stage ids carry no run-order numbers: the numbers live on the prompt and
#' instruction files only, so `render_step("check1")` finds
#' `inst/templates/check1_survival.qmd`.
#'
#' @param stage A stage id: `"prep1"`, `"prep2"`, `"prep3"`, `"check1"`,
#'   `"check2"`, `"check3"`, `"check4"` or `"decision"`.
#' @param spec_file Spec id passed to the document as a parameter.
#' @param out_dir Directory to render into. Created if absent.
#' @param formats Quarto output formats. Word is primary, HTML secondary.
#' @param min_n Suppression minimum passed to the document. `NULL` lets the
#'   document read it from the spec, which is what it should normally do.
#' @param save_results When `TRUE` and `run_<stage>()` exists, computes the
#'   stage's results and writes `<stage>_results.rds` into `results_dir`. Off
#'   by default: in dev mode the document computes its own results, so turning
#'   this on computes them a second time. Turn it on to feed the Decision
#'   stage, which is the natural RDS consumer. `[PROPOSED - CONFIRM]`
#' @param results_dir Where saved results go — deliberately **not** `out_dir`.
#'   The documents there are shareable; the results objects are not, because
#'   Check 2's `$flags` and `$profile` are one row per student. Side by side,
#'   zipping the output directory would ship the student rows with the
#'   documents.
#' @param results_file An existing `<stage>_results.rds` to render *from*
#'   instead of recomputing. Every stage `.qmd` already accepts this and
#'   branches on it; this is the way to pass one. The Decision stage is the
#'   consumer that needs it.
#' @param check_disclosure Scan the produced files for student identifiers and
#'   **stop** if any appear (see `R/disclosure.R`). Defaults to `TRUE`
#'   whenever something was rendered. Set `FALSE` on a real delivery, where
#'   the package cannot assemble the bundle itself, and call
#'   [check_output_disclosure()] against your own bundle instead.
#' @param open Whether to open the rendered document. Defaults to interactive
#'   use only.
#' @param quiet Passed to Quarto. `FALSE` shows the CLI's own output, which is
#'   the only way to see why a render failed.
#' @param qmd Optional explicit path to a `.qmd`, bypassing stage-id lookup.
#'   Used by the smoke test.
#'
#' @return Invisibly, a character vector of the files written.
#' @export
#' @examples
#' \dontrun{
#' render_step("check1")
#' render_step("decision", save_results = TRUE)
#' }
render_step <- function(stage,
                        spec_file = "00_SIM_EL_Spec",
                        out_dir = "reports",
                        formats = c("docx", "html"),
                        min_n = NULL,
                        save_results = FALSE,
                        results_dir = "results",
                        results_file = NULL,
                        check_disclosure = NULL,
                        open = interactive(),
                        quiet = TRUE,
                        qmd = NULL) {
  stopifnot(is.character(stage), length(stage) == 1L)

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' is required to render. install.packages(\"quarto\")",
         call. = FALSE)
  }
  src <- if (is.null(qmd)) stage_qmd_path(stage) else qmd
  if (!file.exists(src)) {
    stop("No document found for stage '", stage, "'. Available: ",
         paste(stage_ids(), collapse = ", "), call. = FALSE)
  }

  pkg <- getwd()          # captured before the render setwd()
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)

  # Render a COPY in a scratch directory, then move the finished documents
  # into out_dir.
  #
  # Why not render directly in out_dir, as one might expect: this repository
  # lives inside a Dropbox tree, and Quarto's post-render cleanup of
  # `<name>_files` fails there with "the process cannot access the file
  # because it is being used by another process" (os error 32) -- the sync
  # client holds a handle on the directory Quarto is trying to delete. The
  # document renders fine and then the CLI exits non-zero on cleanup, so the
  # loop looks broken while actually having produced its output, and a stale
  # `_files` directory is left behind to poison the next run. Rendering
  # outside the synced tree avoids the contention entirely and keeps the
  # promise that matters: `_files/` never appears in the source tree.
  # [PROPOSED - CONFIRM] -- this deviates from CLAUDE.md 9's "render there".
  scratch <- file.path(tempdir(), paste0("stateELP_render_", stage))
  unlink(scratch, recursive = TRUE, force = TRUE)
  dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

  copy <- file.path(scratch, basename(src))
  file.copy(src, copy, overwrite = TRUE)

  written <- character()
  saved <- character()

  # Saved results go somewhere OTHER than out_dir, even though out_dir is
  # where they used to land. The documents in out_dir are shareable and the
  # results objects are not -- Check 2's `$flags` and `$profile` are one row
  # per student -- so putting them side by side means anyone who zips the
  # output directory to send a colleague a stage ships the student rows with
  # it. The documents were never the leak; the directory was.
  if (isTRUE(save_results)) {
    dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
    rds <- .save_stage_results(stage, spec_file,
                               normalizePath(results_dir, winslash = "/",
                                             mustWork = TRUE))
    if (!is.na(rds)) saved <- c(saved, rds)
  }

  params <- list(pkg_path = pkg, spec_file = spec_file)
  if (!is.null(min_n)) params$min_n <- min_n
  # RDS mode: every stage `.qmd` already accepts `results_file` and branches
  # on it, but there was no way to pass one, so the read half of the loop was
  # unreachable through this helper. The Decision stage is the consumer that
  # needs it.
  if (!is.null(results_file)) {
    params$results_file <- normalizePath(results_file, winslash = "/",
                                         mustWork = TRUE)
  }

  for (fmt in formats) {
    quarto::quarto_render(input = copy, output_format = fmt,
                          execute_params = params, quiet = quiet)
    produced <- list.files(scratch, pattern = paste0("\\.", fmt, "$"),
                           full.names = TRUE)
    for (f in produced) {
      dest <- file.path(out_dir, basename(f))
      file.copy(f, dest, overwrite = TRUE)
      written <- c(written, dest)
    }
  }
  written <- unique(written)

  # The backstop: scan what was actually produced. Defaults on whenever the
  # bundle is reachable, because a check you have to remember to ask for is
  # not a guardrail. Errors rather than warns -- a document carrying student
  # rows should not survive the render that made it.
  if (is.null(check_disclosure)) check_disclosure <- length(written) > 0L
  if (isTRUE(check_disclosure) && length(written)) {
    bundle <- tryCatch(.render_bundle(spec_file), error = function(e) NULL)
    if (!is.null(bundle)) {
      check_output_disclosure(written, bundle, action = "error")
    } else if (!quiet) {
      message("  (disclosure scan skipped: no bundle for '", spec_file, "')")
    }
  }

  if (isTRUE(open) && length(written)) {
    utils::browseURL(written[[1]])
  }
  message("Rendered ", stage, ": ",
          paste(basename(written), collapse = ", "))
  if (length(saved)) {
    message("  results saved (NOT shareable -- student-level slots): ",
            paste(saved, collapse = ", "))
  }
  invisible(c(written, saved))
}

# The bundle a render was built from, for the disclosure scan. The shipped
# simulation is the only delivery the package knows how to assemble on its
# own; a real delivery is assembled by the analyst, who should pass
# `check_disclosure = FALSE` and run `check_output_disclosure()` against their
# own bundle instead.
.render_bundle <- function(spec_file) {
  spec <- read_el_spec(spec_file)
  as_el_data(get("el_long_sim"), roster = get("el_roster_sim"),
             academic = get("academic_long_sim"), spec = spec)
}

#' Stage ids and their documents
#'
#' @param stage A stage id.
#' @return `stage_ids()` the eight ids in run order; `stage_qmd_path()` the
#'   path to that stage's document, or `NA` when it does not exist yet.
#' @export
stage_ids <- function() {
  c("prep1", "prep2", "prep3",
    "check1", "check2", "check3", "check4",
    "decision")
}

#' @rdname stage_ids
#' @export
stage_qmd_path <- function(stage) {
  dir <- system.file("templates", package = "stateELP")
  if (!nzchar(dir)) return(NA_character_)
  hit <- list.files(dir, pattern = paste0("^", stage, "_.*\\.qmd$"),
                    full.names = TRUE)
  if (!length(hit)) return(NA_character_)
  hit[[1]]
}

# Compute and save a stage's results, when that stage has been built. Returns
# NA when the stage's run_*() does not exist yet, so the shell renders before
# any stage is written.
.save_stage_results <- function(stage, spec_file, out_dir) {
  fn <- paste0("run_", stage)
  if (!exists(fn, envir = asNamespace("stateELP"), inherits = FALSE)) {
    message("No ", fn, "() yet; skipping the results RDS.")
    return(NA_character_)
  }
  run <- get(fn, envir = asNamespace("stateELP"))
  spec <- read_el_spec(spec_file)

  # Prep 1 and Prep 3 take the spec alone -- a data argument is an error --
  # and the Decision stage takes prior results, so it is not saved this way.
  res <- if (stage %in% c("prep1", "prep3")) {
    run(spec)
  } else {
    ns <- asNamespace("stateELP")
    run(as_el_data(get("el_long_sim", envir = ns),
                   roster = get("el_roster_sim", envir = ns),
                   academic = get("academic_long_sim", envir = ns),
                   spec = spec),
        spec)
  }
  path <- file.path(out_dir, paste0(stage, "_results.rds"))
  saveRDS(res, path)
  path
}
