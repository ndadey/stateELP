# The fifty-times-a-day loop.
#
# Renders one stage document. The copy-in / render / copy-out dance exists so
# that Quarto's `<name>_files/` directory and its intermediates never land in
# the source tree -- rendering in place leaves build litter inside
# inst/templates/ that then ships in the package. The copy is rendered in a
# scratch folder under tempdir() and only the finished documents reach
# `out_dir` (see the Dropbox note below).

#' Render one stage document
#'
#' Resolves a stage id to its `.qmd`, copies it to a scratch folder, renders
#' it there in each requested format, copies the finished documents into
#' `out_dir`, and optionally saves the stage's results object into
#' `results_dir` (never beside the documents).
#'
#' **Rendering a state's own data.** Compute the results with your bundle,
#' save them, and pass the file as `results_file` together with that same
#' `bundle`: the disclosure scan then checks the documents against your
#' students' ids. Without `bundle`, a render from saved state results cannot
#' be scanned, and `render_step()` says so with a warning whatever `quiet` is.
#'
#' Stage ids carry no run-order numbers: the numbers live on the prompt and
#' instruction files only, so `render_step("check1")` finds
#' `inst/templates/check1_survival.qmd`.
#'
#' @param stage A stage id: `"prep1"`, `"prep2"`, `"prep3"`, `"check1"`,
#'   `"check2"`, `"check3"`, `"check4"` or `"decision"`.
#' @param spec_file Spec id, or a path to a spec file, passed to the document
#'   as a parameter. A path is made absolute first, because the document runs
#'   in another folder.
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
#' @param bundle The `el_data` bundle the rendered results were computed
#'   from, for the disclosure scan. Pass it whenever `results_file` holds a
#'   state's own results. `NULL` scans against the shipped simulation, which
#'   is right only for the simulation's own results.
#' @param check_disclosure Scan the produced files for student identifiers and
#'   **stop** if any appear (see `R/disclosure.R`). Defaults to `TRUE`
#'   whenever something was rendered. The ids come from `bundle`, or from the
#'   shipped simulation when the document was rendered from it. When neither
#'   applies -- saved results from a state delivery and no `bundle` -- the
#'   scan cannot run, and a warning says so; pass `bundle`, or set this to
#'   `FALSE` and call [check_output_disclosure()] yourself.
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
#'
#' # A state's own data: compute, save, render from the file, scan against
#' # the same bundle.
#' saveRDS(run_check1(my_bundle, my_spec), "results/check1_results.rds")
#' render_step("check1", spec_file = "specs/XX_EL_Spec.R",
#'             results_file = "results/check1_results.rds",
#'             bundle = my_bundle)
#' }
render_step <- function(stage,
                        spec_file = "00_SIM_EL_Spec",
                        out_dir = "reports",
                        formats = c("docx", "html"),
                        min_n = NULL,
                        save_results = FALSE,
                        results_dir = "results",
                        results_file = NULL,
                        bundle = NULL,
                        check_disclosure = NULL,
                        open = interactive(),
                        quiet = TRUE,
                        qmd = NULL) {
  stopifnot(is.character(stage), length(stage) == 1L,
            is.null(bundle) || inherits(bundle, "el_data"))
  # A spec given as a file path must survive the move to the scratch folder.
  if (file.exists(spec_file)) {
    spec_file <- normalizePath(spec_file, winslash = "/", mustWork = TRUE)
  }

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' is required to render. install.packages(\"quarto\")",
         call. = FALSE)
  }
  src <- if (is.null(qmd)) stage_qmd_path(stage) else qmd
  if (!file.exists(src)) {
    stop("No document found for stage '", stage, "'. Available: ",
         paste(stage_ids(), collapse = ", "), call. = FALSE)
  }

  # The document load_all()s `pkg_path` when it has an R/ folder, which is
  # right for the package source and wrong for an analyst's own project that
  # happens to have one. Pass the working directory only when it IS the
  # stateELP source; otherwise the document uses the installed package.
  pkg <- if (.is_stateelp_source(getwd())) getwd() else ""
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
    scan_with <- .render_scan_bundle(bundle, results_file, spec_file)
    if (!is.null(scan_with$bundle)) {
      check_output_disclosure(written, scan_with$bundle, action = "error")
    } else {
      # A skipped scan is never silent: the documents may be about to leave
      # the machine, and nothing has checked them.
      warning("Disclosure scan NOT run on ", paste(basename(written),
              collapse = ", "), ": ", scan_with$why, " Pass the bundle the ",
              "results came from (`bundle = my_bundle`), or call ",
              "check_output_disclosure(files, my_bundle) before sharing.",
              call. = FALSE)
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
# own; a real delivery is assembled by the analyst, who passes it as `bundle`.
.render_bundle <- function(spec_file) {
  spec <- read_el_spec(spec_file)
  as_el_data(get("el_long_sim"), roster = get("el_roster_sim"),
             academic = get("academic_long_sim"), spec = spec)
}

# Which ids to scan against, or why there are none. In order: the caller's
# bundle; for a render from saved results, the simulation only when those
# results say they came from it (a state's results carry no simulation
# contract version, and scanning a state's report for simulated ids proves
# nothing); otherwise -- the document built its own results from the
# simulation -- the simulation.
.render_scan_bundle <- function(bundle, results_file, spec_file) {
  if (!is.null(bundle)) return(list(bundle = bundle, why = NA_character_))
  if (!is.null(results_file)) {
    res <- tryCatch(readRDS(results_file), error = function(e) NULL)
    v <- res$meta$sim_contract_version
    if (is.null(v) || all(is.na(v))) {
      return(list(bundle = NULL, why = paste0(
        "the results in '", basename(results_file), "' come from a state ",
        "delivery, and no `bundle` was passed to take its student ids from.")))
    }
  }
  b <- tryCatch(.render_bundle(spec_file), error = function(e) e)
  if (inherits(b, "error")) {
    return(list(bundle = NULL, why = paste0(
      "the simulated bundle could not be built under spec '",
      basename(spec_file), "' (", conditionMessage(b), ").")))
  }
  list(bundle = b, why = NA_character_)
}

# Whether a folder is the stateELP package source, which is the only folder
# the stage documents should load_all().
.is_stateelp_source <- function(dir) {
  desc <- file.path(dir, "DESCRIPTION")
  if (!file.exists(desc) || !dir.exists(file.path(dir, "R"))) return(FALSE)
  pkg <- tryCatch(read.dcf(desc, fields = "Package")[1, 1],
                  error = function(e) NA_character_)
  identical(unname(pkg), "stateELP")
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
