# The render loop. The stage documents do not exist yet -- each stage session
# writes its own -- so this builds a minimal document at test time and renders
# that, which keeps the loop under regression without shipping a placeholder.

test_that("stage ids and document lookup behave", {
  expect_length(stage_ids(), 8L)
  expect_identical(stage_ids()[1], "prep1")
  expect_identical(stage_ids()[8], "decision")
  # Ids carry no run-order numbers; those live on the prompt and instruction
  # files only.
  expect_false(any(grepl("[0-9]_", stage_ids())))
  # A stage with no document yet resolves to NA rather than erroring.
  expect_true(is.na(stage_qmd_path("nosuchstage")))
})

test_that("render_step refuses a stage that has no document", {
  expect_error(render_step("nosuchstage", open = FALSE), "No document found")
})

test_that("stage documents name the component, never say \"headline\"", {
  # "Headline" is the package's word for which component a state's analyses
  # run on. A reader has no use for it: the documents name the component
  # itself, through spec_headline_label(). This is the guard that stops the
  # word creeping back in when a later stage is written.
  dir <- testthat::test_path("..", "..", "inst", "templates")
  skip_if(!dir.exists(dir), "templates not reachable from the test dir")

  # Every stage document built so far is guarded. A document mid-conversion
  # may be parked here, and this list should only ever shrink.
  pending <- character()
  qmds <- setdiff(list.files(dir, pattern = "\\.qmd$"), pending)
  skip_if(length(qmds) == 0, "no converted stage documents yet")

  for (f in qmds) {
    txt <- readLines(file.path(dir, f), warn = FALSE)
    # The accessor names are code, not prose, and may appear freely.
    txt <- gsub("elp_headline\\(|spec_headline_label\\(|spec_headline\\(", "",
                txt)
    hits <- grep("headline", txt, ignore.case = TRUE, value = TRUE)
    expect_identical(hits, character(), info = f)
  }
})

test_that("render_step renders, collects output, and leaves no litter", {
  skip_on_cran()
  skip_if_not_installed("quarto")
  have_cli <- tryCatch(nzchar(quarto::quarto_path() %||% ""),
                       error = function(e) FALSE)
  skip_if(!isTRUE(have_cli), "Quarto CLI not available")

  qmd <- file.path(tempdir(), "zz_smoke.qmd")
  writeLines(c(
    "---",
    'title: "render_step regression"',
    "params:",
    '  pkg_path: "."',
    '  spec_file: "00_SIM_EL_Spec"',
    "format:",
    "  html: default",
    "---",
    "",
    "## Sample",
    "",
    "Not applicable.",
    "",
    "```{r}",
    "#| echo: false",
    "cat('spec_file param:', params$spec_file, '\\n')",
    "```"
  ), qmd)

  od <- file.path(tempdir(), "zz_out")
  unlink(od, recursive = TRUE, force = TRUE)
  out <- render_step("zz", out_dir = od, formats = "html",
                     open = FALSE, qmd = qmd)

  expect_length(out, 1L)
  expect_match(out, "\\.html$")
  expect_true(file.exists(out))

  # The copy is gone and no Quarto intermediates were left in the output.
  left <- list.files(od)
  expect_false(any(grepl("\\.qmd$", left)))
  expect_false(any(grepl("_files$", left)))
})

# ---- the disclosure scan's bundle, and where the document loads from --------
# (2026-11, state test run item 16: a state render skipped the scan with
# no message at the default quiet = TRUE.)

test_that("a render from a state's saved results is never scanned silently", {
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(meta = list(sim_contract_version = NA_character_)), f)
  got <- .render_scan_bundle(NULL, f, "00_SIM_EL_Spec")
  expect_null(got$bundle)
  expect_match(got$why, "state delivery")

  # Passing the bundle scans against it.
  b <- as_el_data(el_long_sim[1:10, ],
                  roster = el_roster_sim[el_roster_sim$STUDENT_ID %in%
                                           el_long_sim$STUDENT_ID[1:10], ],
                  spec = read_el_spec("00_SIM_EL_Spec"))
  expect_identical(.render_scan_bundle(b, f, "00_SIM_EL_Spec")$bundle, b)

  # A spec the simulation cannot be built under is a reason, not a crash.
  bad <- .render_scan_bundle(NULL, NULL, "no_such_spec")
  expect_null(bad$bundle)
  expect_match(bad$why, "could not be built")
})

test_that("documents load_all() only the stateELP source", {
  pkg_root <- test_path("..", "..")
  skip_if_not(file.exists(file.path(pkg_root, "DESCRIPTION")),
              "package source not reachable from the test dir")
  expect_true(.is_stateelp_source(pkg_root))
  other <- withr::local_tempdir()
  dir.create(file.path(other, "R"))
  writeLines("Package: myproject", file.path(other, "DESCRIPTION"))
  expect_false(.is_stateelp_source(other))
  expect_false(.is_stateelp_source(withr::local_tempdir()))
})
