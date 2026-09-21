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

  # Prep 2's document is converted when its own session's work lands; this
  # list should only ever shrink.
  pending <- "prep2_population.qmd"
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
