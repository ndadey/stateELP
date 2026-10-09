# Dev-side entry point for the render loop. Not part of the package.
#
#   source("dev/render_step.R")
#   rs("check1")                       # docx + html into reports/
#   rs("check1", formats = "html")     # faster inner loop
#   rs("decision", save_results = TRUE)
#   rs_site()                          # refresh the README's example reports
#
# The package function is stateELP::render_step(); this wrapper only does the
# load_all() so an edit to R/ is picked up without reinstalling.

rs <- function(stage, ...) {
  devtools::load_all(".", quiet = TRUE)
  stateELP::render_step(stage, ...)
}

# Render every stage that has a document yet -- the check before a commit that
# the shell still renders end to end.
rs_all <- function(formats = c("docx", "html"), ...) {
  devtools::load_all(".", quiet = TRUE)
  ids <- stateELP::stage_ids()
  have <- ids[!is.na(vapply(ids, stateELP::stage_qmd_path, ""))]
  if (!length(have)) {
    message("No stage documents exist yet.")
    return(invisible(character()))
  }
  for (s in have) stateELP::render_step(s, formats = formats, open = FALSE, ...)
  invisible(have)
}

# Refresh the example reports the README links to. docs/ is the folder GitHub
# Pages serves; it is .Rbuildignored, so it never ships in the package. Run it
# with everything committed, so each report matches the code on GitHub. Then
# update the README's "Example reports" table (any newly built stage, and the
# rendered-from commit and date) and commit docs/ with it.
rs_site <- function(formats = c("html", "docx"), ...) {
  rs_all(formats = formats, out_dir = "docs", ...)
}

# Bootstrap note: renv's lockfile records the package's own dependencies, not
# the dev toolchain. On a fresh clone, after renv::restore(), run
#   renv::hydrate(c("devtools", "roxygen2"))
# or `renv::install(c("devtools", "roxygen2", "testthat"))` if the machine's
# user library is older than devtools needs.
