# Dev-side entry point for the render loop. Not part of the package.
#
#   source("dev/render_step.R")
#   rs("check1")                       # docx + html into reports/
#   rs("check1", formats = "html")     # faster inner loop
#   rs("decision", save_results = TRUE)
#   rs_site()                          # refresh the README's example reports
#   rs_site_index()                    # rebuild the site page from the README
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
# rendered-from commit and date), run rs_site_index(), and commit docs/ with
# the README.
rs_site <- function(formats = c("html", "docx"), ...) {
  rs_all(formats = formats, out_dir = "docs", ...)
  rs_site_index()
}

# The site's landing page is the README, generated rather than copied so the
# two cannot drift: edit README.md, never docs/index.md. Links are rewritten
# for where the page is served -- docs/ is the site root, everything else in
# the repository lives on GitHub -- and GitHub's alert syntax (> [!NOTE] and
# friends), which Pages would print literally, is dropped.
rs_site_index <- function(readme = "README.md", out = "docs/index.md",
                          repo = "https://github.com/ndadey/stateELP",
                          site = "https://ndadey.github.io/stateELP/") {
  x <- readLines(readme, encoding = "UTF-8", warn = FALSE)
  x <- x[!grepl("^>\\s*\\[!(NOTE|TIP|IMPORTANT|WARNING|CAUTION)\\]\\s*$", x)]
  # Repository files first, so the docs/ links below are not caught by it.
  x <- gsub("\\]\\((?!https?:|#|mailto:|docs/)([^)\\s]+)\\)",
            paste0("](", repo, "/blob/main/\\1)"), x, perl = TRUE)
  x <- gsub("](docs/", "](", x, fixed = TRUE)
  x <- gsub(paste0("](", site), "](", x, fixed = TRUE)

  h1 <- grep("^# ", x)[1]
  x <- append(x, c("", paste0("[View the source on GitHub](", repo, ")")),
              after = h1)
  x <- c("<!-- Generated from README.md by rs_site_index() in",
         "     dev/render_step.R. Edit README.md, not this file. -->", "", x)
  writeLines(x, out, useBytes = TRUE)
  invisible(out)
}

# Bootstrap note: renv's lockfile records the package's own dependencies, not
# the dev toolchain. On a fresh clone, after renv::restore(), run
#   renv::hydrate(c("devtools", "roxygen2"))
# or `renv::install(c("devtools", "roxygen2", "testthat"))` if the machine's
# user library is older than devtools needs.
