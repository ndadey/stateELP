# Dev-side entry point for the render loop. Not part of the package.
#
#   source("dev/render_step.R")
#   rs("check1")                       # docx + html into reports/
#   rs("check1", formats = "html")     # faster inner loop
#   rs("decision", save_results = TRUE)
#   rs_site()                          # refresh the README's example reports
#   rs_site_index()                    # rebuild the site page from the README
#   rs_guide()                         # re-render the walkthrough guide
#   rs_guide("check1")                 # one guide page
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

# Render the walkthrough guide: guide/<page>.qmd -> docs/guide/<page>.html.
# The pages execute their code against the package (load_all() of this
# directory), so run it with everything committed. Same copy-out dance as
# render_step(): Quarto cannot clean up inside Dropbox, so each page renders
# in tempdir() and only the finished HTML comes back. Every page is then
# scanned for the simulated data's student ids and the run stops on a hit.
rs_guide <- function(pages = NULL, quiet = TRUE) {
  devtools::load_all(".", quiet = TRUE)
  src <- if (is.null(pages)) {
    list.files("guide", pattern = "\\.qmd$", full.names = TRUE)
  } else {
    file.path("guide", paste0(pages, ".qmd"))
  }
  out <- file.path("docs", "guide")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  pkg <- normalizePath(".", winslash = "/")
  written <- character()
  for (f in src) {
    scratch <- file.path(tempdir(), "stateELP_guide")
    unlink(scratch, recursive = TRUE, force = TRUE)
    dir.create(scratch)
    copy <- file.path(scratch, basename(f))
    file.copy(f, copy)
    quarto::quarto_render(copy, output_format = "html",
                          execute_params = list(pkg_path = pkg), quiet = quiet)
    dest <- file.path(out, sub("\\.qmd$", ".html", basename(f)))
    file.copy(sub("\\.qmd$", ".html", copy), dest, overwrite = TRUE)
    written <- c(written, dest)
    message("Rendered ", dest)
  }
  roster <- get("el_roster_sim", envir = asNamespace("stateELP"))
  stateELP::check_output_disclosure(written, roster, action = "error")
  invisible(written)
}

# Render a briefing deck: decks/<deck>.qmd -> reports/<deck>.html (revealjs,
# one self-contained file). Same copy-out dance as rs_guide(); the theme and
# assets travel with the copy. `params_file` (YAML; keep a state's own outside
# the repository) and `params` override the deck's defaults, later winning.
# The output is scanned for student ids -- against the bundle in
# `bundle_file` when one is given, else against the simulation -- and the run
# stops on a hit.
#
#   rs_deck()                                    # the context deck on the sim
#   rs_deck(params = list(districts = list(list(id = "D03", label = "D03",
#                                               setting = "mid-size"))))
rs_deck <- function(deck = "context", params_file = NULL, params = list(),
                    out_dir = "reports", quiet = TRUE) {
  devtools::load_all(".", quiet = TRUE)
  src <- file.path("decks", paste0(deck, ".qmd"))
  stopifnot(file.exists(src))
  scratch <- file.path(tempdir(), "stateELP_deck")
  unlink(scratch, recursive = TRUE, force = TRUE)
  dir.create(file.path(scratch, "assets"), recursive = TRUE)
  file.copy(src, scratch)
  file.copy(file.path("decks", "deck.scss"), scratch)
  file.copy(list.files(file.path("decks", "assets"), full.names = TRUE),
            file.path(scratch, "assets"))

  p <- list(pkg_path = normalizePath(".", winslash = "/"))
  if (!is.null(params_file)) p <- utils::modifyList(p, yaml::read_yaml(params_file))
  p <- utils::modifyList(p, params)
  for (k in c("bundle_file", "spec_file")) {
    if (!is.null(p[[k]]) && nzchar(p[[k]]) && file.exists(p[[k]])) {
      p[[k]] <- normalizePath(p[[k]], winslash = "/")
    }
  }

  quarto::quarto_render(file.path(scratch, basename(src)), execute_params = p,
                        quiet = quiet)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(out_dir, paste0(deck, ".html"))
  file.copy(file.path(scratch, paste0(deck, ".html")), dest, overwrite = TRUE)

  ids_from <- if (!is.null(p$bundle_file) && nzchar(p$bundle_file)) {
    readRDS(p$bundle_file)
  } else {
    get("el_roster_sim", envir = asNamespace("stateELP"))
  }
  stateELP::check_output_disclosure(dest, ids_from, action = "error")
  message("Rendered ", dest)
  invisible(dest)
}

# Bootstrap note: renv's lockfile records the package's own dependencies, not
# the dev toolchain. On a fresh clone, after renv::restore(), run
#   renv::hydrate(c("devtools", "roxygen2"))
# or `renv::install(c("devtools", "roxygen2", "testthat"))` if the machine's
# user library is older than devtools needs.
