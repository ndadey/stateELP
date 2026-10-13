#!/usr/bin/env bash
#
# Bootstrap an R + Quarto toolchain inside a Claude cloud sandbox, so that a
# review session can render a stage document itself instead of handing the
# .qmd back and waiting for a local render.
#
#   bash dev/bootstrap_sandbox.sh           # from the package root
#   QUARTO_VERSION=1.8.24 bash dev/bootstrap_sandbox.sh
#
# ---------------------------------------------------------------------------
# DO NOT RUN THIS ON YOUR OWN MACHINE.
#
# It apt-installs system-wide as root and writes a stand-in R package into the
# site library. It is for a disposable Linux container only. The guard below
# refuses to run anywhere that does not look like one, including WSL.
# ---------------------------------------------------------------------------
#
# Why this file exists
#
# The sandbox is rebuilt for every new conversation, so the toolchain has to be
# reinstalled each time. Most of the cost the first time was not installing --
# it was discovering which package sources the egress proxy allows. Those
# findings are recorded here so the next session does not rediscover them:
#
#   REACHABLE   archive.ubuntu.com / security.ubuntu.com  (apt, precompiled)
#               github.com release assets  ->  objects.githubusercontent.com
#               pypi.org, registry.npmjs.org
#
#   BLOCKED     cran.r-project.org                403 at the proxy
#   (403)       packagemanager.posit.co           403 -- no P3M binaries
#               *.r-universe.dev                  403
#               codeload.github.com               403 -- so remotes::install_
#                                                 github() cannot fetch source
#               api.github.com                    gated; cannot query "latest"
#                                                 release, hence the pinned
#                                                 QUARTO_VERSION below
#
# Consequence: every R package must come from Ubuntu's archive as an r-cran-*
# deb. That works here only because renv.lock is 17 packages deep and entirely
# CRAN-sourced -- no GitHub remotes. If a GitHub-only dependency is ever added
# (SGP, say), this script stops being sufficient and the gap should be made
# loud rather than worked around quietly.
#
# The one package with no deb is `quarto`, the thin R binding that
# render_step() calls. Step 3 installs a local stand-in for it. See the notes
# there before trusting a render that behaves oddly.

set -euo pipefail

QUARTO_VERSION="${QUARTO_VERSION:-1.8.24}"
SHIM_BUILD_DIR="${TMPDIR:-/tmp}/quarto-shim"

say() { printf '\n=== %s\n' "$*"; }

# ---- 0. Guard ---------------------------------------------------------------
# Three conditions together, because any one alone is too easy to satisfy by
# accident: Linux, root, and the sandbox's own upload mount.

if [ "$(uname -s)" != "Linux" ] || [ "$(id -u)" != "0" ] || [ ! -d /mnt/user-data ]; then
  cat >&2 <<'GUARD'
Refusing to run: this does not look like a Claude cloud sandbox.

This script installs system packages as root and is meant for a disposable
container. If you are on your own machine, you do not need it -- use renv:

    renv::restore()
    renv::hydrate(c("devtools", "roxygen2"))
GUARD
  exit 1
fi

# ---- 1. R and the CRAN dependency set --------------------------------------
# Ubuntu noble ships R 4.3.3. renv.lock pins 4.4.2, so this is deliberate
# drift -- see the report in step 4. The explicit list is the package's own
# Imports and Suggests plus what Quarto needs to execute an R chunk; the
# remaining lockfile entries (cli, glue, vctrs, pillar, ...) arrive as apt
# dependencies of these.

say "Installing R and CRAN packages from the Ubuntu archive"
export DEBIAN_FRONTEND=noninteractive

# The base image carries a download.docker.com source that the proxy refuses.
# It is not ours and nothing here needs it, so its 403 is filtered out rather
# than left to look like a failure of this script.
apt-get update -qq 2>&1 \
  | grep -v -e 'download\.docker\.com' -e 'Some index files failed' || true
apt-get install -y --no-install-recommends \
  r-base r-base-dev \
  r-cran-dplyr r-cran-rlang r-cran-tibble r-cran-withr \
  r-cran-devtools r-cran-roxygen2 r-cran-testthat \
  r-cran-knitr r-cran-rmarkdown r-cran-yaml r-cran-jsonlite \
  >/dev/null
R --version | head -1

# ---- 2. Quarto CLI ----------------------------------------------------------
# Release assets download fine; the API that would tell us the current version
# does not. QUARTO_VERSION is therefore pinned and bumped by hand -- check a
# candidate before editing it:
#
#   curl -sS -o /dev/null -w '%{http_code}\n' -L \
#     https://github.com/quarto-dev/quarto-cli/releases/download/vX.Y.Z/quarto-X.Y.Z-linux-amd64.tar.gz

if command -v quarto >/dev/null 2>&1 && [ "$(quarto --version)" = "$QUARTO_VERSION" ]; then
  say "Quarto $QUARTO_VERSION already installed"
else
  say "Installing Quarto $QUARTO_VERSION"
  tarball="${TMPDIR:-/tmp}/quarto-${QUARTO_VERSION}.tar.gz"
  curl -sSL --max-time 600 -o "$tarball" \
    "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"

  # A blocked host returns a short HTML or JSON body with a 200-ish shape, so
  # check that what arrived is actually a gzip archive before unpacking it.
  if ! gzip -t "$tarball" 2>/dev/null; then
    echo "Download is not a gzip archive -- the proxy probably refused it:" >&2
    head -c 300 "$tarball" >&2; echo >&2
    exit 1
  fi

  rm -rf /opt/quarto && mkdir -p /opt/quarto
  tar -xzf "$tarball" -C /opt/quarto --strip-components=1
  ln -sf /opt/quarto/bin/quarto /usr/local/bin/quarto
  rm -f "$tarball"
fi
quarto --version | sed 's/^/quarto /'

# ---- 3. The `quarto` R binding, as a local stand-in -------------------------
# render_step() calls quarto::quarto_render(), and the real package is
# unreachable from here (step 0's notes: CRAN, P3M, r-universe and codeload are
# all 403). What follows is a minimal namespace of the same name that shells
# out to the CLI with a compatible signature.
#
# This is a sandbox accommodation, not a dependency of the package, and it must
# never be committed into R/ or inst/. Two things to know when a render here
# disagrees with a render on your machine:
#
#   * execute_params is serialised with yaml::write_yaml and passed as
#     --execute-params; the real package does the same, but edge cases in how
#     R types map to YAML are the likeliest place for the two to diverge.
#   * only the arguments render_step() uses are implemented faithfully
#     (input, output_format, output_file, execute_params, execute_dir,
#     pandoc_args, quiet). as_job is accepted and ignored.

say "Installing the local quarto binding"
rm -rf "$SHIM_BUILD_DIR" && mkdir -p "$SHIM_BUILD_DIR/quarto/R"

cat > "$SHIM_BUILD_DIR/quarto/DESCRIPTION" <<'EOF'
Package: quarto
Type: Package
Title: Minimal Local Binding to the Quarto CLI
Version: 0.0.0.9000
Authors@R: person("stateELP", "sandbox", role = c("aut", "cre"),
    email = "noreply@example.com")
Description: A sandbox-only stand-in for the CRAN 'quarto' package, used where
    CRAN is unreachable. Implements quarto_render() by shelling out to the
    Quarto CLI. Not for distribution and not part of stateELP.
License: MIT + file LICENSE
Imports: yaml
Encoding: UTF-8
EOF

cat > "$SHIM_BUILD_DIR/quarto/LICENSE" <<'EOF'
YEAR: 2026
COPYRIGHT HOLDER: stateELP sandbox
EOF

cat > "$SHIM_BUILD_DIR/quarto/NAMESPACE" <<'EOF'
export(quarto_render)
export(quarto_path)
export(quarto_version)
EOF

cat > "$SHIM_BUILD_DIR/quarto/R/quarto_render.R" <<'EOF'
#' Path to the Quarto CLI
#' @export
quarto_path <- function() {
  p <- Sys.getenv("QUARTO_PATH", unset = NA_character_)
  if (!is.na(p) && nzchar(p)) return(p)
  p <- unname(Sys.which("quarto"))
  if (!nzchar(p)) stop("Quarto CLI not found on PATH.", call. = FALSE)
  p
}

#' Quarto CLI version
#' @export
quarto_version <- function() {
  out <- system2(quarto_path(), "--version", stdout = TRUE, stderr = TRUE)
  numeric_version(trimws(out[[1]]))
}

#' Render a Quarto document with the Quarto CLI
#'
#' Signature-compatible with quarto::quarto_render() for the arguments
#' stateELP::render_step() passes. `as_job` is accepted and ignored.
#' @export
quarto_render <- function(input = NULL,
                          output_format = NULL,
                          output_file = NULL,
                          execute_params = NULL,
                          execute_dir = NULL,
                          pandoc_args = NULL,
                          quiet = FALSE,
                          as_job = FALSE,
                          ...) {
  if (is.null(input)) stop("`input` is required.", call. = FALSE)
  input <- normalizePath(input, winslash = "/", mustWork = TRUE)

  args <- c("render", basename(input))
  if (!is.null(output_format)) {
    args <- c(args, "--to", paste(output_format, collapse = ","))
  }
  if (!is.null(output_file)) args <- c(args, "--output", output_file)

  if (!is.null(execute_params)) {
    pf <- tempfile(fileext = ".yml")
    yaml::write_yaml(execute_params, pf)
    on.exit(unlink(pf), add = TRUE)
    args <- c(args, "--execute-params", normalizePath(pf, winslash = "/"))
  }
  if (!is.null(execute_dir)) {
    args <- c(args, "--execute-dir", normalizePath(execute_dir, winslash = "/"))
  }
  if (!is.null(pandoc_args)) args <- c(args, pandoc_args)
  if (isTRUE(quiet)) args <- c(args, "--quiet")

  # render_step() already renders a copy in a scratch directory; this setwd is
  # what makes `--output` and the _files/ sidecar land beside that copy.
  owd <- setwd(dirname(input))
  on.exit(setwd(owd), add = TRUE)

  status <- system2(quarto_path(), args,
                    stdout = if (isTRUE(quiet)) FALSE else "",
                    stderr = if (isTRUE(quiet)) FALSE else "")
  if (!identical(as.integer(status), 0L)) {
    stop("Quarto render failed (exit status ", status, ") for '",
         basename(input), "'.", call. = FALSE)
  }
  invisible(status)
}
EOF

R CMD INSTALL "$SHIM_BUILD_DIR/quarto" >/dev/null 2>&1
rm -rf "$SHIM_BUILD_DIR"
Rscript -e 'cat("quarto binding:", as.character(packageVersion("quarto")),
                "| CLI", as.character(quarto::quarto_version()), "\n")'

# ---- 4. Version drift against renv.lock -------------------------------------
# The sandbox cannot reproduce the pinned library, so the drift is stated
# rather than left to be discovered in a rendered number. Anything that has to
# be reproducible belongs on a machine where renv::restore() has run.

if [ -f renv.lock ]; then
  say "Version drift vs renv.lock (expected -- sandbox is for iteration, not reproduction)"
  Rscript -e '
    lock <- jsonlite::fromJSON("renv.lock", simplifyVector = FALSE)
    rows <- list()
    rows[[1]] <- c("R", lock$R$Version, paste0(R.version$major, ".", R.version$minor))
    for (nm in names(lock$Packages)) {
      want <- lock$Packages[[nm]]$Version
      have <- tryCatch(as.character(packageVersion(nm)), error = function(e) "-- absent --")
      if (!identical(want, have)) rows[[length(rows) + 1L]] <- c(nm, want, have)
    }
    if (length(rows) == 0L) {
      cat("  none\n")
    } else {
      df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      names(df) <- c("package", "locked", "sandbox")
      print(df, row.names = FALSE)
      cat("\n  renv itself is absent on purpose: the sandbox uses the system",
          "\n  library, so activating the project library would empty it.\n")
    }
  '
fi

# ---- 5. Smoke test ----------------------------------------------------------
# Renders a throwaway document through render_step()'s own code path -- the
# copy-to-scratch, execute-params and both output formats -- rather than
# calling the CLI directly, so a green result means the real loop works.

if [ -f DESCRIPTION ] && grep -q '^Package: stateELP' DESCRIPTION; then
  say "Smoke test: render_step() end to end"
  probe="dev/_bootstrap_probe.qmd"
  cat > "$probe" <<'EOF'
---
title: "Bootstrap probe"
params:
  pkg_path: "."
  spec_file: "00_SIM_EL_Spec"
format:
  docx: default
  html:
    embed-resources: true
---

```{r}
#| echo: false
#| message: false
suppressMessages(devtools::load_all(params$pkg_path, quiet = TRUE))
spec <- read_el_spec(params$spec_file)
d <- as_el_data(el_long_sim, roster = el_roster_sim,
                academic = academic_long_sim, spec = spec)
```

Spec `r params$spec_file`: headline `r spec_headline(spec)`,
min cell `r spec_min_cell(spec)`, `r nrow(elp_headline(d))` headline rows.

```{r}
#| echo: false
knitr::kable(utils::head(el_coverage(d), 3))
```
EOF

  trap 'rm -f "$probe" reports/_bootstrap_probe.docx reports/_bootstrap_probe.html' EXIT

  Rscript -e '
    suppressMessages(devtools::load_all(".", quiet = TRUE))
    out <- stateELP::render_step("check1", qmd = "dev/_bootstrap_probe.qmd",
                                 formats = c("docx", "html"),
                                 open = FALSE, quiet = TRUE)
    stopifnot(length(out) == 2L, all(file.exists(out)))
    cat("  rendered:", paste(basename(out), collapse = ", "), "\n")
  '
  say "Toolchain ready"
else
  say "Not in the stateELP package root -- skipped the smoke test"
fi
