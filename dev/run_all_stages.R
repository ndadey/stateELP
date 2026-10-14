# Run every built stage once and keep each stage's results list.
#
# A copy-and-edit template. Copy this file, edit section 1 to point at your
# own spec and delivery, and run it from the package root -- or from anywhere,
# with stateELP installed:
#
#   source("run_all_stages.R")
#
# It builds the data bundle once, runs every stage that has a run_<stage>()
# function into one named list, `results`, saves each stage's list as
# results/<stage>_results.rds, and leaves `slot_report`: every slot of every
# stage, whether it was filled, and why not when it was not.
#
# `results` always carries all eight stage ids in run order. A stage with no
# run function yet (Prep 1, Prep 3 and Decision today) stays a NULL
# placeholder, and is picked up here with no edit once its function exists.
#
# The saved files are student-level in places -- Check 2's $flags and
# $profile are one row per student -- so results/ is gitignored and
# .Rbuildignored. Keep it apart from rendered documents and never send it
# with them (CLAUDE.md 9). A saved file can feed a stage document:
#   render_step("check1", results_file = "results/check1_results.rds")
#
# Do not edit this copy in place: the walkthrough guide (guide/index.qmd) runs
# it as written on the simulated data.

if (!isNamespaceLoaded("stateELP")) {
  if (file.exists("DESCRIPTION") && dir.exists("R")) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(stateELP)
  }
}

# ---- 1. Your spec and your delivery (edit these) ----------------------------

spec <- read_el_spec("00_SIM_EL_Spec")   # or read_el_spec("path/to/MY_Spec.R")

bundle <- as_el_data(el_long_sim,          # the base table
                     roster   = el_roster_sim,
                     academic = academic_long_sim,
                     spec     = spec)

results_dir <- "results"                   # never the folder of reports

# Arguments for individual stages. A stage left out runs on its defaults:
# Checks 1-2 take start_grade; Checks 3-4 take subject, grade and year.
stage_args <- list(
  check1 = list(start_grade = "K"),
  check2 = list(start_grade = "K")
)

# ---- 2. Run every stage that has a run function ------------------------------

ns <- asNamespace("stateELP")
results <- stats::setNames(vector("list", length(stage_ids())), stage_ids())

for (s in stage_ids()) {
  fn <- paste0("run_", s)
  if (!exists(fn, envir = ns, inherits = FALSE)) {
    message(s, ": not built yet (no ", fn, "()), left NULL")
    next
  }
  run <- get(fn, envir = ns)
  t0 <- Sys.time()
  res <- switch(s,
    prep1 = , prep3 = run(spec),           # these take the spec alone
    decision = run(results[setdiff(names(results), "decision")], spec),
    do.call(run, c(list(bundle, spec), stage_args[[s]])))
  results[s] <- list(res)                  # `[<-` keeps the name if res is NULL
  message(s, ": done in ", format(round(Sys.time() - t0, 1)))
}

# ---- 3. Save each stage's results --------------------------------------------

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
for (s in names(results)) {
  if (is.null(results[[s]])) next
  saveRDS(results[[s]], file.path(results_dir, paste0(s, "_results.rds")))
}

# ---- 4. Slot report: what was filled, and why the rest was not ---------------

# Slots that only fill when the spec declares one of these exit-rule Types.
type_gates <- list(criteria_crosswalk    = 2:6,
                   blocked_groups        = 2:6,
                   contested_zone        = 2:6,
                   type3_covariate       = 3,
                   contested_zone_models = c(2, 4, 5, 6),
                   policy_as_whole       = 2:6)

# Slots empty whatever the delivery, until the package supports them.
not_supported <- c(dependence = "No package support yet (the copula analysis)")

types_label <- function(x) {
  x <- sort(unique(x))
  if (length(x) > 2L && all(diff(x) == 1L)) {
    return(paste0(x[1], "-", x[length(x)]))
  }
  paste(x, collapse = ", ")
}

why_null <- function(slot, res) {
  if (slot %in% res$meta$pending) return("Later pass: not built yet")
  if (slot %in% names(not_supported)) return(not_supported[[slot]])
  gate <- type_gates[[slot]]
  if (!is.null(gate) && !spec_has_type(spec, gate)) {
    return(paste0("Needs exit-rule Type ", types_label(gate),
                  "; this spec declares Type ",
                  types_label(spec_exit_types(spec))))
  }
  # The stage's own note, where it keeps one under a guessable name.
  stem <- sub("_.*$", "", slot)
  for (key in unique(c(paste0(slot, "_note"), paste0(stem, "_note")))) {
    note <- res$meta[[key]]
    usable <- is.character(note) && length(note) == 1L &&
      !is.na(note) && nzchar(note)
    if (usable) return(gsub("\\s+", " ", note))
  }
  "No input for it in this delivery; the stage document says why"
}

describe <- function(x) {
  if (is.data.frame(x)) {
    return(sprintf("%d %s x %d columns", nrow(x),
                   if (nrow(x) == 1L) "row" else "rows", ncol(x)))
  }
  if (is.list(x)) return(sprintf("list of %d", length(x)))
  sprintf("%s, length %d", class(x)[1], length(x))
}

slot_report <- do.call(rbind, lapply(names(results), function(s) {
  res <- results[[s]]
  if (is.null(res)) {
    return(data.frame(STAGE = s, SLOT = NA_character_, FILLED = FALSE,
                      DETAIL = "Stage not built yet: no run function"))
  }
  slots <- setdiff(names(res), "meta")
  filled <- !vapply(res[slots], is.null, logical(1))
  data.frame(STAGE = s, SLOT = slots, FILLED = filled,
             DETAIL = vapply(slots, function(x) {
               if (is.null(res[[x]])) why_null(x, res) else describe(res[[x]])
             }, character(1)))
}))
rownames(slot_report) <- NULL

message(sum(slot_report$FILLED), " of ", nrow(slot_report),
        " slots filled; results saved to ", results_dir,
        "/. See `slot_report`.")
slot_report
