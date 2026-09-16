# The shipped data against its two descriptions.
#
# sim_contract.md 3.5 / 4.2 / 5.1 is the codebook and R/data.R mirrors it.
# Three descriptions of the same columns drift unless something checks them, so
# this file compares all three: contract tables, roxygen \item names, and the
# objects themselves.

# Both source files are .Rbuildignore'd, so they are absent under R CMD check
# on an installed package. Skip rather than fail there; the check that matters
# runs in development and in CI from the source tree.
repo_file <- function(...) {
  p <- testthat::test_path("..", "..", ...)
  if (file.exists(p)) p else NA_character_
}

# Column names out of a markdown table: every backticked token in the first
# cell of each row, so that a row naming five race columns at once still
# yields five names.
contract_columns <- function(path, heading) {
  lines <- readLines(path, warn = FALSE)
  start <- grep(heading, lines, fixed = TRUE)[1]
  if (is.na(start)) return(character())
  rest <- lines[(start + 1):length(lines)]
  nxt <- grep("^#{2,3} ", rest)
  if (length(nxt)) rest <- rest[seq_len(nxt[1] - 1L)]

  rows <- grep("^\\|\\s*`", rest, value = TRUE)
  first_cell <- sub("^\\|([^|]*)\\|.*$", "\\1", rows)
  toks <- unlist(regmatches(first_cell, gregexpr("`[^`]+`", first_cell)))
  toks <- gsub("`", "", toks)
  # A cell naming five race columns at once yields the ", " between them as a
  # match too; keep only things shaped like column names.
  toks[grepl("^[A-Za-z_][A-Za-z0-9_]*$", toks)]
}

# \item{name}{...} entries from a roxygen block, scoped to one dataset.
doc_columns <- function(path, object) {
  lines <- readLines(path, warn = FALSE)
  ends <- grep('^"', lines)
  obj_line <- grep(paste0('^"', object, '"$'), lines)
  if (!length(obj_line)) return(character())
  prev_end <- ends[ends < obj_line]
  start <- if (length(prev_end)) max(prev_end) + 1L else 1L
  block <- lines[start:obj_line]
  m <- regmatches(block, regexpr("\\\\item\\{[^}]+\\}", block))
  gsub("\\\\item\\{|\\}", "", m)
}

cases <- list(
  list(obj = "el_roster_sim",     heading = "### 3.5 Column reference"),
  list(obj = "el_long_sim",       heading = "### 4.2 Column reference"),
  list(obj = "academic_long_sim", heading = "### 5.1 Columns and coverage")
)

test_that("contract, R/data.R and the shipped objects name the same columns", {
  contract <- repo_file("sim_contract.md")
  datafile <- repo_file("R", "data.R")
  skip_if(is.na(contract) || is.na(datafile),
          "source tree not reachable (installed-package check)")

  for (cs in cases) {
    obj <- get(cs$obj, envir = asNamespace("stateELP"))
    from_contract <- contract_columns(contract, cs$heading)
    from_docs <- doc_columns(datafile, cs$obj)

    expect_setequal(from_contract, names(obj))
    expect_setequal(from_docs, names(obj))
  }
})

test_that("every shipped column name is ALL CAPS", {
  for (cs in cases) {
    nm <- names(get(cs$obj, envir = asNamespace("stateELP")))
    expect_identical(nm, toupper(nm), info = cs$obj)
  }
})

test_that("the objects have the grain and size the contract states", {
  expect_identical(dim(stateELP::el_roster_sim), c(461040L, 52L))
  expect_identical(dim(stateELP::el_long_sim), c(1824716L, 6L))
  expect_identical(dim(stateELP::academic_long_sim), c(568202L, 7L))

  key <- c("STUDENT_ID", "YEAR")
  expect_identical(anyDuplicated(stateELP::el_roster_sim[key]), 0L)
  expect_identical(
    anyDuplicated(stateELP::el_long_sim[c(key, "COMPONENT")]), 0L)
  expect_identical(
    anyDuplicated(stateELP::academic_long_sim[c(key, "SUBJECT")]), 0L)
})

test_that("each object carries its contract version", {
  for (cs in cases) {
    obj <- get(cs$obj, envir = asNamespace("stateELP"))
    expect_identical(attr(obj, "sim_contract_version"), "0.4.0")
  }
})

test_that("the retired components object no longer ships", {
  # 0.3.0 made the component-long table the base and named it el_long_sim.
  expect_false(exists("elp_components_sim", envir = asNamespace("stateELP"),
                      inherits = FALSE))
})

test_that("the roster carries no measurement columns", {
  # The base table is the single source of ELP measurement; the roster caches
  # nothing (contract 4.3, generator guardrail 5).
  nm <- names(stateELP::el_roster_sim)
  expect_false(any(c("SCALE_SCORE", "ACHIEVEMENT_LEVEL", "COMPONENT",
                     "ELP_LEVEL", "ELP_SCALE", "ELP_BAND") %in% nm))
})

test_that("the base key and the academic key are never the same word", {
  # WIDA's `reading` domain and an academic Reading subject conflate in a join
  # if both key columns are called the same thing.
  expect_true("COMPONENT" %in% names(stateELP::el_long_sim))
  expect_true("SUBJECT" %in% names(stateELP::academic_long_sim))
  expect_false("SUBJECT" %in% names(stateELP::el_long_sim))
  expect_false("COMPONENT" %in% names(stateELP::academic_long_sim))
  expect_type(stateELP::el_long_sim$COMPONENT, "character")
  expect_true(all(stateELP::el_long_sim$COMPONENT %in% el_component_vocabulary()))
})

test_that("the package's exit year is the sim's flagged year plus one", {
  # The off-by-one sim_contract 3.3 fixes: the sim flags exit on the last
  # CURRENT row; the package dates it to the first FORMER year. Asserted so
  # the difference is visible rather than silent.
  ro <- stateELP::el_roster_sim
  flagged <- ro[ro$EXIT_OBSERVED == 1L, c("STUDENT_ID", "YEAR")]
  names(flagged)[2] <- "FLAGGED_YEAR"

  spans <- build_student_spans(ro)
  both <- merge(flagged, spans[c("STUDENT_ID", "EXIT_YEAR")], by = "STUDENT_ID")

  observed <- both[!is.na(both$EXIT_YEAR), ]
  expect_gt(nrow(observed), 10000L)
  expect_identical(observed$EXIT_YEAR, observed$FLAGGED_YEAR + 1L)

  # ... and every package exit has its flag: none is invented. (A flag with no
  # following former row -- a grade-12 or final-panel-year exit -- is
  # unobservable under first-former dating and stays unexited.)
  pkg_exits <- spans$STUDENT_ID[spans$EXITED]
  expect_true(all(pkg_exits %in% both$STUDENT_ID))
})

test_that("the package's recomputed YEAR_IN_STATUS and cohorts match the sim's", {
  # The sim's D-tier columns are validation inputs: on students whose entry
  # was observed, the package's own derivations must reproduce them.
  ro <- stateELP::el_roster_sim
  spec <- read_el_spec("00_SIM_EL_Spec")

  pkg <- add_year_in_status(ro)             # replaces the sim's column
  chk <- dplyr::inner_join(
    pkg[c("STUDENT_ID", "YEAR", "YEAR_IN_STATUS")],
    ro[ro$EL_STATUS == "current" & !ro$LEFT_TRUNCATED,
       c("STUDENT_ID", "YEAR", "YEAR_IN_STATUS")],
    by = c("STUDENT_ID", "YEAR"), suffix = c("_PKG", "_SIM"))
  expect_gt(nrow(chk), 100000L)
  expect_identical(chk$YEAR_IN_STATUS_PKG, chk$YEAR_IN_STATUS_SIM)

  co <- build_cohort(ro, spec, start_grade = "K", entry_years = 2015:2025)
  sim_lab <- unique(ro[!is.na(ro$COHORT_LABEL) & ro$ENTRY_GRADE %in% "K",
                       c("STUDENT_ID", "COHORT_LABEL")])
  expect_setequal(co$STUDENT_ID, sim_lab$STUDENT_ID)
  m <- dplyr::inner_join(co, sim_lab, by = "STUDENT_ID",
                         suffix = c("_PKG", "_SIM"))
  expect_identical(m$COHORT_LABEL_PKG, m$COHORT_LABEL_SIM)

  # Five followable K cohorts, derived from the panel, never named.
  expect_identical(sort(unique(build_cohort(ro, spec)$COHORT_LABEL)),
                   paste0("K-", 2015:2019))
})

test_that("formers and never-ELs have no base-table rows, by design", {
  ro <- stateELP::el_roster_sim
  cur <- unique(ro[ro$EL_STATUS == "current", c("STUDENT_ID", "YEAR")])
  base <- unique(stateELP::el_long_sim[c("STUDENT_ID", "YEAR")])
  expect_identical(nrow(dplyr::anti_join(base, cur,
                                         by = c("STUDENT_ID", "YEAR"))), 0L)
})

test_that("alternate-form rows carry a level and no scale score", {
  b <- stateELP::el_long_sim
  alt <- b$ELP_FORM == "alternate"
  expect_identical(sum(alt), 6595L)
  expect_true(all(is.na(b$SCALE_SCORE[alt])))
  expect_false(anyNA(b$ACHIEVEMENT_LEVEL[alt]))
})

test_that("the academic table is missing 2020 entirely, not empty for it", {
  yrs <- sort(unique(stateELP::academic_long_sim$YEAR))
  expect_false(2020L %in% yrs)
  expect_true(all(c(2019L, 2021L) %in% yrs))
  expect_setequal(unique(stateELP::academic_long_sim$GRADE),
                  as.character(3:11))
  expect_type(stateELP::academic_long_sim$ACHIEVEMENT_LEVEL, "integer")
})

test_that("weighting a statewide share changes it materially", {
  # The failure this prevents: an EL share reported unweighted reads 52.7%.
  ro <- stateELP::el_roster_sim
  y <- ro[ro$YEAR == 2023L, ]
  unw <- mean(y$EL_STATUS == "current")
  wtd <- weighted_share(y$EL_STATUS == "current", y$SAMPLE_WEIGHT)
  expect_equal(round(100 * unw, 1), 52.7)
  expect_equal(round(100 * wtd, 1), 17.8)
})
