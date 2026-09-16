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
  list(obj = "el_long_sim",        heading = "### 3.5 Column reference"),
  list(obj = "elp_components_sim", heading = "### 4.2 Column reference"),
  list(obj = "academic_long_sim",  heading = "### 5.1 Columns and coverage")
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

test_that("the objects have the grain and size the contract states", {
  expect_identical(dim(stateELP::el_long_sim), c(461040L, 54L))
  expect_identical(dim(stateELP::elp_components_sim), c(1824716L, 6L))
  expect_identical(dim(stateELP::academic_long_sim), c(568202L, 7L))

  # Grain: the spine is unique on student x year, and is the only object that
  # may be used to count students.
  sp <- stateELP::el_long_sim
  expect_identical(anyDuplicated(sp[c("student_id", "school_year")]), 0L)
})

test_that("each object carries its contract version", {
  for (cs in cases) {
    obj <- get(cs$obj, envir = asNamespace("stateELP"))
    expect_identical(attr(obj, "sim_contract_version"), "0.2.0")
  }
})

test_that("the components key and the academic key are never the same word", {
  # WIDA's `reading` domain and an academic Reading subject conflate in a join
  # if both key columns are called the same thing.
  expect_true("component" %in% names(stateELP::elp_components_sim))
  expect_true("subject" %in% names(stateELP::academic_long_sim))
  expect_false("subject" %in% names(stateELP::elp_components_sim))
  expect_false("component" %in% names(stateELP::academic_long_sim))
  expect_type(stateELP::elp_components_sim$component, "character")
})

test_that("the package's exit year is the sim's flagged year plus one", {
  # The off-by-one sim_contract 3.3 fixes: the sim flags exit on the last
  # CURRENT row; the package dates it to the first FORMER year. Asserted so
  # the difference is visible rather than silent.
  sp <- stateELP::el_long_sim
  flagged <- sp[sp$exit_observed == 1L, c("student_id", "school_year")]
  names(flagged)[2] <- "flagged_year"

  spans <- build_student_spans(sp[sp$el_status %in% c("current", "former"), ])
  both <- merge(flagged, spans[c("student_id", "exit_year")], by = "student_id")
  both <- both[!is.na(both$exit_year), ]

  expect_gt(nrow(both), 0L)
  expect_identical(both$exit_year, both$flagged_year + 1L)
})

test_that("formers and never-ELs carry no ELP, by design", {
  sp <- stateELP::el_long_sim
  non_current <- sp$el_status != "current"
  expect_true(all(is.na(sp$elp_level[non_current])))
  expect_true(all(is.na(sp$elp_scale[non_current])))

  # ... and no component row belongs to a non-current student-year.
  cur <- unique(sp[sp$el_status == "current", c("student_id", "school_year")])
  comp <- unique(stateELP::elp_components_sim[c("student_id", "school_year")])
  expect_identical(nrow(dplyr::anti_join(comp, cur,
                                         by = c("student_id", "school_year"))),
                   0L)
})

test_that("alternate-form students carry no headline level", {
  sp <- stateELP::el_long_sim
  alt <- !is.na(sp$elp_form) & sp$elp_form == "alternate"
  expect_gt(sum(alt), 0L)
  expect_true(all(is.na(sp$elp_level[alt])))
})

test_that("the academic table is missing 2020 entirely, not empty for it", {
  yrs <- sort(unique(stateELP::academic_long_sim$school_year))
  expect_false(2020L %in% yrs)
  expect_true(all(c(2019L, 2021L) %in% yrs))
  expect_setequal(unique(stateELP::academic_long_sim$grade),
                  as.character(3:11))
})

test_that("weighting a statewide share changes it materially", {
  # The failure this prevents: an EL share reported unweighted reads 52.7%.
  sp <- stateELP::el_long_sim
  y <- sp[sp$school_year == 2023L, ]
  unw <- mean(y$el_status == "current")
  wtd <- weighted_share(y$el_status == "current", y$sample_weight)
  expect_equal(round(100 * unw, 1), 52.7)
  expect_equal(round(100 * wtd, 1), 17.8)
})
