# Disclosure control at the render boundary (R/disclosure.R).
#
# The rule: student-level data may live on disk in the saved results objects,
# but must never reach a .docx, .html or .xlsx. These tests have to prove the
# check CATCHES a leak, not merely that it runs on clean files -- a scanner
# that always passes is worse than none, because it is trusted.

make_disclosure_bundle <- function() {
  spec <- read_el_spec("00_SIM_EL_Spec")
  as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
}

test_that("an identifier in an HTML file is found", {
  b <- make_disclosure_bundle()
  id <- el_roster(b)$STUDENT_ID[[1]]

  f <- tempfile(fileext = ".html")
  writeLines(c("<html><body><table><tr><td>", id,
               "</td><td>4.5</td></tr></table></body></html>"), f)

  expect_identical(scan_output_for_ids(f, id), id)
  expect_error(check_output_disclosure(f, b), "reached rendered output")
})

test_that("a clean file passes", {
  b <- make_disclosure_bundle()
  f <- tempfile(fileext = ".html")
  writeLines("<p>2,373 students in the K-2015 cohort</p>", f)

  expect_identical(scan_output_for_ids(f, el_roster(b)$STUDENT_ID[1:50]),
                   character())
  expect_identical(nrow(check_output_disclosure(f, b)), 0L)
})

test_that("identifiers buried in a .docx are found, not just loose text", {
  # A Word file is a zip of XML parts and its text lives inside them, so
  # reading the container as text would see nothing. This is the case the
  # scanner exists for: data that is present but not visible.
  skip_if_not_installed("flextable")
  b <- make_disclosure_bundle()
  ids <- el_roster(b)$STUDENT_ID[1:3]

  f <- tempfile(fileext = ".docx")
  ft <- flextable::flextable(data.frame(STUDENT_ID = ids, EXIT = c(1, 0, 1)))
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, ft)
  print(doc, target = f)

  # The bytes on disk must NOT contain the id in the clear -- the parts are
  # deflated. Otherwise this test is not exercising the unzip path at all.
  # Read as raw, not as text: a .docx is binary and readLines() would both
  # warn and mangle it.
  bytes <- readBin(f, "raw", n = file.size(f))
  needle <- charToRaw(ids[[1]])
  found_raw <- length(needle) > 0 &&
    length(grepRaw(needle, bytes, fixed = TRUE, all = FALSE)) > 0
  expect_false(found_raw)

  # Through the scanner, they are found.
  expect_setequal(scan_output_for_ids(f, ids), ids)
})

test_that("identifiers buried in an .xlsx are found", {
  skip_if_not_installed("openxlsx")
  b <- make_disclosure_bundle()
  ids <- el_roster(b)$STUDENT_ID[1:3]

  f <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(data.frame(STUDENT_ID = ids, FLAG = c(1, 0, 1)), f)

  expect_setequal(scan_output_for_ids(f, ids), ids)
  expect_error(check_output_disclosure(f, b), "reached rendered output")
})

test_that("matching is on a word boundary, not a substring", {
  # A student id that happens to be a substring of an unrelated number must
  # not register, or the check cries wolf and gets switched off.
  f <- tempfile(fileext = ".html")
  writeLines("<p>scale score 1234567 and n = 2373</p>", f)

  expect_identical(scan_output_for_ids(f, "23456"), character())
  expect_identical(scan_output_for_ids(f, "2373"), "2373")
})

test_that("an identifier containing regex metacharacters is matched literally", {
  # Ids are data, not patterns. A delivery is free to use "S.1" or "A(2)".
  f <- tempfile(fileext = ".html")
  writeLines("<p>S.1 appears here</p>", f)

  expect_identical(scan_output_for_ids(f, "S.1"), "S.1")
  # The dot is escaped, so it does not match an arbitrary character.
  expect_identical(scan_output_for_ids(f, "SX1"), character())
})

test_that("a scan tests every id, so a small leak cannot slip through", {
  # The bug this replaced: the check sampled 200 of 88,279 ids, which found a
  # five-row leak about one time in ninety. Searching output-first makes
  # testing all of them cheaper than sampling was.
  b <- make_disclosure_bundle()
  all_ids <- unique(as.character(el_roster(b)$STUDENT_ID))
  expect_gt(length(all_ids), 10000)

  # One row, from deep in the id space -- the hardest case for a sample.
  one <- all_ids[[length(all_ids) - 7L]]
  f <- tempfile(fileext = ".html")
  writeLines(paste0("<table><tr><td>", one, "</td></tr></table>"), f)

  expect_identical(scan_output_for_ids(f, all_ids), one)
  expect_error(check_output_disclosure(f, b), "reached rendered output")
})

test_that("repeated scans of one file agree", {
  b <- make_disclosure_bundle()
  f <- tempfile(fileext = ".html")
  writeLines("<p>clean</p>", f)

  a <- check_output_disclosure(f, b, action = "report")
  z <- check_output_disclosure(f, b, action = "report")
  expect_identical(a, z)
})

test_that("action controls whether a hit stops, warns or reports", {
  b <- make_disclosure_bundle()
  id <- el_roster(b)$STUDENT_ID[[1]]
  f <- tempfile(fileext = ".html")
  writeLines(paste("<td>", id, "</td>"), f)

  expect_error(check_output_disclosure(f, b, action = "error"))
  expect_warning(check_output_disclosure(f, b, action = "warn"))
  hits <- suppressWarnings(
    check_output_disclosure(f, b, action = "report"))
  expect_identical(hits$ID, id)
  expect_identical(hits$FILE, basename(f))
})

test_that("the message points at the cause rather than at itself", {
  # A guardrail that only says "failed" invites being suppressed. This one
  # has to name the places rows get in.
  b <- make_disclosure_bundle()
  id <- el_roster(b)$STUDENT_ID[[1]]
  f <- tempfile(fileext = ".html")
  writeLines(paste("<td>", id, "</td>"), f)

  msg <- tryCatch(check_output_disclosure(f, b),
                  error = conditionMessage)
  expect_match(msg, "may be saved to disk but must never reach")
  expect_match(msg, "htmlwidget")
  expect_match(msg, "echo: true")
  expect_match(msg, "rather than suppressing")
})

test_that("Check 2's student-level slots are the reason this exists", {
  # If these stop being student-level the rule can relax; a test records
  # which slots it is protecting so that is a decision rather than a drift.
  spec <- read_el_spec("00_SIM_EL_Spec")
  b <- as_el_data(el_long_sim, roster = el_roster_sim,
                  academic = academic_long_sim, spec = spec)
  res <- run_check2(b, spec)

  for (slot in c("flags", "profile")) {
    expect_true("STUDENT_ID" %in% names(res[[slot]]), info = slot)
  }
  # ... and the rendered crosstab built from them is not.
  expect_false("STUDENT_ID" %in% names(res$crosstab))
})

test_that("no stage document renders a student-level slot", {
  # The documents are clean today because no stage has had reason to render
  # those slots. This is what keeps it that way.
  qmds <- list.files(test_path("..", "..", "inst", "templates"),
                     pattern = "\\.qmd$", full.names = TRUE)
  skip_if(length(qmds) == 0, "templates not reachable from the test dir")

  code <- unlist(lapply(qmds, readLines, warn = FALSE))
  expect_identical(grep("res\\$(flags|profile)\\b", code, value = TRUE),
                   character())
})
