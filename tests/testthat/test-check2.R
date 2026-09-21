# Check 2 -- On-time vs prolonged exiters. run_check2(data, spec, start_grade).
# Sample: the same cohort as Check 1.
# Built per internal_docs/steps/6_check2_flows.md.
#
# The nested indicators and the on-time/prolonged split are already tested in
# test-utils.R; what is tested here is the crosstab built on them, and that
# Check 2 follows Check 1's cohort rather than selecting its own.

check2_spec <- function() make_check2_spec()

# The cohort the fixture builds, by its label. The label is derived from the
# panel (the cohort enters one year after it opens), never typed as a year.
check2_fixture_flags <- function(...) {
  bundle <- make_check2_bundle(make_check2_roster(...))
  co <- compute_cohorts(bundle, check2_spec())
  list(bundle = bundle,
       flags = compute_exit_flags(bundle, check2_spec(),
                                  cohort = co$COHORT_LABEL[co$SELECTED]))
}

# run_check2() on the full sim, once for the file.
check2_sim <- local({
  r <- NULL
  function() {
    if (is.null(r)) r <<- run_check2(make_check2_sim_bundle(), check2_spec())
    r
  }
})

# ---- the exit indicators for one cohort --------------------------------------

test_that("the flags reproduce the stated exit outcomes of the fixture", {
  f <- check2_fixture_flags()$flags

  # 12 students in each of five groups, by construction.
  expect_identical(nrow(f), 60L)
  expect_identical(sum(f$EXIT_EVER), 36L)     # on time + late in max + past max
  expect_identical(sum(f$EXIT_MAX), 24L)      # exit in year 6 or sooner
  expect_identical(sum(f$EXIT_ONTIME), 12L)   # exit in year 5 or sooner
  expect_identical(sum(f$NO_EXIT), 12L)
  expect_identical(sum(f$DISAPPEARED), 12L)
  # Prolonged is late exiters plus the never-exited, and nothing else.
  expect_identical(sum(f$PROLONGED), 36L)
})

test_that("the flags nest, and exiters + no-exit + disappeared make the cohort", {
  f <- check2_fixture_flags()$flags

  # Ever >= Max >= OnTime: each is a subset of the one before.
  expect_true(all(f$EXIT_ONTIME <= f$EXIT_MAX))
  expect_true(all(f$EXIT_MAX <= f$EXIT_EVER))

  # Disjoint outcomes: every student is exactly one of the three.
  one_of_three <- f$EXIT_EVER + f$NO_EXIT + f$DISAPPEARED
  expect_true(all(one_of_three == 1L))

  # Disappeared students are in neither group: censored is not did-not-exit.
  expect_false(any(f$DISAPPEARED & (f$ON_TIME | f$PROLONGED)))
  expect_identical(f$ON_TIME, f$EXIT_ONTIME)
  expect_identical(f$PROLONGED, (f$EXIT_EVER & !f$EXIT_ONTIME) | f$NO_EXIT)
})

test_that("compute_exit_flags refuses a cohort the panel does not support", {
  b <- make_check2_bundle()
  expect_error(compute_exit_flags(b, check2_spec(), cohort = "K-1900"),
               "No cohort labelled")
})

# ---- the crosstab ------------------------------------------------------------

test_that("the crosstab's columns sum to All", {
  # Disappeared is its own column and is never folded into either group.
  # Exit_Max and Exit_OnTime are nested inside Exit_Ever, so they are not part
  # of the sum -- what adds up is Ever + No_Exit + Disappeared.
  ct <- compute_crosstab(check2_fixture_flags()$flags, check2_spec())
  n <- stats::setNames(ct$N, ct$COLUMN)

  expect_identical(n[["ALL"]], 60L)
  expect_identical(n[["EXIT_EVER"]] + n[["NO_EXIT"]] + n[["DISAPPEARED"]],
                   n[["ALL"]])
  expect_identical(n[["EXIT_ONTIME"]], 12L)

  pct <- stats::setNames(ct$PCT, ct$COLUMN)
  expect_equal(pct[["ALL"]], 1)
  expect_equal(pct[["EXIT_EVER"]] + pct[["NO_EXIT"]] + pct[["DISAPPEARED"]], 1)
  expect_equal(pct[["EXIT_ONTIME"]], 12 / 60)
})

test_that("the crosstab has the deck's six columns, in order, in ALL-CAPS names", {
  ct <- compute_crosstab(check2_fixture_flags()$flags, check2_spec())

  expect_identical(ct$COLUMN, c("ALL", "EXIT_EVER", "EXIT_MAX", "EXIT_ONTIME",
                                "NO_EXIT", "DISAPPEARED"))
  expect_identical(names(ct), c("BLOCK", "ROW", "COLUMN", "N", "PCT",
                                "SUPPRESSED"))
  expect_identical(names(ct), toupper(names(ct)))
  expect_true(all(ct$BLOCK == "Total"))
})

test_that("suppressed crosstab cells are NA, never dropped rows", {
  # 5 disappeared students is under the SIM spec's min_cell of 10.
  ct <- compute_crosstab(check2_fixture_flags(n_disappear = 5L)$flags,
                         check2_spec())

  expect_identical(nrow(ct), 6L)               # the row is kept
  gone <- ct[ct$COLUMN == "DISAPPEARED", ]
  expect_true(gone$SUPPRESSED)
  expect_true(is.na(gone$N))                   # even the count is blanked
  expect_true(is.na(gone$PCT))

  # Nothing else is touched.
  rest <- ct[ct$COLUMN != "DISAPPEARED", ]
  expect_false(any(rest$SUPPRESSED))
  expect_false(anyNA(rest$N))
  expect_false(anyNA(rest$PCT))
})

test_that("the threshold is the spec's, not a literal", {
  spec <- check2_spec()
  spec$policy$suppression$min_cell <- 13L
  # The columns of 12 students (on time, never exited, disappeared) are now
  # below the minimum; Exit_Max (24), Exit_Ever (36) and All (60) are not.
  ct <- compute_crosstab(check2_fixture_flags()$flags, spec)

  small <- ct[ct$COLUMN %in% c("EXIT_ONTIME", "NO_EXIT", "DISAPPEARED"), ]
  expect_true(all(small$SUPPRESSED))
  expect_true(all(is.na(small$N)))
  expect_false(any(ct$SUPPRESSED[!ct$COLUMN %in% small$COLUMN]))
})

test_that("an empty column is shown as zero, not suppressed", {
  # Zero is not a small cell. Blanking it would tell the reader a column was
  # hidden when it is simply empty -- the shipped sim has no disappearances.
  ct <- compute_crosstab(check2_fixture_flags(n_disappear = 0L)$flags,
                         check2_spec())
  gone <- ct[ct$COLUMN == "DISAPPEARED", ]

  expect_identical(gone$N, 0L)
  expect_equal(gone$PCT, 0)
  expect_false(gone$SUPPRESSED)
})

# ---- the stage ---------------------------------------------------------------

test_that("Check 2 follows Check 1's cohort instead of selecting its own", {
  res <- check2_sim()
  co <- compute_cohorts(make_check2_sim_bundle(), check2_spec())

  expect_identical(res$meta$cohort, co$COHORT_LABEL[co$SELECTED])
  expect_identical(unique(res$flags$COHORT_LABEL), res$meta$cohort)
  expect_identical(nrow(res$flags), co$N_ENTRY[co$SELECTED])
  expect_identical(res$meta$cohort_n, co$N_ENTRY[co$SELECTED])
})

test_that("one K cohort clears every crosstab cell at the shipped scale", {
  # k_cohort_n = 2400 is set by this constraint; never lower min_cell or pool
  # grades to make cells appear.
  ct <- check2_sim()$crosstab
  expect_false(anyNA(ct$N))
  expect_false(anyNA(ct$PCT))
  expect_false(any(ct$SUPPRESSED))

  n <- stats::setNames(ct$N, ct$COLUMN)
  expect_identical(n[["EXIT_EVER"]] + n[["NO_EXIT"]] + n[["DISAPPEARED"]],
                   n[["ALL"]])
})

test_that("the Sample line names the cohort, its size and the on-time rule", {
  res <- check2_sim()
  s <- res$meta$sample

  expect_match(s, res$meta$cohort, fixed = TRUE)
  expect_match(s, format(res$meta$cohort_n, big.mark = ","), fixed = TRUE)
  expect_match(s, "Data Check 1", fixed = TRUE)
  # The SIM spec's timeline is provisional, and the derivation flag travels
  # with the number it qualifies (CLAUDE.md, derivation transparency).
  expect_match(s, "DERIVED", fixed = TRUE)
  expect_identical(res$meta$on_time_text, "5 years")
})

test_that("a panel with no usable cohort degrades to NULL slots and a note", {
  # Three panel years cannot follow anyone for the six-year maximum timeline.
  roster <- make_check2_roster(panel = 2015:2017)
  roster <- roster[roster$YEAR %in% 2015:2017, ]
  b <- make_check2_bundle(roster)

  res <- run_check2(b, check2_spec())
  expect_null(res$flags)
  expect_null(res$crosstab)
  expect_match(res$meta$sample, "No entry cohort")
  expect_true(is.character(res$meta$note))
})

test_that("the ELPA21 smoke fixture runs: no WIDA assumptions in Check 2", {
  # Borrows Check 1's ELPA21-shaped bundle (a categorical level, no bands): the
  # crosstab reads no component and no band, so it must run unchanged.
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  res <- run_check2(make_check1_elpa21_bundle(spec), spec)

  expect_false(is.null(res$crosstab))
  # ELPA21 is Type 6, so the Additional check applies -- but is not built.
  expect_true(res$meta$type_gate_applies)
  expect_null(res$blocked_groups)
})

# ---- presentation ------------------------------------------------------------

test_that("the crosstab renders with an em dash for a suppressed cell", {
  ct <- compute_crosstab(check2_fixture_flags(n_disappear = 5L)$flags,
                         check2_spec())
  res <- list(crosstab = ct)

  d <- .check2_crosstab_display(res)
  expect_identical(d$ROW, c("Total students", "% of all EL students"))
  expect_identical(d$DISAPPEARED, rep(intToUtf8(0x2014), 2L))
  expect_identical(d$ALL, c("53", "100%"))
  expect_s3_class(ft_check2_crosstab(res), "flextable")
})

test_that("captions say the indicators are window-relative", {
  # The caption lives in the document, so the guard reads the document.
  dir <- testthat::test_path("..", "..", "inst", "templates")
  skip_if(!dir.exists(dir), "templates not reachable from the test dir")
  qmd <- file.path(dir, "check2_flows.qmd")
  skip_if(!file.exists(qmd), "check2 document not found")

  txt <- paste(readLines(qmd, warn = FALSE), collapse = "\n")
  expect_match(txt, "relative to the years observed", fixed = TRUE)
  expect_match(txt, "not never exiting", fixed = TRUE)
})

# ---- slots still to build ----------------------------------------------------

test_that("the blocked-groups slot is NULL under a Type 1 spec", {
  res <- check2_sim()
  expect_null(res$blocked_groups)
  expect_false(res$meta$type_gate_applies)
})

test_that("the characteristic row blocks are built one at a time", {
  skip("check2 not built: $crosstab row blocks")
})

test_that("the academic summary is NULL without the academic table", {
  skip("check2 not built: $academic_summary")
})

test_that("cohort mean ELP is never plotted without conditioning on exit group", {
  # Slide-81 caution.
  skip("check2 not built: $trajectories")
})
