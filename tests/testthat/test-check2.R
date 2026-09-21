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
  expect_identical(names(ct), c("BLOCK", "BLOCK_TYPE", "FIELD", "ROW",
                                "COLUMN", "N", "N_BASE", "PCT", "SUPPRESSED"))
  expect_identical(names(ct), toupper(names(ct)))
  # No profile passed, so the Total block is all there is.
  expect_true(all(ct$BLOCK == "Total"))
  expect_true(all(ct$BLOCK_TYPE == "total"))
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
  # grades to make cells appear. The Total block is what the constraint is
  # about; a thin cell inside a characteristic block is expected and has its
  # own test below.
  ct <- check2_sim()$crosstab
  ct <- ct[ct$BLOCK == "Total", , drop = FALSE]
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

# ---- the characteristic row blocks -------------------------------------------

test_that("the profile reads characteristics at entry, and derives two", {
  f <- check2_fixture_flags()
  pr <- compute_cohort_profile(f$bundle, check2_spec(), f$flags)

  # One row per cohort student, in the same order as the flags.
  expect_identical(pr$STUDENT_ID, f$flags$STUDENT_ID)
  # The initial ELP band is derived, not delivered: the fixture's roster has no
  # INITIAL_ELP column at all, and the band still resolves.
  expect_false("INITIAL_ELP" %in% names(el_roster(f$bundle)))
  expect_true("ELP_BAND" %in% names(pr))
  expect_true(all(stats::na.omit(pr$ELP_BAND) %in% spec_bands(check2_spec())$labels))
})

test_that("the initial band agrees with the one Check 1 splits its curve on", {
  # The two stages must not disagree about a student's band. This is the test
  # that would fail if either derivation drifted.
  b <- make_check2_sim_bundle()
  spec <- check2_spec()
  res <- check2_sim()

  c1 <- .check1_initial_band(b, spec)
  at <- match(res$profile$STUDENT_ID, c1$STUDENT_ID)
  expect_identical(res$profile$ELP_BAND, c1$ELP_BAND[at])
})

test_that("a distribution block's shares sum to one within every column", {
  ct <- check2_sim()$crosstab
  d <- ct[ct$BLOCK_TYPE == "distribution", , drop = FALSE]
  expect_gt(nrow(d), 0L)

  for (b in unique(d$BLOCK)) {
    for (cl in .CHECK2_COLUMNS) {
      cell <- d[d$BLOCK == b & d$COLUMN == cl, , drop = FALSE]
      # A suppressed cell is NA and drops out of the sum, so the identity is
      # asserted on the blocks that have nothing suppressed; an all-zero
      # column (the sim has no disappearances) has no shares at all.
      if (any(cell$SUPPRESSED) || all(cell$N_BASE == 0L)) next
      expect_equal(sum(cell$PCT), 1, tolerance = 1e-8,
                   info = paste(b, cl))
    }
  }
})

test_that("an indicator block's rows are independent shares, not a partition", {
  ct <- check2_sim()$crosstab
  ind <- ct[ct$BLOCK_TYPE == "indicator" & ct$COLUMN == "ALL", , drop = FALSE]
  expect_gt(nrow(ind), 1L)
  # Male + low SES + special education is well over 100%: they overlap.
  expect_gt(sum(ind$PCT, na.rm = TRUE), 1)
  # And each is a share of the students whose value is reported.
  expect_true(all(ind$N <= ind$N_BASE, na.rm = TRUE))
})

test_that("a not-reported value keeps a distribution honest, and an indicator out", {
  f <- check2_fixture_flags()
  spec <- check2_spec()
  roster <- el_roster(f$bundle)
  # Half the cohort has no service model and no disability flag delivered.
  half <- unique(roster$STUDENT_ID)[c(TRUE, FALSE)]
  roster$SERVICE_MODEL <- ifelse(roster$STUDENT_ID %in% half, NA, "esl_pullout")
  roster$IEP <- ifelse(roster$STUDENT_ID %in% half, NA, "Y")
  b <- as_el_data(el_base(f$bundle), roster = roster, spec = spec)
  fl <- compute_exit_flags(b, spec, f$flags$COHORT_LABEL[[1]])
  ct <- compute_crosstab(fl, spec, compute_cohort_profile(b, spec, fl))

  # The distribution keeps them, as their own row, so the shares still sum to 1.
  svc <- ct[ct$BLOCK == "Type of EL services" & ct$COLUMN == "ALL", ]
  expect_true(.CHECK2_MISSING %in% svc$ROW)
  expect_equal(sum(svc$PCT), 1, tolerance = 1e-8)

  # The indicator drops them from its denominator rather than calling them "no".
  sped <- ct[ct$ROW == "Special education" & ct$COLUMN == "ALL", ]
  expect_lt(sped$N_BASE, nrow(fl))
  expect_equal(sped$PCT, 1)
})

test_that("a block whose field is not delivered is omitted, not rendered empty", {
  f <- check2_fixture_flags()
  spec <- check2_spec()
  # The fixture roster carries no demographics at all.
  res <- run_check2(f$bundle, spec)

  expect_false("Primary language" %in% res$crosstab$BLOCK)
  expect_false("Other demographics" %in% res$crosstab$BLOCK)
  # The derived initial-ELP block still lands: it needs no delivered column.
  expect_true("Initial ELP" %in% res$crosstab$BLOCK)
})

test_that("row values stay coded in the slot and are labelled only at render", {
  res <- check2_sim()
  lang <- res$crosstab[res$crosstab$FIELD %in% "HOME_LANGUAGE", ]
  expect_true("spa" %in% lang$ROW)          # the delivery's own code
  expect_false("Spanish" %in% lang$ROW)

  shown <- .check2_crosstab_display(res)
  expect_true("Spanish" %in% shown$ROW)     # ... and the reader's word
  expect_false("spa" %in% shown$ROW)
})

# ---- the academic summary ----------------------------------------------------

test_that("the academic summary is NULL without the academic table", {
  spec <- check2_spec()
  b <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
  co <- compute_cohorts(b, spec)
  fl <- compute_exit_flags(b, spec, co$COHORT_LABEL[co$SELECTED])

  expect_null(compute_academic_summary(b, spec, fl))
  expect_null(run_check2(b, spec)$academic_summary)
})

test_that("the academic summary compares groups at the same year in status", {
  d <- check2_sim()$academic_summary
  expect_true(all(c("EXIT_GROUP", "YEAR_IN_STATUS", "PCT_PROFICIENT") %in%
                    names(d)))
  # Both groups appear at each year, so every row has a counterpart to read
  # across to.
  wide <- table(d$YEAR_IN_STATUS, d$EXIT_GROUP)
  expect_true(all(wide == 1L))
  expect_true(all(d$EXIT_GROUP %in% .CHECK2_GROUPS))
  # Proficiency is a rate, and scored students never exceed the students there.
  expect_true(all(d$PCT_PROFICIENT >= 0 & d$PCT_PROFICIENT <= 1, na.rm = TRUE))
  expect_true(all(d$N_SCORED <= d$N, na.rm = TRUE))
})

test_that("the summary skips the years the assessment does not reach", {
  # Grades below the spec's tested span have no rows at all, and neither does
  # the simulation's missing assessment year -- a missing year, not an empty
  # one, and never imputed.
  spec <- check2_spec()
  d <- check2_sim()$academic_summary
  years <- sort(unique(d$YEAR_IN_STATUS))
  gap <- setdiff(seq(min(years), max(years)), years)

  expect_gt(min(years), 1L)                 # K-2 are not tested
  expect_length(gap, 1L)                    # exactly the missing year
  # And the note says so rather than leaving the reader to notice.
  expect_match(check2_academic_note(check2_sim()), "no assessment was administered")
})

test_that("a group with nobody left to measure reads as zero, not suppressed", {
  # On-time exiters leave the panel after monitoring; a reader has to be able to
  # tell "nobody left" from "too few to report".
  d <- check2_sim()$academic_summary
  empty <- d[!is.na(d$N_SCORED) & d$N_SCORED == 0L, , drop = FALSE]

  expect_gt(nrow(empty), 0L)
  expect_false(any(empty$SUPPRESSED))
  expect_true(all(is.na(empty$PCT_PROFICIENT)))
})

test_that("the academic summary refuses a subject the spec does not declare", {
  f <- check2_fixture_flags()
  expect_error(
    compute_academic_summary(make_check2_sim_bundle(), check2_spec(),
                             check2_sim()$flags, subject = "Nonesuch"),
    "not in the spec")
})

# ---- the trajectories, and the slide-81 guardrail ---------------------------

test_that("cohort mean ELP is never plotted without conditioning on exit group", {
  # Slide-81 caution, as three executable claims.
  res <- check2_sim()
  lv <- res$trajectories$levels

  # 1. The compute has no unconditioned form: every row carries a group, and
  #    more than one group is present.
  expect_true("EXIT_GROUP" %in% names(lv))
  expect_false(anyNA(lv$EXIT_GROUP))
  expect_gt(length(unique(lv$EXIT_GROUP)), 1L)

  # 2. The plot refuses a frame that has lost the split.
  expect_error(plot_trajectories(lv[c("YEAR_IN_STATUS", "MEAN_LEVEL")]),
               "EXIT_GROUP")

  # 3. The document draws no pooled line.
  dir <- testthat::test_path("..", "..", "inst", "templates")
  skip_if(!dir.exists(dir), "templates not reachable from the test dir")
  txt <- paste(readLines(file.path(dir, "check2_flows.qmd"), warn = FALSE),
               collapse = "\n")
  expect_match(txt, "no pooled cohort line", fixed = TRUE)
})

test_that("the composition shift that makes a pooled mean misleading is shown", {
  # The mechanism, not just the warning: the on-time group's share of the
  # students still being assessed falls as it exits, so any pooled average is
  # taken over a changing population.
  comp <- check2_sim()$trajectories$composition
  ot <- comp[comp$EXIT_GROUP == "on_time", , drop = FALSE]
  ot <- ot[order(ot$YEAR_IN_STATUS), ]

  expect_gt(ot$PCT_OF_MEASURED[[1]], 0.2)
  expect_equal(utils::tail(ot$PCT_OF_MEASURED, 1)[[1]], 0)
  # Shares within a year sum to one wherever anyone is measured.
  by_year <- tapply(comp$PCT_OF_MEASURED, comp$YEAR_IN_STATUS,
                    function(z) sum(z, na.rm = TRUE))
  expect_true(all(by_year %in% c(0, 1) | abs(by_year - 1) < 1e-8))
})

test_that("trajectories are NULL for a categorical level scale", {
  # The level is stored as an ordinal index, so it would average without
  # complaint -- but the mean of two category names is not a level.
  spec <- make_check2_elpa21_spec()
  b <- make_check2_elpa21_blocked_bundle(spec)
  expect_identical(spec$elp$level_scale$type, "categorical")
  expect_null(run_check2(b, spec)$trajectories)
})

# ---- Additional check (Types 2-6) -------------------------------------------

test_that("the blocked-groups slot is NULL under a Type 1 spec", {
  res <- check2_sim()
  expect_null(res$blocked_groups)
  expect_false(res$meta$type_gate_applies)
  expect_match(res$meta$blocked_note, "Type 1")
})

test_that("the blocked-groups slot populates under a Type 6 rule", {
  spec <- make_check2_elpa21_spec()
  b <- make_check2_elpa21_blocked_bundle(spec)
  res <- run_check2(b, spec)

  expect_true(res$meta$type_gate_applies)
  bg <- res$blocked_groups
  expect_false(is.null(bg))
  # One group per criterion that blocks on its own; the fixture is built with
  # two, and the criterion names come from the spec's terms, never typed here.
  expect_identical(length(unique(bg$BLOCKED_BY)), 2L)
  expect_true(all(bg$COLUMN %in% .CHECK2_COLUMNS))
  expect_true(all(bg$N <= bg$N_BASE, na.rm = TRUE))
  expect_s3_class(ft_check2_blocked_groups(res), "flextable")
})

test_that("the blocked groups are Prep 2's, not a second derivation", {
  spec <- make_check2_elpa21_spec()
  b <- make_check2_elpa21_blocked_bundle(spec)
  res <- run_check2(b, spec)

  cw <- compute_criteria_crosswalk(b, spec)
  prep2_criteria <- sort(unique(cw$blocked_groups$BLOCKED_BY))
  expect_identical(sort(unique(sub(" only$", "", res$blocked_groups$BLOCKED_BY))),
                   prep2_criteria)
})

test_that("a Type 2-6 rule with no blocked students says so, distinctly", {
  # "Does not apply" and "applies but finds nothing" are different facts. The
  # shipped ELPA21 spec asks for a level above its own scale, so every student
  # fails every criterion and none is blocked by exactly one.
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  b <- make_check2_elpa21_blocked_bundle(make_check2_elpa21_spec())
  res <- run_check2(b, spec)

  expect_true(res$meta$type_gate_applies)
  expect_null(res$blocked_groups)
  expect_match(res$meta$blocked_note, "more than one criterion")
  expect_no_match(res$meta$blocked_note, "Type 1")
})
