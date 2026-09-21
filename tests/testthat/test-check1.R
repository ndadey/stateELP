# Check 1 -- Cohort survival analysis. run_check1(data, spec, start_grade = "K").
# Sample: one stable entry cohort, LEFT_TRUNCATED excluded by construction.
# Built per internal_docs/steps/5_check1_survival.md.
#
# The exit-dating convention this stage rests on is ALREADY tested, in
# test-utils.R, against the deck's A-J example and its counter-test. What is
# tested here is the life table built on top of it, and the cohort selection
# (D1) and data-fields declaration (D2) decided 2026-09-20.

check1_spec <- function() make_check1_spec()

# run_check1() on the full sim, once for the file.
check1_sim <- local({
  r <- NULL
  function() {
    if (is.null(r)) r <<- run_check1(make_check1_sim_bundle(), check1_spec())
    r
  }
})

# The same run without the academic table: what a delivery that cannot
# support Model 2 produces. Also built once.
check1_sim_no_academic <- local({
  r <- NULL
  function() {
    if (is.null(r)) {
      spec <- check1_spec()
      r <<- run_check1(as_el_data(el_long_sim, roster = el_roster_sim,
                                  spec = spec), spec)
    }
    r
  }
})

# ---- the slide-52 worked numbers --------------------------------------------

test_that("the slide-52 life table reproduces D, E and F to two decimals", {
  # Test the CELLS, not the callouts: two A cells in the slide are off by one
  # (19,306 vs 19,305; 8,339 vs 8,340) so the recurrence is checked at +/-1
  # tolerance, and one callout has a typo (0.06 for 0.58).
  s <- make_check1_slide52()
  rates <- .life_table_rates(s$A, s$B, s$C)

  expect_equal(round(rates$D, 2), s$D)
  expect_equal(round(rates$E, 2), s$E)
  expect_equal(round(rates$F, 2), s$F)
})

test_that("the slide's own A column follows the recurrence within one student", {
  s <- make_check1_slide52()
  last <- nrow(s)
  expected <- s$A[-last] - s$B[-last] - s$C[-last]   # A_t - B_t - C_t

  expect_true(all(abs(s$A[-1] - expected) <= 1))
  # ... and exactly two of the slide's cells are the ones off by one.
  expect_identical(sum(s$A[-1] != expected), 2L)
})

test_that("the life table's last row has an empty B and C equal to A", {
  lt <- check1_sim()$life_table
  last <- nrow(lt)

  # No year after the window's edge, so no exit can be observed in it ...
  expect_true(is.na(lt$B[[last]]))
  # ... and everyone still at risk is censored there.
  expect_identical(lt$C[[last]], lt$A[[last]])
  expect_equal(lt$E[[last]], 1)
  expect_equal(lt$D[[last]], 0)
  expect_true(is.na(lt$F[[last]]))

  # The recurrence holds on every other row, exactly.
  expect_identical(lt$A[-1], (lt$A - lt$B - lt$C)[-last])
})

test_that("the life table starts from the cohort and F only ever rises", {
  res <- check1_sim()
  lt <- res$life_table
  co <- res$cohorts

  # A_1 is the cohort: the life table and $cohorts count the same students.
  expect_identical(lt$A[[1]], co$N_ENTRY[co$SELECTED])
  expect_identical(nrow(lt), co$FOLLOWUP_YEARS[co$SELECTED])

  # Exit is absorbing, so the cumulative column never decreases.
  expect_true(all(diff(stats::na.omit(lt$F)) >= 0))
  # Rows are years in status, in order, from 1.
  expect_identical(lt$YEAR_IN_STATUS, seq_len(nrow(lt)))
})

test_that("the life table agrees with the generator's own duration table", {
  # sim_contract.md 4.7 reports K-2015 exiting 59.8% within 5 years and 69.6%
  # within 6, under the SIM's last-current dating. The package dates exit to
  # the first former year, so those shares land one row later in F. Two
  # independent derivations -- the generator's and this package's -- meeting
  # is the strongest check that the dating chain is right end to end.
  res <- check1_sim()
  expect_identical(res$meta$cohort, "K-2015")
  expect_equal(round(100 * res$life_table$F[[5]], 1), 59.8)
  expect_equal(round(100 * res$life_table$F[[6]], 1), 69.6)
})

# ---- guardrails --------------------------------------------------------------

test_that("no mean or average time in status is computed anywhere", {
  # Time in status is censored; its mean is not defined on this data
  # (sim_contract.md 8). Guarded over this stage's sources ...
  files <- list.files(test_path("..", ".."), pattern = "^check1_.*\\.R$",
                      full.names = TRUE, recursive = TRUE)
  skip_if(length(files) == 0, "package sources not reachable from the test dir")
  code <- unlist(lapply(files, function(f) {
    l <- readLines(f, warn = FALSE)
    l[!grepl("^\\s*#", l)]
  }))
  expect_identical(
    grep("(mean|median)\\([^)]*(YEAR_IN_STATUS|EXIT_YEAR)", code, value = TRUE),
    character())

  # ... and over what the stage actually returns.
  res <- check1_sim()
  expect_false(any(grepl("MEAN|AVERAGE", names(res$life_table))))
  expect_false(any(grepl("MEAN|AVERAGE", names(res$cohorts))))
})

test_that("cohorts are derived from the panel range, never year literals", {
  files <- list.files(test_path("..", ".."), pattern = "^check1_.*\\.R$",
                      full.names = TRUE, recursive = TRUE)
  skip_if(length(files) == 0, "package sources not reachable from the test dir")
  code <- unlist(lapply(files, function(f) {
    l <- readLines(f, warn = FALSE)
    l[!grepl("^\\s*#", l)]                       # dated decisions live in comments
  }))
  expect_identical(grep("\\b(19|20)[0-9]{2}\\b", code, value = TRUE),
                   character())
})

# ---- D1: selection ----------------------------------------------------------

test_that("compute_cohorts() returns one row per candidate, no outcomes", {
  co <- compute_cohorts(make_check1_sim_bundle(), check1_spec())

  expect_s3_class(co, "tbl_df")
  expect_identical(names(co), c("COHORT_LABEL", "COHORT_START_YEAR", "N_ENTRY",
                                "FOLLOWUP_YEARS", "SELECTED", "SUPPRESSED"))
  expect_identical(nrow(co), 5L)                  # five usable K cohorts
  expect_identical(sum(co$SELECTED), 1L)
  # Follow-up shrinks with every later entry year, and equals the number of
  # life-table rows.
  expect_identical(co$FOLLOWUP_YEARS, 11:7)
  expect_true(all(co$N_ENTRY > 2000))
})

test_that("selection is the qualifying candidate with the longest follow-up", {
  co <- compute_cohorts(make_check1_bundle(
    make_check1_roster(entry_offset = c(0L, 1L, 2L), n = c(20L, 20L, 20L))),
    check1_spec())

  expect_identical(which(co$SELECTED), 1L)
  expect_identical(co$FOLLOWUP_YEARS[co$SELECTED], max(co$FOLLOWUP_YEARS))
})

test_that("a candidate below the minimum cell size is never selected", {
  spec <- check1_spec()
  # The longest follow-up is also the smallest cohort: selection must skip it.
  co <- compute_cohorts(make_check1_bundle(
    make_check1_roster(entry_offset = c(0L, 1L),
                       n = c(spec_min_cell(spec) - 1L, 20L))), spec)

  expect_true(co$SUPPRESSED[[1]])
  expect_true(is.na(co$N_ENTRY[[1]]))            # the count itself is disclosive
  expect_identical(nrow(co), 2L)                  # the row stays, never dropped
  expect_identical(which(co$SELECTED), 2L)
})

test_that("selection follows the panel, never a year literal", {
  spec <- check1_spec()
  roster <- make_check1_roster(entry_offset = c(0L, 1L), n = c(20L, 20L))
  base_pick <- compute_cohorts(make_check1_bundle(roster), spec)
  first_label <- base_pick$COHORT_LABEL[base_pick$SELECTED]

  # Drop the panel's first year: its entrants are no longer K entrants, so
  # selection moves to the next entry year.
  later <- roster[roster$YEAR > min(roster$YEAR), ]
  moved <- compute_cohorts(make_check1_bundle(later), spec)
  expect_false(moved$COHORT_LABEL[moved$SELECTED] == first_label)
  expect_identical(nrow(moved), 1L)

  # Drop LEFT_TRUNCATED and the same thing happens for the same reason:
  # build_cohort() cannot tell entry in the panel's first year from entry
  # before it, so it excludes that cohort.
  roster$LEFT_TRUNCATED <- NULL
  untruncated <- compute_cohorts(make_check1_bundle(roster), spec)
  expect_false(untruncated$COHORT_LABEL[untruncated$SELECTED] == first_label)
  expect_identical(untruncated$COHORT_LABEL[untruncated$SELECTED],
                   moved$COHORT_LABEL[moved$SELECTED])
})

test_that("no qualifying candidate leaves the slots NULL, with a note", {
  spec <- check1_spec()

  # Every candidate below min_cell: candidates render, nothing is selected.
  small <- run_check1(make_check1_bundle(
    make_check1_roster(entry_offset = c(0L, 1L), n = c(5L, 5L))), spec)
  expect_null(small$life_table)
  expect_false(is.null(small$cohorts))
  expect_true(all(small$cohorts$SUPPRESSED))
  expect_match(small$meta$note, "minimum cell size")
  expect_match(small$meta$sample, "No entry cohort")

  # A panel too short for any cohort: no candidates at all.
  short <- run_check1(make_check1_bundle(
    make_check1_roster(entry_offset = 0L, n = 20L,
                       panel = seq_len(spec_max_years(spec)))), spec)
  expect_null(short$cohorts)
  expect_null(short$life_table)
  expect_match(short$meta$note, "No candidate cohort")
  expect_match(short$meta$sample, "too short")
})

test_that("the Sample statement is built from the selected cohort", {
  res <- check1_sim()
  co <- res$cohorts[res$cohorts$SELECTED, ]

  expect_identical(res$meta$sample, .check1_sample_text(
    res$cohorts, "K", max(el_roster(make_check1_sim_bundle())$YEAR),
    check1_spec()))
  expect_match(res$meta$sample, co$COHORT_LABEL, fixed = TRUE)
  expect_match(res$meta$sample, format(co$N_ENTRY, big.mark = ","), fixed = TRUE)
  expect_match(res$meta$sample, as.character(co$FOLLOWUP_YEARS), fixed = TRUE)
  expect_match(res$meta$sample, "kindergarten", fixed = TRUE)
})

# ---- D2: the data-fields declaration ----------------------------------------

test_that("a missing required field stops the stage, naming it", {
  spec <- check1_spec()
  bundle <- make_check1_bundle(make_check1_roster())
  required <- c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS")

  for (field in required) {
    # Removed from the BUILT bundle: as_el_data() rejects such a roster long
    # before run_check1() would see it.
    broken <- bundle
    broken$roster[[field]] <- NULL
    expect_error(run_check1(broken, spec), field, fixed = TRUE)
  }
})

test_that("$meta$data_fields is the declaration, filtered to computed slots", {
  res <- check1_sim()
  fields <- res$meta$data_fields

  computed <- c("cohorts", "life_table", "exit_curve",
                "exit_curve_by_initial_band", "survival_est",
                "at_max_timeline", "model2")
  expect_setequal(fields$FIELD, .check1_fields_for(computed)$FIELD)
  expect_true(all(c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS") %in%
                    fields$FIELD[fields$KIND == "required"]))

  # The box has grown with each pass: the first pass's fields are still
  # listed, and the curves and Model 2 added their own.
  first_pass <- .check1_fields_for(c("cohorts", "life_table"))$FIELD
  expect_true(all(first_pass %in% fields$FIELD))
  expect_true(all(c("ELP_BAND", "ELP_FORM", "SUBJECT", "PERSON_PERIOD") %in%
                    setdiff(fields$FIELD, first_pass)))
  expect_true(all(fields$PRESENT[fields$TABLE == "roster"]))

  # And it tracks what actually ran, not what could have: without the
  # academic table Model 2 does not run, and the fields only it reads -- the
  # academic ones -- leave with it. Model 1 fits on the language score alone,
  # so it and its person-period frame stay.
  without <- check1_sim_no_academic()$meta$data_fields
  expect_false(any(without$TABLE == "academic"))
  expect_false("SUBJECT" %in% without$FIELD)
  expect_true("PERSON_PERIOD" %in% without$FIELD)
  expect_true("SCALE_SCORE" %in% without$FIELD[without$TABLE == "base"])
})

test_that("an absent optional field is reported with its fallback", {
  spec <- check1_spec()
  bundle <- make_check1_bundle(make_check1_roster())
  bundle$roster$LEFT_TRUNCATED <- NULL

  res <- run_check1(bundle, spec)
  fields <- res$meta$data_fields
  lt_row <- fields[fields$FIELD == "LEFT_TRUNCATED", ]

  expect_false(lt_row$PRESENT)
  expect_identical(lt_row$KIND, "optional")

  shown <- .check1_fields_display(res)
  expect_match(shown$USED_FOR[shown$FIELD == "LEFT_TRUNCATED"],
               "Not delivered")
  expect_match(shown$USED_FOR[shown$FIELD == "LEFT_TRUNCATED"],
               "left-truncated")
})

test_that("YOUR_COLUMN resolves through the spec's crosswalk", {
  nj <- read_el_spec("NJ_elp_2023")
  bundle <- make_check1_bundle(make_check1_roster())

  fields <- .check1_data_fields(bundle, nj, c("cohorts", "life_table"))
  expect_identical(fields$YOUR_COLUMN[fields$FIELD == "STUDENT_ID"],
                   "Scrambled_SID")
  # A field the crosswalk does not rename keeps its canonical name ...
  expect_identical(fields$YOUR_COLUMN[fields$FIELD == "EL_STATUS"],
                   "EL_STATUS")
  # ... and derived fields have no delivered column at all.
  expect_true(all(is.na(fields$YOUR_COLUMN[fields$TABLE == "derived"])))

  # On an identity crosswalk the column is not rendered, because it would
  # simply repeat Field down the page.
  res <- check1_sim()
  expect_false("YOUR_COLUMN" %in% names(.check1_fields_display(res)))
})

test_that("the derived-fields line names them and says they are recomputed", {
  note <- check1_derived_note(check1_sim())
  expect_match(note, "EL_STATUS transitions", fixed = TRUE)
  expect_match(note, "COHORT_LABEL", fixed = TRUE)
  expect_match(note, "life table's A", fixed = TRUE)
})

# ---- tables ------------------------------------------------------------------

test_that("the stage's tables build from the results object", {
  res <- check1_sim()
  expect_s3_class(ft_check1_life_table(res), "flextable")
  expect_s3_class(ft_check1_cohorts(res), "flextable")
  expect_s3_class(ft_check1_data_fields(res), "flextable")
})

# ---- the exit curves ---------------------------------------------------------

test_that("compute_exit_curve() returns one row per group and year", {
  cur <- check1_sim()$exit_curve

  expect_s3_class(cur, "tbl_df")
  expect_identical(names(cur), c("GROUP", "YEAR_IN_STATUS", "N_AT_ENTRY",
                                 "N_EXITED_CUM", "CUM_EXIT_RATE", "SUPPRESSED"))
  expect_identical(unique(cur$GROUP), "All")
  expect_identical(cur$YEAR_IN_STATUS, seq_len(nrow(cur)))
})

test_that("the cumulative exit rate is reported as a lower bound", {
  res <- check1_sim()
  cur <- res$exit_curve
  lt <- res$life_table

  # Exit is absorbing, so the curve only ever rises ...
  expect_true(all(diff(cur$CUM_EXIT_RATE) >= 0))
  # ... it counts the cohort, not the survivors ...
  expect_true(all(cur$N_AT_ENTRY == res$cohorts$N_ENTRY[res$cohorts$SELECTED]))
  # ... and it stops at what was observed: censored students are not exiters,
  # so the final value is a lower bound, well short of 1.
  expect_lt(max(cur$CUM_EXIT_RATE), 1)
  # The students behind that shortfall are censored, not non-exiters: the
  # cohort minus the exiters is what the life table's last row censors.
  expect_identical(max(cur$N_EXITED_CUM) + lt$C[[nrow(lt)]],
                   cur$N_AT_ENTRY[[1]] - sum(lt$C[-nrow(lt)]))

  # The curve and the life table are two views of one derivation, and must
  # not drift: F at life-table row t is the share exited by year t + 1
  # (CLAUDE.md 6's dating), which is the curve one row later.
  expect_equal(cur$CUM_EXIT_RATE[-1], lt$F[-nrow(lt)])
  expect_equal(max(cur$CUM_EXIT_RATE), max(lt$F, na.rm = TRUE))
})

test_that("the curve splits by an initial band derived from the headline", {
  res <- check1_sim()
  spec <- check1_spec()
  bb <- res$exit_curve_by_initial_band
  first <- bb[bb$YEAR_IN_STATUS == 1L, ]

  # Groups are the spec's own band labels, plus the no-headline group --
  # never a literal typed here.
  expect_true(all(bb$GROUP %in% c(spec_bands(spec)$labels, .CHECK1_NO_BAND)))
  # Band order is the spec's, and the no-headline group sorts last.
  expect_identical(unique(bb$GROUP),
                   c(intersect(spec_bands(spec)$labels, bb$GROUP),
                     .CHECK1_NO_BAND))

  # Every student is in exactly one group: the visible sizes plus what the
  # suppressed groups can be holding account for the whole cohort.
  n_cohort <- res$cohorts$N_ENTRY[res$cohorts$SELECTED]
  hidden <- sum(first$SUPPRESSED)
  expect_lte(sum(first$N_AT_ENTRY, na.rm = TRUE), n_cohort)
  expect_lt(n_cohort - sum(first$N_AT_ENTRY, na.rm = TRUE),
            hidden * spec_min_cell(spec))

  # Each group's own curve is still a cumulative one.
  for (g in unique(bb$GROUP)) {
    d <- bb[bb$GROUP == g & !bb$SUPPRESSED, ]
    if (nrow(d) > 1L) expect_true(all(diff(d$CUM_EXIT_RATE) >= 0), info = g)
  }
})

test_that("a band below the minimum cell size is blanked, never dropped", {
  bb <- check1_sim()$exit_curve_by_initial_band
  small <- bb[bb$SUPPRESSED, ]

  # sim_contract.md 4.5: for K entrants the bands at 4.0 and above are
  # near-empty by construction, so this is exercised by the shipped data.
  expect_gt(nrow(small), 0L)
  expect_true(all(is.na(small$N_AT_ENTRY)))
  expect_true(all(is.na(small$CUM_EXIT_RATE)))
  # The rows survive, so the reader sees which bands were suppressed: every
  # group still carries one row per year in status, index intact.
  per_group <- table(bb$GROUP)
  expect_true(all(per_group == length(unique(bb$YEAR_IN_STATUS))))
  expect_false(any(is.na(bb$YEAR_IN_STATUS)))
})

test_that("the derived initial band reproduces the sim's INITIAL_ELP", {
  # sim_contract.md 3.5 marks INITIAL_ELP D-tier: the package derives the
  # band from the first annual headline and treats the delivered column as a
  # validation input, exactly as it does YEAR_IN_STATUS and COHORT_LABEL.
  b <- make_check1_sim_bundle()
  band <- .check1_initial_band(b, check1_spec())
  sim <- el_roster_sim[!duplicated(el_roster_sim$STUDENT_ID),
                       c("STUDENT_ID", "INITIAL_ELP")]
  both <- merge(band, sim, by = "STUDENT_ID")

  expect_gt(nrow(both), 50000L)
  expect_identical(both$ELP_BAND, both$INITIAL_ELP)
})

test_that("students with no annual headline are their own group, not a band", {
  spec <- check1_spec()
  bb <- check1_sim()$exit_curve_by_initial_band
  no_band <- bb[bb$GROUP == .CHECK1_NO_BAND, ]

  expect_gt(nrow(no_band), 0L)
  expect_false(.CHECK1_NO_BAND %in% spec_bands(spec)$labels)
  # Alternate-form students never exit in this sim (contract 3.3), which is a
  # modelling limitation and not a finding -- but they are still counted.
  expect_gt(no_band$N_AT_ENTRY[[1]], 0L)
})

test_that("the split degrades to NULL without a headline, with a note", {
  spec <- check1_spec()
  bundle <- make_check1_bundle(make_check1_roster())
  # A base table whose only component is not the headline: nothing to band.
  other <- setdiff(spec_components(spec), spec_headline(spec))[[1]]
  bundle$elp$COMPONENT <- other

  res <- run_check1(bundle, spec)
  expect_false(is.null(res$exit_curve))          # the overall curve survives
  expect_null(res$exit_curve_by_initial_band)
  expect_match(res$meta$band_note, "not split by initial ELP band")
})

test_that("the curves run against a categorical headline with no bands", {
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  b <- make_check1_elpa21_bundle(spec)
  res <- run_check1(b, spec)

  expect_null(spec_bands(spec))                  # the non-WIDA branch
  expect_false(is.null(res$exit_curve))
  expect_true(all(diff(res$exit_curve$CUM_EXIT_RATE) >= 0))

  bb <- res$exit_curve_by_initial_band
  expect_false(is.null(bb))
  # Bands default to one per level, so the groups are the level labels in
  # the level scale's order.
  expect_true(all(bb$GROUP %in% spec$elp$level_scale$levels))
  expect_identical(unique(bb$GROUP),
                   intersect(spec$elp$level_scale$levels, bb$GROUP))
})

test_that("the curve plots, split and unsplit", {
  res <- check1_sim()
  expect_s3_class(plot_exit_curve(res$exit_curve, check1_spec()), "ggplot")
  expect_s3_class(plot_exit_curve(res$exit_curve_by_initial_band,
                                  check1_spec()), "ggplot")
  expect_s3_class(plot_exit_curve(res$exit_curve), "ggplot")   # spec optional
  expect_error(plot_exit_curve(res$life_table), "matching slot")
})


# ---- the survival companion and the timeline statistics ---------------------

test_that("the survival estimate compounds the at-risk rates", {
  res <- check1_sim()
  s <- res$survival_est
  lt <- res$life_table

  expect_identical(s$YEAR_IN_STATUS, lt$YEAR_IN_STATUS)
  expect_identical(s$A, lt$A)
  # 1 - prod(1 - D), with the final interval's absent D read as no exits.
  d <- ifelse(is.na(s$HAZARD), 0, s$HAZARD)
  expect_equal(s$CUM_EXIT_EST, 1 - cumprod(1 - d))
  expect_true(all(diff(s$CUM_EXIT_EST) >= 0))          # exit is absorbing
  expect_true(all(s$SURVIVING >= 0 & s$SURVIVING <= 1))

  # The estimate can only exceed what was observed: censoring can hide
  # exits, never invent them.
  both <- !is.na(s$CUM_EXIT_OBS)
  expect_true(all(s$CUM_EXIT_EST[both] >= s$CUM_EXIT_OBS[both] - 1e-12))
})

test_that("with nothing censored before the edge, estimate equals observed", {
  # A property of the shipped sim rather than of the method: no K-cohort
  # student disappears, so the two columns coincide until the last row. On a
  # delivery with attrition they separate, and that gap is the point.
  s <- check1_sim()$survival_est
  both <- !is.na(s$CUM_EXIT_OBS)
  expect_equal(s$CUM_EXIT_EST[both], s$CUM_EXIT_OBS[both])
})

test_that("the survival estimate is computed from unsuppressed counts", {
  # Suppression blanks D. If the product read a blank as "no exits" it would
  # flatten the curve silently, so the estimate is built from the counts
  # before suppression, not from the rendered life table.
  spec <- check1_spec()
  spec$policy$suppression$min_cell <- 10000L      # suppress every row
  b <- make_check1_sim_bundle()
  s <- compute_survival_est(b, spec, cohort = make_check1_selected())

  expect_true(all(s$SUPPRESSED))
  expect_true(all(is.na(s$HAZARD)))               # the rendered rate is blank
  # ... and the underlying estimate still rose, because the counts were real.
  plain <- compute_survival_est(b, check1_spec(), cohort = make_check1_selected())
  expect_gt(max(plain$CUM_EXIT_EST), 0.5)
})

test_that("median years to exit is on the same basis as the exit curve", {
  res <- check1_sim()
  y <- res$years_to_50
  curve <- res$exit_curve

  expect_identical(nrow(y), 2L)
  expect_true(all(y$REACHED))
  # The life table's row t accumulates exits dated t + 1, so the median is
  # converted to exit-year terms; that is what makes it agree with the year
  # the curve crosses half. Two slots, one answer.
  crossing <- curve$YEAR_IN_STATUS[which(curve$CUM_EXIT_RATE >= 0.5)[[1]]]
  observed <- y$YEARS_TO_50[y$BASIS == "Observed exits, against the cohort as it started"]
  expect_identical(observed, crossing)
})

test_that("a cohort that never reaches half is reported as such, not as NA years", {
  s <- tibble::tibble(YEAR_IN_STATUS = 1:4,
                      CUM_EXIT_OBS = c(0.05, 0.10, 0.15, 0.20),
                      CUM_EXIT_EST = c(0.05, 0.11, 0.17, 0.22))
  y <- compute_years_to_50(s)
  expect_true(all(is.na(y$YEARS_TO_50)))
  expect_false(any(y$REACHED))
})

test_that("the timeline outcomes nest, partition, and match the flags", {
  res <- check1_sim()
  d <- res$at_max_timeline
  n <- attr(d, "n_cohort")

  expect_identical(n, res$cohorts$N_ENTRY[res$cohorts$SELECTED])
  # Nested rows: on time is a subset of within-maximum, which is a subset of
  # exited at all.
  nested <- d$N[d$NESTED]
  expect_true(all(diff(nested) >= 0))
  # The partition accounts for every student exactly once.
  expect_identical(d$N[3] + d$N[4] + d$N[5], n)
  # ... and the exited row is the exit curve's final value.
  expect_equal(d$PCT[3], max(res$exit_curve$CUM_EXIT_RATE))
})

test_that("a zero outcome is shown, not suppressed", {
  # Nobody in this cohort disappears. Blanking that would read as "hidden"
  # when the finding is "this did not happen", and a count of none discloses
  # no student.
  d <- check1_sim()$at_max_timeline
  disappeared <- d[grepl("Disappeared", d$OUTCOME), ]
  expect_identical(disappeared$N, 0L)
  expect_false(disappeared$SUPPRESSED)
})

# ---- Model 2 ----------------------------------------------------------------

test_that("Model 2 is NULL without the academic table", {
  res <- check1_sim_no_academic()

  expect_null(res$model2)
  expect_match(res$meta$model2_note, "No academic table")
  # Model 1 is unaffected, and so is the half of the index it feeds.
  expect_false(is.null(res$life_table))
  expect_false(is.null(res$retention_share))
  expect_true(is.na(res$selection_strength$value))
  expect_true(all(is.na(res$grade_weights$WEIGHT)))
})

test_that("Model 2 fits the person-period frame and reports its ratio", {
  m <- check1_sim()$model2

  expect_identical(m$subject, spec_academic_subjects(check1_spec())[[1]])
  expect_true(is.finite(m$ratio))
  expect_length(m$ci, 2L)
  expect_lt(m$ci[[1]], m$ratio)
  expect_gt(m$ci[[2]], m$ratio)
  expect_gt(m$n_events, 0L)
  expect_lte(m$n_person_periods, m$n_person_periods_all)
  expect_true(all(c("INITIAL_ELP_Z", "ACADEMIC_LAG_Z") %in% m$coefficients$TERM))
})

test_that("rows the model cannot use are counted and never imputed", {
  m <- check1_sim()$model2
  d <- m$dropped

  expect_identical(sum(d$N) + m$n_person_periods, m$n_person_periods_all)
  # The missing assessment year has its own line, separate from the grades
  # content testing never reaches.
  gap_row <- grep("no assessment anywhere", d$REASON)
  expect_length(gap_row, 1L)
  expect_gt(d$N[gap_row], 0L)
  expect_match(m$note, "never imputed")
})

test_that("no fitted model uses a score from the year it predicts", {
  # THE guardrail on this stage (2026-09-21). The exit rule is applied to
  # that year's ELP result, so a contemporaneous score is the decision
  # restated: it separates the fit, and -- far worse -- it conditions every
  # other covariate on the outcome's own cause, which drove Model 2's
  # achievement ratio to 1 and left the skimming test unable to detect
  # skimming. Every covariate must be fixed at entry or lagged.
  b <- make_check1_sim_bundle()
  spec <- check1_spec()
  pp <- .check1_person_period(b, spec, make_check1_selected(), "K",
                              subject = spec_academic_subjects(spec)[[1]])

  # INITIAL_ELP is one value per student, so it cannot be that year's score.
  per_student <- tapply(pp$INITIAL_ELP, pp$STUDENT_ID,
                        function(x) length(unique(x[!is.na(x)])))
  expect_true(all(per_student <= 1L))
  expect_false("ELP_SCALE" %in% names(pp))

  # And the consequence, which is the reason the guardrail exists: nothing
  # separates, so every ratio is a quantity a reader can use.
  res <- check1_sim()
  expect_false(res$model1$quasi_separated)
  expect_false(res$model1_baseline$quasi_separated)
  expect_false(res$model2$quasi_separated)
})

test_that("the skimming test can register achievement either way", {
  # The defect this replaced made the answer a foregone conclusion: beside a
  # same-year language score, lagged achievement came out at 1.05 whatever
  # was true. The test has power when the ratio is free to move, which is
  # what is asserted here -- not any particular value, since on sim data the
  # value is a generator parameter (sim_contract.md 5.2-5.3).
  m <- check1_sim()$model2
  lag <- m$coefficients[m$coefficients$TERM == "ACADEMIC_LAG_Z", ]

  expect_true(is.finite(lag$RATIO))
  expect_true(is.finite(lag$CI_LO) && is.finite(lag$CI_HI))
  expect_lt(lag$CI_HI / lag$CI_LO, 10)     # an identified, usable interval
})

test_that("the Type 3 covariate slot is NULL under a Type 1 spec", {
  expect_null(check1_sim()$type3_covariate)
  expect_false(spec_has_type(check1_spec(), 3))
})

test_that("a Type 3 spec puts its non-ELP criterion in the model", {
  spec <- make_check1_type3_spec()
  expect_true(spec_has_type(spec, 3))

  res <- run_check1(make_check1_sim_bundle(), spec)
  t3 <- res$type3_covariate

  expect_false(is.null(t3))
  expect_identical(t3$criterion_subject, spec_academic_subjects(spec)[[2]])
  expect_true("CRITERION_Z" %in% t3$model$coefficients$TERM)
  # The circularity this creates downstream is stated, not left implicit.
  expect_match(t3$caption, "circular")
})

# ---- the experimental index -------------------------------------------------

test_that("the weighting index renders only with an experimental label", {
  res <- check1_sim()

  expect_true(all(res$grade_weights$EXPERIMENTAL))
  expect_true(res$selection_strength$experimental)
  expect_equal(res$selection_strength$value, abs(res$model2$ratio - 1))
  # Retention share is the life table's A against the cohort.
  expect_equal(res$retention_share$RETENTION_SHARE,
               res$life_table$A / res$life_table$A[[1]])
  expect_identical(res$retention_share$RETENTION_SHARE[[1]], 1)
})

test_that("the hard floor zeroes a year with too few students left", {
  spec <- check1_spec()
  spec$policy$suppression$min_cell <- 1000L       # a floor the tail fails
  b <- make_check1_sim_bundle()
  s <- compute_survival_est(b, spec, cohort = make_check1_selected())
  w <- compute_grade_weights(s, check1_sim()$model2, spec)$grade_weights

  expect_true(any(w$BELOW_FLOOR))
  expect_true(all(w$WEIGHT[w$BELOW_FLOOR] == 0))
  expect_true(all(w$WEIGHT[!w$BELOW_FLOOR] > 0))
})

test_that("every Check 1 slot the instruction file names is now filled", {
  res <- check1_sim()
  named <- c("cohorts", "life_table", "exit_curve",
             "exit_curve_by_initial_band", "survival_est", "years_to_50",
             "at_max_timeline", "model2", "retention_share",
             "selection_strength", "grade_weights")
  for (slot in named) expect_false(is.null(res[[slot]]), info = slot)
  expect_length(res$meta$pending, 0L)
})

# ---- Model 1's fitted half ---------------------------------------------------

test_that("the unconditional fit reproduces the life table exactly", {
  # The whole reason to fit a model with no covariate: being saturated, its
  # fitted hazards ARE column D. That identity is what ties the fitted half
  # to the descriptive half -- otherwise they are two accounts of one cohort
  # with nothing holding them together.
  res <- check1_sim()
  m <- res$model1
  lt <- res$life_table

  yrs <- as.integer(sub("^factor\\(YEAR_IN_STATUS\\)", "",
                        grep("YEAR_IN_STATUS", m$coefficients$TERM,
                             value = TRUE)))
  intercept <- m$coefficients$ESTIMATE[m$coefficients$TERM == "(Intercept)"]
  # The reference level is the earliest year the frame still holds, and a
  # year with no exits is not in the frame at all -- so it is the first year
  # anyone exited, not the first year in the table.
  first <- min(lt$YEAR_IN_STATUS[!is.na(lt$B) & lt$B > 0])

  # Tolerance is glm's own convergence epsilon, not a fudge: the identity is
  # exact in the algebra and the fit stops iterating at about eight
  # significant figures.
  inv_logit <- function(x) 1 / (1 + exp(-x))
  expect_equal(inv_logit(intercept), lt$D[[first]], tolerance = 1e-6)
  for (i in seq_along(yrs)) {
    fitted <- inv_logit(intercept +
                          m$coefficients$ESTIMATE[m$coefficients$TERM ==
                            paste0("factor(YEAR_IN_STATUS)", yrs[i])])
    expect_equal(fitted, lt$D[[yrs[i]]], tolerance = 1e-6)
  }
})

test_that("the unconditional fit carries no covariate and keeps every row", {
  m <- check1_sim()$model1

  expect_false(m$baseline_elp)
  expect_true(is.na(m$ratio))                    # nothing to report a ratio for
  expect_false(any(m$coefficients$FOCAL))
  expect_false(any(grepl("ELP|ACADEMIC", m$coefficients$TERM)))
  expect_true(any(grepl("YEAR_IN_STATUS", m$coefficients$TERM)))
  # It asks nothing of a student but being at risk, so the only rows it loses
  # are years with no exits to identify a term.
  expect_identical(nrow(m$dropped), 1L)
  expect_match(m$dropped$REASON, "no exits")
  expect_identical(sum(m$dropped$N) + m$n_person_periods,
                   m$n_person_periods_all)
})

test_that("the entry-score fit reports a ratio on where students started", {
  m <- check1_sim()$model1_baseline

  expect_true(m$baseline_elp)
  expect_true(is.finite(m$ratio))
  expect_lt(m$ci[[1]], m$ratio)
  expect_gt(m$ci[[2]], m$ratio)
  expect_true("INITIAL_ELP_Z" %in% m$coefficients$TERM)
  # The lag is Model 2's term; this fit is the step before it.
  expect_false("ACADEMIC_LAG_Z" %in% m$coefficients$TERM)
  expect_match(m$note, "never imputed")
  expect_identical(sum(m$dropped$N) + m$n_person_periods,
                   m$n_person_periods_all)
})

test_that("each fit keeps the rows its own covariates need, and no fewer", {
  # Requiring a prior-year content score costs rows that the earlier
  # questions never needed, which is why the three are fitted separately
  # rather than one being read off another.
  res <- check1_sim()

  expect_gt(res$model1$n_person_periods, res$model1_baseline$n_person_periods)
  expect_gt(res$model1_baseline$n_person_periods, res$model2$n_person_periods)
  expect_identical(res$model1$n_person_periods_all,
                   res$model2$n_person_periods_all)
  expect_gt(sum(res$model2$dropped$N[grep("prior-year",
                                          res$model2$dropped$REASON)]), 0L)
})

test_that("the nested note compares the two fits and names the row gap", {
  res <- check1_sim()
  note <- check1_nested_note(res)

  expect_false(is.na(note))
  expect_match(note, "do not use the same rows")
  expect_match(note, format(res$model2$n_person_periods, big.mark = ","),
               fixed = TRUE)
  # With only one of the two fitted there is nothing to compare.
  expect_true(is.na(check1_nested_note(check1_sim_no_academic())))
})

test_that("intervals are clustered on the student, not on the row", {
  # A student contributes about four and a half rows. Model-based errors
  # would count each as an independent observation; the CR1 sandwich counts
  # students. It widens the interval here, and the note says so.
  res <- check1_sim()
  m <- res$model1_baseline
  focal <- m$coefficients[m$coefficients$TERM == "INITIAL_ELP_Z", ]

  b <- make_check1_sim_bundle()
  spec <- check1_spec()
  pp <- .check1_person_period(b, spec, make_check1_selected(), "K")
  pp <- pp[!is.na(pp$INITIAL_ELP), ]
  pp <- .check1_drop_eventless(pp)$frame
  pp$INITIAL_ELP_Z <- .check1_z(pp$INITIAL_ELP)
  plain <- glm(EXITED_THIS_YEAR ~ factor(YEAR_IN_STATUS) + INITIAL_ELP_Z,
               data = pp, family = binomial())
  naive <- summary(plain)$coefficients["INITIAL_ELP_Z", "Std. Error"]

  expect_equal(focal$ESTIMATE,
               unname(coef(plain)["INITIAL_ELP_Z"]))   # estimates untouched
  expect_gt(focal$SE, naive)                            # intervals widened
  expect_match(check1_model1_baseline_note(res), "clustered on the student")
})

test_that("a headline with no scale keeps the fit that needs no score", {
  # ELPA21 reports a determination: a level and nothing behind it. The two
  # models carrying an ELP term have no covariate to fit -- but the
  # unconditional one never needed a score, and losing it would cost the
  # stage its link between the life table and the fitted half for no reason.
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  res <- run_check1(make_check1_elpa21_bundle(spec), spec)

  expect_false(is.null(res$model1))
  expect_null(res$model1_baseline)
  expect_null(res$model2)
  expect_match(res$meta$model1_note, "no scale score")
  expect_false(is.null(res$life_table))
  expect_false(is.null(res$survival_est))

  # D2: the box lists what ran. The score no model could use is not claimed
  # as a field that did any work.
  fields <- res$meta$data_fields
  expect_false("SCALE_SCORE" %in% fields$FIELD[fields$TABLE == "base"])
  expect_true("PERSON_PERIOD" %in% fields$FIELD)   # the unconditional fit's
})

test_that("a cohort with no exits degrades to NULL rather than erroring", {
  # Nobody in this fixture ever exits, so there is no model to fit. A stage
  # must never crash on a thin delivery (CLAUDE.md 8).
  spec <- check1_spec()
  res <- run_check1(make_check1_bundle(make_check1_roster()), spec)

  expect_null(res$model1)
  expect_match(res$meta$model1_note, "cannot be fitted")
  expect_false(is.null(res$life_table))
})

test_that("an unfittable model raises its own condition, not a bare error", {
  # run_check1() catches exactly this class. A blanket tryCatch() there would
  # swallow real bugs, so the class has to exist and be the one raised.
  spec <- check1_spec()
  bundle <- make_check1_bundle(make_check1_roster())

  expect_error(
    compute_model1(bundle, spec,
                   cohort = compute_cohorts(bundle, spec)$COHORT_LABEL[1]),
    class = "check1_unfittable")
})

# ---- the confidence band -----------------------------------------------------

test_that("Greenwood's band brackets the estimate and stays in [0, 1]", {
  s <- check1_sim()$survival_est

  expect_true(all(s$CI_LO <= s$CUM_EXIT_EST + 1e-12, na.rm = TRUE))
  expect_true(all(s$CI_HI >= s$CUM_EXIT_EST - 1e-12, na.rm = TRUE))
  expect_true(all(s$CI_LO >= 0 & s$CI_HI <= 1, na.rm = TRUE))
  expect_true(all(s$SE >= 0, na.rm = TRUE))
  expect_false(any(is.nan(c(s$SE, s$CI_LO, s$CI_HI))))

  # Before any exit the estimate is 0 with no variance: a band there would be
  # inventing uncertainty about something the data settles.
  first <- which(s$CUM_EXIT_EST == 0)
  if (length(first)) {
    expect_equal(s$CI_LO[first], rep(0, length(first)))
    expect_equal(s$CI_HI[first], rep(0, length(first)))
  }
})

test_that("Greenwood's variance matches the formula on a worked table", {
  # Two intervals, by hand: sigma^2 = sum B / (A (A - B)).
  A <- c(100, 80)
  B <- c(20, 10)
  surviving <- cumprod(1 - B / A)
  out <- .check1_greenwood(A, B, surviving)

  sigma2 <- cumsum(B / (A * (A - B)))
  expect_equal(out$SE, surviving * sqrt(sigma2))

  # ... and the interval is the complementary log-log one, not a symmetric
  # band on the share, which would run outside [0, 1] at the ends.
  half <- stats::qnorm(0.975) * sqrt(sigma2) / -log(surviving)
  expect_equal(out$CI_LO, 1 - surviving^exp(-half))
  expect_equal(out$CI_HI, 1 - surviving^exp(half))
})

test_that("only the estimate is banded, never the observed share", {
  # The observed column counts against the starting cohort, which includes
  # students nobody is following any more. It is not an estimator, so it gets
  # no interval -- a structural guard, because adding one would be easy and
  # wrong.
  s <- check1_sim()$survival_est

  expect_true(all(c("SE", "CI_LO", "CI_HI") %in% names(s)))
  expect_false(any(grepl("OBS", c("SE", "CI_LO", "CI_HI"))))
  expect_false(any(c("CUM_EXIT_OBS_LO", "CUM_EXIT_OBS_HI") %in% names(s)))
})

test_that("a suppressed row suppresses its band with it", {
  spec <- check1_spec()
  spec$policy$suppression$min_cell <- 10000L
  s <- compute_survival_est(make_check1_sim_bundle(), spec,
                            cohort = make_check1_selected())

  expect_true(all(s$SUPPRESSED))
  expect_true(all(is.na(s$CI_LO)))
  expect_true(all(is.na(s$CI_HI)))
})

test_that("the banded curve plots, and the split curve ignores the band", {
  res <- check1_sim()

  banded <- plot_exit_curve(res$exit_curve, check1_spec(),
                            survival_est = res$survival_est)
  expect_s3_class(banded, "ggplot")
  expect_true(any(vapply(banded$layers,
                         function(l) inherits(l$geom, "GeomRibbon"), TRUE)))

  # The estimate is a whole-cohort number; drawing one band behind several
  # band-specific curves would attach it to each of them.
  split <- plot_exit_curve(res$exit_curve_by_initial_band, check1_spec(),
                           survival_est = res$survival_est)
  expect_false(any(vapply(split$layers,
                          function(l) inherits(l$geom, "GeomRibbon"), TRUE)))
})
