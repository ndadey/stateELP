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

  expect_setequal(fields$FIELD,
                  .check1_fields_for(c("cohorts", "life_table"))$FIELD)
  expect_true(all(c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS") %in%
                    fields$FIELD[fields$KIND == "required"]))
  # A slot that did not run drops its rows, so the box grows with later
  # passes rather than promising fields nothing read.
  expect_false(any(grepl("model2", fields$SLOTS)))
  expect_true(all(fields$PRESENT[fields$TABLE == "roster"]))
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
  roster <- make_check1_roster()
  nj <- read_el_spec("NJ_elp_2023")

  fields <- .check1_data_fields(roster, nj, c("cohorts", "life_table"))
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

# ---- later passes ------------------------------------------------------------

test_that("the cumulative exit rate is reported as a lower bound", {
  skip("check1 later pass: $exit_curve, $survival_est")
})

test_that("Model 2 is NULL without the academic table", {
  skip("check1 later pass: $model2")
})

test_that("2021 person-period rows drop from Model 2 and are never imputed", {
  # No 2020 academic score exists, so the lagged-ELA covariate is NA there.
  skip("check1 later pass: $model2")
})

test_that("the weighting index renders only with an experimental label", {
  # Untested against the Indiana pipeline; goes nowhere mechanically.
  skip("check1 later pass: $retention_share, $selection_strength, $grade_weights")
})

test_that("the Type 3 covariate slot is NULL under a Type 1 spec", {
  skip("check1 later pass: $type3_covariate")
})
