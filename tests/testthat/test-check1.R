# Check 1 -- Cohort survival analysis. run_check1(data, spec, start_grade = "K").
# Sample: one stable entry cohort, LEFT_TRUNCATED excluded by construction.
# Built per internal_docs/steps/5_check1_survival.md.
#
# The exit-dating convention this stage rests on is ALREADY tested, in
# test-utils.R, against the deck's A-J example and its counter-test.

test_that("the slide-52 life table reproduces D, E and F to two decimals", {
  # Test the CELLS, not the callouts: two A cells in the slide are off by one
  # (19,306 vs 19,305; 8,339 vs 8,340) so the recurrence is checked at +/-1
  # tolerance, and one callout has a typo (0.06 for 0.58).
  skip("check1 not built: $life_table, and the slide-52 fixture with it")
})

test_that("the life table's last row has an empty B and C equal to A", {
  skip("check1 not built: $life_table")
})

test_that("no mean or average time in status is computed anywhere", {
  skip("check1 not built")
})

test_that("cohorts are derived from the panel range, never year literals", {
  skip("check1 not built: $cohorts")
})

test_that("the cumulative exit rate is reported as a lower bound", {
  skip("check1 not built: $exit_curve, $survival_est")
})

test_that("Model 2 is NULL without the academic table", {
  skip("check1 not built: $model2")
})

test_that("2021 person-period rows drop from Model 2 and are never imputed", {
  # No 2020 academic score exists, so the lagged-ELA covariate is NA there.
  skip("check1 not built: $model2")
})

test_that("the weighting index renders only with an experimental label", {
  # Untested against the Indiana pipeline; goes nowhere mechanically.
  skip("check1 not built: $retention_share, $selection_strength, $grade_weights")
})

test_that("the Type 3 covariate slot is NULL under a Type 1 spec", {
  skip("check1 not built: $type3_covariate")
})
