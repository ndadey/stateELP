# Check 2 -- On-time vs prolonged exiters. run_check2(data, spec, start_grade).
# Sample: the same cohort as Check 1.
# Built per internal_docs/steps/6_check2_flows.md.
#
# The nested indicators and the on-time/prolonged split are already tested in
# test-utils.R; what is left here is the crosstab and its captions.

test_that("the crosstab's columns sum to All", {
  # Disappeared is its own column and is never folded into either group.
  skip("check2 not built: $crosstab")
})

test_that("suppressed crosstab cells are NA, never dropped rows", {
  skip("check2 not built: $crosstab")
})

test_that("one K cohort clears every crosstab cell at the shipped scale", {
  # k_cohort_n = 2400 is set by this constraint; never lower min_cell or pool
  # grades to make cells appear.
  skip("check2 not built: $crosstab")
})

test_that("captions say the indicators are window-relative", {
  skip("check2 not built: $crosstab captions")
})

test_that("the academic summary is NULL without the academic table", {
  skip("check2 not built: $academic_summary")
})

test_that("cohort mean ELP is never plotted without conditioning on exit group", {
  # Slide-81 caution.
  skip("check2 not built: $trajectories")
})

test_that("the blocked-groups slot is NULL under a Type 1 spec", {
  skip("check2 not built: $blocked_groups")
})
