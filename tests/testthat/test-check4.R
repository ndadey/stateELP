# Check 4 -- Decision consistency + dependence. run_check4(data, spec).
# Sample: identical to Check 3's paired cross-section, by grade and year.
# Built per internal_docs/steps/8_check4_consistency.md.

test_that("Youden's J is computed, not raw consistency relabelled", {
  # THE checkpoint for this stage, and a demonstrated gap rather than a
  # theoretical one. The J-vs-consistency fixture belongs to this session: a
  # small paired sample with a proficiency base rate well below 50%, where the
  # raw-consistency peak and the J optimum demonstrably diverge. The code must
  # return the J optimum.
  skip("check4 not built: $youden_j, $j_optimal, and the skewed fixture with it")
})

test_that("the consistency curve is reported across candidate cuts", {
  skip("check4 not built: $consistency_curve")
})

test_that("the output is a candidate region, not a single number", {
  skip("check4 not built: $candidate_region")
})

test_that("the region is annotated with per-grade reliability from Check 1", {
  skip("check4 not built: $candidate_region")
})

test_that("Prep 3's error tolerance renders beside the J-optimal point", {
  # J assumes the two errors are equally costly; a stated asymmetry is a
  # documented reason to land off it -- a values input, not a departure from
  # the evidence.
  skip("check4 not built: $j_optimal captions")
})

test_that("the dependence slot is NULL and the document says turnkey vs custom", {
  # The copula / Kendall's-tau analysis has no package support yet.
  skip("check4 not built: $dependence")
})

test_that("the policy-as-whole slot is NULL under a Type 1 spec", {
  skip("check4 not built: $policy_as_whole")
})
