# Check 3 -- Academic patterns: box plots + logistic. run_check3(data, spec).
# Sample: cross-sectional, ALL ELs per grade and year -- not the K cohort.
# Built per internal_docs/steps/7_check3_academics.md.

test_that("results are keyed by grade and never pooled across grades", {
  skip("check3 not built: structure test on $summary_table")
})

test_that("the grade loop is driven by the spec, not the observed grade span", {
  # Driving it from the data renders empty panels for K, 1, 2 and 12.
  skip("check3 not built: $coverage")
})

test_that("the join runs academic left-joined to spine, not the reverse", {
  # A spine-left join silently drops the former and never-EL reference groups,
  # because they have no ELP rows. Row-count guard.
  skip("check3 not built: $box_data")
})

test_that("the 2020 panel is NULL -- a missing year, not an empty one", {
  skip("check3 not built: $box_data")
})

test_that("alternate-academic takers are counted in n and excluded from the distribution", {
  skip("check3 not built: $summary_table")
})

test_that("the never-EL n is reported weighted or labelled as a weighted sample", {
  skip("check3 not built: $summary_table")
})

test_that("the landing band is where the share at or above the cut reaches 50%", {
  skip("check3 not built: $landing")
})

test_that("the logistic reports the scale score where fitted probability crosses 50%", {
  skip("check3 not built: $logistic")
})

test_that("fairness-check output is captioned as a generator parameter on sim data", {
  # Every EL/EO gap in the sim is a parameter; the machinery is real, the
  # number is by construction.
  skip("check3 not built: $fairness_boxes, $fairness_logistic")
})

test_that("contested-zone models are NULL under a Type 1 spec", {
  skip("check3 not built: $contested_zone_models")
})
