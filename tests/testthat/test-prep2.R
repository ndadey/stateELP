# Prep 2 -- Population analysis. run_prep2(data, spec).
# Sample: full statewide EL population, current plus former, >= 5 years.
# Built per internal_docs/steps/3_prep2_population.md.

test_that("run_prep2() returns the documented slots", {
  skip("prep2 not built")
})

test_that("EL share of enrollment is weighted, and NULL without never-ELs", {
  # Unweighted this reads 52.7% on the sim and nobody will believe the rest
  # of the document.
  skip("prep2 not built: $el_share")
})

test_that("district bars carry the two-thirds-share and <100 reference lines", {
  skip("prep2 not built: $districts (deck slides 33-34)")
})

test_that("district bands come from the spec, never the sim's tertile column", {
  skip("prep2 not built: $district_bands")
})

test_that("alternate-form students are excluded from the initial-ELP bars", {
  skip("prep2 not built: $initial_elp -- excluded with a rendered note")
})

test_that("resist-the-urge-to-average renders as a guardrail", {
  skip("prep2 not built: slide-38 guardrail")
})

test_that("the criteria crosswalk is NULL under a Type 1 spec", {
  # Type-gated: Types 2-6 only. The SIM spec is Type 1 and skips it.
  skip("prep2 not built: $criteria_crosswalk, $contested_zone")
})
