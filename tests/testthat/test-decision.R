# Decision & next steps. run_decision(results, spec) -- a named list of prior
# stage results, not a data bundle.
# Built per internal_docs/steps/9_decision_synthesis.md.

test_that("run_decision() takes prior stage results", {
  skip("decision not built")
})

test_that("the evidence map lists every stage with its sample and run status", {
  skip("decision not built: $evidence_map")
})

test_that("the evidence map degrades gracefully when a stage has not run", {
  # The stage renders whatever has run; it is only meaningful last.
  skip("decision not built: $evidence_map")
})

test_that("the output is a defensible region, never a single true number", {
  skip("decision not built: $defensible_region")
})

test_that("the error-tolerance answer and any fairness gap are named as drivers", {
  skip("decision not built: $defensible_region")
})

test_that("a monitoring plan renders whether or not anything changes", {
  skip("decision not built: $monitoring_plan")
})

test_that("Check 1's grade weights carry their experimental label into the map", {
  skip("decision not built: $evidence_map")
})
