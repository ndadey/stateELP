# Prep 1 -- Policy documentation. run_prep1(spec); a data argument is an error.
# Built in its own session per internal_docs/steps/2_prep1_policies.md.

test_that("run_prep1() takes the spec alone", {
  skip("prep1 not built: run_prep1(spec) does not exist yet")
})

test_that("the policy table renders every value with its verification status", {
  skip("prep1 not built: $policy_table")
})

test_that("the components table renders kind, measures and from-weights", {
  skip("prep1 not built: $components_table")
})

test_that("every exit pathway renders with who holds decision authority", {
  skip("prep1 not built: $exit_table")
})

test_that("a discretionary or committee pathway is flagged distinctly", {
  # The Indiana review found selection effects concentrate there, and
  # Checks 1-2 track it separately.
  skip("prep1 not built: $exit_table discretionary flag")
})

test_that("Type identification renders with the typology definitions", {
  # This gates every Additional check downstream.
  skip("prep1 not built: $types")
})

test_that("exit dating renders as an explicit confirm question", {
  skip("prep1 not built: $confirm_questions")
})
