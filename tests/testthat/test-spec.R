# Spec layer: constructor, validator, the three shipped instantiations, and the
# guardrail that keeps component names out of the rest of the package.

# ---- the three shipped specs -------------------------------------------------

test_that("all three shipped specs construct and validate", {
  for (id in c("00_SIM_EL_Spec", "NJ_elp_2023", "00_ELPA21_EL_Spec")) {
    spec <- read_el_spec(id)
    expect_s3_class(spec, "el_spec")
    expect_silent(validate_el_spec(spec))
  }
})

test_that("SIM spec matches sim_contract.md v0.2.0", {
  spec <- read_el_spec("00_SIM_EL_Spec")

  # 4.3 headline, 4.5 bands, 2.2 P block, 5.4 cut convention.
  expect_identical(spec_headline(spec), "overall")
  expect_identical(spec_bands(spec)$breaks, c(1, 2, 3, 4, 4.5, 5, 7))
  expect_length(spec_bands(spec)$labels, 6L)
  expect_identical(spec_min_cell(spec), 10L)
  expect_identical(spec_max_years(spec), 6L)
  expect_identical(spec$academic$ELA$cuts, c(700, 735, 750, 790))
  expect_identical(spec$academic$ELA$op, ">=")
  expect_identical(spec$academic$ELA$grades, as.character(3:11))

  # Type 1 means every Type-gated Additional check is skipped.
  expect_identical(spec_exit_types(spec), 1L)
  expect_false(spec_has_type(spec, 2:6))

  # Synthetic data must never be mistaken for a state's values.
  expect_identical(spec$verification$status, "synthetic")
})

test_that("ELPA21 spec exercises the non-WIDA branches", {
  spec <- read_el_spec("00_ELPA21_EL_Spec")

  # A categorical headline that reports a level and no scale score.
  expect_identical(spec_headline(spec), "determination")
  expect_identical(spec$elp$level_scale$type, "categorical")
  expect_identical(spec$elp$components$determination$measures, "level")
  # ... and the mirror case: a scale score with no level.
  expect_identical(spec$elp$components$overall$measures, "scale")
  # No declared bands: the package default of one band per level applies.
  expect_null(spec_bands(spec))
  # Type 6 gates the Additional checks on.
  expect_identical(spec_exit_types(spec), 6L)
  expect_true(spec_has_type(spec, 6))
})

test_that("NJ spec migrated: renamed components keep their native labels", {
  spec <- read_el_spec("NJ_elp_2023")

  # `literacy` and `composite_overall` are not vocabulary terms; they map.
  expect_true(all(c("written", "overall") %in% spec_components(spec)))
  expect_false(any(c("literacy", "composite_overall") %in% spec_components(spec)))
  expect_match(spec$elp$components$written$native_name, "Literacy")

  # policy_benchmark was an exit rule, not a level label: it did not survive
  # into level_scale.
  expect_null(spec$elp$level_scale$policy_benchmark)
  # scale_scores -> cuts, NULL in the shell, and never fills a level.
  expect_null(spec$elp$cuts)
  # cohort_anchor_grade is gone; the starting cohort is a function argument.
  expect_null(spec$years)
  expect_identical(spec$verification$status, "unverified")
})

# ---- the policy / measurement boundary --------------------------------------

test_that("every policy value carries a source", {
  for (id in el_spec_files()) {
    if (id == "00_Demographics_Lookups") next
    spec <- read_el_spec(id)
    for (nm in names(spec$policy)) {
      expect_true(is.character(spec$policy[[nm]]$source),
                  info = paste0(id, ": policy$", nm, " has no source"))
    }
  }
})

test_that("a policy value without a source is rejected", {
  expect_error(
    make_spec_base(policy = list(suppression = list(min_cell = 10L,
                                                    source = NULL))),
    "must carry a `source`"
  )
})

test_that("policy fields are rejected under elp", {
  expect_error(
    make_spec_base(elp = list(timelines = list(max_years_to_exit = 6L))),
    "Policy field\\(s\\) found under `elp`"
  )
})

test_that("a field outside the fixed policy list is rejected", {
  expect_error(
    make_spec_base(policy = list(exit_threshold = list(source = "x"))),
    "outside the fixed list"
  )
})

# ---- the component model -----------------------------------------------------

test_that("a component outside the vocabulary is rejected", {
  expect_error(
    make_spec_base(elp = list(components = list(
      literacy = list(kind = "composite", measures = "level")))),
    "not in the controlled vocabulary"
  )
})

test_that("the headline must name a declared component", {
  expect_error(make_spec_base(elp = list(headline = "expressive")),
               "is not one of `elp\\$components`")
})

test_that("`from` weights must reference declared components", {
  expect_error(
    make_spec_base(elp = list(components = list(
      overall = list(kind = "composite", measures = "level",
                     from = c(expressive = 1))))),
    "references unknown component"
  )
})

test_that("bands must be well formed", {
  expect_error(
    make_spec_base(elp = list(bands = list(breaks = c(1, 3, 5),
                                           labels = c("a", "b", "c")))),
    "length\\(breaks\\) must be length\\(labels\\) \\+ 1"
  )
  expect_error(
    make_spec_base(elp = list(bands = list(breaks = c(1, 5, 3, 7),
                                           labels = c("a", "b", "c")))),
    "strictly increasing"
  )
})

# ---- exit rule: validated, never evaluated -----------------------------------

test_that("an exit term must reference a real component and one of its measures", {
  expect_error(
    make_spec_base(policy = list(exit = list(pathways = list(
      list(rule = "x", terms = list(list(component = "expressive",
                                         measure = "level",
                                         op = ">=", value = 4))))))),
    "must name one of `elp\\$components`"
  )
  # `overall` in the fixture reports scale and level; ask for a measure it
  # does not report.
  expect_error(
    make_spec_base(elp = list(components = list(
      overall = list(kind = "composite", measures = "level"))),
      policy = list(exit = list(pathways = list(
        list(rule = "x", terms = list(list(component = "overall",
                                           measure = "scale",
                                           op = ">=", value = 400))))))),
    "is not reported by component"
  )
})

test_that("the exit rule is never evaluated, only checked", {
  # A term whose value could not possibly be met still validates: the package
  # records the rule, it does not apply it.
  expect_s3_class(
    make_spec_base(policy = list(exit = list(pathways = list(
      list(rule = "overall level >= 99", terms = list(
        list(component = "overall", measure = "level",
             op = ">=", value = 99))))))),
    "el_spec"
  )
})

# ---- the Type declaration cross-check ---------------------------------------

test_that("Type 6 with a single overall term is an error", {
  # This is the cross-check CLAUDE.md 5 names explicitly.
  expect_error(
    make_spec_base(policy = list(exit = list(types = 6L))),
    "Type 6 declared"
  )
})

test_that("Type 1 must be a single term on the headline", {
  expect_error(
    make_spec_base(policy = list(exit = list(
      types = 1L,
      pathways = list(
        list(rule = "two terms", terms = list(
          list(component = "overall", measure = "level", op = ">=", value = 4.5),
          list(component = "reading", measure = "level", op = ">=", value = 3.5)
        ))
      )))),
    "Type 1 declared"
  )
})

test_that("Type 2 needs a pathway with two or more terms", {
  expect_error(make_spec_base(policy = list(exit = list(types = 2L))),
               "Type 2 declared")
  expect_s3_class(
    make_spec_base(policy = list(exit = list(
      types = 2L,
      pathways = list(
        list(rule = "composite plus floor", terms = list(
          list(component = "overall", measure = "level", op = ">=", value = 4.4),
          list(component = "written", measure = "level", op = ">=", value = 3.5)
        ))
      ))),
      elp = list(components = list(
        written = list(kind = "composite", measures = c("scale", "level"),
                       from = c(reading = 0.5, writing = 0.5))))),
    "el_spec"
  )
})

test_that("Type 3 needs a term drawn from a non-ELP source", {
  expect_error(make_spec_base(policy = list(exit = list(types = 3L))),
               "Type 3 declared")
  expect_s3_class(
    make_spec_base(policy = list(exit = list(
      types = 3L,
      pathways = list(
        list(rule = "composite plus ELA", terms = list(
          list(component = "overall", measure = "level", op = ">=", value = 4.0),
          list(component = "ELA", from = "academic", measure = "level",
               op = ">=", value = 3)
        ))
      )))),
    "el_spec"
  )
})

test_that("Type 5 needs a second pathway or a discretionary flag", {
  expect_error(make_spec_base(policy = list(exit = list(types = 5L))),
               "Type 5 declared")
  expect_s3_class(
    make_spec_base(policy = list(exit = list(
      types = 5L,
      pathways = list(
        list(rule = "auto", terms = list(
          list(component = "overall", measure = "level", op = ">=", value = 5.0))),
        list(rule = "committee", discretionary = TRUE, terms = list(
          list(component = "overall", measure = "level", op = ">=", value = 4.3)))
      )))),
    "el_spec"
  )
})

test_that("types is required and bounded to the typology", {
  expect_error(make_spec_base(policy = list(exit = list(types = NULL))),
               "`policy\\$exit\\$types` is required")
  expect_error(make_spec_base(policy = list(exit = list(types = 7L))),
               "within 1:6")
  expect_length(el_exit_typology(), 6L)
})

# ---- decided conventions -----------------------------------------------------

test_that("exit dating is pinned to the first former year", {
  expect_error(
    make_spec_base(policy = list(timelines = list(exit_dating = "last_current_year"))),
    "must be \"first_former_year\""
  )
})

test_that("academic cuts are level minimums read with >=", {
  expect_error(
    make_spec_base(academic = list(ELA = list(op = ">"))),
    "must be \">=\""
  )
  expect_error(
    make_spec_base(academic = list(ELA = list(cuts = c(790, 700)))),
    "strictly increasing"
  )
})

# ---- the abstraction guardrail ----------------------------------------------

test_that("no component name is hardcoded outside the spec file", {
  # R/spec.R defines the vocabulary, so it is the one exemption -- the same
  # carve-out the generator makes for its own spec block.
  files <- list.files(test_path("..", ".."), pattern = "\\.R$",
                      full.names = TRUE, recursive = TRUE)
  files <- files[grepl("[/\\\\]R[/\\\\]", files)]
  files <- files[!grepl("spec\\.R$", files)]
  skip_if(length(files) == 0, "package sources not reachable from the test dir")

  vocab <- el_component_vocabulary()
  hits <- character()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)]          # drop comment lines
    for (cp in vocab) {
      pat <- paste0("\"", cp, "\"")
      if (any(grepl(pat, lines, fixed = TRUE))) {
        hits <- c(hits, paste0(basename(f), ": ", pat))
      }
    }
  }
  expect_identical(hits, character(),
                   info = paste("component literals found:",
                                paste(hits, collapse = "; ")))
})
