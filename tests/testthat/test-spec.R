# Spec layer: constructor, validator, the five shipped instantiations, and the
# guardrail that keeps component names out of the rest of the package.

# ---- the shipped specs -------------------------------------------------------

test_that("every shipped spec constructs and validates", {
  # Driven off el_spec_files() rather than a literal list, because that is
  # what the package itself enumerates: a spec dropped into inst/specs/ with
  # no test of its own would otherwise go unvalidated. The setequal pins the
  # roster so an addition is a decision rather than a drift.
  ids <- el_spec_files()
  expect_setequal(ids, c("00_SIM_EL_Spec", "00_TEMPLATE_EL_Spec",
                         "00_ELPA21_EL_Spec", "00_MA_EL_Spec",
                         "00_IL_EL_Spec"))
  for (id in ids) {
    spec <- read_el_spec(id)
    expect_s3_class(spec, "el_spec")
    expect_silent(validate_el_spec(spec))
  }
})

test_that("SIM spec matches sim_contract.md v0.4.0", {
  spec <- read_el_spec("00_SIM_EL_Spec")

  # 4.3 headline, 4.5 bands, 2.2 P block, 5.4 cut convention.
  expect_identical(spec_headline(spec), "overall")
  expect_identical(spec_bands(spec)$breaks, c(1, 2, 3, 4, 4.5, 5, 7))
  expect_identical(spec_bands(spec)$labels,
                   c("1.0-1.9", "2.0-2.9", "3.0-3.9", "4.0-4.4", "4.5-4.9",
                     "5.0+"))
  expect_identical(spec_min_cell(spec), 10L)
  expect_identical(spec_max_years(spec), 6L)
  expect_identical(spec_essa_years(spec), 5L)
  expect_identical(spec$policy$timelines$lt_el_years, 6L)
  # Level 4's minimum of c(700, 735, 750, 790), the same in every grade.
  for (g in as.character(3:11)) {
    expect_identical(spec_proficient_cut(spec, "ELA", g), 750)
    expect_identical(spec_proficient_cut(spec, "Math", g), 750)
  }
  expect_identical(spec$academic$ELA$op, ">=")
  expect_identical(spec_academic_grades(spec, "ELA"), as.character(3:11))
  expect_identical(spec$data$entry_date_rule, "calendar_year")

  # Type 1 means every Type-gated Additional check is skipped.
  expect_identical(spec_exit_types(spec), 1L)
  expect_false(spec_has_type(spec, 2:6))

  # Synthetic data must never be mistaken for a state's values.
  expect_identical(spec$verification$status, "synthetic")
})

test_that("the shipped tables need no crosswalk under the SIM spec", {
  # The sim emits the canonical ALL-CAPS names, so the SIM spec carries no
  # `delivery` crosswalk and every canonical column is already there.
  spec <- read_el_spec("00_SIM_EL_Spec")
  expect_null(spec$data$columns)
  expect_true(all(c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS", "ELP_FORM")
                  %in% names(stateELP::el_roster_sim)))
  expect_true(all(c("STUDENT_ID", "YEAR", "COMPONENT", "SCALE_SCORE",
                    "ACHIEVEMENT_LEVEL") %in% names(stateELP::el_long_sim)))
  expect_true(all(c("STUDENT_ID", "YEAR", "GRADE", "SUBJECT", "SCALE_SCORE")
                  %in% names(stateELP::academic_long_sim)))
})

test_that("ELPA21 spec exercises the non-WIDA branches", {
  spec <- read_el_spec("00_ELPA21_EL_Spec")

  # A categorical score that reports a level and no scale score.
  expect_identical(spec_headline(spec), "determination")
  expect_identical(spec$elp$level_scale$type, "categorical")
  expect_identical(spec$elp$level_scale$levels,
                   c("Emerging", "Progressing", "Proficient"))
  expect_identical(spec$elp$components$determination$measures, "level")
  # No declared bands: the package default of one band per level applies.
  expect_null(spec_bands(spec))
  # Exit is the Proficient determination, a position on the category list.
  terms <- spec$policy$exit$pathways[[1]]$terms
  expect_length(terms, 1L)
  expect_identical(terms[[1]]$component, "determination")
  expect_identical(terms[[1]]$value, 3)
  expect_identical(spec_exit_types(spec), 1L)
  # No academic block, so the academic analyses have no subject to run.
  expect_length(spec_academic_subjects(spec), 0L)
})

test_that("the template shows every optional field, crosswalk included", {
  # The SIM spec needs no crosswalk, so the template is the suite's case of a
  # non-identity one: a canonical name on the left, the file's on the right.
  spec <- read_el_spec("00_TEMPLATE_EL_Spec")
  cols <- spec$data$columns
  expect_identical(cols$roster[["STUDENT_ID"]], "Student_UID")
  expect_identical(cols$base[["YEAR"]], "school_year")
  # A field the crosswalk does not mention keeps its canonical name.
  expect_false("GRADE" %in% names(cols$roster))
  expect_length(spec$elp$level_scale$labels, 6L)
  expect_identical(spec_headline_label(spec), "Overall Composite")
  expect_identical(spec$verification$status, "unverified")
})

test_that("MA spec carries the sourced exit rule and nothing else sourced", {
  # The one policy fact the source document states, pinned so a later edit
  # cannot drift it: MA DESE, "Understanding and Implementing ACCESS Tests"
  # (14 August 2024), slide 3 -- 4.2 overall and 3.9 literacy.
  spec <- read_el_spec("00_MA_EL_Spec")

  expect_identical(spec$program$short_name, "MA")
  expect_identical(spec$program$state, "Massachusetts")
  expect_identical(spec_headline(spec), "overall")

  # Composite plus floor: one conjunctive pathway, two level terms.
  expect_identical(spec_exit_types(spec), 2L)
  expect_true(spec_has_type(spec, 2))
  expect_false(spec_has_type(spec, c(1, 3, 4, 5, 6)))
  expect_length(spec$policy$exit$pathways, 1L)

  terms <- spec$policy$exit$pathways[[1]]$terms
  expect_length(terms, 2L)
  expect_identical(spec$policy$exit$pathways[[1]]$combine, "all")
  expect_identical(terms[[1]]$value, 4.2)
  expect_identical(terms[[2]]$value, 3.9)
  expect_true(all(vapply(terms, function(t) t$measure, "") == "level"))
  expect_true(all(vapply(terms, function(t) t$op, "") == ">="))

  # Only the exit rule is sourced; every other policy field is provisional,
  # so print.el_spec() and Prep 1 list them as [DERIVED - VERIFY].
  expect_false(identical(spec$policy$exit$source, "provisional"))
  expect_match(spec$policy$exit$source, "4\\.2 overall and 3\\.9 literacy")
  others <- setdiff(names(spec$policy), "exit")
  for (nm in others) {
    expect_identical(spec$policy[[nm]]$source, "provisional", info = nm)
  }
  expect_identical(spec$verification$status, "provisional")

  # The placeholders are left out rather than placeheld: no on-time rule or
  # long-term year, so their absence reads as unknown.
  expect_null(spec$policy$timelines$on_time_rule)
  expect_null(spec$policy$timelines$lt_el_years)

  # No MCAS cuts are invented: there is no academic block, so the spec
  # declares no subjects (CLAUDE.md 5) and the benchmark stays unset.
  expect_length(spec_academic_subjects(spec), 0L)
  expect_true(is.na(spec_proficient_level(spec)))
})

test_that("the MA rule's headline term comes first", {
  # el_spec() puts the score's own criterion first whatever order `exit`
  # names them in, so a rule reads the way a state writes it. Check 3 and
  # Check 4 both match the headline term by name, so the order is no longer
  # load-bearing, but neither term may set `from`.
  spec <- read_el_spec("00_MA_EL_Spec")
  terms <- spec$policy$exit$pathways[[1]]$terms

  first_level <- Find(function(t) identical(t$measure, "level") &&
                        is.null(t$from), terms)
  expect_identical(first_level$component, spec_headline(spec))
  expect_identical(first_level$value, 4.2)
  # Neither term may name `from`, or that reader skips it entirely.
  expect_true(all(vapply(terms, function(t) is.null(t$from), TRUE)))
})

test_that("MA is the first shipped spec whose Type gate opens on a real rule", {
  # Prep 2's crosswalk and contested zone are Type 2-6 only, and until this
  # spec the sole Type 2 exercise was a synthetic fixture. Run on the sim's
  # tables, which are WIDA-shaped with the same component keys, so the rule
  # resolves against real rows.
  spec <- read_el_spec("00_MA_EL_Spec")
  b <- as_el_data(stateELP::el_long_sim, roster = stateELP::el_roster_sim,
                  spec = spec)
  cw <- compute_criteria_crosswalk(b, spec)

  # The near-cut window centres on the state's own cut, not on a default.
  expect_identical(unname(cw$crosswalk$window[["cut"]]), 4.2)
  expect_identical(cw$contested_zone$cut, 4.2)
  expect_identical(cw$crosswalk$combine, "all")
  expect_identical(nrow(cw$crosswalk$criteria), 2L)

  # Blocked by exactly one criterion: with two criteria, both directions
  # must appear, or the second criterion is doing no independent work.
  expect_gt(nrow(cw$blocked_groups), 0L)
  expect_length(unique(cw$blocked_groups$BLOCKED_BY), 2L)
})

test_that("IL spec carries its four sourced fields and marks the rest", {
  # Pinned against the documents the file's header cites, so a later edit
  # cannot drift them: an Overall Composite of 4.8 from the 2017 ACCESS, and
  # the 13 August 2025 Proficient cuts by grade.
  spec <- read_el_spec("00_IL_EL_Spec")

  expect_identical(spec$program$short_name, "IL")
  expect_identical(spec$program$state, "Illinois")
  expect_identical(spec_headline(spec), "overall")
  expect_identical(spec_headline_label(spec), "Overall Composite")

  # Type 1: one criterion on the score, no floor, no discretionary route.
  expect_identical(spec_exit_types(spec), 1L)
  expect_false(spec_has_type(spec, 2:6))
  p <- spec$policy$exit$pathways
  expect_length(p, 1L)
  expect_false(p[[1]]$discretionary)
  expect_identical(p[[1]]$terms[[1]],
                   list(component = "overall", measure = "level", op = ">=",
                        value = 4.8))

  # The timelines are converted to the package's count of years in status,
  # a K entrant's target; the header shows the arithmetic.
  tl <- spec$policy$timelines
  expect_identical(spec_max_years(spec), 8L)
  expect_identical(tl$on_time_rule$default_years, 8L)
  expect_identical(tl$lt_el_years, 8L)
  expect_identical(spec_min_cell(spec), 10L)

  # Grades 3-8 read the 2025 Proficient cut; grade 11 (SAT, then ACT) has
  # none and falls back to the delivered level >= 3.
  expect_identical(spec_academic_subjects(spec), c("ELA", "Math"))
  ela <- c(735, 737, 739, 741, 743, 745)
  math <- c(732, 740, 740, 742, 745, 745)
  for (i in seq_along(3:8)) {
    g <- as.character((3:8)[i])
    expect_identical(spec_proficient_cut(spec, "ELA", g), ela[i], info = g)
    expect_identical(spec_proficient_cut(spec, "Math", g), math[i], info = g)
  }
  expect_true("11" %in% spec_academic_grades(spec, "ELA"))
  expect_true(is.na(spec_proficient_cut(spec, "ELA", "11")))
  expect_true(is.na(spec_proficient_cut(spec, "Math", "11")))
  expect_identical(spec_proficient_level(spec), 3L)

  # Sourced: exit, timelines, suppression, academic. Everything else is a
  # package default and reads [DERIVED - VERIFY].
  sourced <- c("exit", "timelines", "suppression", "academic_benchmark")
  for (nm in sourced) {
    expect_false(identical(spec$policy[[nm]]$source, "provisional"), info = nm)
  }
  expect_match(spec$policy$exit$source, "overall composite of 4\\.8")
  for (nm in setdiff(names(spec$policy), sourced)) {
    expect_identical(spec$policy[[nm]]$source, "provisional", info = nm)
  }
  expect_identical(spec$verification$status, "provisional")
})

# ---- the short form el_spec() takes ------------------------------------------

test_that("the short form expands to the conventions every stage reads", {
  spec <- do.call(el_spec, make_spec_short())
  tl <- spec$policy$timelines
  p <- spec$policy$exit$pathways
  expect_length(p, 1L)
  expect_identical(p[[1]]$combine, "all")
  expect_identical(spec$policy$exit$combination, "any_pathway")
  expect_identical(p[[1]]$terms[[1]],
                   list(component = "overall", measure = "level", op = ">=",
                        value = 4.5))
  expect_identical(tl$exit_dating, "first_former_year")
  expect_identical(tl$initial_elp_source, "first_annual")
  # Defaults: on-time falls back to the maximum; LTEL is not set.
  expect_null(tl$on_time_rule)
  expect_null(tl$lt_el_years)
  expect_identical(spec_min_cell(spec), 10L)
  expect_identical(spec$policy$district_size_bands$labels,
                   c("<100", "101-999", "1,000+"))
  expect_identical(spec$policy$error_tolerance$costlier_error, "unknown")
  expect_length(spec_academic_subjects(spec), 0L)
  # Every vocabulary component is accepted in a delivery.
  expect_setequal(spec_components(spec), el_component_vocabulary())
  expect_identical(spec$program$short_name, "TS")
})

test_that("the Type is derived from the rule's shape", {
  one <- do.call(el_spec, make_spec_short())
  expect_identical(spec_exit_types(one), 1L)
  floor <- do.call(el_spec, make_spec_short(exit = c(written = 3.9,
                                                     overall = 4.2)))
  expect_identical(spec_exit_types(floor), 2L)
  # The score's criterion is put first whatever order `exit` gives.
  expect_identical(floor$policy$exit$pathways[[1]]$terms[[1]]$component,
                   "overall")
  profile <- do.call(el_spec, make_spec_short(
    exit = c(listening = 4, speaking = 4, reading = 4, writing = 4)))
  expect_identical(spec_exit_types(profile), 6L)
  expect_true(spec_has_type(profile, 2:6))
})

test_that("band labels are generated from the band edges", {
  b <- .bands_from_edges(c(1, 2, 3, 4, 4.5, 5), 6)
  expect_identical(b$labels, c("1.0-1.9", "2.0-2.9", "3.0-3.9", "4.0-4.4",
                               "4.5-4.9", "5.0+"))
  expect_identical(b$breaks, c(1, 2, 3, 4, 4.5, 5, 7))
  # Edges in hundredths label to hundredths.
  expect_identical(.bands_from_edges(c(1, 4.25), 6)$labels,
                   c("1.00-4.24", "4.25+"))
  expect_error(do.call(el_spec, make_spec_short(
    elp = list(test = "T", score = "overall", levels = c(1, 6),
               bands = c(1, 7)))), "within `elp\\$levels`")
})

test_that("district-size edges give correct labels", {
  d <- .district_bands(c(0, 50, 500))
  expect_identical(d$labels, c("<50", "50-499", "500+"))
  expect_identical(d$breaks, c(0, 50, 500, Inf))
  expect_error(do.call(el_spec, make_spec_short(district_sizes = c(10, 100))),
               "starting at 0")
})

test_that("the exit rule is checked against the score and its levels", {
  expect_error(do.call(el_spec, make_spec_short(exit = c(written = 4))),
               "single exit criterion must be on `elp\\$score`")
  expect_error(do.call(el_spec, make_spec_short(exit = c(overall = 7))),
               "within `elp\\$levels`")
  expect_error(do.call(el_spec, make_spec_short(exit = c(literacy = 4))),
               "not components")
  expect_error(do.call(el_spec, make_spec_short(exit = 4.5)),
               "named numeric vector")
  # A categorical level is a position in the list of names.
  cat_elp <- list(test = "T", score = "determination",
                  levels = c("Low", "Mid", "High"))
  expect_error(do.call(el_spec, make_spec_short(
    elp = cat_elp, exit = c(determination = 4))), "a position, 1 to 3")
  s <- do.call(el_spec, make_spec_short(elp = cat_elp,
                                        exit = c(determination = 3)))
  expect_identical(s$elp$components$determination$measures, "level")
  expect_identical(spec_headline_label(s), "determination composite")
})

test_that("a short-form spec reports every problem at once", {
  err <- tryCatch(
    el_spec(id = "X", status = "maybe",
            elp = list(test = "T", score = "overal", levels = c(1, 6),
                       headline = "overall"),
            exit = c(overall = 4.5), timelines = list(max = 0, essa = 5)),
    error = conditionMessage)
  expect_match(err, "`status` must be one of")
  expect_match(err, "`elp` does not take: headline")
  expect_match(err, "`elp\\$score` must be one of")
  expect_match(err, "`timelines` does not take: essa")
  expect_match(err, "`timelines\\$max`")
  # The old long form is refused by name.
  expect_error(el_spec(schema_version = "1.0.0", policy = list()),
               "does not take: schema_version, policy")
})

test_that("proficient cuts are one for every grade, or one per grade", {
  s <- do.call(el_spec, make_spec_short(academic = list(
    ELA = list(grades = 3:5, proficient_cut = c("3" = 440, "4" = 450)),
    Math = list(grades = 3:5, proficient_cut = 500))))
  expect_identical(spec_proficient_cut(s, "ELA", "3"), 440)
  expect_identical(spec_proficient_cut(s, "ELA", 4), 450)
  # A grade with no cut has none -- never another grade's.
  expect_true(is.na(spec_proficient_cut(s, "ELA", "5")))
  expect_true(is.na(spec_proficient_cut(s, "ELA")))
  expect_identical(spec_proficient_cut(s, "Math", "5"), 500)
  expect_identical(spec_academic_grades(s, "ELA"), c("3", "4", "5"))
  # No proficient level given: the fallback is undefined, not invented.
  expect_true(is.na(spec_proficient_level(s)))

  expect_error(do.call(el_spec, make_spec_short(academic = list(
    ELA = list(grades = 3:5, proficient_cut = c("8" = 440))))),
    "names grade\\(s\\) not in")
  expect_error(do.call(el_spec, make_spec_short(academic = list(
    ELA = list(grades = 3:5, proficient_cut = c(440, 450))))),
    "one number for every grade")
  expect_error(do.call(el_spec, make_spec_short(academic = list(
    ELA = list(grades = 3:5, proficient_cut = 440, proficient_level = 3),
    Math = list(grades = 3:5, proficient_cut = 500, proficient_level = 4)))),
    "same in every subject")
  expect_error(do.call(el_spec, make_spec_short(academic = list(
    ELA = list(grades = 3:5, cuts = c(400, 440))))),
    "`academic\\$ELA` does not take: cuts")
})

test_that("sources mark a field sourced; the rest read [DERIVED - VERIFY]", {
  s <- do.call(el_spec, make_spec_short(sources = c(exit = "Reg. 1.2")))
  expect_identical(s$policy$exit$source, "Reg. 1.2")
  expect_identical(s$policy$timelines$source, "provisional")
  expect_identical(s$policy$suppression$source, "provisional")
  out <- capture.output(print(s))
  expect_true(any(grepl("DERIVED - VERIFY.*min_cell", out)))
  expect_false(any(grepl("DERIVED - VERIFY.*exit", out)))
  expect_error(do.call(el_spec, make_spec_short(sources = c(growth = "x"))),
               "cannot source: growth")
})

test_that("delivery carries the crosswalk and Prep 2's entry dating", {
  s <- do.call(el_spec, make_spec_short(delivery = list(
    roster = c(STUDENT_ID = "SASID"),
    value_maps = list(GRADE = c("0" = "K")),
    entry_date_rule = "calendar_year")))
  expect_identical(s$data$columns$roster, c(STUDENT_ID = "SASID"))
  expect_identical(s$data$columns$value_maps$GRADE, c("0" = "K"))
  expect_identical(s$data$entry_date_rule, "calendar_year")
  expect_error(do.call(el_spec, make_spec_short(delivery = list(
    columns = list()))), "`delivery` does not take: columns")
  # The full validator still guards the crosswalk's shape.
  expect_error(do.call(el_spec, make_spec_short(delivery = list(
    roster = c(student_id = "x")))), "canonical names are ALL CAPS")
})

# ---- the policy / measurement boundary --------------------------------------

test_that("every policy value carries a source", {
  for (id in el_spec_files()) {
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

test_that("the headline has a display name for the documents to use", {
  # Rendered text names the component a state reports; "headline" is this
  # package's word for the abstraction and never reaches a reader.
  expect_identical(spec_headline_label(read_el_spec("00_SIM_EL_Spec")),
                   "Overall Composite")
  expect_identical(spec_headline_label(read_el_spec("00_TEMPLATE_EL_Spec")),
                   "Overall Composite")
  # The ELPA21 headline is a categorical determination, and its own name
  # travels with it -- the same sentence has to read correctly for both.
  expect_identical(spec_headline_label(read_el_spec("00_ELPA21_EL_Spec")),
                   "Overall Performance Determination")

  # With no native_name the label falls back to the vocabulary key and the
  # component's kind, so it still reads as a noun phrase.
  spec <- make_spec_base()
  expect_identical(spec_headline_label(spec),
                   paste(spec_headline(spec),
                         spec$elp$components[[spec_headline(spec)]]$kind))
})

test_that("the on-time rule and the LTEL year are structured, and checked", {
  expect_error(
    make_spec_base(policy = list(timelines = list(lt_el_years = "six"))),
    "lt_el_years` must be a whole number"
  )
  expect_error(
    make_spec_base(policy = list(timelines = list(on_time_rule = list(
      by_start_grade = c(5, 6))))),
    "by_start_grade` must be a named"
  )
  expect_error(
    make_spec_base(policy = list(timelines = list(on_time_rule = list(
      by_initial_band = c(nosuchband = 5))))),
    "not in `elp\\$bands\\$labels`"
  )
  expect_s3_class(
    make_spec_base(policy = list(timelines = list(
      lt_el_years = 6L,
      on_time_rule = list(default_years = 5L,
                          by_start_grade = c(K = 5L, "3" = 6L),
                          by_initial_band = c(low = 6L))))),
    "el_spec"
  )
})

test_that("delivery crosswalks use ALL-CAPS canonical names", {
  expect_error(
    make_spec_base(data = list(columns = list(
      roster = c(student_id = "student_id")))),
    "canonical names are ALL CAPS"
  )
  expect_error(
    make_spec_base(data = list(columns = list(roster = c("STUDENT_ID")))),
    "must be a named character vector"
  )
})

test_that("CONTENT_AREA value maps must land on spec keys", {
  expect_error(
    make_spec_base(data = list(columns = list(value_maps = list(
      CONTENT_AREA = list(base = c(READING = "expressive")))))),
    "maps to unknown component"
  )
  expect_error(
    make_spec_base(data = list(columns = list(value_maps = list(
      CONTENT_AREA = list(academic = c(MATHEMATICS = "Math")))))),
    "maps to unknown subject"
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

# ---- academic cut scores by grade, and the validator's added checks ---------
# (2026-11, from a state test run: its test's cuts differ by grade, and one
# vector per subject applied a single grade's cut to every grade.)

test_that("cut scores given by grade are read grade by grade", {
  spec <- make_spec_base(
    academic = list(ELA = list(
      grades = c("3", "4"), level_labels = c("1", "2", "3", "4", "5"),
      cuts = list("3" = c(690, 725, 740, 780), "4" = c(700, 735, 750, 790)))),
    policy = list(academic_benchmark = list(proficient_level = 4L,
                                            source = "provisional")))
  expect_identical(spec_academic_cuts(spec, "ELA", "3"), c(690, 725, 740, 780))
  expect_identical(spec_proficient_cut(spec, "ELA", "3"), 740)
  expect_identical(spec_proficient_cut(spec, "ELA", "4"), 750)
  # No grade, or a grade without an entry: no cut -- never another grade's.
  expect_null(spec_academic_cuts(spec, "ELA"))
  expect_true(is.na(spec_proficient_cut(spec, "ELA", "5")))
  expect_identical(spec_proficient_level(spec), 4L)

  # One vector is one cut for every grade, whatever grade is asked for.
  sim <- read_el_spec("00_SIM_EL_Spec")
  expect_identical(spec_proficient_cut(sim, "ELA", "3"),
                   spec_proficient_cut(sim, "ELA", "11"))
  expect_identical(spec_proficient_cut(sim, "ELA"), 750)
})

test_that("the validator checks cut scores given by grade", {
  expect_error(make_spec_base(academic = list(ELA = list(
    cuts = list("3" = c(790, 700))))), "strictly increasing")
  expect_error(make_spec_base(academic = list(ELA = list(
    cuts = list("12" = c(700, 735, 750, 790))))),
    "not in `academic\\$ELA\\$grades`")
  expect_error(make_spec_base(academic = list(ELA = list(
    cuts = list(c(700, 735))))), "named by grade")
  expect_error(make_spec_base(academic = list(ELA = list(
    level_labels = c("1", "2", "3"), cuts = c(700, 735, 750, 790)))),
    "expected 2")
})

test_that("a numeric level scale's labels must name every whole level", {
  expect_error(
    make_spec_base(elp = list(level_scale = list(
      type = "numeric", range = c(1, 6), labels = "Entering"))),
    "expected 6 label")
  expect_s3_class(
    make_spec_base(elp = list(level_scale = list(
      type = "numeric", range = c(1, 6),
      labels = c("a", "b", "c", "d", "e", "f")))),
    "el_spec")
})

test_that("an NA proficient level is an error once a subject is declared", {
  expect_error(
    make_spec_base(policy = list(academic_benchmark = list(
      proficient_level = NA_integer_, source = "provisional"))),
    "proficient_level")
  expect_error(
    make_spec_base(
      academic = list(ELA = list(level_labels = c("1", "2", "3", "4", "5"),
                                 cuts = c(700, 735, 750, 790))),
      policy = list(academic_benchmark = list(proficient_level = 6L,
                                              source = "provisional"))),
    "above the 5 levels")
  # A pointer-form spec declares no subject, so it may leave it NA (MA does).
  ma <- read_el_spec("00_MA_EL_Spec")
  expect_true(is.na(spec_proficient_level(ma)))
})

test_that("the template is a complete spec on its own", {
  # Item 3 of the test-run feedback: an analyst filling in the template alone
  # had nowhere to enter cut scores, the long-term EL year or a grade map.
  tmpl <- read_el_spec("00_TEMPLATE_EL_Spec")
  subjects <- spec_academic_subjects(tmpl)
  expect_true(length(subjects) >= 2L)
  for (sb in subjects) {
    a <- tmpl$academic[[sb]]
    expect_false(is.null(names(a$proficient_cut)))   # the by-grade layout
    expect_setequal(names(a$proficient_cut), a$grades)
    for (g in a$grades) {
      expect_false(is.na(spec_proficient_cut(tmpl, sb, g)))
    }
  }
  expect_identical(spec_proficient_level(tmpl), 3L)
  expect_true(!is.null(tmpl$policy$timelines$lt_el_years))
  expect_identical(unname(tmpl$data$columns$value_maps$GRADE), "K")
  expect_true(all(unname(tmpl$data$columns$value_maps$CONTENT_AREA$academic)
                  %in% subjects))
  # Still a placeholder, and labelled so.
  expect_identical(tmpl$verification$status, "unverified")
  expect_identical(tmpl$academic[[subjects[[1]]]]$source, "provisional")
})

# ---- the spec builder ---------------------------------------------------------

test_that("build_el_spec() writes a spec that validates, cuts by grade", {
  cuts <- function(g) paste(400 + 5 * (g - 3) + c(0, 50, 100), collapse = ", ")
  ans <- c(
    "XX", "Example Department of Education", "XX_ACCESS", "", "2025", "",
    "8", "", "", "",
    "An Overall Composite proficiency level of 4.5 or higher", "", "", "", "",
    "4.5", "", "", "", "", "", "",
    "6", "5", "", "6", "", "",
    "", "", "",
    "", "", "",
    "", "", "State Reading", "3-8", "BB, B, P, A", "", vapply(3:8, cuts, ""),
    "", "State Math", "3-8", "BB, B, P, A", "", vapply(3:8, cuts, ""),
    "3", "",
    rep("", 9),
    "ID", rep("", 9), "0", rep("", 4), "ID", "", "", "CONTENT_AREA", "", "",
    "READING", "MATHEMATICS",
    "", "", "")
  out <- file.path(withr::local_tempdir(), "XX_EL_Spec.R")
  capture.output(spec <- build_el_spec(out, input = ans))

  expect_s3_class(spec, "el_spec")
  expect_identical(spec_headline(spec), spec_headline(read_el_spec("00_SIM_EL_Spec")))
  expect_identical(spec_exit_types(spec), 1L)
  expect_identical(spec_proficient_cut(spec, "ELA", "5"), 460)
  expect_identical(spec_proficient_cut(spec, "Math", "8"), 475)
  expect_identical(spec$academic$ELA$level_labels, c("BB", "B", "P", "A"))
  expect_identical(spec$data$columns$value_maps$GRADE, c("0" = "K"))
  expect_identical(spec$data$columns$value_maps$CONTENT_AREA$academic,
                   c(READING = "ELA", MATHEMATICS = "Math"))
  expect_identical(spec$policy$timelines$lt_el_years, 6L)
  expect_true(file.exists(sub("\\.R$", "_answers.rds", out)))

  # A rerun keeps every saved answer: all-Enter reproduces the same spec.
  n_prompts <- length(ans)
  capture.output(again <- build_el_spec(out, input = rep("", n_prompts)))
  expect_identical(again$academic, spec$academic)
  expect_identical(again$policy$exit, spec$policy$exit)

  # A wrong number of cut scores for a grade is refused, not written.
  short <- ans
  short[which(ans == cuts(3))[1]] <- "400, 450"
  expect_error(capture.output(build_el_spec(
    file.path(withr::local_tempdir(), "YY_EL_Spec.R"), input = short)),
    "needs 3 increasing cut scores")
})

test_that("the builder's presets use vocabulary keys", {
  vocab <- el_component_vocabulary()
  for (p in .sb_presets()) {
    expect_true(all(names(p$components) %in% vocab))
    expect_true(p$headline %in% names(p$components))
  }
  expect_true(file.exists(spec_intake_form()))
})
