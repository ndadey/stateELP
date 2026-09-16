# The delivery boundary: validation, the coverage table, and the guarantee that
# every accepted delivery form produces the same bundle.

el_sim_spec <- function() read_el_spec("00_SIM_EL_Spec")

# ---- the separate-objects form -----------------------------------------------

test_that("the sim's own call builds a bundle", {
  b <- as_el_data(el_long_sim, components = elp_components_sim,
                  academic = academic_long_sim, spec = el_sim_spec())
  expect_s3_class(b, "el_data")
  expect_identical(nrow(el_spine(b)), 461040L)
  expect_identical(b$meta$sim_contract_version, "0.2.0")
  expect_false(is.null(el_components(b)))
  expect_false(is.null(el_academic(b)))
})

test_that("components and academic are optional and degrade to NULL", {
  b <- as_el_data(el_long_sim)
  expect_null(el_components(b))
  expect_null(el_academic(b))
  # The coverage table still builds; it just cannot report academic counts.
  expect_true(all(is.na(el_coverage(b)$n_academic)))
})

# ---- validation --------------------------------------------------------------

test_that("the five minimum-viable columns are required", {
  expect_error(as_el_data(data.frame(student_id = "A")),
               "missing required column")
})

test_that("a duplicated student-year is rejected", {
  sp <- tibble::tibble(student_id = c("A", "A"), school_year = c(2015L, 2015L),
                       grade = "K", el_status = "current")
  expect_error(as_el_data(sp), "not unique on student_id x school_year")
})

test_that("el_status keeps exactly three values", {
  sp <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "K",
                       el_status = "monitoring_1")
  expect_error(as_el_data(sp), "Monitoring years are sub-states of `former`")
})

test_that("academic grade is required, with no fill-from-spine fallback", {
  sp <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "3",
                       el_status = "current")
  ac <- tibble::tibble(student_id = "A", school_year = 2015L,
                       subject = "ELA", scale_score = 700)
  expect_error(as_el_data(sp, academic = ac), "no fill-from-spine")
})

test_that("academic grade must agree with the spine", {
  sp <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "3",
                       el_status = "current")
  ac <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "4",
                       subject = "ELA", scale_score = 700)
  expect_error(as_el_data(sp, academic = ac), "disagree with the spine")
})

test_that("an academic row with no spine row is rejected", {
  sp <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "3",
                       el_status = "current")
  ac <- tibble::tibble(student_id = "B", school_year = 2015L, grade = "3",
                       subject = "ELA", scale_score = 700)
  expect_error(as_el_data(sp, academic = ac), "no spine row")
})

test_that("component form must agree with the spine's elp_form", {
  sp <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "K",
                       el_status = "current", elp_form = "annual")
  cp <- tibble::tibble(student_id = "A", school_year = 2015L,
                       component = "overall", form = "alternate",
                       scale_score = 400, level = 4)
  expect_error(as_el_data(sp, components = cp), "disagrees with the spine")
})

test_that("the two key names may never be swapped", {
  sp <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "K",
                       el_status = "current")
  cp <- tibble::tibble(student_id = "A", school_year = 2015L,
                       subject = "overall", level = 4)
  expect_error(as_el_data(sp, components = cp), "must key on `component`")

  ac <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "K",
                       component = "ELA", scale_score = 700)
  expect_error(as_el_data(sp, academic = ac), "must key on `subject`")
})

test_that("component must be character, never a factor", {
  sp <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "K",
                       el_status = "current")
  cp <- tibble::tibble(student_id = "A", school_year = 2015L,
                       component = factor("overall"), level = 4)
  expect_error(as_el_data(sp, components = cp), "must be character")
})

test_that("a component outside the spec's model is rejected", {
  sp <- tibble::tibble(student_id = "A", school_year = 2015L, grade = "K",
                       el_status = "current")
  cp <- tibble::tibble(student_id = "A", school_year = 2015L,
                       component = "expressive", level = 4)
  expect_error(as_el_data(sp, components = cp, spec = el_sim_spec()),
               "not in the spec's component model")
})

# ---- delivery forms produce identical bundles --------------------------------

test_that("SGP long and separate objects give the same bundle", {
  spec <- el_sim_spec()
  from_sgp <- as_el_data_sgp(make_el_data_sgp_long(), spec)
  from_sep <- as_el_data(make_el_data_expected_spine(),
                         components = make_el_data_expected_components(),
                         spec = spec)

  cols <- c("student_id", "school_year", "grade", "el_status",
            "elp_form", "elp_level", "elp_scale")
  a <- dplyr::arrange(el_spine(from_sgp)[cols], .data$student_id,
                      .data$school_year)
  b <- dplyr::arrange(el_spine(from_sep)[cols], .data$student_id,
                      .data$school_year)
  expect_equal(a, b)

  ccols <- c("student_id", "school_year", "component", "scale_score",
             "level", "form")
  expect_equal(
    dplyr::arrange(el_components(from_sgp)[ccols], .data$student_id,
                   .data$school_year),
    dplyr::arrange(el_components(from_sep)[ccols], .data$student_id,
                   .data$school_year))
})

test_that("CONTENT_AREA maps by value through the spec, never by label", {
  spec <- el_sim_spec()
  b <- as_el_data_sgp(make_el_data_sgp_long(), spec)
  # READING mapped to the HEADLINE composite, not to a reading domain.
  expect_setequal(unique(el_components(b)$component), spec_headline(spec))

  # A spec without the map cannot read the delivery at all.
  spec2 <- spec
  spec2$data$content_area_map <- NULL
  expect_error(as_el_data_sgp(make_el_data_sgp_long(), spec2),
               "content_area_map")

  # An unmapped CONTENT_AREA is an error, never a silent drop.
  d <- make_el_data_sgp_long()
  d$CONTENT_AREA[1] <- "WRITING"
  expect_error(as_el_data_sgp(d, spec), "absent from the spec")
})

test_that("VALID_CASE is honored and GRADE 0 becomes K", {
  b <- as_el_data_sgp(make_el_data_sgp_long(), el_sim_spec())
  expect_false("S9" %in% el_spine(b)$student_id)     # the INVALID_CASE row
  expect_identical(nrow(el_spine(b)), 4L)
  expect_true("K" %in% el_spine(b)$grade)
  expect_false("0" %in% el_spine(b)$grade)
})

test_that("integer-only levels warn when the spec bands on a decimal", {
  d <- make_el_data_sgp_long()
  d$ACHIEVEMENT_LEVEL <- c(2, 3, 4, 5, 6)     # rounded delivery
  expect_warning(as_el_data_sgp(d, el_sim_spec()), "cannot be populated")
})

test_that("as_academic_long renames a stateSummative-shaped table", {
  out <- as_academic_long(make_el_data_summative_academic())
  expect_true(all(c("student_id", "school_year", "grade", "subject",
                    "scale_score", "perf_level") %in% names(out)))
  expect_false(any(c("ID", "YEAR", "CONTENT_AREA", "VALID_CASE") %in% names(out)))
  expect_type(out$student_id, "character")
  expect_type(out$school_year, "integer")
  # CONTENT_AREA became `subject`, not `component`.
  expect_false("component" %in% names(out))
})

# ---- coverage ----------------------------------------------------------------

test_that("the coverage table reports the pairing without filtering it", {
  b <- as_el_data(el_long_sim, academic = academic_long_sim)
  cv <- el_coverage(b)

  expect_true(all(c("school_year", "grade", "n_spine", "n_current",
                    "n_former", "n_never", "n_elp", "n_academic",
                    "n_alternate") %in% names(cv)))
  # 2020 has spine rows and no academic rows: the missing year is visible
  # here rather than being silently dropped.
  y20 <- cv[cv$school_year == 2020L, ]
  expect_gt(sum(y20$n_spine), 0L)
  expect_identical(sum(y20$n_academic), 0L)
  # Untested grades are visible too.
  k <- cv[cv$grade == "K", ]
  expect_identical(sum(k$n_academic), 0L)
})

# ---- the no-WIDA-assumptions smoke test --------------------------------------

test_that("every built derivation runs against a categorical headline", {
  spec  <- read_el_spec("00_ELPA21_EL_Spec")
  spine <- make_el_data_elpa21_spine()
  b <- as_el_data(spine, spec = spec)
  expect_s3_class(b, "el_data")

  # Banding: no declared bands, so one band per level, and the level is a word.
  expect_identical(assign_elp_band(spine$elp_level, spec), spine$elp_level)

  yis <- add_year_in_status(spine)
  expect_identical(yis$year_in_status[yis$student_id == "E1"], 1:3)

  spans <- build_student_spans(spine)
  flags <- add_exit_flags(spans, spec)
  expect_identical(flags$exit_year_in_status[flags$student_id == "E1"], 3L)
  expect_true(flags$exit_ever[flags$student_id == "E1"])

  co <- build_cohort(spine, spec, start_grade = "K", entry_years = 2015L)
  expect_setequal(co$student_id, c("E1", "E2", "E3"))

  x <- apply_suppression(tibble::tibble(n = c(2L, 50L), pct = c(0.1, 0.2)), spec)
  expect_identical(x$pct, c(NA, 0.2))

  expect_s3_class(el_coverage(b), "data.frame")
})
