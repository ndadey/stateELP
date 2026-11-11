# The delivery boundary: validation, the headline accessor, the coverage table,
# and the guarantee that every accepted delivery form produces the same bundle.

# One full-sim bundle for the whole file: building it joins 1.8M base rows
# against the roster, so it is built once, lazily.
sim_bundle <- local({
  b <- NULL
  function() {
    if (is.null(b)) {
      b <<- as_el_data(el_long_sim, roster = el_roster_sim,
                       academic = academic_long_sim,
                       spec = make_el_data_sim_spec())
    }
    b
  }
})

tiny_roster <- function(...) {
  tibble::tibble(STUDENT_ID = "A", YEAR = 2015L, GRADE = "3",
                 EL_STATUS = "current", ELP_FORM = "annual", ...)
}
tiny_base <- function(spec = make_el_data_sim_spec(), ...) {
  x <- tibble::tibble(STUDENT_ID = "A", YEAR = 2015L,
                      COMPONENT = spec_headline(spec), ELP_FORM = "annual",
                      SCALE_SCORE = 400, ACHIEVEMENT_LEVEL = 4)
  mods <- list(...)
  for (nm in names(mods)) x[[nm]] <- mods[[nm]]
  x
}

# ---- the separate-objects form -----------------------------------------------

test_that("the sim's own call builds a bundle", {
  b <- sim_bundle()
  expect_s3_class(b, "el_data")
  expect_identical(nrow(el_roster(b)), 461040L)
  expect_identical(nrow(el_base(b)), 1824716L)
  expect_identical(nrow(el_academic(b)), 568202L)
  expect_identical(b$meta$sim_contract_version, "0.4.0")
  expect_identical(b$meta$spec_id, "SIM")
  # The base table comes first and carries measurement only.
  expect_identical(names(b)[1], "elp")
  expect_identical(names(el_base(b)),
                   c("STUDENT_ID", "YEAR", "COMPONENT", "ELP_FORM",
                     "SCALE_SCORE", "ACHIEVEMENT_LEVEL"))
})

test_that("academic is optional and degrades to NULL", {
  spec <- make_el_data_sim_spec()
  b <- as_el_data(tiny_base(spec), roster = tiny_roster(), spec = spec)
  expect_null(el_academic(b))
  # The coverage table still builds; it just cannot report academic counts.
  expect_true(all(is.na(el_coverage(b)$N_ACADEMIC_SCORED)))
})

# ---- validation --------------------------------------------------------------

test_that("the four minimum-viable roster columns are required", {
  expect_error(as_el_data(tiny_base(), roster = data.frame(STUDENT_ID = "A")),
               "roster is missing required column")
})

test_that("a duplicated student-year is rejected", {
  r <- dplyr::bind_rows(tiny_roster(), tiny_roster())
  expect_error(as_el_data(tiny_base(), roster = r),
               "not unique on STUDENT_ID x YEAR")
})

test_that("EL_STATUS keeps exactly three values", {
  expect_error(
    as_el_data(tiny_base(),
               roster = dplyr::mutate(tiny_roster(), EL_STATUS = "monitoring_1")),
    "Monitoring years are sub-states of `former`")
})

test_that("the base table is unique on student x year x component", {
  expect_error(
    as_el_data(dplyr::bind_rows(tiny_base(), tiny_base()), roster = tiny_roster()),
    "not unique on STUDENT_ID x YEAR x COMPONENT")
})

test_that("every base row needs a roster row", {
  expect_error(as_el_data(tiny_base(STUDENT_ID = "B"), roster = tiny_roster()),
               "base-table row\\(s\\) have no roster row")
})

test_that("base ELP_FORM must agree with the roster's", {
  expect_error(as_el_data(tiny_base(ELP_FORM = "alternate"),
                          roster = tiny_roster()),
               "disagrees with the roster's")
})

test_that("ELP_FORM is carried from the roster, and required somewhere", {
  b <- tiny_base()
  b$ELP_FORM <- NULL
  expect_identical(el_base(as_el_data(b, roster = tiny_roster()))$ELP_FORM,
                   "annual")
  r <- tiny_roster()
  r$ELP_FORM <- NULL
  expect_error(as_el_data(b, roster = r), "`ELP_FORM` is required")
})

test_that("the base table carries measurement only", {
  expect_error(as_el_data(tiny_base(GRADE = "3"), roster = tiny_roster()),
               "non-base column\\(s\\): GRADE")
})

test_that("academic GRADE is required, with no fill-from-roster fallback", {
  ac <- tibble::tibble(STUDENT_ID = "A", YEAR = 2015L, SUBJECT = "ELA",
                       SCALE_SCORE = 700)
  expect_error(as_el_data(tiny_base(), roster = tiny_roster(), academic = ac),
               "no fill-from-roster")
})

test_that("academic GRADE must agree with the roster", {
  ac <- tibble::tibble(STUDENT_ID = "A", YEAR = 2015L, GRADE = "4",
                       SUBJECT = "ELA", SCALE_SCORE = 700)
  expect_error(as_el_data(tiny_base(), roster = tiny_roster(), academic = ac),
               "disagree with the roster on `GRADE`")
})

test_that("an academic row with no roster row is rejected", {
  ac <- tibble::tibble(STUDENT_ID = "B", YEAR = 2015L, GRADE = "3",
                       SUBJECT = "ELA", SCALE_SCORE = 700)
  expect_error(as_el_data(tiny_base(), roster = tiny_roster(), academic = ac),
               "academic row\\(s\\) have no roster row")
})

test_that("COMPONENT and SUBJECT are never the same key", {
  b <- tiny_base()
  names(b)[names(b) == "COMPONENT"] <- "SUBJECT"
  expect_error(as_el_data(b, roster = tiny_roster()),
               "must key on `COMPONENT`, not `SUBJECT`")

  ac <- tibble::tibble(STUDENT_ID = "A", YEAR = 2015L, GRADE = "3",
                       COMPONENT = "ELA", SCALE_SCORE = 700)
  expect_error(as_el_data(tiny_base(), roster = tiny_roster(), academic = ac),
               "must key on `SUBJECT`, not `COMPONENT`")

  # ... and on the shipped bundle the two tables keep their own key.
  s <- sim_bundle()
  expect_true("COMPONENT" %in% names(el_base(s)))
  expect_false("SUBJECT" %in% names(el_base(s)))
  expect_true("SUBJECT" %in% names(el_academic(s)))
  expect_false("COMPONENT" %in% names(el_academic(s)))
})

test_that("COMPONENT must be character, never a factor", {
  spec <- make_el_data_sim_spec()
  expect_error(
    as_el_data(tiny_base(spec, COMPONENT = factor(spec_headline(spec))),
               roster = tiny_roster()),
    "must be character")
})

test_that("a COMPONENT outside the spec's model is rejected", {
  spec <- make_el_data_sim_spec()
  outside <- setdiff(el_component_vocabulary(), spec_components(spec))[1]
  expect_error(
    as_el_data(tiny_base(spec, COMPONENT = outside), roster = tiny_roster(),
               spec = spec),
    "not in the spec's component model")
})

test_that("the spec's crosswalk renames a delivered column", {
  spec <- make_el_data_sim_spec()
  spec$data$columns$roster[["STUDENT_ID"]] <- "SASID"
  r <- tiny_roster()
  names(r)[names(r) == "STUDENT_ID"] <- "SASID"
  b <- as_el_data(tiny_base(spec), roster = r, spec = spec)
  expect_true("STUDENT_ID" %in% names(el_roster(b)))
  expect_false("SASID" %in% names(el_roster(b)))
})

# ---- the headline accessor ---------------------------------------------------

test_that("elp_headline() resolves the headline from the base table", {
  b <- sim_bundle()
  h <- elp_headline(b)
  expect_identical(names(h), c("STUDENT_ID", "YEAR", "ACHIEVEMENT_LEVEL",
                               "SCALE_SCORE", "ELP_BAND"))
  # sim_contract.md 4.5: the six-band distribution of the annual headline.
  expect_identical(nrow(h), 224071L)
  tab <- table(h$ELP_BAND)
  expect_identical(as.integer(tab[spec_bands(b$spec)$labels]),
                   c(31480L, 58860L, 67186L, 32666L, 24136L, 9743L))
  expect_false(anyDuplicated(h[c("STUDENT_ID", "YEAR")]) != 0L)
})

test_that("only current, annual-form student-years resolve a headline", {
  b <- sim_bundle()
  h <- elp_headline(b)
  status <- dplyr::left_join(h[c("STUDENT_ID", "YEAR")],
                             el_roster(b)[c("STUDENT_ID", "YEAR", "EL_STATUS",
                                            "ELP_FORM")],
                             by = c("STUDENT_ID", "YEAR"))
  expect_true(all(status$EL_STATUS == "current"))
  expect_true(all(status$ELP_FORM == "annual"))
  # Alternate-form students take the test and never enter the headline.
  expect_gt(sum(el_base(b)$ELP_FORM == "alternate"), 0L)
})

test_that("elp_headline() needs a spec and reads the headline from it", {
  spec <- make_el_data_sim_spec()
  b <- as_el_data(tiny_base(spec), roster = tiny_roster())
  expect_error(elp_headline(b), "needs a spec")
  h <- elp_headline(b, spec)
  expect_identical(h$ELP_BAND, "4.0-4.4")
})

# ---- delivery forms produce identical bundles --------------------------------

bundle_tables <- function(b) {
  key <- function(d, extra = NULL) {
    # Rebuilt from its columns, so only content is compared -- not row names
    # or a table-level attribute one path keeps and another drops.
    d <- as.data.frame(as.list(d), check.names = FALSE)
    d[do.call(order, d[c("STUDENT_ID", "YEAR", extra)]), , drop = FALSE] |>
      `rownames<-`(NULL)
  }
  list(base = key(el_base(b), "COMPONENT"),
       roster = key(el_roster(b)),
       academic = if (is.null(el_academic(b))) NULL
                  else key(el_academic(b), "SUBJECT"))
}

test_that("a merged single long file splits into the separate-objects bundle", {
  spec <- make_el_data_sim_spec()
  ids <- unique(el_roster_sim$STUDENT_ID[el_roster_sim$COHORT_LABEL %in% "K-2017"])[1:25]
  ids <- c(ids, unique(el_roster_sim$STUDENT_ID[
    el_roster_sim$EL_STATUS == "never"])[1:5])
  roster <- el_roster_sim[el_roster_sim$STUDENT_ID %in% ids, ]
  base <- el_long_sim[el_long_sim$STUDENT_ID %in% ids, ]

  # Roster attributes repeated on every component row; student-years with no
  # measurement carry one row with COMPONENT NA.
  merged <- dplyr::left_join(roster, base,
                             by = c("STUDENT_ID", "YEAR", "ELP_FORM"))
  expect_gt(nrow(merged), nrow(roster))

  from_merged <- as_el_data(merged, spec = spec)
  from_sep <- as_el_data(base, roster = roster, spec = spec)
  expect_identical(bundle_tables(from_merged), bundle_tables(from_sep))
})

test_that("SGP LONG_DATA and separate objects give the same bundle", {
  spec <- make_el_data_sim_spec()
  from_sgp <- as_el_data_sgp(make_el_data_sgp_long(), spec,
                             academic = make_el_data_sgp_academic())
  from_sep <- as_el_data(make_el_data_expected_base(spec),
                         roster = make_el_data_expected_roster(),
                         academic = make_el_data_summative_academic(),
                         spec = spec)
  expect_identical(bundle_tables(from_sgp), bundle_tables(from_sep))
  expect_identical(el_coverage(from_sgp), el_coverage(from_sep))
})

test_that("CONTENT_AREA maps by value through the spec, never by label", {
  spec <- make_el_data_sim_spec()
  b <- as_el_data_sgp(make_el_data_sgp_long(), spec)
  # READING mapped to the HEADLINE composite, not to a reading domain.
  expect_identical(unique(el_base(b)$COMPONENT), spec_headline(spec))

  # A spec without the map cannot read the delivery at all.
  spec2 <- spec
  spec2$data$columns$value_maps$CONTENT_AREA$base <- NULL
  expect_error(as_el_data_sgp(make_el_data_sgp_long(), spec2),
               "value_maps\\$CONTENT_AREA\\$base` is required")

  # An unmapped CONTENT_AREA is an error, never a silent drop.
  d <- make_el_data_sgp_long()
  d$CONTENT_AREA[1] <- "WRITING"
  expect_error(as_el_data_sgp(d, spec), "absent from the spec's value map")
})

test_that("VALID_CASE is honored, GRADE 0 becomes K, and YYYY_YYYY is the spring year", {
  b <- as_el_data_sgp(make_el_data_sgp_long(), make_el_data_sim_spec())
  r <- el_roster(b)
  expect_false("S9" %in% r$STUDENT_ID)     # the INVALID_CASE row
  expect_identical(nrow(r), 5L)
  expect_true("K" %in% r$GRADE)
  expect_false("0" %in% r$GRADE)
  expect_identical(sort(unique(r$YEAR)), 2015:2017)
  # The former year with no measurement is a roster row, not a base row.
  expect_identical(nrow(el_base(b)), 4L)
})

test_that("labeled levels parse against the spec, and integer-only levels warn", {
  spec <- make_el_data_sim_spec()
  d <- make_el_data_sgp_long()
  d$ACHIEVEMENT_LEVEL <- c("Entering", "Emerging", "Expanding", "Expanding",
                           NA, "Reaching")
  expect_warning(b <- as_el_data_sgp(d, spec), "cannot be populated")
  expect_identical(el_base(b)$ACHIEVEMENT_LEVEL[order(el_base(b)$STUDENT_ID,
                                                      el_base(b)$YEAR)],
                   c(1, 2, 4, 4))

  d$ACHIEVEMENT_LEVEL[1] <- "Transitioning"
  expect_error(suppressWarnings(as_el_data_sgp(d, spec)),
               "not in the spec's level scale")
})

test_that("as_academic_long is a near-identity for ReportR-shaped input", {
  x <- make_el_data_summative_academic()
  expect_identical(as_academic_long(x), x)
})

test_that("as_academic_long renames and recodes an SGP-shaped table", {
  spec <- make_el_data_sim_spec()
  out <- as_academic_long(make_el_data_sgp_academic(), spec)
  expect_identical(out, make_el_data_summative_academic())
  expect_false(any(c("ID", "CONTENT_AREA", "VALID_CASE") %in% names(out)))
  # CONTENT_AREA became SUBJECT, not COMPONENT.
  expect_false("COMPONENT" %in% names(out))

  # Without the spec the aliases still apply, but values are left as sent.
  raw <- as_academic_long(make_el_data_sgp_academic())
  expect_true("MATHEMATICS" %in% raw$SUBJECT)

  both <- make_el_data_sgp_academic()
  both$STUDENT_ID <- both$ID
  expect_error(as_academic_long(both), "is an alias of `STUDENT_ID`")
})

# ---- the pairing: join direction ---------------------------------------------

test_that("Check 3's universe is academic left-joined to roster, not the reverse", {
  b <- sim_bundle()
  ac <- el_academic(b)
  status <- el_roster(b)[c("STUDENT_ID", "YEAR", "EL_STATUS")]

  # academic LEFT JOIN roster: every academic row survives, and the former
  # and never reference groups are in it.
  pairing <- dplyr::left_join(ac, status, by = c("STUDENT_ID", "YEAR"))
  expect_identical(nrow(pairing), nrow(ac))
  expect_true(all(c("former", "never") %in% pairing$EL_STATUS))

  # Starting from the ELP side instead silently drops both reference groups,
  # because they have no ELP rows. This is the failure the direction guards.
  from_elp <- dplyr::inner_join(elp_headline(b)[c("STUDENT_ID", "YEAR")], ac,
                                by = c("STUDENT_ID", "YEAR")) |>
    dplyr::left_join(status, by = c("STUDENT_ID", "YEAR"))
  expect_lt(nrow(from_elp), nrow(ac))
  expect_false(any(c("former", "never") %in% from_elp$EL_STATUS))
})

# ---- coverage ----------------------------------------------------------------

test_that("the coverage table reports the pairing without filtering it", {
  cv <- el_coverage(sim_bundle())

  expect_identical(names(cv),
                   c("YEAR", "GRADE", "N_CURRENT", "N_CURRENT_ELP",
                     "N_ALTERNATE", "N_FORMER", "N_NEVER_SAMPLE",
                     "N_NEVER_WEIGHTED", "N_ACADEMIC_SCORED"))
  # 2020 has roster rows and no academic rows: the missing year is visible
  # here rather than being silently dropped.
  y20 <- cv[cv$YEAR == 2020L, ]
  expect_gt(sum(y20$N_CURRENT), 0L)
  expect_identical(sum(y20$N_ACADEMIC_SCORED), 0L)
  # Untested grades are visible too.
  expect_identical(sum(cv$N_ACADEMIC_SCORED[cv$GRADE == "K"]), 0L)
  # Every current EL with a same-year headline is counted, and no more.
  expect_identical(sum(cv$N_CURRENT_ELP), 224071L)
  expect_true(all(cv$N_CURRENT_ELP + cv$N_ALTERNATE <= cv$N_CURRENT))
})

test_that("no unweighted never-EL total appears in the coverage table", {
  cv <- el_coverage(sim_bundle())
  # Never-EL counts are weighted, or labeled as sample rows -- never a bare
  # total that reads as the population.
  never_cols <- grep("NEVER", names(cv), value = TRUE)
  expect_setequal(never_cols, c("N_NEVER_SAMPLE", "N_NEVER_WEIGHTED"))
  expect_equal(sum(cv$N_NEVER_WEIGHTED), 10 * sum(cv$N_NEVER_SAMPLE))
})

# ---- the no-WIDA-assumptions smoke test --------------------------------------

test_that("every built derivation runs against the ELPA21 headline", {
  spec   <- read_el_spec("00_ELPA21_EL_Spec")
  roster <- make_el_data_elpa21_roster()
  base   <- make_el_data_elpa21_base(spec)
  b <- as_el_data(base, roster = roster, spec = spec)
  expect_s3_class(b, "el_data")

  # The headline is a categorical determination with no scale score, and it
  # resolves through the same accessor as WIDA's numeric composite.
  h <- elp_headline(b)
  expect_identical(nrow(h), 5L)
  expect_true(all(is.na(h$SCALE_SCORE)))
  lv <- spec$elp$level_scale$levels
  expect_true(all(h$ELP_BAND %in% lv))
  expect_identical(h$ELP_BAND[h$STUDENT_ID == "E1" & h$YEAR == 2016L],
                   lv[length(lv)])

  yis <- add_year_in_status(roster)
  expect_identical(yis$YEAR_IN_STATUS[yis$STUDENT_ID == "E1"], 1:3)
  expect_true(all(is.na(add_lt_el(yis, spec)$LT_EL)))   # text-only definition

  spans <- build_student_spans(roster)
  flags <- add_exit_flags(spans, spec)
  expect_identical(flags$EXIT_YEAR_IN_STATUS[flags$STUDENT_ID == "E1"], 3L)
  expect_true(flags$EXIT_EVER[flags$STUDENT_ID == "E1"])

  co <- build_cohort(roster, spec, start_grade = "K", entry_years = 2015L)
  expect_setequal(co$STUDENT_ID, c("E1", "E2", "E3"))

  x <- apply_suppression(tibble::tibble(N = c(2L, 50L), PCT = c(0.1, 0.2)), spec)
  expect_identical(x$PCT, c(NA, 0.2))

  cv <- el_coverage(b)
  expect_s3_class(cv, "data.frame")
  expect_identical(sum(cv$N_CURRENT_ELP), 5L)
})

# ---- intake: values outside the format, the academic translation, the check --
# (2026-11, the state test run's items 1 and 2.)

test_that("as_el_data() translates academic codes and labels itself", {
  # Item 2: the academic table went in with READING / MATHEMATICS and lettered levels
  # and every academic result came back NULL. as_el_data() now applies the
  # spec's value map and level labels on intake, after the crosswalk.
  spec <- make_el_data_sim_spec()
  spec$academic$ELA$level_labels <- c("BB", "B", "P", "A", "AD")
  roster <- tiny_roster()
  ac <- tibble::tibble(ID = "A", YEAR = 2015L, GRADE = "3",
                       CONTENT_AREA = "READING", SCALE_SCORE = 700,
                       ACHIEVEMENT_LEVEL = "P")
  spec$data$columns$value_maps$CONTENT_AREA$academic <- c(READING = "ELA")
  b <- as_el_data(tiny_base(spec), roster = roster, academic = ac, spec = spec)
  expect_identical(el_academic(b)$SUBJECT, "ELA")
  expect_identical(el_academic(b)$ACHIEVEMENT_LEVEL, 3L)

  # A table already through as_academic_long() is unchanged by the second pass.
  pre <- as_academic_long(ac, spec)
  b2 <- as_el_data(tiny_base(spec), roster = roster, academic = pre, spec = spec)
  expect_identical(el_academic(b2), el_academic(b))
})

test_that("an academic subject the spec does not know is said out loud", {
  spec <- make_el_data_sim_spec()
  spec$data$columns$value_maps$CONTENT_AREA$academic <- NULL
  ac <- tibble::tibble(STUDENT_ID = "A", YEAR = 2015L, GRADE = "3",
                       SUBJECT = "READING", SCALE_SCORE = 700,
                       ACHIEVEMENT_LEVEL = 3L)
  expect_warning(as_el_data(tiny_base(spec), roster = tiny_roster(),
                            academic = ac, spec = spec),
                 "No academic row matches a subject")
})

test_that("ELP_FORM outside annual / alternate stops the build", {
  # Item 1: "general" passed intake and left elp_headline() with 0 rows.
  spec <- make_el_data_sim_spec()
  expect_error(as_el_data(tiny_base(spec, ELP_FORM = "general"),
                          roster = dplyr::select(tiny_roster(), -"ELP_FORM"),
                          spec = spec),
               "\"general\"")
  expect_error(as_el_data(tiny_base(spec),
                          roster = dplyr::mutate(tiny_roster(),
                                                 ELP_FORM = "Annual"),
                          spec = spec),
               "must be \"annual\" or \"alternate\"")
})

test_that("a kindergarten coded 0 and a grade 13 are warned about", {
  spec <- make_el_data_sim_spec()
  spec$data$columns$value_maps$GRADE <- NULL
  r0 <- dplyr::mutate(tiny_roster(), GRADE = "0")
  expect_warning(as_el_data(tiny_base(spec), roster = r0, spec = spec),
                 "value_maps\\$GRADE")
  r13 <- dplyr::mutate(tiny_roster(), GRADE = "13")
  expect_warning(as_el_data(tiny_base(spec), roster = r13, spec = spec),
                 "outside K and 1-12")
  # Mapped, it is K and nothing is said.
  spec$data$columns$value_maps$GRADE <- c("0" = "K")
  expect_silent(b <- as_el_data(tiny_base(spec), roster = r0, spec = spec))
  expect_identical(el_roster(b)$GRADE, "K")
})

test_that("el_data_format() names every required column the intake requires", {
  f <- el_data_format()
  expect_setequal(unique(f$TABLE), c("roster", "base", "academic"))
  req <- function(t) f$COLUMN[f$TABLE == t & f$NEED == "required"]
  expect_true(all(ROSTER_REQUIRED %in% req("roster")))
  expect_true(all(ACADEMIC_REQUIRED %in% req("academic")))
  expect_true(all(c(KEY, "COMPONENT") %in% req("base")))
  # The base table's allowed columns are exactly the documented ones.
  expect_setequal(f$COLUMN[f$TABLE == "base"], BASE_COLUMNS)
  expect_identical(nrow(el_data_format("roster")), sum(f$TABLE == "roster"))
})

test_that("check_el_delivery() passes the shipped simulation", {
  chk <- check_el_delivery(el_long_sim, roster = el_roster_sim,
                           academic = academic_long_sim,
                           spec = make_el_data_sim_spec())
  expect_s3_class(chk, "el_delivery_check")
  expect_true(attr(chk, "ok"))
  expect_identical(sum(chk$SEVERITY == "error"), 0L)
  expect_output(print(chk), "0 error")
})

test_that("check_el_delivery() lists every test-run problem at once", {
  spec <- make_el_data_sim_spec()
  spec$data$columns$value_maps$GRADE <- NULL
  spec$data$columns$value_maps$CONTENT_AREA$academic <- NULL
  head <- spec_headline(spec)
  roster <- tibble::tibble(
    STUDENT_ID = c("A", "A", "A", "B", "C"),
    YEAR = c(2014L, 2015L, 2016L, 2015L, 2015L),
    GRADE = c("0", "1", "2", "13", "3"),
    EL_STATUS = c("current", NA, "current", "never", "current"),
    ELP_FORM = c("general", NA, "general", NA, "general"),
    IEP = c("1", "0", "1", "0", "SPED"),
    LEFT_TRUNCATED = c("Y", "Y", "Y", "N", "N"))
  base <- tibble::tibble(
    STUDENT_ID = c("A", "A", "C"), YEAR = c(2014L, 2016L, 2015L),
    COMPONENT = head, ELP_FORM = "general", SCALE_SCORE = c(300, 340, NA),
    ACHIEVEMENT_LEVEL = c("2.1", "3.4", "P2"))
  ac <- tibble::tibble(
    STUDENT_ID = c("C", "C", "Z"), YEAR = 2015L, GRADE = "3",
    SUBJECT = c("READING", "MATHEMATICS", "READING"),
    SCALE_SCORE = c(700, 710, 650), ACHIEVEMENT_LEVEL = c("P", "BB", "B"))

  chk <- check_el_delivery(base, roster = roster, academic = ac, spec = spec)
  expect_false(attr(chk, "ok"))
  hit <- function(table, column, severity) {
    any(chk$TABLE == table & grepl(column, chk$COLUMN, fixed = TRUE) &
          chk$SEVERITY == severity)
  }
  expect_true(hit("roster", "GRADE", "error"))          # "0" for K
  expect_true(hit("roster", "GRADE", "warning"))        # "13"
  expect_true(hit("roster", "ELP_FORM", "error"))       # "general"
  expect_true(hit("roster", "EL_STATUS", "warning"))    # NA inside a run
  expect_true(hit("roster", "EL_STATUS", "note"))       # NA at all
  expect_true(hit("roster", "IEP", "warning"))          # "SPED"
  expect_true(hit("roster", "LEFT_TRUNCATED", "error")) # not logical
  expect_true(hit("base", "ACHIEVEMENT_LEVEL", "error"))# "P2"
  expect_true(hit("base", "ELP_FORM", "error"))
  expect_true(hit("base", "COMPONENT / ELP_FORM", "error"))  # no annual rows
  expect_true(hit("academic", "SUBJECT", "error"))      # READING unmapped
  expect_true(hit("academic", "ACHIEVEMENT_LEVEL", "error"))  # N/A/P unlabelled
  expect_true(hit("academic", "STUDENT_ID x YEAR", "error"))  # Z: no roster row
  expect_output(print(chk), "fix:")
  # The check changes nothing and never stops.
  expect_identical(roster$GRADE[[1]], "0")
})

test_that("mark_alternate_elp() makes a delivery with alternate codes build", {
  spec <- make_el_data_sim_spec()
  head <- spec_headline(spec)
  base <- tibble::tibble(
    STUDENT_ID = c("A", "B", "B"), YEAR = 2015L,
    COMPONENT = c(head, head, spec_components(spec)[[1]]),
    ELP_FORM = "general", SCALE_SCORE = c(340, NA, NA),
    ACHIEVEMENT_LEVEL = c("4.5", "P2", "3.1"))
  out <- mark_alternate_elp(base, annual = "general")
  expect_identical(out$ELP_FORM, c("annual", "alternate", "alternate"))
  expect_identical(out$ACHIEVEMENT_LEVEL, c(4.5, NA, NA))
  expect_identical(attr(out, "n_alternate"), 1L)

  roster <- tibble::tibble(STUDENT_ID = c("A", "B"), YEAR = 2015L,
                           GRADE = "3", EL_STATUS = "current")
  b <- as_el_data(out, roster = roster, spec = spec)
  expect_identical(nrow(elp_headline(b)), 1L)    # only A's annual score
})
