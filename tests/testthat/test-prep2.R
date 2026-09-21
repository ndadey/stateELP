# Prep 2 -- Population analysis. run_prep2(data, spec).
# Sample: full statewide EL population, current plus former, >= 5 years.
# Built per internal_docs/steps/3_prep2_population.md.
#
# Small-fixture tests use make_prep2_roster() (helper-prep2.R), whose expected
# values are derivable by hand; sim tests check structure and agreement with an
# independent calculation, never a literal read off a previous run.

prep2_spec <- function() make_prep2_spec()

# Every data frame held anywhere in a results object, for the sweeps below.
prep2_frames <- function(res) {
  out <- list()
  walk <- function(x, path) {
    if (is.data.frame(x)) out[[path]] <<- x
    else if (is.list(x) && !is.null(names(x))) {
      for (nm in names(x)) walk(x[[nm]], paste(path, nm, sep = "$"))
    }
  }
  walk(res[setdiff(names(res), c("meta", "analytic_sample_memo"))], "res")
  out
}

# ---- the results object ------------------------------------------------------

test_that("run_prep2() returns the documented slots", {
  res <- make_prep2_sim_run()
  documented <- c("counts_by_year", "el_share", "by_grade_span", "by_grade",
                  "by_district", "district_bins", "entry_grade", "initial_elp",
                  "profile", "monitoring", "analytic_sample_memo",
                  "criteria_crosswalk", "contested_zone", "meta")
  expect_true(all(documented %in% names(res)))
  # Additions to the instruction file's slot list [PROPOSED - CONFIRM].
  expect_true(all(c("entry_sources", "entry_by_year", "entry_cohorts",
                    "time_in_status", "blocked_groups") %in% names(res)))
  # A slot that is NULL is still present: the name is the contract.
  expect_true("criteria_crosswalk" %in% names(res))
  expect_null(res$criteria_crosswalk)

  m <- res$meta
  expect_identical(m$stage, "prep2")
  expect_identical(m$spec_id, "SIM")
  expect_identical(m$latest_year, max(el_roster_sim$YEAR))
  expect_identical(m$n_years, 11L)
  expect_false(m$short_panel)
  expect_identical(m$sim_contract_version, "0.4.0")
})

test_that("the Sample statement is not a single cohort and names the panel", {
  s <- make_prep2_sim_run()$meta$sample
  expect_match(s, "Full statewide EL population", fixed = TRUE)
  expect_match(s, "Not a single cohort", fixed = TRUE)
  expect_match(s, "11 years", fixed = TRUE)
  expect_match(s, "Never-EL students enter only as the enrollment denominator",
               fixed = TRUE)
})

test_that("a missing required roster field stops the stage, naming it", {
  bundle <- make_prep2_bundle()
  for (field in c("STUDENT_ID", "YEAR", "GRADE", "EL_STATUS")) {
    broken <- bundle
    broken$roster[[field]] <- NULL
    expect_error(run_prep2(broken, prep2_spec()), field, fixed = TRUE)
  }
})

# ---- counts by year (the real element) ---------------------------------------

test_that("compute_counts_by_year() counts the roster, by hand-checked values", {
  spec <- prep2_spec()
  cnt <- compute_counts_by_year(make_prep2_bundle(), spec)

  expect_s3_class(cnt, "tbl_df")
  expect_identical(names(cnt), c("YEAR", "N_CURRENT", "N_ANNUAL_HEADLINE",
                                 "N_ALTERNATE", "N_NO_HEADLINE", "N_FORMER",
                                 "N_NEVER_WEIGHTED", "SUPPRESSED"))
  expect_identical(cnt$YEAR, 2015:2020)
  expect_identical(cnt$N_CURRENT, c(12L, 36L, 39L, 39L, 29L, 29L))
  # Twelve alternate-form takers from 2016; the annual count is what is left.
  expect_identical(cnt$N_ALTERNATE, c(0L, 12L, 12L, 12L, 12L, 12L))
  expect_identical(cnt$N_ANNUAL_HEADLINE, c(12L, 24L, 27L, 27L, 17L, 17L))
  expect_identical(cnt$N_NO_HEADLINE, rep(0L, 6))
  expect_identical(cnt$N_FORMER, c(0L, 0L, 0L, 0L, 10L, 10L))
  # Never-ELs only ever appear weighted: 15 students x weight 10.
  expect_equal(cnt$N_NEVER_WEIGHTED, rep(150, 6))
})

test_that("current ELs split exactly into annual, alternate and no-headline", {
  cnt <- make_prep2_sim_run()$counts_by_year
  expect_identical(cnt$N_CURRENT,
                   cnt$N_ANNUAL_HEADLINE + cnt$N_ALTERNATE + cnt$N_NO_HEADLINE)
  # Counted from the roster -- independently of the coverage table.
  r <- el_roster_sim
  expect_identical(cnt$N_CURRENT,
                   as.integer(tapply(r$EL_STATUS == "current", r$YEAR, sum)))
  expect_identical(cnt$N_FORMER,
                   as.integer(tapply(r$EL_STATUS == "former", r$YEAR, sum)))
})

test_that("counts: suppressed cells are NA, zeros stay zero, no row is dropped", {
  spec <- prep2_spec()
  spec$policy$suppression$min_cell <- 20L
  cnt <- compute_counts_by_year(make_prep2_bundle(), spec)

  expect_identical(nrow(cnt), 6L)                       # rows are never dropped
  expect_true(is.na(cnt$N_CURRENT[1]))                  # 12 < 20
  expect_true(is.na(cnt$N_ALTERNATE[2]))                # 12 < 20
  expect_identical(cnt$N_ALTERNATE[1], 0L)              # a true zero is not suppressed
  expect_identical(cnt$N_CURRENT[2], 36L)               # 36 >= 20
  # The row flag is "some cell in this row was suppressed": every row here has
  # one (12 alternate-form students, 10 former ELs, 12 current in 2015) ...
  expect_true(all(cnt$SUPPRESSED))
  # ... and at the spec's own min_cell of 10, none does.
  expect_false(any(compute_counts_by_year(make_prep2_bundle(), prep2_spec())$SUPPRESSED))
})

# ---- EL share of enrollment --------------------------------------------------

test_that("EL share of enrollment is weighted", {
  # Unweighted this reads 52.7% on the sim and nobody will believe the rest
  # of the document.
  sh <- compute_el_share(make_prep2_bundle(), prep2_spec())
  # 2016: 36 current ELs of 36 + 15 x 10 weighted enrollment.
  expect_equal(sh$N_ENROLLMENT_WEIGHTED[sh$YEAR == 2016], 186)
  expect_equal(sh$PCT_EL[sh$YEAR == 2016], 36 / 186)
  # Former rows are enrolled students: 2019 is 29 current + 10 former + 15 +
  # 150 never.
  expect_equal(sh$N_ENROLLMENT_WEIGHTED[sh$YEAR == 2019], 189)
  expect_true(all(sh$PCT_EL < 0.25))

  # The unweighted figure is several times higher, and is what a careless
  # denominator would give.
  cnt <- compute_counts_by_year(make_prep2_bundle(), prep2_spec())
  unweighted <- cnt$N_CURRENT[2] / (cnt$N_CURRENT[2] + 15)
  expect_gt(unweighted, 0.7)
})

test_that("the sim's EL share matches an independent weighted calculation", {
  sh <- make_prep2_sim_run()$el_share
  r <- el_roster_sim
  w <- r$SAMPLE_WEIGHT
  by_year <- tapply(seq_len(nrow(r)), r$YEAR, function(i) {
    sum(w[i][r$EL_STATUS[i] == "current"]) / sum(w[i])
  })
  expect_equal(sh$PCT_EL, as.numeric(by_year))
  expect_true(all(sh$PCT_EL > 0.10 & sh$PCT_EL < 0.25))
})

test_that("EL share is NULL, with no crash, when there are no never-ELs", {
  roster <- make_prep2_roster()
  roster <- roster[roster$EL_STATUS != "never", ]
  bundle <- make_prep2_bundle(roster)
  expect_null(compute_el_share(bundle, prep2_spec()))
  res <- run_prep2(bundle, prep2_spec())
  expect_null(res$el_share)
  expect_false(res$meta$has_never)
  expect_false(grepl("enrollment denominator", res$meta$sample))
})

test_that("no unweighted never-EL total appears in any slot", {
  frames <- prep2_frames(make_prep2_sim_run())
  cols <- unique(unlist(lapply(frames, names)))
  never <- grep("NEVER|(^|_)EO(_|$)", cols, value = TRUE)
  expect_true(all(grepl("WEIGHTED", never)))
  # The one place enrollment is totalled says it is weighted, in its name.
  expect_true("N_ENROLLMENT_WEIGHTED" %in% cols)
  expect_false(any(c("N_ENROLLMENT", "N_NEVER", "N_NEVER_SAMPLE") %in% cols))
})

# ---- grade distribution ------------------------------------------------------

test_that("grade shares are shares of the year, and sum to one", {
  # Regression: ifelse() with a length-1 test recycled the first share, so
  # every grade in a year read the same percentage.
  by_grade <- make_prep2_sim_run()$by_grade
  totals <- tapply(by_grade$PCT, by_grade$YEAR, sum, na.rm = TRUE)
  expect_equal(as.numeric(totals), rep(1, length(totals)))
  expect_gt(length(unique(by_grade$PCT[by_grade$YEAR == 2015])), 1L)

  span <- make_prep2_sim_run()$by_grade_span
  expect_equal(as.numeric(tapply(span$PCT, span$YEAR, sum)), rep(1, 11))
})

test_that("by_grade is zero-filled K-12, suppressed cells NA, rows kept", {
  bg <- compute_by_grade(make_prep2_bundle(), prep2_spec())
  expect_identical(nrow(bg), 6L * 13L)                   # every year x grade is a row
  expect_identical(names(bg), c("YEAR", "GRADE", "N", "PCT", "SUPPRESSED"))
  # 2019: A11-A24 in grade 3 (14), B in grade 7 (12), C's three in grade 2.
  y <- bg[bg$YEAR == 2019, ]
  expect_identical(y$N[y$GRADE == "3"], 14L)
  expect_identical(y$N[y$GRADE == "7"], 12L)
  expect_true(is.na(y$N[y$GRADE == "2"]))                # 3 < min_cell
  expect_true(y$SUPPRESSED[y$GRADE == "2"])
  expect_identical(y$N[y$GRADE == "K"], 0L)              # an empty grade is 0, not NA
  expect_equal(y$PCT[y$GRADE == "3"], 14 / 29)           # shares use the true total
})

test_that("grade span is K-5 against 6-12", {
  sp <- compute_by_grade_span(make_prep2_bundle(), prep2_spec())
  expect_identical(nrow(sp), 12L)
  y <- sp[sp$YEAR == 2016, ]
  # 2016: A (24) in K; B (12) in grade 4 -- all K-5.
  expect_identical(y$N[y$SPAN == "K-5"], 36L)
  expect_identical(y$N[y$SPAN == "6-12"], 0L)
  # 2019: B is in grade 7.
  expect_identical(sp$N[sp$YEAR == 2019 & sp$SPAN == "6-12"], 12L)
})

test_that("student-years outside K-12 are noted, not silently dropped", {
  roster <- make_prep2_roster()
  roster$GRADE[roster$STUDENT_ID == "B01" & roster$YEAR == 2015L] <- "PK"
  bundle <- make_prep2_bundle(roster)
  bg <- compute_by_grade(bundle, prep2_spec())
  expect_identical(attr(bg, "off_grid"), 1L)
  expect_match(run_prep2(bundle, prep2_spec())$meta$notes$grade, "outside K-12")
})

# ---- districts ---------------------------------------------------------------

test_that("district bars are ranked, with the two-thirds rank and small-N line", {
  d <- compute_by_district(make_prep2_bundle(), prep2_spec())
  # 2020: D1 = A11, A12 + B (14); D2 = A13-A24 (12); D3 = C (3).
  expect_identical(d$DISTRICT_ID, c("D1", "D2", "D3"))
  expect_identical(d$RANK, 1:3)
  expect_equal(d$PCT_CUM, c(14, 26, 29) / 29)
  # The first rank whose cumulative share reaches two-thirds is 2.
  expect_identical(d$TWO_THIRDS, c(TRUE, TRUE, FALSE))
  # The deck's small-district line is the spec's first interior break.
  expect_identical(attr(d, "small_n"), 100)
  expect_identical(attr(d, "year"), 2020L)
})

test_that("a district below min_cell keeps its rank and cumulative share", {
  d <- compute_by_district(make_prep2_bundle(), prep2_spec())
  expect_identical(nrow(d), 3L)                          # the row stays
  expect_true(is.na(d$N[3]))                             # ... its count does not show
  expect_true(d$SUPPRESSED[3])
  expect_equal(d$PCT_CUM[3], 1)                          # ... but the total still adds up
})

test_that("district bins come from the spec, never the delivery's own column", {
  roster <- make_prep2_roster()
  # The delivery's tertile column says everyone is "large".
  roster$DISTRICT_SIZE_BAND <- "large"
  bundle <- make_prep2_bundle(roster)

  spec <- prep2_spec()
  spec$policy$district_size_bands <- list(
    breaks = c(0, 5, 13, Inf), labels = c("small", "mid", "big"),
    source = "provisional")
  bins <- compute_district_bins(bundle, spec)
  d <- compute_by_district(bundle, spec)

  expect_identical(bins$SIZE_BAND, c("small", "mid", "big"))
  expect_identical(bins$N_DISTRICTS, c(1L, 1L, 1L))     # D3 = 3, D2 = 12, D1 = 14
  expect_identical(d$SIZE_BAND, c("big", "mid", "small"))
  expect_identical(attr(d, "small_n"), 5)

  code <- unlist(lapply(
    list.files(test_path("..", ".."), pattern = "^prep2_.*\\.R$",
               full.names = TRUE, recursive = TRUE),
    function(f) {
      l <- readLines(f, warn = FALSE)
      l[!grepl("^\\s*#", l)]
    }))
  skip_if(length(code) == 0, "package sources not reachable from the test dir")
  expect_identical(grep("DISTRICT_SIZE_BAND", code, value = TRUE), character())
})

test_that("bins are left-closed like the ELP bands", {
  spec <- prep2_spec()
  labels <- spec$policy$district_size_bands$labels
  # A district exactly on a break lands in the band above it.
  expect_identical(.p2_size_band(c(99, 100, 999, 1000), spec),
                   labels[c(1, 2, 2, 3)])
})

test_that("districts degrade to NULL without DISTRICT_ID or without bands", {
  roster <- make_prep2_roster()
  roster$DISTRICT_ID <- NULL
  bundle <- make_prep2_bundle(roster)
  expect_null(compute_by_district(bundle, prep2_spec()))
  expect_null(compute_district_bins(bundle, prep2_spec()))

  spec <- prep2_spec()
  spec$policy$district_size_bands <- NULL
  b2 <- make_prep2_bundle(spec = spec)
  expect_null(compute_district_bins(b2, spec))
  expect_false(is.null(compute_by_district(b2, spec)))   # ranked, just not binned
  expect_true(is.na(attr(compute_by_district(b2, spec), "small_n")))
})

# ---- entry into EL status ----------------------------------------------------

test_that("with no delivered field the panel dates entry, and B is unknown", {
  entry <- compute_entry(make_prep2_bundle(), prep2_spec())
  s <- entry$students
  expect_setequal(s$STUDENT_ID, c(sprintf("A%02d", 1:24), sprintf("B%02d", 1:12),
                                  sprintf("C%02d", 1:3)))   # never-ELs are not here

  a <- s[substr(s$STUDENT_ID, 1, 1) == "A", ]
  expect_true(all(a$ENTRY_YEAR == 2016L))
  expect_true(all(a$ENTRY_GRADE == "K"))
  expect_true(all(a$ENTRY_SOURCE == "first current year (entry observed in panel)"))
  expect_false(any(a$LEFT_TRUNCATED))

  # Already EL when the panel opened, in grade 3, nothing to date them.
  b <- s[substr(s$STUDENT_ID, 1, 1) == "B", ]
  expect_true(all(is.na(b$ENTRY_YEAR)))
  expect_true(all(b$LEFT_TRUNCATED))
  expect_false(any(b$ENTRY_KNOWN))
  expect_true(all(b$ENTRY_STATUS == "unknown"))

  expect_identical(entry$sources$N, c(27L, 12L))
})

test_that("a delivered entry field dates the students the panel cannot", {
  bundle <- make_prep2_bundle(make_prep2_roster(entry_fields = TRUE))
  entry <- compute_entry(bundle, prep2_spec())
  s <- entry$students
  b <- s[substr(s$STUDENT_ID, 1, 1) == "B", ]
  expect_true(all(b$ENTRY_YEAR == 2013L))
  expect_true(all(b$ENTRY_KNOWN))
  expect_true(all(b$LEFT_TRUNCATED))                    # 2013 precedes the panel
  expect_true(all(b$ENTRY_STATUS == "before panel"))
  expect_true(all(s$ENTRY_SOURCE == "COHORT_START_YEAR"))

  # OBSERVED, BEFORE_PANEL, UNKNOWN: B is now dated, before the panel opened.
  eg <- compute_entry_grade(entry, prep2_spec())
  expect_identical(unname(attr(eg, "counts")), c(27L, 12L, 0L))
})

test_that("YEAR_IN_STATUS is read as 1-based or 0-based from in-panel entrants", {
  for (zero in c(FALSE, TRUE)) {
    spec <- prep2_spec()
    spec$data$entry_sources <- "YEAR_IN_STATUS"
    bundle <- make_prep2_bundle(make_prep2_roster(entry_fields = TRUE, yis_zero = zero))
    entry <- compute_entry(bundle, spec)

    expect_identical(entry$yis_first, if (zero) 0L else 1L)
    expect_match(paste(entry$notes, collapse = " "),
                 if (zero) "completed years" else "counting the current year")
    # Either coding recovers the same entry years.
    s <- entry$students
    expect_true(all(s$ENTRY_YEAR[substr(s$STUDENT_ID, 1, 1) == "A"] == 2016L))
    expect_true(all(s$ENTRY_YEAR[substr(s$STUDENT_ID, 1, 1) == "B"] == 2013L))
  }
})

test_that("the spec can pin the YEAR_IN_STATUS convention", {
  spec <- prep2_spec()
  spec$data$entry_sources <- "YEAR_IN_STATUS"
  spec$data$years_in_status_first_value <- 0L
  bundle <- make_prep2_bundle(make_prep2_roster(entry_fields = TRUE, yis_zero = TRUE))
  entry <- compute_entry(bundle, spec)
  expect_identical(entry$yis_first, 0L)
  expect_match(paste(entry$notes, collapse = " "), "per the spec")
})

test_that("a delivered start date maps to a spring year by the spec's rule", {
  d <- as.Date(c("2023-09-15", "2024-03-01", "2024-06-30", "2024-07-01"))
  # Academic calendar: 2023-24 is 2024; July starts the next end-year.
  expect_identical(.p2_entry_year_from_date(d, "academic_end_year"),
                   c(2024L, 2024L, 2024L, 2025L))
  expect_identical(.p2_entry_year_from_date(d, "calendar_year"),
                   c(2023L, 2024L, 2024L, 2024L))
  # Delivered as text.
  expect_identical(.p2_entry_year_from_date(c("09/15/2023", "", NA), "academic_end_year"),
                   c(2024L, NA_integer_, NA_integer_))
})

test_that("on the sim, the spec's date rule reproduces entry exactly", {
  notes <- paste(make_prep2_sim_run()$meta$notes$entry, collapse = " ")
  expect_match(notes, "matches the first current year for 100.0%", fixed = TRUE)
  expect_match(notes, "agrees with the delivered `LEFT_TRUNCATED` flag for 100.0%",
               fixed = TRUE)

  # Under the package default the sim's September-of-the-spring-year stamps
  # land a year late: the validation line is what catches that.
  spec <- prep2_spec()
  spec$data$entry_date_rule <- NULL
  late <- compute_entry(make_prep2_sim_bundle(), spec)
  expect_match(paste(late$notes, collapse = " "),
               "matches the first current year for 0.0%", fixed = TRUE)
})

test_that("the sim's entry grade agrees with an independent first-current count", {
  r <- el_roster_sim
  fc <- r[r$EL_STATUS == "current", ]
  fc <- fc[order(fc$STUDENT_ID, fc$YEAR), ]
  fc <- fc[!duplicated(fc$STUDENT_ID), ]
  eg <- make_prep2_sim_run()$entry_grade
  expect_identical(eg$N[eg$ENTRY_GRADE == "K"],
                   sum(!fc$LEFT_TRUNCATED & fc$GRADE == "K"))
  expect_identical(sum(eg$N), sum(!fc$LEFT_TRUNCATED))
})

test_that("entry grade and entry-by-year are zero-filled and suppressed", {
  entry <- compute_entry(make_prep2_bundle(), prep2_spec())
  eg <- compute_entry_grade(entry, prep2_spec())
  expect_identical(eg$ENTRY_GRADE, c("K", as.character(1:12)))
  expect_identical(eg$N[eg$ENTRY_GRADE == "K"], 27L)       # A (24) + C (3)
  expect_identical(eg$N[eg$ENTRY_GRADE == "1"], 0L)
  expect_identical(attr(eg, "counts")[["UNKNOWN"]], 12L)   # B
  expect_identical(attr(eg, "counts")[["OBSERVED"]], 27L)

  by_year <- compute_entry_by_year(entry, prep2_spec())
  expect_identical(sort(unique(by_year$ENTRY_YEAR)), c(2016L, 2017L))
  expect_identical(by_year$N[by_year$ENTRY_YEAR == 2016L & by_year$ENTRY_GRADE == "K"], 24L)
  expect_true(is.na(by_year$N[by_year$ENTRY_YEAR == 2017L & by_year$ENTRY_GRADE == "K"]))  # 3 < 10
})

test_that("candidate K cohorts are the same cohorts Check 1 selects from", {
  bundle <- make_prep2_sim_bundle()
  spec <- prep2_spec()
  kc <- make_prep2_sim_run()$entry_cohorts
  c1 <- compute_cohorts(bundle, spec)

  usable <- kc[kc$FULL_WINDOW, ]
  expect_identical(usable$COHORT_LABEL, c1$COHORT_LABEL)
  expect_identical(usable$N_ENTRY, c1$N_ENTRY)
  expect_identical(usable$FOLLOWUP_YEARS, c1$FOLLOWUP_YEARS)

  # The shorter cohorts are listed too, marked as unable to see the window.
  expect_gt(nrow(kc), nrow(c1))
  expect_false(any(kc$FULL_WINDOW[!kc$COHORT_LABEL %in% c1$COHORT_LABEL]))
  expect_identical(kc$FULL_WINDOW, kc$FOLLOWUP_YEARS >= spec_max_years(spec) + 1L)
})

test_that("cohorts are suppressed below min_cell and NULL for a short panel", {
  kc <- compute_entry_cohorts(make_prep2_bundle(), prep2_spec())
  # C's three K entrants in 2017 are below min_cell; the row stays.
  expect_true(kc$SUPPRESSED[kc$COHORT_START_YEAR == 2017L])
  expect_true(is.na(kc$N_ENTRY[kc$COHORT_START_YEAR == 2017L]))
  expect_identical(kc$N_ENTRY[kc$COHORT_START_YEAR == 2016L], 24L)
  expect_false(any(kc$FULL_WINDOW))                       # 5 and 4 years < 7

  roster <- make_prep2_roster()
  roster <- roster[roster$YEAR == 2016L, ]
  expect_null(compute_entry_cohorts(make_prep2_bundle(roster), prep2_spec()))
})

# ---- time in status ----------------------------------------------------------

test_that("the long-term share is a range while entry is unknown", {
  tis <- compute_time_in_status(make_prep2_bundle(), prep2_spec(),
                                compute_entry(make_prep2_bundle(), prep2_spec()))
  y <- tis$by_year
  # 2019: 29 current, B (12) with unknown entry and only 5 years observed.
  y19 <- y[y$YEAR == 2019L, ]
  expect_equal(y19$PCT_LT_MIN, 0)
  expect_equal(y19$PCT_LT_MAX, 12 / 29)
  expect_equal(y19$PCT_UNKNOWN, 12 / 29)
  # 2020: B has now been observed 6 years, so the bounds meet.
  y20 <- y[y$YEAR == 2020L, ]
  expect_equal(y20$PCT_LT_MIN, 12 / 29)
  expect_equal(y20$PCT_LT_MAX, y20$PCT_LT_MIN)
  # First-year shares count dated entries only.
  expect_equal(y$PCT_NEW[y$YEAR == 2016L], 24 / 36)
  expect_equal(y$PCT_NEW[y$YEAR == 2017L], 3 / 39)
})

test_that("a delivered entry field collapses the range to a point", {
  bundle <- make_prep2_bundle(make_prep2_roster(entry_fields = TRUE))
  spec <- prep2_spec()
  tis <- compute_time_in_status(bundle, spec, compute_entry(bundle, spec))
  y19 <- tis$by_year[tis$by_year$YEAR == 2019L, ]
  # B entered in 2013: seven years in status by 2019.
  expect_equal(y19$PCT_LT_MIN, 12 / 29)
  expect_equal(y19$PCT_LT_MAX, y19$PCT_LT_MIN)
  expect_equal(y19$PCT_UNKNOWN, 0)
})

test_that("time in status has grade and distribution views over K-12", {
  bundle <- make_prep2_bundle()
  spec <- prep2_spec()
  tis <- compute_time_in_status(bundle, spec, compute_entry(bundle, spec))
  expect_identical(tis$by_grade$GRADE, c("K", as.character(1:12)))
  expect_identical(nrow(tis$distribution), 13L * length(.p2_dur_levels(6L)))

  # 2020: A11-A24 (14) in grade 4 at year 5; B (12) in grade 8, entry unknown
  # but observed six years; C (3) in grade 3 at year 4.
  d <- tis$distribution
  expect_equal(sum(d$PCT[d$GRADE == "4"]), 1)
  expect_equal(d$PCT[d$GRADE == "4" & d$DUR_BIN == "5"], 1)
  expect_equal(d$PCT[d$GRADE == "8" & d$DUR_BIN == "6+"], 1)
  expect_true(is.na(d$PCT[d$GRADE == "3" & d$DUR_BIN == "4"]))   # 3 < min_cell
})

test_that("time in status is NULL without a structured long-term definition", {
  spec <- prep2_spec()
  spec$policy$timelines$lt_el_years <- NULL
  bundle <- make_prep2_bundle(spec = spec)
  expect_null(compute_time_in_status(bundle, spec, compute_entry(bundle, spec)))
  res <- run_prep2(bundle, spec)
  expect_null(res$time_in_status)
  expect_null(res$profile$TIME_IN_STATUS)                 # the profile drops it too
  expect_true(is.character(res$analytic_sample_memo))     # the memo drops that bullet
})

test_that("no mean or average time in status is computed anywhere", {
  # Time in status is censored; its mean is not defined (CLAUDE.md 9).
  files <- list.files(test_path("..", ".."), pattern = "^prep2_.*\\.R$",
                      full.names = TRUE, recursive = TRUE)
  skip_if(length(files) == 0, "package sources not reachable from the test dir")
  code <- unlist(lapply(files, function(f) {
    l <- readLines(f, warn = FALSE)
    l[!grepl("^\\s*#", l)]
  }))
  expect_identical(
    grep("(mean|median)\\([^)]*(YEAR_IN_STATUS|YEARS_IN_STATUS|YEARS_OBSERVED|DURATION)",
         code, value = TRUE),
    character())

  res <- make_prep2_sim_run()
  cols <- unique(unlist(lapply(prep2_frames(res), names)))
  expect_false(any(grepl("MEAN|AVERAGE|MEDIAN", cols)))
})

# ---- initial ELP -------------------------------------------------------------

test_that("alternate-form students are excluded from the initial-ELP bands", {
  bundle <- make_prep2_bundle()
  spec <- prep2_spec()
  ie <- compute_initial_elp(bundle, spec, compute_entry(bundle, spec))

  # 27 observed entrants: A01-A12 (annual, level 1.5) + C (3), and A13-A24
  # (alternate: a different scale, never banded).
  expect_identical(ie$N[ie$INITIAL == "1.0-1.9"], 15L)
  expect_identical(ie$N[ie$INITIAL == .P2_ALT_LABEL], 12L)
  expect_identical(sum(ie$N), 27L)                       # the shares sum to the entrants
  expect_false(ie$IN_BANDS[ie$INITIAL == .P2_ALT_LABEL])
  expect_identical(ie$INITIAL[ie$IN_BANDS], spec$elp$bands$labels)

  # The bars show bands only; the alternate group is in the table, not a bar.
  p <- plot_initial_elp(ie)
  expect_identical(nrow(p$data), length(spec$elp$bands$labels))
  expect_false(.P2_ALT_LABEL %in% p$data$INITIAL)
})

test_that("the initial-level rule follows the spec", {
  bundle <- make_prep2_bundle()
  spec <- prep2_spec()
  entry <- compute_entry(bundle, spec)
  expect_identical(attr(compute_initial_elp(bundle, spec, entry), "source"),
                   "first_annual")
  spec$policy$timelines$initial_elp_source <- "entry_year"
  expect_identical(attr(compute_initial_elp(bundle, spec, entry), "source"),
                   "entry_year")
})

test_that("the recomputed initial band is checked against the delivered one", {
  ie <- make_prep2_sim_run()$initial_elp
  expect_match(paste(attr(ie, "notes"), collapse = " "),
               "agrees with the delivered `INITIAL_ELP` for 100.0%", fixed = TRUE)
  expect_true(all(c("alternate form (excluded from bands)") %in% ie$INITIAL))
})

# ---- profile and monitoring --------------------------------------------------

test_that("profile shares are within-EL, suppressed to NA, rows kept", {
  bundle <- make_prep2_bundle()
  spec <- prep2_spec()
  p <- compute_profile(bundle, spec, compute_entry(bundle, spec))

  hl <- p$HOME_LANGUAGE
  # 2020 current: A11-A24 (14, spa), B (12, hat), C (3, por -> suppressed).
  expect_identical(hl$VALUE, c("spa", "hat", "por"))
  expect_identical(hl$N[1:2], c(14L, 12L))
  expect_true(is.na(hl$N[3]))
  expect_true(is.na(hl$PCT[3]))
  expect_true(hl$SUPPRESSED[3])
  expect_equal(hl$PCT[1], 14 / 29)

  # Only variables the delivery carries are profiled.
  expect_false("SERVICE_MODEL" %in% names(p))
  expect_true("ELP_FORM" %in% names(p))
})

test_that("the heritage slot is NULL and does not error", {
  p <- make_prep2_sim_run()$profile
  expect_true("HERITAGE" %in% names(p))                  # present, deliberately empty
  expect_null(p$HERITAGE)
  expect_no_error(ft_prep2_profile(make_prep2_sim_run()))
})

test_that("time in status is profiled in its natural order", {
  p <- make_prep2_sim_run()$profile$TIME_IN_STATUS
  expect_identical(p$VALUE[1], "1 year (new)")
  expect_match(p$VALUE[2], "^2-5 years")
  expect_match(p$VALUE[3], "long-term")
})

test_that("monitoring counts are zero-filled, and NULL without a monitoring field", {
  m <- compute_monitoring(make_prep2_bundle(), prep2_spec())
  expect_identical(m$N[m$YEAR == 2019L & m$MONITOR == "Monitoring year 1"], 10L)
  expect_identical(m$N[m$YEAR == 2020L & m$MONITOR == "Monitoring year 2"], 10L)
  expect_identical(m$N[m$YEAR == 2015L & m$MONITOR == "Monitoring year 1"], 0L)

  roster <- make_prep2_roster()
  roster$EL_MONITOR_YEAR <- NULL
  expect_null(compute_monitoring(make_prep2_bundle(roster), prep2_spec()))
  roster <- roster[roster$EL_STATUS != "former", ]
  expect_null(compute_monitoring(make_prep2_bundle(roster), prep2_spec()))
})

# ---- the Additional check: Type-gated ---------------------------------------

test_that("the criteria crosswalk is NULL under a Type 1 spec", {
  # Type-gated: Types 2-6 only. The SIM spec is Type 1 and skips it.
  res <- make_prep2_sim_run()
  expect_null(res$criteria_crosswalk)
  expect_null(res$contested_zone)
  expect_null(res$blocked_groups)
  expect_null(compute_criteria_crosswalk(make_prep2_sim_bundle(), prep2_spec()))
})

# The crosswalk on the full sim, once for the file.
prep2_type2_cw <- local({
  cw <- NULL
  function() {
    if (is.null(cw)) {
      cw <<- compute_criteria_crosswalk(make_prep2_sim_bundle(),
                                        make_prep2_type2_spec())
    }
    cw
  }
})

test_that("a Type 2 rule populates the crosswalk, matching an independent count", {
  cw <- prep2_type2_cw()
  expect_false(is.null(cw))
  expect_named(cw, c("crosswalk", "contested_zone", "blocked_groups"))

  # Independent: overall level >= 4.5 AND reading level >= 4.0 (exempt when the
  # domain is waived), over the annual student-years the sim carries.
  b <- el_long_sim[el_long_sim$ELP_FORM == "annual", ]
  keys <- unique(paste(b$STUDENT_ID, b$YEAR))
  level <- function(comp) {
    s <- b[b$COMPONENT == comp, ]
    s$ACHIEVEMENT_LEVEL[match(keys, paste(s$STUDENT_ID, s$YEAR))]
  }
  ov <- level("overall")
  rd <- level("reading")
  pass_ov <- !is.na(ov) & ov >= 4.5
  pass_rd <- is.na(rd) | rd >= 4.0
  expect_identical(cw$contested_zone$n_total, length(keys))
  expect_identical(cw$contested_zone$n_disagree, sum(pass_ov != pass_rd))

  z <- cw$contested_zone
  expect_identical(z$cut, 4.5)
  dis <- ov[pass_ov != pass_rd & !is.na(ov)]
  expect_equal(z$range, range(dis))

  # Blocked by exactly one criterion, near the cut.
  near <- !is.na(ov) & abs(ov - 4.5) <= 0.5
  expect_identical(nrow(cw$blocked_groups), sum(near & (pass_ov != pass_rd)))
  expect_setequal(unique(cw$blocked_groups$BLOCKED_BY),
                  c("OVERALL_LEVEL", "READING_LEVEL"))

  # Pass/fail patterns near the cut sum to the window (unless a rare pattern
  # was suppressed, which can only lower the visible total).
  pat <- cw$crosswalk$pattern
  expect_identical(cw$crosswalk$n_window, sum(near))
  if (!any(pat$SUPPRESSED)) expect_identical(sum(pat$N), sum(near))
  else expect_lt(sum(pat$N, na.rm = TRUE), sum(near))
  expect_true(all(c("OVERALL_LEVEL", "READING_LEVEL") %in% names(pat)))
  expect_true(all(unlist(pat[c("OVERALL_LEVEL", "READING_LEVEL")]) %in% c("pass", "fail")))
})

test_that("the crosswalk tables and notes build from a Type 2 result", {
  cw <- prep2_type2_cw()
  res <- list(criteria_crosswalk = cw$crosswalk, contested_zone = cw$contested_zone,
              blocked_groups = cw$blocked_groups)

  expect_s3_class(ft_prep2_crosswalk_pattern(res), "flextable")
  expect_s3_class(ft_prep2_crosswalk_blocked(res), "flextable")
  expect_s3_class(ft_prep2_crosswalk_by_level(res), "flextable")
  expect_match(prep2_crosswalk_note(res), "OVERALL_LEVEL", fixed = TRUE)
  expect_match(prep2_crosswalk_note(res), "conjunctive", fixed = TRUE)
  expect_match(prep2_contested_note(res), "Contested zone", fixed = TRUE)
})

# ---- no WIDA assumptions -----------------------------------------------------

test_that("run_prep2() runs on the ELPA21 fixture without naming a component", {
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  bundle <- as_el_data(make_el_data_elpa21_base(spec),
                       roster = make_el_data_elpa21_roster(), spec = spec)
  expect_no_error(res <- run_prep2(bundle, spec))

  # Degrades, never crashes: no districts, no never-ELs, no LTEL definition,
  # no monitoring field.
  expect_null(res$by_district)
  expect_null(res$district_bins)
  expect_null(res$el_share)
  expect_null(res$time_in_status)
  expect_null(res$monitoring)

  # A categorical headline with no declared bands: one band per level.
  expect_identical(res$initial_elp$INITIAL[res$initial_elp$IN_BANDS],
                   spec$elp$level_scale$levels)

  # Type 6 opens the gate: the Additional-check slots populate structurally.
  expect_false(is.null(res$criteria_crosswalk))
  expect_false(is.null(res$contested_zone))
  expect_true(all(c("LISTENING_LEVEL", "SPEAKING_LEVEL", "READING_LEVEL",
                    "WRITING_LEVEL") %in% names(res$criteria_crosswalk$pattern)))
  # No criterion sits on the headline, so there is no cut to window around.
  expect_true(is.na(res$criteria_crosswalk$window[["cut"]]))
  expect_match(prep2_crosswalk_note(res), "no criterion sits on the headline")
  expect_match(prep2_contested_note(res), "never disagree")
})

test_that("a declared Type with no two-term pathway degrades with a note", {
  # Type 5 (tiered): two pathways of one term each. The gate is open, but
  # there is nothing to set against anything else.
  s5 <- prep2_spec()
  s5$policy$exit$pathways <- list(
    s5$policy$exit$pathways[[1]],
    modifyList(s5$policy$exit$pathways[[1]],
               list(name = "committee", discretionary = TRUE)))
  s5$policy$exit$types <- 5L
  validate_el_spec(s5)

  bundle <- make_prep2_bundle(spec = s5)
  expect_null(compute_criteria_crosswalk(bundle, s5))
  res <- run_prep2(bundle, s5)
  expect_null(res$criteria_crosswalk)
  expect_match(res$meta$notes$crosswalk, "two or more ELP terms")
})

# ---- figures and tables ------------------------------------------------------

test_that("every figure builds from its slot without recomputing", {
  res <- make_prep2_sim_run()
  plots <- list(
    plot_counts_by_year(res$counts_by_year),
    plot_el_share(res$el_share),
    plot_by_grade_span(res$by_grade_span),
    plot_by_grade_span(res$by_grade_span, type = "count"),
    plot_by_grade(res$by_grade),
    plot_by_district(res$by_district),
    plot_entry_grade(res$entry_grade),
    plot_time_in_status(res$time_in_status),
    plot_initial_elp(res$initial_elp))
  for (p in plots) {
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
  # The district chart's reference lines come from the compute step.
  expect_identical(sum(res$by_district$TWO_THIRDS), 6L)
  expect_error(plot_by_district(res$counts_by_year), "matching slot")
})

test_that("every table builds from the results object", {
  res <- make_prep2_sim_run()
  fts <- list(ft_prep2_coverage(res), ft_prep2_el_share(res),
              ft_prep2_grade_shares(res), ft_prep2_district_bins(res),
              ft_prep2_districts(res), ft_prep2_entry_sources(res),
              ft_prep2_entry_by_year(res), ft_prep2_entry_cohorts(res),
              ft_prep2_time_by_year(res), ft_prep2_time_by_grade(res),
              ft_prep2_initial_elp(res), ft_prep2_profile(res),
              ft_prep2_monitoring(res))
  for (ft in fts) expect_s3_class(ft, "flextable")
  # A slot that is NULL says so, rather than printing an empty table.
  expect_error(ft_prep2_el_share(list(el_share = NULL)), "NULL")
})

test_that("table years are text, so Word never prints 2,015", {
  ft <- ft_prep2_coverage(make_prep2_sim_run())
  expect_identical(ft$body$dataset$YEAR[1], "2015")
})

# ---- the document ------------------------------------------------------------

test_that("the memo is built from the slots and names the K cohorts", {
  memo <- make_prep2_sim_run()$analytic_sample_memo
  expect_true(any(grepl("^\\*\\*Panel\\.\\*\\*", memo)))
  expect_true(any(grepl("two-thirds", memo)))
  expect_true(any(grepl("K-start cohorts that cover the full exit window", memo)))
  expect_true(any(grepl("Alternate-form ELP", memo)))
})

test_that("the document opens with Sample and carries the guardrail text", {
  qmd <- stage_qmd_path("prep2")
  skip_if(is.na(qmd), "prep2 document not found")
  txt <- readLines(qmd, warn = FALSE)
  expect_true(any(grepl("^## Sample", txt)))
  expect_lt(which(grepl("^## Sample", txt))[1], which(grepl("^## Looking at", txt))[1])
  # Slide 38: resist the urge to average.
  expect_true(any(grepl("Resist the urge to average", txt, fixed = TRUE)))
  expect_true(any(grepl("## Decide", txt, fixed = TRUE)))
  # The Type-gated check comes before the closing Decide box (CLAUDE.md 6).
  expect_lt(which(grepl("^## Additional check", txt))[1],
            which(grepl("^## Decide before moving on", txt))[1])
})

test_that("the package sources for this stage are ASCII", {
  files <- list.files(test_path("..", ".."), pattern = "^prep2_.*\\.R$",
                      full.names = TRUE, recursive = TRUE)
  skip_if(length(files) == 0, "package sources not reachable from the test dir")
  for (f in files) {
    # NA from iconv marks a line that is not plain ASCII.
    lines <- readLines(f, warn = FALSE)
    expect_false(anyNA(iconv(lines, "UTF-8", "ASCII")), label = basename(f))
  }
})
