# Shared derivations. The A-J block is the convention test: it does not just
# assert that first-former dating gives the deck's numbers, it also shows that
# last-current dating gives different ones, so the convention is proved rather
# than declared.

sim_spec <- function() read_el_spec("00_SIM_EL_Spec")

# ---- exit dating: the A-J example -------------------------------------------

test_that("year in status counts the exit year, and E exits in year 4", {
  spine <- make_utils_aj_spine()
  spans <- build_student_spans(spine, last_year = make_utils_window(spine))

  yrs <- stats::setNames(spans$exit_year_in_status, spans$student_id)

  # sim_contract.md 3.3 names student E as the check on this convention.
  expect_identical(yrs[["E"]], 4L)

  # The full set, read straight off slides 43-51.
  expect_identical(yrs[c("A", "C", "E", "F", "H", "J")],
                   c(A = 7L, C = 6L, E = 4L, F = 6L, H = 5L, J = 9L))
  expect_true(all(is.na(yrs[c("B", "D", "G", "I")])))
})

test_that("A-J reproduces the deck's two headline statistics", {
  spine <- make_utils_aj_spine()
  spec  <- sim_spec()
  flags <- add_exit_flags(
    build_student_spans(spine, last_year = make_utils_window(spine)), spec)

  # Slide 51: "40% within 6 years of starting", with max_years_to_exit = 6.
  expect_identical(spec_max_years(spec), 6L)
  expect_equal(mean(flags$exit_max), 0.40)

  # Slide 50: 50% of the base cohort has exited by year 7 (grade 6), and not
  # before -- so the cumulative curve first reaches 50% at year 7.
  cum_by <- function(y) mean(flags$exited & flags$exit_year_in_status <= y)
  expect_equal(cum_by(6L), 0.40)
  expect_equal(cum_by(7L), 0.50)
  expect_identical(Position(function(y) cum_by(y) >= 0.5, 1:13), 7L)
})

test_that("last-current dating demonstrably fails the same example", {
  # The counter-test. The simulator flags exit on the last current row; if the
  # package adopted that, both of the deck's numbers would move.
  spine <- make_utils_aj_spine()
  alt   <- make_utils_aj_last_current(spine)

  alt_cum <- function(y) mean(alt$exited & alt$exit_year_in_status <= y)

  expect_equal(alt_cum(6L), 0.50)                  # not the deck's 40%
  expect_identical(Position(function(y) alt_cum(y) >= 0.5, 1:13), 6L)  # not 7

  # Stated as the relationship sim_contract.md 3.3 fixes: the package's exit
  # year is the flagged row's year + 1, every time.
  pkg <- build_student_spans(spine, last_year = make_utils_window(spine))
  both <- merge(pkg[pkg$exited, c("student_id", "exit_year_in_status")],
                alt[alt$exited, c("student_id", "exit_year_in_status")],
                by = "student_id", suffixes = c("_pkg", "_sim"))
  expect_identical(both$exit_year_in_status_pkg,
                   both$exit_year_in_status_sim + 1L)
})

test_that("censored is not the same as did-not-exit", {
  spine <- make_utils_aj_spine()
  flags <- add_exit_flags(
    build_student_spans(spine, last_year = make_utils_window(spine)),
    sim_spec())

  # B, D, G and I never exit, but they disappeared -- they are not evidence
  # that a student stayed in status.
  expect_setequal(flags$student_id[flags$disappeared], c("B", "D", "G", "I"))
  expect_true(all(!flags$exit_ever[flags$disappeared]))

  # Disappeared and no_exit are disjoint, so Check 2's columns sum to the
  # cohort (open decision 11).
  expect_false(any(flags$disappeared & flags$no_exit))
  expect_identical(sum(flags$exit_ever) + sum(flags$no_exit) +
                     sum(flags$disappeared), nrow(flags))
})

test_that("the window decides disappeared vs still-enrolled", {
  spine <- make_utils_aj_spine()
  # Without the deck's grade-12 window, D and G -- last seen in the last year
  # anyone is seen -- read as still enrolled rather than disappeared.
  narrow <- build_student_spans(spine)                       # window defaults
  wide   <- build_student_spans(spine, last_year = make_utils_window(spine))

  expect_identical(narrow$censor_type[narrow$student_id == "D"], "window_edge")
  expect_identical(wide$censor_type[wide$student_id == "D"], "disappeared")
  # Either way they are censored and neither is an exiter.
  expect_true(all(narrow$censored[narrow$student_id %in% c("B", "D", "G", "I")]))
})

test_that("the nested exit indicators really nest", {
  spine <- make_utils_aj_spine()
  flags <- add_exit_flags(
    build_student_spans(spine, last_year = make_utils_window(spine)),
    sim_spec())
  expect_true(all(!flags$exit_ontime | flags$exit_max))
  expect_true(all(!flags$exit_max | flags$exit_ever))
  # Prolonged is late exiters plus the never-exited, and never the disappeared.
  expect_false(any(flags$prolonged & flags$disappeared))
  expect_true(all(flags$on_time == flags$exit_ontime))
})

test_that("year_in_status is calendar-based, so a gap year does not renumber", {
  spine <- tibble::tibble(
    student_id  = "X",
    school_year = c(2015L, 2016L, 2018L),   # 2017 missing
    grade       = c("K", "1", "3"),
    el_status   = c("current", "current", "current")
  )
  out <- add_year_in_status(spine)
  expect_identical(out$year_in_status, c(1L, 2L, 4L))
})

test_that("never-EL rows carry no year in status", {
  spine <- tibble::tibble(
    student_id  = c("N1", "N1"),
    school_year = c(2015L, 2016L),
    grade       = c("3", "4"),
    el_status   = c("never", "never")
  )
  expect_true(all(is.na(add_year_in_status(spine)$year_in_status)))
})

# ---- suppression -------------------------------------------------------------

test_that("suppression blanks cells and never drops rows", {
  x <- tibble::tibble(group = c("a", "b", "c"),
                      n = c(50L, 9L, 10L),
                      pct = c(0.5, 0.9, 0.1))
  out <- apply_suppression(x, sim_spec())

  expect_identical(nrow(out), 3L)                 # the row survives
  expect_identical(out$pct, c(0.5, NA, 0.1))      # only the small cell blanks
  expect_identical(out$n, c(50L, 9L, 10L))        # n stays visible by default
  expect_identical(out$suppressed, c(FALSE, TRUE, FALSE))
  # min_cell = 10 is a floor, not a ceiling: exactly 10 survives.
  expect_false(out$suppressed[3])
})

test_that("min_cell is read from the spec, never a literal", {
  spec <- sim_spec()
  spec$policy$suppression$min_cell <- 25L
  x <- tibble::tibble(n = c(20L, 30L), pct = c(0.1, 0.2))
  expect_identical(apply_suppression(x, spec)$pct, c(NA, 0.2))
})

# ---- banding -----------------------------------------------------------------

test_that("bands are left-closed, so a level at a break goes up", {
  spec <- sim_spec()
  # breaks c(1, 2, 3, 4, 4.5, 5, 7). 4.5 IS the exit threshold.
  expect_identical(assign_elp_band(c(1.0, 1.9, 2.0, 4.4, 4.5, 4.9, 5.0), spec),
                   c("1.0-1.9", "1.0-1.9", "2.0-2.9", "4.0-4.4",
                     "4.5-4.9", "4.5-4.9", "5.0+"))
  expect_true(is.na(assign_elp_band(NA_real_, spec)))
})

test_that("a spec with no declared bands gets one band per level", {
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  expect_null(spec_bands(spec))
  lv <- c("Emerging", "Progressing", "Proficient")
  expect_identical(assign_elp_band(lv, spec), lv)
})

# ---- weighting ---------------------------------------------------------------

test_that("weighted shares differ from unweighted when weights differ", {
  x <- c(TRUE, TRUE, FALSE, FALSE)
  w <- c(1, 1, 10, 10)
  expect_equal(weighted_share(x), 0.5)
  expect_equal(weighted_share(x, w), 2 / 22)
  expect_equal(weighted_n(w), 22)
})

# ---- cohorts -----------------------------------------------------------------

test_that("cohorts exclude left-truncated students", {
  spine <- tibble::tibble(
    student_id     = rep(c("A", "B"), each = 3),
    school_year    = rep(2015:2017, times = 2),
    grade          = rep(c("K", "1", "2"), times = 2),
    el_status      = "current",
    left_truncated = rep(c(FALSE, TRUE), each = 3)
  )
  co <- build_cohort(spine, sim_spec(), start_grade = "K",
                     entry_years = 2015L)
  expect_identical(co$student_id, "A")
  expect_identical(co$cohort_label, "K-2015")
})

test_that("entry years are derived from the panel, never named as literals", {
  # Two entry years; only the earlier one has enough follow-up for a
  # six-year timeline within a panel ending in 2021.
  spine <- dplyr::bind_rows(
    tibble::tibble(student_id = "A", school_year = 2015:2021,
                   grade = c("K", as.character(1:6)), el_status = "current"),
    tibble::tibble(student_id = "B", school_year = 2019:2021,
                   grade = c("K", "1", "2"), el_status = "current")
  )
  co <- build_cohort(spine, sim_spec(), start_grade = "K")
  expect_identical(co$student_id, "A")
})
