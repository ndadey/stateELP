# Shared derivations. The A-J block is the convention test: it does not just
# assert that first-former dating gives the deck's numbers, it also shows that
# last-current dating gives different ones, so the convention is proved rather
# than declared.

sim_spec <- function() read_el_spec("00_SIM_EL_Spec")

aj_flags <- function(spec = sim_spec()) {
  roster <- make_utils_aj_roster()
  add_exit_flags(
    build_student_spans(roster, last_year = make_utils_window(roster)), spec)
}

# ---- exit dating: the A-J example -------------------------------------------

test_that("year in status counts the exit year, and E exits in year 4", {
  roster <- make_utils_aj_roster()
  spans <- build_student_spans(roster, last_year = make_utils_window(roster))

  yrs <- stats::setNames(spans$EXIT_YEAR_IN_STATUS, spans$STUDENT_ID)

  # sim_contract.md 3.3 names student E as the check on this convention.
  expect_identical(yrs[["E"]], 4L)

  # The full set, read straight off slides 43-51.
  expect_identical(yrs[c("A", "C", "E", "F", "H", "J")],
                   c(A = 7L, C = 6L, E = 4L, F = 6L, H = 5L, J = 9L))
  expect_true(all(is.na(yrs[c("B", "D", "G", "I")])))
})

test_that("A-J reproduces the deck's two headline statistics", {
  spec <- sim_spec()
  flags <- aj_flags(spec)

  # Slide 51: "40% within 6 years of starting", with max_years_to_exit = 6.
  expect_identical(spec_max_years(spec), 6L)
  expect_equal(mean(flags$EXIT_MAX), 0.40)

  # Slide 50: 50% of the base cohort has exited by year 7 (grade 6), and not
  # before -- so the cumulative curve first reaches 50% at year 7.
  cum_by <- function(y) mean(flags$EXITED & flags$EXIT_YEAR_IN_STATUS <= y)
  expect_equal(cum_by(6L), 0.40)
  expect_equal(cum_by(7L), 0.50)
  expect_identical(Position(function(y) cum_by(y) >= 0.5, 1:13), 7L)
})

test_that("last-current dating demonstrably fails the same example", {
  # The counter-test. The simulator flags exit on the last current row; if the
  # package adopted that, both of the deck's numbers would move.
  roster <- make_utils_aj_roster()
  alt    <- make_utils_aj_last_current(roster)

  alt_cum <- function(y) mean(alt$EXITED & alt$EXIT_YEAR_IN_STATUS <= y)

  expect_equal(alt_cum(6L), 0.50)                  # not the deck's 40%
  expect_identical(Position(function(y) alt_cum(y) >= 0.5, 1:13), 6L)  # not 7

  # Stated as the relationship sim_contract.md 3.3 fixes: the package's exit
  # year is the flagged row's year + 1, every time.
  pkg <- build_student_spans(roster, last_year = make_utils_window(roster))
  both <- merge(pkg[pkg$EXITED, c("STUDENT_ID", "EXIT_YEAR_IN_STATUS")],
                alt[alt$EXITED, c("STUDENT_ID", "EXIT_YEAR_IN_STATUS")],
                by = "STUDENT_ID", suffixes = c("_PKG", "_SIM"))
  expect_identical(nrow(both), 6L)
  expect_identical(both$EXIT_YEAR_IN_STATUS_PKG,
                   both$EXIT_YEAR_IN_STATUS_SIM + 1L)
})

test_that("censored is not the same as did-not-exit", {
  flags <- aj_flags()

  # B, D, G and I never exit, but they disappeared -- they are not evidence
  # that a student stayed in status.
  expect_setequal(flags$STUDENT_ID[flags$DISAPPEARED], c("B", "D", "G", "I"))
  expect_true(all(!flags$EXIT_EVER[flags$DISAPPEARED]))
  expect_true(all(flags$CENSORED[flags$DISAPPEARED]))

  # Disappeared is dated to the first absent year: B is last seen in year 7.
  b <- flags[flags$STUDENT_ID == "B", ]
  expect_identical(b$LAST_YEAR_IN_STATUS, 7L)
  expect_identical(b$CENSOR_YEAR, b$LAST_OBSERVED_YEAR + 1L)

  # Disappeared and NO_EXIT are disjoint, so Check 2's columns sum to the
  # cohort (open decision 11).
  expect_false(any(flags$DISAPPEARED & flags$NO_EXIT))
  expect_identical(sum(flags$EXIT_EVER) + sum(flags$NO_EXIT) +
                     sum(flags$DISAPPEARED), nrow(flags))
})

test_that("the window decides disappeared vs still-enrolled", {
  roster <- make_utils_aj_roster()
  # Without the deck's grade-12 window, D and G -- last seen in the last year
  # anyone is seen -- read as still enrolled rather than disappeared.
  narrow <- build_student_spans(roster)                      # window defaults
  wide   <- build_student_spans(roster, last_year = make_utils_window(roster))

  expect_identical(narrow$CENSOR_TYPE[narrow$STUDENT_ID == "D"], "window_edge")
  expect_identical(wide$CENSOR_TYPE[wide$STUDENT_ID == "D"], "disappeared")
  # Either way they are censored and neither is an exiter.
  expect_true(all(narrow$CENSORED[narrow$STUDENT_ID %in% c("B", "D", "G", "I")]))
})

test_that("the nested exit indicators really nest", {
  flags <- aj_flags()
  expect_true(all(!flags$EXIT_ONTIME | flags$EXIT_MAX))
  expect_true(all(!flags$EXIT_MAX | flags$EXIT_EVER))
  # Prolonged is late exiters plus the never-exited, and never the disappeared.
  expect_false(any(flags$PROLONGED & flags$DISAPPEARED))
  expect_identical(flags$ON_TIME, flags$EXIT_ONTIME)
  # SIM on-time default is five years: E (4) and H (5) are on time; A, C, F
  # and J exited late.
  expect_setequal(flags$STUDENT_ID[flags$ON_TIME], c("E", "H"))
  expect_setequal(flags$STUDENT_ID[flags$PROLONGED], c("A", "C", "F", "J"))
})

test_that("the on-time rule is read from the spec, keyed by grade then band", {
  roster <- make_utils_aj_roster()
  spans <- build_student_spans(roster, last_year = make_utils_window(roster))
  spans$INITIAL_ELP <- ifelse(spans$STUDENT_ID %in% c("C", "F"),
                              "1.0-1.9", "2.0-2.9")

  spec <- sim_spec()
  spec$policy$timelines$on_time_rule <- list(
    default_years   = 4L,
    by_start_grade  = c(K = 5L),          # every A-J student starts in K
    by_initial_band = c("1.0-1.9" = 6L)   # ... but band wins for C and F
  )
  flags <- add_exit_flags(spans, spec)
  expect_identical(
    flags$ON_TIME_YEARS[match(c("E", "C"), flags$STUDENT_ID)], c(5L, 6L))
  expect_setequal(flags$STUDENT_ID[flags$ON_TIME], c("C", "E", "F", "H"))

  # A band-keyed rule without INITIAL_ELP on the spans is an error, not a
  # silent fall back to the default.
  spans$INITIAL_ELP <- NULL
  expect_error(add_exit_flags(spans, spec), "need INITIAL_ELP")
})

test_that("a gap ends observation: the student is censored at the first absence", {
  roster <- tibble::tibble(
    STUDENT_ID = "X",
    YEAR       = c(2015L, 2016L, 2018L, 2019L),      # 2017 absent
    GRADE      = c("K", "1", "3", "4"),
    EL_STATUS  = c("current", "current", "current", "former")
  )
  sp <- build_student_spans(roster, last_year = 2020L)
  expect_false(sp$EXITED)                  # the 2019 exit is after the gap
  expect_identical(sp$CENSOR_TYPE, "disappeared")
  expect_identical(sp$CENSOR_YEAR, 2017L)
  expect_identical(sp$LAST_YEAR_IN_STATUS, 2L)
})

test_that("a student seen only as former has no span: never observed at risk", {
  roster <- tibble::tibble(
    STUDENT_ID = c("P", "P", "Q"),
    YEAR       = c(2015L, 2016L, 2015L),
    GRADE      = c("4", "5", "K"),
    EL_STATUS  = c("former", "former", "current")
  )
  expect_identical(build_student_spans(roster)$STUDENT_ID, "Q")
})

# ---- year in status ----------------------------------------------------------

test_that("year in status is calendar-based, so a gap year does not renumber", {
  roster <- tibble::tibble(
    STUDENT_ID = "X",
    YEAR       = c(2015L, 2016L, 2018L),   # 2017 missing
    GRADE      = c("K", "1", "3"),
    EL_STATUS  = c("current", "current", "current")
  )
  expect_identical(add_year_in_status(roster)$YEAR_IN_STATUS, c(1L, 2L, 4L))
})

test_that("the exit year is a year in status; monitoring years are not", {
  roster <- make_utils_aj_roster()
  e <- add_year_in_status(roster[roster$STUDENT_ID == "E", ])
  expect_identical(e$YEAR_IN_STATUS, 1:4)       # EL, EL, EL, R -> 4

  mon <- tibble::tibble(
    STUDENT_ID = "M",
    YEAR       = 2015:2019,
    GRADE      = c("K", "1", "2", "3", "4"),
    EL_STATUS  = c("current", "current", "former", "former", "current")
  )
  # Exit absorbing: the later current row is not back in status.
  expect_identical(add_year_in_status(mon)$YEAR_IN_STATUS,
                   c(1L, 2L, 3L, NA, NA))
})

test_that("never-EL rows carry no year in status, and a delivered one is replaced", {
  roster <- tibble::tibble(
    STUDENT_ID     = c("N1", "N1"),
    YEAR           = c(2015L, 2016L),
    GRADE          = c("3", "4"),
    EL_STATUS      = c("never", "never"),
    YEAR_IN_STATUS = c(7L, 8L)             # delivered, and wrong
  )
  out <- add_year_in_status(roster)
  expect_true(all(is.na(out$YEAR_IN_STATUS)))
  expect_identical(sum(names(out) == "YEAR_IN_STATUS"), 1L)
})

test_that("LT_EL comes from the spec's structured year, and is NA without one", {
  roster <- add_year_in_status(make_utils_aj_roster())
  spec <- sim_spec()
  expect_identical(spec$policy$timelines$lt_el_years, 6L)

  lt <- add_lt_el(roster, spec)
  # Current in year 6 or later: A (EL through year 6, then exits), B, D, G, J.
  expect_setequal(unique(lt$STUDENT_ID[lt$LT_EL]), c("A", "B", "D", "G", "J"))
  expect_false(any(lt$LT_EL & lt$EL_STATUS != "current"))

  spec$policy$timelines$lt_el_years <- NULL
  expect_true(all(is.na(add_lt_el(roster, spec)$LT_EL)))
})

# ---- suppression -------------------------------------------------------------

test_that("suppression blanks cells and never drops rows", {
  x <- tibble::tibble(GROUP = c("a", "b", "c"),
                      N = c(50L, 9L, 10L),
                      PCT = c(0.5, 0.9, 0.1))
  out <- apply_suppression(x, sim_spec())

  expect_identical(nrow(out), 3L)                 # the row survives
  expect_identical(out$PCT, c(0.5, NA, 0.1))      # only the small cell blanks
  expect_identical(out$N, c(50L, 9L, 10L))        # N stays visible by default
  expect_identical(out$SUPPRESSED, c(FALSE, TRUE, FALSE))
  # min_cell = 10 is a floor, not a ceiling: exactly 10 survives.
  expect_false(out$SUPPRESSED[3])

  expect_identical(apply_suppression(x, sim_spec(), blank_n = TRUE)$N,
                   c(50L, NA, 10L))
})

test_that("min_cell is read from the spec, never a literal", {
  spec <- sim_spec()
  spec$policy$suppression$min_cell <- 25L
  x <- tibble::tibble(N = c(20L, 30L), PCT = c(0.1, 0.2))
  expect_identical(apply_suppression(x, spec)$PCT, c(NA, 0.2))
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
  lv <- spec$elp$level_scale$levels
  # Stored as the ordinal index, banded as the level's label ...
  expect_identical(assign_elp_band(c(1, 3, NA), spec), c(lv[1], lv[3], NA))
  # ... and a label delivered as a label passes through.
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
  roster <- tibble::tibble(
    STUDENT_ID     = rep(c("A", "B"), each = 3),
    YEAR           = rep(2015:2017, times = 2),
    GRADE          = rep(c("K", "1", "2"), times = 2),
    EL_STATUS      = "current",
    LEFT_TRUNCATED = rep(c(FALSE, TRUE), each = 3)
  )
  co <- build_cohort(roster, sim_spec(), start_grade = "K",
                     entry_years = 2015L)
  expect_identical(co$STUDENT_ID, "A")
  expect_identical(co$COHORT_LABEL, "K-2015")
})

test_that("without LEFT_TRUNCATED, first-panel-year entry is treated as truncated", {
  roster <- tibble::tibble(
    STUDENT_ID = c("A", "A", "B", "B"),
    YEAR       = c(2015L, 2016L, 2016L, 2017L),
    GRADE      = c("K", "1", "K", "1"),
    EL_STATUS  = "current"
  )
  co <- build_cohort(roster, sim_spec(), entry_years = 2015:2016)
  expect_identical(co$STUDENT_ID, "B")
})

test_that("entry years are derived from the panel, never named as literals", {
  # Two entry years; only the earlier one has the max timeline plus one year
  # of follow-up inside a panel ending in 2021.
  roster <- dplyr::bind_rows(
    tibble::tibble(STUDENT_ID = "A", YEAR = 2015:2021,
                   GRADE = c("K", as.character(1:6)), EL_STATUS = "current",
                   LEFT_TRUNCATED = FALSE),
    tibble::tibble(STUDENT_ID = "B", YEAR = 2019:2021,
                   GRADE = c("K", "1", "2"), EL_STATUS = "current",
                   LEFT_TRUNCATED = FALSE)
  )
  co <- build_cohort(roster, sim_spec(), start_grade = "K")
  expect_identical(co$STUDENT_ID, "A")
})

# ---- no mean time in status --------------------------------------------------

test_that("no mean or average time in status is computed in R/", {
  # Time in status is censored; its mean is not defined on this data
  # (sim_contract.md 8). A grep guard over the package source.
  files <- list.files(test_path("..", "..", "R"), pattern = "\\.R$",
                      full.names = TRUE)
  skip_if(length(files) == 0, "package sources not reachable from the test dir")
  code <- unlist(lapply(files, function(f) {
    l <- readLines(f, warn = FALSE)
    l[!grepl("^\\s*#", l)]
  }))
  hits <- grep("mean\\([^)]*(YEAR_IN_STATUS|EXIT_YEAR_IN_STATUS|LAST_YEAR_IN_STATUS)",
               code, value = TRUE)
  expect_identical(hits, character())
})
