# Prep 2's context views (R/prep2_context*.R), the computations behind
# decks/context.qmd. A file of its own rather than more of test-prep2.R so
# the deck's tests stay separable from the stage's; it borrows Prep 2's
# fixtures (helper-prep2.R), whose values are derivable by hand:
#
#   A  A01-A24 K entrants 2016, current 2016-2020 (grades K-4); A01-A12 in D1,
#      A13-A24 in D2 and on the ALTERNATE form. A01-A10 are former in 2019
#      and 2020 (exit year 2019; last current year 2018, grade 2).
#   B  B01-B12 grade 3 in 2015, current 2015-2020, D1, entry undated.
#   C  C01-C03 K entrants 2017, D3.
#   N  15 never-ELs in D1, weight 10.
# Panel 2015-2020, min_cell 10. At-risk years are 2015-2019.

ctx_spec <- function() make_prep2_spec()

# ---- exit rates ----------------------------------------------------------------

test_that("exit rates by grade: happy path, all current ELs", {
  x <- compute_exit_rates(make_prep2_bundle(), ctx_spec(), by = "grade",
                          population = "all")
  r <- x[!x$POOLED & x$FIRST_YEAR == 2018L & x$GRADE == "2", ]
  expect_identical(r$N_AT_RISK, 24L)         # A01-A24
  expect_identical(r$N_EXITED, 10L)          # A01-A10, former in 2019
  expect_identical(r$N_NOT_OBSERVED, 0L)
  expect_equal(r$PCT_EXITED, 10 / 24)
  expect_identical(attr(x, "latest"), 2019L)
  expect_identical(attr(x, "pooled"), c(2015L, 2019L))
})

test_that("the tested population drops students with no annual score", {
  x <- compute_exit_rates(make_prep2_bundle(), ctx_spec(), by = "grade")
  r <- x[!x$POOLED & x$FIRST_YEAR == 2018L & x$GRADE == "2", ]
  expect_identical(r$N_AT_RISK, 12L)         # A13-A24 took the alternate form
  expect_equal(r$PCT_EXITED, 10 / 12)
  expect_identical(attr(x, "population"), "tested")
})

test_that("exit rates: structure, one row per year and group plus pooled", {
  x <- compute_exit_rates(make_prep2_bundle(), ctx_spec(), by = "grade",
                          population = "all")
  expect_named(x, c("FIRST_YEAR", "LAST_YEAR", "POOLED", "GRADE", "N_AT_RISK",
                    "N_EXITED", "N_NOT_OBSERVED", "PCT_EXITED", "SUPPRESSED"))
  expect_identical(nrow(x), 13L * 6L)        # 5 years + pooled, K-12 each
  expect_identical(x$GRADE[1:13], c("K", as.character(1:12)))
  p <- x[x$POOLED, ]
  expect_true(all(p$FIRST_YEAR == 2015L & p$LAST_YEAR == 2019L))
  # Pooled is total exits over total followed, never an average of rates.
  s <- compute_exit_rates(make_prep2_bundle(), ctx_spec(), by = "state",
                          population = "all")
  expect_identical(s$N_AT_RISK[s$POOLED], 155L)   # 12 + 36 + 39 + 39 + 29
  expect_equal(s$PCT_EXITED[s$POOLED], 10 / 155)
})

test_that("exit rates: a small cell is NA, never dropped; a zero stays 0", {
  x <- compute_exit_rates(make_prep2_bundle(), ctx_spec(), by = "grade",
                          population = "all")
  r <- x[!x$POOLED & x$FIRST_YEAR == 2018L & x$GRADE == "1", ]   # C01-C03
  expect_identical(nrow(r), 1L)
  expect_true(r$SUPPRESSED)
  expect_true(is.na(r$N_AT_RISK) && is.na(r$PCT_EXITED))
  b <- x[!x$POOLED & x$FIRST_YEAR == 2018L & x$GRADE == "6", ]   # B, no exits
  expect_identical(b$N_EXITED, 0L)
  expect_identical(b$PCT_EXITED, 0)
  expect_false(b$SUPPRESSED)
})

test_that("a student with no next-year record is censored, not a non-exiter", {
  roster <- make_prep2_roster()
  roster <- roster[!(substr(roster$STUDENT_ID, 1, 1) == "B" & roster$YEAR == 2020L), ]
  x <- compute_exit_rates(make_prep2_bundle(roster), ctx_spec(), by = "grade",
                          population = "all")
  r <- x[!x$POOLED & x$FIRST_YEAR == 2019L & x$GRADE == "7", ]
  expect_identical(r$N_AT_RISK, 12L)
  expect_identical(r$N_NOT_OBSERVED, 12L)
  expect_true(is.na(r$PCT_EXITED))           # nobody followed: no rate
  expect_false(r$SUPPRESSED)                 # and not a suppression
})

test_that("exit is absorbing: a current row after the first exit is not at risk", {
  roster <- make_prep2_roster()
  a1 <- roster$STUDENT_ID == "A01"
  roster$EL_STATUS[a1 & roster$YEAR == 2018L] <- "former"
  roster$EL_STATUS[a1 & roster$YEAR == 2019L] <- "current"
  x <- compute_exit_rates(make_prep2_bundle(roster), ctx_spec(), by = "state",
                          population = "all")
  y19 <- x[!x$POOLED & x$FIRST_YEAR == 2019L, ]
  expect_identical(y19$N_AT_RISK, 29L)       # A11-A24, B, C: A01 not back at risk
})

test_that("exit rates by years in status use Prep 2's entry dating", {
  x <- compute_exit_rates(make_prep2_bundle(), ctx_spec(), by = "years_in_status",
                          population = "all")
  r <- x[!x$POOLED & x$FIRST_YEAR == 2018L & x$YEARS_IN_STATUS == "3", ]
  expect_identical(r$N_AT_RISK, 24L)         # A, entered 2016: year 3 in 2018
  expect_identical(r$N_EXITED, 10L)
  expect_identical(attr(x, "n_unknown_entry"), 60L)   # B: 12 students x 5 years
  expect_identical(attr(x, "top"), 8L)       # max timeline 6 + 2
  expect_identical(utils::tail(unique(x$YEARS_IN_STATUS), 1L), "8+")
})

test_that("exit rates degrade to NULL", {
  roster <- make_prep2_roster()
  roster$DISTRICT_ID <- NULL
  expect_null(compute_exit_rates(make_prep2_bundle(roster), ctx_spec(), by = "district"))
  one <- make_prep2_roster()
  one <- one[one$YEAR == 2020L, ]            # no next year for anyone
  expect_null(compute_exit_rates(make_prep2_bundle(one), ctx_spec()))
})

# ---- districts by EL share -------------------------------------------------------

test_that("districts by EL share: weighted enrollment, left-closed bands", {
  x <- compute_district_share(make_prep2_bundle(), ctx_spec())
  expect_identical(x$SHARE_BAND, c("Less than 5%", "5–19%", "20–39%",
                                   "40% or more"))
  # D1: 14 ELs over 12 + 12 + 15 x 10 = 174 (8%); D2 and D3 are all EL.
  expect_identical(x$N_DISTRICTS, c(0L, 1L, 0L, 2L))
  expect_equal(sum(x$PCT_DISTRICTS), 1)
  expect_equal(attr(x, "pct_state"), 29 / 189)
  expect_identical(attr(x, "year"), 2020L)
})

test_that("districts by EL share need district ids and never-EL rows", {
  roster <- make_prep2_roster()
  expect_null(compute_district_share(
    make_prep2_bundle(roster[roster$EL_STATUS != "never", ]), ctx_spec()))
  roster$DISTRICT_ID <- NULL
  expect_null(compute_district_share(make_prep2_bundle(roster), ctx_spec()))
})

# ---- districts by EL enrollment ----------------------------------------------------

ctx_roster_d4 <- function() {
  # D4: never-ELs only, so the "None" band has a district in it.
  r <- make_prep2_roster()
  d4 <- r[r$STUDENT_ID %in% c("N01", "N02"), ]
  d4$STUDENT_ID <- sub("^N", "M", d4$STUDENT_ID)
  d4$DISTRICT_ID <- "D4"
  rbind(r, d4)
}

test_that("concentration: shares of districts against shares of ELs", {
  x <- compute_district_concentration(make_prep2_bundle(ctx_roster_d4()), ctx_spec(),
                                      breaks = c(0, 1, 10, 100))
  expect_identical(x$SIZE_BAND, c("None", "1–9", "10–99", "100+"))
  expect_identical(x$N_DISTRICTS, c(1L, 1L, 2L, 0L))
  expect_equal(x$PCT_DISTRICTS, c(1, 1, 2, 0) / 4)
  expect_identical(x$N[[1]], 0L)             # a true zero, shown
  expect_false(x$SUPPRESSED[[1]])
  expect_true(x$SUPPRESSED[[2]])             # D3's 3 ELs
  expect_true(is.na(x$N[[2]]) && is.na(x$PCT[[2]]))
  expect_equal(x$PCT[[3]], 26 / 29)
  tp <- attr(x, "top")
  expect_identical(tp$N_DISTRICTS, 2L)       # 14 + 12 of 29 passes two-thirds
  expect_identical(tp$MIN_N, 12L)
  sm <- attr(x, "small")
  expect_identical(sm$N, 100)                # the spec's first size break
  expect_identical(sm$PCT_DISTRICTS, 1)
})

test_that("concentration split at the average district", {
  x <- compute_district_concentration(make_prep2_bundle(), ctx_spec(),
                                      breaks = c(0, 1, 100), split_at_mean = TRUE)
  expect_equal(attr(x, "mean_n"), 29 / 3)
  expect_identical(attr(x, "mean_break"), 10)
  expect_identical(x$SIZE_BAND, c("None", "1–9", "10–99", "100+"))
  expect_identical(x$ABOVE_MEAN, c(FALSE, FALSE, TRUE, TRUE))
  sp <- attr(x, "split")
  expect_equal(sp$below$PCT_DISTRICTS, 1 / 3)
  expect_true(is.na(sp$below$PCT))           # 3 ELs: suppressed
  expect_equal(sp$above$PCT, 26 / 29)
})

test_that("concentration falls back to the spec's size bands", {
  x <- compute_district_concentration(make_prep2_bundle(), ctx_spec())
  expect_identical(x$SIZE_BAND, ctx_spec()$policy$district_size_bands$labels)
  expect_identical(x$N_DISTRICTS, c(3L, 0L, 0L))
  s <- ctx_spec()
  s$policy$district_size_bands <- NULL
  expect_null(compute_district_concentration(make_prep2_bundle(), s))
})

# ---- district profiles ---------------------------------------------------------------

test_that("profiles pick settings from the data, each district once", {
  p <- compute_district_profiles(make_prep2_bundle(), ctx_spec())
  s <- p$summary
  expect_identical(s$DISTRICT_ID, c("D1", "D2"))   # D3 has 3 ELs: not eligible
  expect_identical(s$SETTING, c("Largest EL enrollment", "Highest EL share"))
})

test_that("profiles: happy path for a named district", {
  p <- compute_district_profiles(make_prep2_bundle(), ctx_spec(),
                                 districts = c("mid-size suburb" = "D1"),
                                 population = "all")
  s <- p$summary
  expect_identical(s$SETTING, "mid-size suburb")
  expect_identical(s$N_EL, 14L)              # A11, A12, B
  expect_identical(s$N_EL_FIRST, 12L)        # B in 2015
  expect_equal(s$PCT_EL, 14 / 174)
  expect_equal(s$PCT_EL_FIRST, 12 / 162)
  expect_equal(s$PCT_OF_STATE_EL, 14 / 29)
  expect_identical(s$N_FORMER, 10L)
  expect_identical(s$N_FORMER_FIRST, 0L)     # a true zero, shown
  expect_identical(s$EXIT_YEAR, 2019L)
  expect_identical(s$PCT_EXITED, 0)          # nobody in D1 exits after 2019
  expect_equal(s$PCT_EXITED_FIRST, 0)
  # Languages among D1's current ELs in 2020: 12 hat, 2 spa (suppressed).
  l <- p$languages
  expect_identical(l$HOME_LANGUAGE, c("hat", "spa"))
  expect_identical(l$N, c(12L, NA))
  expect_identical(s$N_LANGUAGES, 2L)
  expect_null(p$schools)                     # no SCHOOL_ID delivered
})

test_that("profiles reject a district not in the roster, and degrade", {
  expect_error(compute_district_profiles(make_prep2_bundle(), ctx_spec(),
                                         districts = "D9"), "D9")
  roster <- make_prep2_roster()
  roster$DISTRICT_ID <- NULL
  expect_null(compute_district_profiles(make_prep2_bundle(roster), ctx_spec()))
})

test_that("profile text names the setting and suppresses what the data do", {
  p <- compute_district_profiles(make_prep2_bundle(), ctx_spec(),
                                 districts = c("mid-size suburb" = "D1"))
  t <- prep2_profile_text(p, "D1", label = "District One", state = "statewide")
  expect_identical(t$title, "District One: mid-size suburb, 14 ELs")
  expect_length(t$cards, 3L)
  expect_match(t$cards[[3]]$small, "^of tested ELs in 2018–19")
  expect_true(all(c("Languages", "Former ELs") %in% names(t$rows)))
  # Shares are of all 14 current ELs; Spanish (2) is suppressed, not dropped
  # from the denominator.
  expect_match(t$rows[["Languages"]], "^Haitian Creole \\(86%\\) leads among 2 ")
  expect_error(prep2_profile_text(p, "D2"), "D2")
})

# ---- assembly, sentences, figures -------------------------------------------------------

test_that("run_prep2_context fills every slot and states its sample", {
  res <- run_prep2_context(make_prep2_bundle(), ctx_spec())
  expect_named(res, c("exit_by_grade", "exit_by_years", "el_share", "district_share",
                      "by_grade", "by_district", "concentration",
                      "concentration_by_mean", "profiles", "meta"))
  expect_identical(res$meta$n_current_latest, 29L)
  expect_identical(res$meta$n_current_off_grid, 0L)
  expect_match(res$meta$sample, "not a cohort")
  expect_match(res$meta$sample, "left out rather than counted as staying")
  expect_match(res$meta$notes$unknown_entry, "^60 ")
})

ctx_sim_run <- local({
  r <- NULL
  function() {
    if (is.null(r)) r <<- run_prep2_context(make_prep2_sim_bundle(), ctx_spec())
    r
  }
})

test_that("on the sim, grade 12 has no observable exit rate", {
  res <- ctx_sim_run()
  g12 <- res$exit_by_grade[res$exit_by_grade$GRADE == "12", ]
  expect_true(all(is.na(g12$PCT_EXITED)))
  expect_false(any(g12$SUPPRESSED))
  expect_identical(g12$N_NOT_OBSERVED, g12$N_AT_RISK)
})

test_that("the context sentences choose from the computed tables", {
  res <- ctx_sim_run()
  n <- prep2_context_notes(res, state = "the simulated state's")
  pooled <- res$exit_by_grade[res$exit_by_grade$POOLED, ]
  top2 <- pooled$GRADE[order(-pooled$PCT_EXITED)][1:2]
  for (g in top2) expect_match(n$exit_grade, g, fixed = TRUE)
  expect_match(n$exit_years, "^\\*\\*Exit rates peak in years ")
  expect_length(n$grade, 3L)
  expect_match(n$share$state, "simulated state's students")
  expect_match(n$top$lead, "^Two-thirds")
  expect_identical(n$small$lead, "No district")   # every sim district has 100+
  expect_identical(c(n$below$lead, n$above$lead), c("Below average", "Above average"))
})

test_that("the context figures build from their slots", {
  res <- ctx_sim_run()
  for (p in list(plot_context_exit_rates(res$exit_by_grade),
                 plot_context_exit_rates(res$exit_by_years, series = "latest"),
                 plot_context_by_grade(res$by_grade),
                 plot_context_district_share(res$district_share),
                 plot_context_concentration(res$concentration),
                 plot_context_concentration(res$concentration_by_mean))) {
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
  expect_error(plot_context_exit_rates(res$by_grade), "PCT_EXITED")
})

test_that("display labels agree with Check 2's reader", {
  v <- c("spa", "esl_pullout", "not_a_code")
  expect_identical(.p2c_label(v[1], "HOME_LANGUAGE"), .check2_label(v[1], "HOME_LANGUAGE"))
  expect_identical(.p2c_label(v[2], "SERVICE_MODEL"), .check2_label(v[2], "SERVICE_MODEL"))
  expect_identical(.p2c_label(v[3], "HOME_LANGUAGE"), "not_a_code")
})
