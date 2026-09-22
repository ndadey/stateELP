# Check 3 -- Academic patterns: box plots + logistic. run_check3(data, spec).
# Sample: cross-sectional, ALL ELs per grade and year -- not the K cohort.
# Built per internal_docs/steps/7_check3_academics.md.

check3_spec <- function() make_check3_spec()

# The focal panel out of a long, keyed slot. Most single-panel assertions read
# better against one panel than against all eighteen.
check3_focal <- function(x) .c3_one_panel(x)

# run_check3() on the full sim, once for the file.
check3_sim <- local({
  r <- NULL
  function() {
    if (is.null(r)) r <<- run_check3(make_check3_sim_bundle(), check3_spec())
    r
  }
})

# ---- the panel derivation ---------------------------------------------------

test_that("the panel is derived from the spec, never chosen", {
  # Hard-coding a grade would be wrong for the next state; picking "the grade
  # with most data" would move the figure whenever a delivery changed.
  spec <- check3_spec()
  p <- compute_panel(make_check3_sim_bundle(), spec)

  subject <- spec_academic_subjects(spec)[[1]]
  expect_identical(p$subject, subject)
  expect_identical(p$grade, spec_academic_grades(spec, subject)[[1]])
  expect_identical(p$year, max(p$years))
})

test_that("the grade loop is driven by the spec, not the observed grade span", {
  # Driving it from the data renders empty panels for K, 1, 2 and 12, and
  # hides a grade the state tests but did not deliver.
  spec <- check3_spec()
  subject <- spec_academic_subjects(spec)[[1]]
  declared <- spec_academic_grades(spec, subject)

  expect_error(compute_panel(make_check3_sim_bundle(), spec, grade = "K"),
               "not a tested grade")
  # Grade 12 is outside the tested span and equally refused, even though the
  # roster carries grade-12 students.
  expect_false("12" %in% declared)
  expect_error(compute_panel(make_check3_sim_bundle(), spec, grade = "12"),
               "not a tested grade")
})

test_that("a subject the spec does not declare is refused", {
  expect_error(
    compute_panel(make_check3_sim_bundle(), check3_spec(), subject = "Science"),
    "not in the spec")
})

# ---- the box-plot panel -----------------------------------------------------

test_that("compute_box_data() returns one row per group, in spec order", {
  spec <- check3_spec()
  bd <- check3_focal(check3_sim()$box_data)

  expect_s3_class(bd, "tbl_df")
  expect_identical(bd$GROUP, .check3_group_levels(spec))
  expect_true(all(c("N", "N_PLOTTED", "N_WEIGHTED", "N_ALTERNATE",
                    "Y_MIN", "Y_P25", "Y_MEDIAN", "Y_MEAN", "Y_P75", "Y_MAX",
                    "SUPPRESSED") %in% names(bd)))
  # The five numbers are ordered, which is what makes them a box.
  ok <- !bd$SUPPRESSED & !is.na(bd$Y_MEDIAN)
  expect_true(all(bd$Y_MIN[ok] <= bd$Y_P25[ok]))
  expect_true(all(bd$Y_P25[ok] <= bd$Y_MEDIAN[ok]))
  expect_true(all(bd$Y_MEDIAN[ok] <= bd$Y_P75[ok]))
  expect_true(all(bd$Y_P75[ok] <= bd$Y_MAX[ok]))
})

test_that("the reference groups are present and are not bands", {
  # Former and never-EL students have no ELP rows at all, so they cannot be
  # banded; if they were missing the figure would lose the two columns that
  # carry its interpretation.
  bd <- check3_focal(check3_sim()$box_data)
  spec <- check3_spec()

  expect_true(all(c("Former ELs", "English-only") %in% bd$GROUP))
  expect_false(any(spec_bands(spec)$labels %in%
                     c("Former ELs", "English-only")))
  expect_gt(bd$N[bd$GROUP == "Former ELs"], 0L)
  expect_gt(bd$N[bd$GROUP == "English-only"], 0L)
})

test_that("the join runs academic left-joined to roster, not the reverse", {
  # Starting from the ELP side silently drops the former and never-EL
  # reference groups, because they have no ELP rows. Row-count guard; the
  # direction itself is already demonstrated in test-el_data.R.
  b <- make_check3_sim_bundle()
  spec <- check3_spec()
  p <- compute_panel(b, spec)
  rows <- .check3_panel_rows(b, spec, p)

  ac <- el_academic(b)
  ac <- ac[ac$SUBJECT %in% p$subject & as.character(ac$GRADE) == p$grade &
             ac$YEAR == p$year, ]
  # Every academic row in the panel survives the join, none is duplicated.
  expect_identical(nrow(rows), nrow(ac))
  expect_identical(anyDuplicated(rows$STUDENT_ID), 0L)
  # And the groups the ELP side could not have supplied are there.
  expect_true(any(rows$GROUP == "Former ELs"))
  expect_true(any(rows$GROUP == "English-only"))
})

test_that("the 2020 panel is NULL -- a missing year, not an empty one", {
  # The sim carries no academic rows for 2020 at all (contract 3.6). Asking
  # for it must return nothing rather than an empty figure implying zero
  # students sat the test.
  b <- make_check3_sim_bundle()
  spec <- check3_spec()
  p <- compute_panel(b, spec)

  expect_false(2020L %in% p$years)
  gap <- compute_panel(b, spec, year = 2020L)
  expect_null(compute_box_data(b, spec, gap))
})

test_that("a group below the minimum cell size is blanked, never dropped", {
  # The ReportR triple's third leg. A suppressed group keeps its column on
  # the axis: a gap says "too few to show", a missing column says "nobody
  # here", and those are different facts.
  spec <- check3_spec()
  small <- spec_min_cell(spec) - 1L
  b <- make_check3_bundle(n_band1 = small)
  bd <- compute_box_data(b, spec)

  expect_identical(nrow(bd), length(.check3_group_levels(spec)))
  thin <- bd[bd$N_PLOTTED %in% small, ]
  expect_identical(nrow(thin), 1L)
  expect_true(thin$SUPPRESSED)
  expect_true(is.na(thin$Y_MEDIAN))
  expect_true(is.na(thin$Y_MIN) && is.na(thin$Y_MAX))
  # The count itself stays: it is the denominator, not a disclosive cell.
  expect_identical(thin$N, small)
})

test_that("suppression keys on the plotted n, not the group total", {
  # A group of forty students with three scores discloses those three.
  spec <- check3_spec()
  b <- make_check3_bundle()
  ac <- el_academic(b)
  keep <- ac$STUDENT_ID
  drop <- head(keep[startsWith(keep, "A")], -2L)   # leave two scores
  ac$SCALE_SCORE[ac$STUDENT_ID %in% drop] <- NA_real_
  b2 <- as_el_data(el_base(b), roster = el_roster(b), academic = ac,
                   spec = spec)

  bd <- compute_box_data(b2, spec)
  thin <- bd[bd$N_PLOTTED == 2L, ]
  expect_identical(nrow(thin), 1L)
  expect_true(thin$SUPPRESSED)
  expect_gt(thin$N, thin$N_PLOTTED)
})

test_that("the never-EL n is reported weighted as well as raw", {
  # Never-ELs are a 10% sample (contract 5.1). An unweighted count presented
  # as a population figure is the error this guards.
  bd <- check3_focal(check3_sim()$box_data)
  eo <- bd[bd$GROUP == "English-only", ]

  expect_true("N_WEIGHTED" %in% names(bd))
  expect_gt(eo$N_WEIGHTED, eo$N)
  # Within-EL groups are not weighted, so their two counts agree.
  el <- bd[!bd$GROUP %in% c("English-only"), ]
  expect_identical(el$N_WEIGHTED, as.numeric(el$N))
})

test_that("alternate-academic takers are counted in n, out of the distribution", {
  spec <- check3_spec()
  b <- make_check3_bundle()
  ac <- el_academic(b)
  alt <- head(ac$STUDENT_ID[startsWith(ac$STUDENT_ID, "B")], 3L)
  ac$ACHIEVEMENT_LEVEL_ALT[ac$STUDENT_ID %in% alt] <- 2L
  b2 <- as_el_data(el_base(b), roster = el_roster(b), academic = ac,
                   spec = spec)

  plain <- compute_box_data(b, spec)
  with_alt <- compute_box_data(b2, spec)
  row <- with_alt$N_ALTERNATE == 3L

  expect_identical(sum(row), 1L)
  # Counted ...
  expect_identical(with_alt$N[row], plain$N[row])
  # ... and excluded: three fewer scores behind the box.
  expect_identical(with_alt$N_PLOTTED[row], plain$N_PLOTTED[row] - 3L)
})

test_that("the proficiency cut comes from the spec, never a literal", {
  spec <- check3_spec()
  subject <- spec_academic_subjects(spec)[[1]]
  lvl <- spec$policy$academic_benchmark$proficient_level

  expect_identical(.check3_cut(spec, subject),
                   as.numeric(spec$academic[[subject]]$cuts[[lvl - 1L]]))
  expect_identical(attr(compute_box_data(make_check3_sim_bundle(), spec), "cut"),
                   .check3_cut(spec, subject))

  # A spec with no benchmark yields no line rather than a guessed one.
  bare <- spec
  bare$policy$academic_benchmark$proficient_level <- NULL
  expect_true(is.na(.check3_cut(bare, subject)))
})

test_that("band labels and group names are never hardcoded in the sources", {
  files <- list.files(test_path("..", ".."), pattern = "^check3_.*\\.R$",
                      full.names = TRUE, recursive = TRUE)
  skip_if(length(files) == 0, "package sources not reachable from the test dir")
  code <- unlist(lapply(files, function(f) {
    l <- readLines(f, warn = FALSE)
    l[!grepl("^\\s*#", l)]
  }))
  # The band scheme lives in the spec; a literal here would be right for WIDA
  # and wrong for everything else.
  expect_identical(grep('"[0-9]\\.[0-9]-[0-9]\\.[0-9]"', code, value = TRUE),
                   character())
  expect_identical(grep("\\b(19|20)[0-9]{2}\\b", code, value = TRUE),
                   character())
})

# ---- the figure -------------------------------------------------------------

test_that("the panel plots from summary statistics alone", {
  # No student-level data may be in scope where a figure is built
  # (CLAUDE.md 8), which is why compute_box_data() returns five numbers.
  bd <- check3_focal(check3_sim()$box_data)
  p <- plot_box_panel(check3_sim()$box_data, check3_spec())

  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), sum(!is.na(bd$Y_MEDIAN)))
  expect_true(any(vapply(p$layers,
                         function(l) inherits(l$geom, "GeomBoxplot"), TRUE)))
  # The cut is drawn.
  expect_true(any(vapply(p$layers,
                         function(l) inherits(l$geom, "GeomHline"), TRUE)))
})

test_that("plot_box_panel() refuses anything but the matching slot", {
  expect_error(plot_box_panel(el_roster(make_check3_sim_bundle())),
               "needs a data frame")
})

# ---- assembly ---------------------------------------------------------------

test_that("run_check3() fills every slot the instruction file names", {
  res <- check3_sim()

  for (s in c("box_data", "summary_table", "logistic", "landing",
              "fairness_boxes", "fairness_logistic", "coverage")) {
    expect_false(is.null(res[[s]]), info = s)
  }
  expect_length(res$meta$pending, 0L)
  # Type-gated, and this spec is Type 1: with one overall criterion there is
  # no second criterion to disagree with it, so no contested zone exists.
  expect_null(res$contested_zone_models)
})

test_that("every tested grade and subject gets its own panel", {
  spec <- check3_spec()
  res <- check3_sim()

  expected <- sum(vapply(spec_academic_subjects(spec),
                         function(s) length(spec_academic_grades(spec, s)),
                         1L))
  expect_identical(res$meta$panels, as.integer(expected))
  expect_identical(nrow(res$landing), as.integer(expected))
  # Keyed, not pooled: one row per panel and no duplicates.
  expect_identical(anyDuplicated(paste(res$landing$SUBJECT,
                                       res$landing$GRADE)), 0L)
  # The focal panel leads, so the figure's grade is the first a reader meets.
  expect_identical(res$landing$SUBJECT[[1]], res$meta$subject)
  expect_identical(res$landing$GRADE[[1]], res$meta$grade)
})

test_that("results are keyed by grade and never pooled across grades", {
  # Two grades give two different panels, and neither is a mixture.
  spec <- check3_spec()
  b <- make_check3_sim_bundle()
  subject <- spec_academic_subjects(spec)[[1]]
  grades <- spec_academic_grades(spec, subject)

  a <- run_check3(b, spec, grade = grades[[1]])
  z <- run_check3(b, spec, grade = grades[[2]])

  expect_identical(a$meta$grade, grades[[1]])
  expect_identical(z$meta$grade, grades[[2]])
  expect_false(identical(a$box_data$N, z$box_data$N))
  # $meta carries exactly one grade, so nothing downstream can read a pooled
  # result as a per-grade one.
  expect_length(a$meta$grade, 1L)
})

test_that("the Sample says cross-sectional and names the panel", {
  # Misstating the sample is the canonical way to misread this stage: it is
  # NOT Checks 1-2's cohort.
  res <- check3_sim()

  expect_match(res$meta$sample, "[Cc]ross-sectional")
  expect_match(res$meta$sample, "not the entry cohort")
  expect_match(res$meta$sample, res$meta$grade)
  expect_match(res$meta$sample, as.character(res$meta$year))
})

test_that("the band-axis hazard is stated, not left in the code", {
  # The top bands hold students the exit rule is about to remove, and nothing
  # in the figure reveals that.
  note <- check3_sim()$meta$band_axis_note

  expect_match(note, "not a proficiency gradient")
  expect_match(note, "final year")
  expect_match(note, "Former")
})

test_that("without an academic table every slot is NULL, with a note", {
  spec <- check3_spec()
  bare <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
  res <- run_check3(bare, spec)

  expect_null(res$box_data)
  expect_match(res$meta$note, "No academic table")
  expect_match(res$meta$sample, "no panel is drawn")
  expect_error(compute_panel(bare, spec), "needs the academic table")
})

# ---- later passes -----------------------------------------------------------







test_that("contested-zone models are NULL under a Type 1 spec", {
  # Structural now, and it stays NULL when the models are built: the SIM spec
  # is Type 1 and Prep 2's contested zone is NULL under the same gate.
  expect_null(check3_sim()$contested_zone_models)
  expect_false(spec_has_type(check3_spec(), 2:6))
})

# ---- later pass: the summary table -------------------------------------------

test_that("the summary table is the figure's own numbers, not a second count", {
  # Recomputing would let the table and the figure disagree about the same
  # panel, which is the failure mode a reader cannot detect.
  spec <- check3_spec()
  b <- make_check3_sim_bundle()
  p <- compute_panel(b, spec)
  bd <- compute_box_data(b, spec, p)
  st <- compute_summary_table(b, spec, p, box_data = bd)

  expect_identical(st$GROUP, bd$GROUP)
  expect_identical(st$Y_MEDIAN, bd$Y_MEDIAN)
  expect_identical(st$N, bd$N)
  expect_equal(st$Y_IQR, bd$Y_P75 - bd$Y_P25)
  expect_true(all(c("N_ALTERNATE", "N_WEIGHTED", "PCT_AT_CUT",
                    "SUPPRESSED") %in% names(st)))
})

# ---- later pass: the logistic ------------------------------------------------

test_that("the logistic reports the scale score where probability crosses 50%", {
  spec <- check3_spec()
  lg <- check3_focal(check3_sim()$logistic)

  expect_identical(nrow(lg), 1L)
  expect_true(is.finite(lg$SLOPE))
  # The crossing is -a/b by construction, which is the whole statistic.
  expect_equal(lg$CROSSING, -lg$INTERCEPT / lg$SLOPE)
  expect_identical(lg$THRESHOLD,
                   .check3_cut(spec, spec_academic_subjects(spec)[[1]]))
  expect_identical(lg$THRESHOLD_KIND, "state proficiency cut")
  expect_gt(lg$N, 0L)
})

test_that("the logistic runs on current ELs, the only group with an ELP score", {
  spec <- check3_spec()
  b <- make_check3_sim_bundle()
  p <- compute_panel(b, spec)
  rows <- .check3_panel_rows(b, spec, p)
  lg <- compute_logistic(b, spec, p)

  eligible <- sum(rows$EL_STATUS %in% "current" & !rows$ALTERNATE &
                    !is.na(rows$ELP_SCALE) & !is.na(rows$SCALE_SCORE))
  expect_identical(lg$N, as.integer(eligible))
  # Former and never-EL students have no ELP score to regress on, so the fit
  # cannot have used them.
  expect_lt(lg$N, nrow(rows))
})

test_that("a crossing outside the observed scores is flagged, not hidden", {
  # Reading the fitted curve past its data gives a number nobody was seen at.
  # It is still worth reporting -- it says the bar is above everyone -- but
  # only labelled as the extrapolation it is.
  l <- check3_sim()$logistic
  out <- l[!l$CROSSING_IN_RANGE %in% TRUE & !is.na(l$CROSSING), ]

  expect_true("CROSSING_IN_RANGE" %in% names(l))
  expect_gt(nrow(out), 0L)             # the sim does produce some
  expect_true(all(is.finite(out$CROSSING)))
})

test_that("a panel where the outcome never varies returns no crossing", {
  # Everyone above the bar, or nobody: there is no curve. That is a fact
  # about the grade, so the row comes back empty rather than erroring.
  spec <- check3_spec()
  b <- make_check3_bundle()
  p <- compute_panel(b, spec)
  lg <- compute_logistic(b, spec, p, threshold = -Inf)   # everyone clears it

  expect_identical(nrow(lg), 1L)
  expect_true(is.na(lg$CROSSING))
  expect_identical(lg$N_ABOVE, lg$N)
})

# ---- later pass: the landing statistics --------------------------------------

test_that("the landing band is where the share at or above the cut reaches 50%", {
  spec <- check3_spec()
  res <- check3_sim()
  bd <- check3_focal(res$box_data)
  ld <- check3_focal(res$landing)
  bands <- spec_bands(spec)$labels
  cut <- .check3_cut(spec, spec_academic_subjects(spec)[[1]])

  b <- bd[match(bands, bd$GROUP), ]
  first <- which(!is.na(b$PCT_AT_CUT) & b$PCT_AT_CUT >= 0.5)[1]
  expect_identical(ld$LANDING_BAND,
                   if (is.na(first)) NA_character_ else bands[[first]])

  # "At or above the cut for half" and "the median clears the cut" are the
  # same statement, and the instruction file says so. Check they agree.
  by_median <- which(!is.na(b$Y_MEDIAN) & b$Y_MEDIAN >= cut)[1]
  expect_identical(is.na(first), is.na(by_median))
})

test_that("no band reaching half is an answer, not a failure", {
  res <- check3_sim()
  none <- res$landing[is.na(res$landing$LANDING_BAND), ]
  skip_if(nrow(none) == 0, "every panel landed in this delivery")

  # The row survives, and says how far the highest band did get -- otherwise
  # an empty landing is unreadable.
  expect_true(all(!is.na(none$TOP_BAND_PCT)))
  expect_true(all(none$TOP_BAND_PCT < 0.5))
  expect_false(any(is.na(none$GRADE)))
})

test_that("a landing inside the exit bands is flagged", {
  # Those bands hold students the exit rule is about to remove, so a landing
  # there describes a population the rule constitutes. The flag is what stops
  # the statistic being quoted flat.
  spec <- check3_spec()
  l <- check3_sim()$landing

  expect_identical(unique(l$EXIT_BAND), .check3_exit_band(spec))
  bands <- spec_bands(spec)$labels
  landed <- l[!is.na(l$LANDING_BAND), ]
  expect_identical(
    landed$LANDING_AT_OR_ABOVE_EXIT,
    match(landed$LANDING_BAND, bands) >= match(landed$EXIT_BAND, bands))
})

test_that("the exit band is read from the rule, never typed", {
  spec <- check3_spec()
  band <- .check3_exit_band(spec)

  expect_true(band %in% spec_bands(spec)$labels)
  # Move the rule's threshold and the band moves with it.
  lower <- spec
  lower$policy$exit$pathways[[1]]$terms[[1]]$value <- 2.5
  expect_false(identical(.check3_exit_band(lower), band))
})

# ---- later pass: the two fairness checks -------------------------------------

test_that("the SES filter narrows the reference group and nothing else", {
  spec <- check3_spec()
  b <- make_check3_sim_bundle()
  p <- compute_panel(b, spec)
  full <- compute_box_data(b, spec, p)
  fair <- compute_fairness_boxes(b, spec, p)

  eo <- fair$GROUP == "English-only"
  expect_lt(fair$N[eo], full$N[eo])          # the reference group shrinks ...
  expect_identical(fair$N[!eo], full$N[!eo]) # ... and no other group moves
  expect_match(attr(fair, "filter"), "crude filter, not a match")
})

test_that("the SES filter reads whatever the delivery calls yes", {
  # A crosswalk state may send 1/0 or TRUE/FALSE. None of those should
  # silently filter the reference group down to nobody.
  expect_true(all(.check3_is_disadvantaged(c("Y", "yes", "1", "TRUE", "t"))))
  expect_false(any(.check3_is_disadvantaged(c("N", "no", "0", "FALSE", NA))))
})

test_that("the second fairness check swaps the bar and keeps the model", {
  res <- check3_sim()
  f <- check3_focal(res$fairness_logistic)
  l <- check3_focal(res$logistic)

  expect_identical(f$THRESHOLD_KIND, "English-only average")
  expect_identical(f$STATE_CUT, l$THRESHOLD)
  expect_false(identical(f$THRESHOLD, l$THRESHOLD))
  # Same universe, same n: only the outcome's definition changed.
  expect_identical(f$N, l$N)
})

test_that("the English-only bar is weighted, never a raw sample mean", {
  # Never-ELs are a 10% sample; an unweighted average is a sample statistic
  # presented as a population one.
  spec <- check3_spec()
  b <- make_check3_sim_bundle()
  p <- compute_panel(b, spec)
  rows <- .check3_panel_rows(b, spec, p)
  eo <- rows[rows$GROUP == "English-only" & !is.na(rows$SCALE_SCORE), ]

  f <- compute_fairness_logistic(b, spec, p)
  expect_equal(f$THRESHOLD,
               stats::weighted.mean(eo$SCALE_SCORE, eo$SAMPLE_WEIGHT))
  expect_identical(f$N_REFERENCE, nrow(eo))
})

test_that("fairness output is captioned as a generator parameter on sim data", {
  # Every EL/EO gap in the sim is a parameter; the machinery is real, the
  # number is by construction.
  note <- check3_fairness_note(check3_sim())

  expect_match(note, "construct-validity signal")
  expect_match(note, "not a threshold recommendation")
  expect_match(note, "Decision stage")
})

# ---- the generated prose -----------------------------------------------------

test_that("the landing note counts what it claims to count", {
  res <- check3_sim()
  note <- check3_landing_note(res)
  l <- res$landing

  expect_match(note, as.character(nrow(l)))
  expect_match(note, as.character(sum(is.na(l$LANDING_BAND))))
  if (all(l$LANDING_AT_OR_ABOVE_EXIT[!is.na(l$LANDING_BAND)] %in% TRUE)) {
    expect_match(note, "Every panel that lands")
  }
})

test_that("the tables render for every panel and the plots build", {
  res <- check3_sim()
  spec <- check3_spec()

  expect_s3_class(ft_check3_summary(res), "flextable")
  expect_s3_class(ft_check3_landing(res), "flextable")
  expect_s3_class(ft_check3_fairness(res), "flextable")
  expect_s3_class(plot_landing(res$landing, spec), "ggplot")

  # A named panel is drawable, and a panel that does not exist is refused
  # rather than silently falling back to another grade.
  g <- res$landing$GRADE[[2]]
  expect_s3_class(plot_box_panel(res$box_data, spec, grade = g), "ggplot")
  expect_error(plot_box_panel(res$box_data, spec, grade = "99"), "No panel")
})

# ---- the crossing interval ---------------------------------------------------

test_that("the crossing carries a delta-method interval that brackets it", {
  l <- check3_sim()$logistic
  ok <- !is.na(l$CROSSING) & !is.na(l$CROSSING_SE)

  expect_true(all(c("CROSSING_SE", "CROSSING_LO", "CROSSING_HI") %in% names(l)))
  expect_true(any(ok))
  expect_true(all(l$CROSSING_SE[ok] > 0))
  expect_true(all(l$CROSSING_LO[ok] < l$CROSSING[ok]))
  expect_true(all(l$CROSSING_HI[ok] > l$CROSSING[ok]))
  # Symmetric on the scale by construction, which is what the delta method
  # gives and what the roxygen says it gives.
  expect_equal(l$CROSSING[ok] - l$CROSSING_LO[ok],
               l$CROSSING_HI[ok] - l$CROSSING[ok])
})

test_that("the interval matches the delta method computed by hand", {
  spec <- check3_spec()
  b <- make_check3_sim_bundle()
  p <- compute_panel(b, spec)
  rows <- .check3_panel_rows(b, spec, p)
  fr <- rows[rows$EL_STATUS %in% "current" & !rows$ALTERNATE &
               !is.na(rows$ELP_SCALE) & !is.na(rows$SCALE_SCORE), ]
  fr$ABOVE <- as.integer(fr$SCALE_SCORE >= .check3_cut(spec, p$subject))
  fit <- stats::glm(ABOVE ~ ELP_SCALE, data = fr, family = stats::binomial())

  a <- unname(stats::coef(fit)[[1]])
  bb <- unname(stats::coef(fit)[[2]])
  g <- c(-1 / bb, a / bb^2)
  se <- sqrt(drop(t(g) %*% stats::vcov(fit) %*% g))

  lg <- compute_logistic(b, spec, p)
  expect_equal(lg$CROSSING_SE, se)
  expect_equal(lg$CROSSING_LO, -a / bb - stats::qnorm(0.975) * se)
})

test_that("an interval is not manufactured where the fit cannot support one", {
  # A non-positive slope has no upward crossing, so there is nothing to put an
  # interval around. Reporting one anyway would dress a non-answer as an
  # estimate.
  spec <- check3_spec()
  b <- make_check3_bundle()
  p <- compute_panel(b, spec)
  lg <- compute_logistic(b, spec, p, threshold = -Inf)

  expect_true(is.na(lg$CROSSING))
  expect_true(is.na(lg$CROSSING_SE))
  expect_true(is.na(lg$CROSSING_LO) && is.na(lg$CROSSING_HI))
})

test_that("the interval is widest exactly where the crossing is extrapolated", {
  # Not a coincidence and not a defect: a crossing far outside the observed
  # scores is one the slope is too uncertain to place, and the interval says
  # so in the units of the statistic.
  l <- check3_sim()$logistic
  l <- l[!is.na(l$CROSSING_SE), ]
  skip_if(length(unique(l$CROSSING_IN_RANGE)) < 2, "no contrast in this run")

  inside <- stats::median(l$CROSSING_SE[l$CROSSING_IN_RANGE %in% TRUE])
  outside <- stats::median(l$CROSSING_SE[!l$CROSSING_IN_RANGE %in% TRUE])
  expect_gt(outside, inside)
})

test_that("the landing table carries the interval beside the crossing", {
  res <- check3_sim()
  expect_true(all(c("CROSSING_LO", "CROSSING_HI") %in% names(res$landing)))
  expect_identical(res$landing$CROSSING_LO, res$logistic$CROSSING_LO)
  expect_s3_class(ft_check3_landing(res), "flextable")
})

# ---- the Type-gated contested-zone check -------------------------------------

test_that("a Type 1 rule has no contested zone to model", {
  # One criterion cannot disagree with itself. The slot is NULL because the
  # rule has no second criterion, not because the machinery is missing.
  spec <- check3_spec()

  expect_false(spec_has_type(spec, c(2L, 4L, 5L, 6L)))
  expect_null(check3_sim()$contested_zone_models)
  expect_null(compute_criterion_pass(make_check3_sim_bundle(), spec))
})

test_that("the derived criterion outcomes reproduce Prep 2's own", {
  # THE guardrail on deriving these in check3_ at all. Prep 2 computes the
  # same pass/fail matrix and reports only aggregates, so Check 3 derives its
  # own -- and if the two ever disagree about whether a student cleared a
  # criterion, this fails rather than the documents quietly diverging.
  spec <- make_check3_type2_spec()
  b <- make_check3_sim_bundle()

  mine <- compute_criterion_pass(b, spec)
  theirs <- compute_criteria_crosswalk(b, spec)

  expect_false(is.null(mine))
  expect_false(is.null(theirs))

  # Prep 2 exposes the students blocked by exactly one criterion near the cut.
  # Reconstruct that set from my own outcomes and require an exact match.
  zone_cut <- theirs$crosswalk$window[["cut"]]
  half <- theirs$crosswalk$window[["half_width"]]
  h <- elp_headline(b, spec)
  lvl <- h$ACHIEVEMENT_LEVEL[match(paste(mine$STUDENT_ID, mine$YEAR),
                                   paste(h$STUDENT_ID, h$YEAR))]
  near <- !is.na(lvl) & abs(lvl - zone_cut) <= half
  one <- near & mine$N_FAILED == 1L

  expect_identical(sum(one), nrow(theirs$blocked_groups))
  expect_setequal(paste(mine$STUDENT_ID[one], mine$YEAR[one]),
                  paste(theirs$blocked_groups$STUDENT_ID,
                        theirs$blocked_groups$YEAR))
})

test_that("a Type 2 rule produces the split and the two curves", {
  spec <- make_check3_type2_spec()
  b <- make_check3_sim_bundle()
  cz <- compute_contested_models(b, spec)

  expect_false(is.null(cz))
  expect_true(cz$n > 0L)
  expect_true(cz$zone$lo <= cz$zone$hi)
  # Both groups are present, which is what makes it a comparison.
  expect_gt(cz$n_cleared, 0L)
  expect_lt(cz$n_cleared, cz$n)

  # Two curves, one per group, over a shared score range.
  curves <- cz$model$curves
  expect_false(is.null(curves))
  expect_identical(length(unique(curves$CLEARED_LABEL)), 2L)
  expect_true(all(curves$FITTED >= 0 & curves$FITTED <= 1))
  expect_true(any(grepl("ELP_SCALE:CLEARED", cz$model$coefficients$TERM)))
})

test_that("the contested zone is Prep 2's, not a second derivation of it", {
  # The zone is a property of the rule and the population; recomputing it in
  # Check 3 would be a third place for it to drift.
  spec <- make_check3_type2_spec()
  b <- make_check3_sim_bundle()

  cz <- compute_contested_models(b, spec)
  p2 <- compute_criteria_crosswalk(b, spec)$contested_zone
  expected <- if (any(is.na(p2$core_90))) p2$range else p2$core_90

  expect_equal(c(cz$zone$lo, cz$zone$hi), c(min(expected), max(expected)))
  expect_identical(cz$zone$cut, p2$cut)
})

test_that("the zone check says where its criterion outcomes came from", {
  # The .qmd renders this: a reader should not have to guess whether the
  # split came from Prep 2 or was derived here.
  cz <- compute_contested_models(make_check3_sim_bundle(),
                                 make_check3_type2_spec())
  expect_match(cz$source, "derived in Check 3")
  expect_match(cz$source, "a test asserts the two agree")
})

test_that("a zone too thin to report returns NULL, not a fitted curve", {
  spec <- make_check3_type2_spec()
  spec$policy$suppression$min_cell <- 1e6L
  expect_null(compute_contested_models(make_check3_sim_bundle(), spec))
})
