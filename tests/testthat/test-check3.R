# Check 3 -- Academic patterns: box plots + logistic. run_check3(data, spec).
# Sample: cross-sectional, ALL ELs per grade and year -- not the K cohort.
# Built per internal_docs/steps/7_check3_academics.md.

check3_spec <- function() make_check3_spec()

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
  bd <- check3_sim()$box_data

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
  bd <- check3_sim()$box_data
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
  bd <- check3_sim()$box_data
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
  expect_identical(attr(check3_sim()$box_data, "cut"),
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
  bd <- check3_sim()$box_data
  p <- plot_box_panel(bd, check3_spec())

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

test_that("run_check3() fills the shell's slot and marks the rest pending", {
  res <- check3_sim()

  expect_false(is.null(res$box_data))
  expect_false(is.null(res$coverage))
  for (s in c("summary_table", "logistic", "landing", "fairness_boxes",
              "fairness_logistic")) {
    expect_null(res[[s]])
    expect_true(s %in% res$meta$pending)
  }
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

test_that("the landing band is where the share at or above the cut reaches 50%", {
  skip("check3 later pass: $landing")
})

test_that("the logistic reports the scale score where fitted probability crosses 50%", {
  skip("check3 later pass: $logistic")
})

test_that("fairness-check output is captioned as a generator parameter on sim data", {
  # Every EL/EO gap in the sim is a parameter; the machinery is real, the
  # number is by construction.
  skip("check3 later pass: $fairness_boxes, $fairness_logistic")
})

test_that("contested-zone models are NULL under a Type 1 spec", {
  # Structural now, and it stays NULL when the models are built: the SIM spec
  # is Type 1 and Prep 2's contested zone is NULL under the same gate.
  expect_null(check3_sim()$contested_zone_models)
  expect_false(spec_has_type(check3_spec(), 2:6))
})
