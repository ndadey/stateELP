# Check 4 -- Decision consistency + dependence. run_check4(data, spec).
# Sample: identical to Check 3's paired cross-section, by grade and year.
# Built per internal_docs/steps/8_check4_consistency.md.

# ---- the checkpoint: J, not consistency relabelled ---------------------------

test_that("Youden's J is computed, not raw consistency relabelled", {
  # THE checkpoint for this stage, and a demonstrated gap rather than a
  # theoretical one: a base rate well below 50%, where the raw-consistency peak
  # and the J optimum diverge. The worked table is in helper-check4.R.
  spec <- make_check4_spec()
  res <- run_check4(make_check4_skewed_bundle(spec = spec), spec)
  y <- res$youden_j
  o <- res$j_optimal
  lv <- make_check4_skewed_levels()

  # The fixture is skewed, and it still proves something: the two statistics
  # peak at different cuts. Without this, a later edit to the fixture could
  # make the test below pass for the wrong reason.
  expect_equal(o$BASE_RATE, 0.16)
  expect_lt(o$BASE_RATE, 0.5)
  expect_identical(y$CUT[which.max(y$AGREE)], lv[[5]])
  expect_identical(y$CUT[which.max(y$J)], lv[[4]])
  expect_false(identical(which.max(y$AGREE), which.max(y$J)))

  # The stage returns the J optimum, not the agreement peak.
  expect_identical(o$J_CUT, lv[[4]])
  expect_identical(o$AGREE_CUT, lv[[5]])
  expect_true(o$OPTIMA_DIVERGE)

  # J is sensitivity + specificity - 1 from the cells, to the hand-worked
  # value, and not a function of agreement.
  expect_equal(o$J, 50 / 80 + 270 / 420 - 1)
  expect_equal(o$SENSITIVITY, 50 / 80)
  expect_equal(o$SPECIFICITY, 270 / 420)
  expect_equal(y$J, y$TP / (y$TP + y$FN) + y$TN / (y$TN + y$FP) - 1)
  expect_equal(y$J, c(0, 0.875 + 90 / 420 - 1, 0.75 + 180 / 420 - 1,
                      0.625 + 270 / 420 - 1, 0.375 + 350 / 420 - 1))
})

test_that("agreement peaks where proficiency crosses one half, J where it crosses the base rate", {
  # The base-rate mechanism, as arithmetic. At the fourth level 20% of
  # students are proficient: above the 16% base rate, below one half. J
  # therefore counts them ready (the optimum sits at that level); agreement
  # counts them not ready (its peak sits one level higher).
  spec <- make_check4_spec()
  lv <- make_check4_skewed_levels()
  o <- run_check4(make_check4_skewed_bundle(spec = spec), spec)$j_optimal
  expect_identical(o$J_CUT, lv[[4]])
  expect_identical(o$AGREE_CUT, lv[[5]])
  # Classifying everyone not ready agrees with 84% of students; agreement's
  # peak is reported against that floor.
  expect_equal(o$AGREE_FLOOR, 1 - 0.16)
  expect_equal(o$AGREE, 0.76)
})

# ---- the curve ---------------------------------------------------------------

test_that("the consistency curve is reported across candidate cuts", {
  spec <- make_check4_spec()
  b <- make_check4_skewed_bundle(spec = spec)
  curve <- compute_consistency_curve(b, spec)

  # One row per observed level, ascending -- the cuts the delivery carries,
  # never a typed grid.
  expect_identical(curve$CUT, make_check4_skewed_levels())
  expect_named(curve, c("SUBJECT", "GRADE", "YEAR", "CUT", "CUT_LABEL", "N",
                        "N_PROFICIENT", "TP", "FP", "FN", "TN", "AGREE",
                        "SUPPRESSED"))
  expect_identical(unique(curve$N), 500L)
  expect_identical(curve$TP + curve$FP + curve$FN + curve$TN,
                   rep(500L, nrow(curve)))
  expect_equal(curve$AGREE, (curve$TP + curve$TN) / curve$N)
  expect_equal(curve$AGREE, c(0.16, 0.32, 0.48, 0.64, 0.76))
  expect_identical(curve$CUT_LABEL, c("1.3", "2.3", "3.3", "4.3", "5.3"))
  expect_false(any(curve$SUPPRESSED))

  # Keyed to the panel compute_panel() derives -- never a typed grade.
  p <- compute_panel(b, spec)
  expect_identical(unique(curve$GRADE), p$grade)
  expect_identical(unique(curve$YEAR), p$year)
  expect_identical(unique(curve$SUBJECT), p$subject)
})

test_that("the sample is current ELs with both scores, nothing else", {
  spec <- make_check4_spec()
  b <- make_check4_skewed_bundle(spec = spec, n_alternate = 15L,
                                 n_no_level = 15L, n_former = 15L,
                                 n_never = 15L)
  curve <- compute_consistency_curve(b, spec)
  # Alternate academic takers (another scale), current ELs with no annual
  # level, former and never-EL students: none is in the classification.
  expect_identical(unique(curve$N), 500L)
  expect_identical(unique(curve$N_PROFICIENT), 80L)
})

test_that("candidate cuts stay inside the spec's level scale", {
  spec <- make_check4_spec()
  b <- make_check4_skewed_bundle(spec = spec)
  # A level outside the declared range is a delivery problem, not a cut.
  b$elp$ACHIEVEMENT_LEVEL[1:20] <- spec$elp$level_scale$range[[2]] + 1
  curve <- compute_consistency_curve(b, spec)
  rng <- spec$elp$level_scale$range
  expect_true(all(curve$CUT >= rng[[1]] & curve$CUT <= rng[[2]]))
  expect_identical(attr(curve, "n_out_of_scale"), 20L)
  expect_identical(unique(curve$N), 480L)
})

test_that("a small cell blanks its cut, and the row stays", {
  spec <- make_check4_spec()
  min_cell <- spec_min_cell(spec)
  # Only (min_cell - 1) proficient students at the lowest level. The cut just
  # above it classifies exactly those students proficient-but-not-ready: one
  # non-zero cell under min_cell. Higher cuts add more students to that cell
  # and clear it.
  b <- make_check4_bundle(levels = make_check4_skewed_levels(),
                          n = rep(100L, 5),
                          prof = c(min_cell - 1L, 10L, 10L, 20L, 30L),
                          spec = spec)
  curve <- compute_consistency_curve(b, spec)
  expect_identical(nrow(curve), 5L)                  # nothing dropped
  hit <- curve$CUT == make_check4_skewed_levels()[[2]]
  expect_identical(curve$SUPPRESSED, hit)
  expect_true(all(is.na(curve$TP[hit]) & is.na(curve$AGREE[hit])))
  expect_identical(unique(curve$N), 500L)            # N stays visible
  # The lowest cut's cells are 0 or large: a zero is shown, not suppressed.
  expect_identical(curve$FN[[1]], 0L)

  # And a suppressed row stays suppressed through J.
  y <- compute_youden_j(curve)
  expect_true(all(is.na(y$J[hit])))
  expect_false(any(is.na(y$J[!hit])))
})

test_that("a panel under min_cell reports no optimum, and keeps its rows", {
  spec <- make_check4_spec()
  b <- make_check4_bundle(levels = c(2.3, 4.3), n = c(4L, 4L),
                          prof = c(1L, 2L), spec = spec)
  res <- run_check4(b, spec)
  expect_identical(nrow(res$consistency_curve), 2L)
  expect_true(all(res$consistency_curve$SUPPRESSED))
  expect_true(all(is.na(res$consistency_curve$N_PROFICIENT)))
  expect_true(res$j_optimal$SUPPRESSED)
  expect_true(is.na(res$j_optimal$J_CUT))
  expect_true(is.na(res$j_optimal$BASE_RATE))
  expect_identical(res$j_optimal$N, 8L)
  expect_match(check4_optimum_note(res), "suppression")
})

test_that("no cut can be recovered by subtracting two reported cuts", {
  # The cells are cumulative, so two reported rows difference to the students
  # between them. A thin level is merged into the next cut rather than
  # exposed: here 3 students at the middle level. The three levels share one
  # band, so no band break (Check 3's published counts) is in play.
  spec <- make_check4_spec()
  min_cell <- spec_min_cell(spec)
  b <- make_check4_bundle(levels = c(2.1, 2.5, 2.9), n = c(100L, 3L, 100L),
                          prof = c(10L, 1L, 30L), spec = spec)
  curve <- compute_consistency_curve(b, spec)
  # Each row on its own would pass; the step from 3.3 to 4.3 would not.
  expect_identical(curve$SUPPRESSED, c(FALSE, FALSE, TRUE))
  expect_identical(nrow(curve), 3L)

  # The property, on the hand fixture and on the shipped sim's panel: every
  # step between consecutive reported cuts is 0 or at least min_cell in each
  # class.
  ok_step <- function(x) all(x == 0L | x >= min_cell)
  for (cv in list(curve, compute_consistency_curve(make_check4_sim_bundle(),
                                                    spec))) {
    r <- cv[!cv$SUPPRESSED, ]
    expect_true(ok_step(-diff(r$TP)))
    expect_true(ok_step(-diff(r$FP)))
  }
})

test_that("Check 3's band table is a published neighbour too", {
  # Check 3 publishes, for the same students, each band's n and share at the
  # cut -- so the cumulative counts at every band break are known. A reported
  # cut beside a break must not difference against it to a small group.
  spec <- make_check4_spec()
  min_cell <- spec_min_cell(spec)
  b <- make_check4_sim_bundle()
  breaks <- spec_bands(spec)$breaks
  ok_step <- function(x) all(x == 0L | x >= min_cell)

  # A thin level just below a band break: the cut at it would difference
  # against the break (4.0) to its 3 students. It goes; its band (3.0-3.9,
  # 103 students) is not thin, so the rest stays.
  thin <- make_check4_bundle(levels = c(2.3, 3.1, 3.8, 4.3),
                             n = c(100L, 100L, 3L, 100L),
                             prof = c(10L, 10L, 1L, 30L), spec = spec)
  expect_identical(compute_consistency_curve(thin, spec)$SUPPRESSED,
                   c(FALSE, FALSE, TRUE, FALSE))

  # A thin level alone in its band: Check 3 shows that band's n but not its
  # share, and Check 4's margins would give the share away -- nothing shows.
  alone <- make_check4_bundle(levels = c(2.3, 3.3, 4.3),
                              n = c(100L, 3L, 100L),
                              prof = c(10L, 1L, 30L), spec = spec)
  expect_true(all(compute_consistency_curve(alone, spec)$SUPPRESSED))

  subj <- spec_academic_subjects(spec)
  for (s in subj) for (g in c("9", "10")) {
    p <- compute_panel(b, spec, subject = s, grade = g)
    rows <- .check4_panel_rows(b, spec, p)
    prof <- rows$SCALE_SCORE >= .check4_cut(spec, s)
    cum <- function(x) c(sum(rows$LEVEL >= x - 1e-9 & prof),
                         sum(rows$LEVEL >= x - 1e-9 & !prof))
    curve <- compute_consistency_curve(b, spec, p)
    shown <- curve$CUT[!curve$SUPPRESSED]
    anchors <- breaks[breaks > min(rows$LEVEL) & breaks <= max(rows$LEVEL)]
    # The lowest cut counts everyone: its cells are the margins, public in
    # every row, so it is a published point like a band break.
    pts <- data.frame(AT = c(shown, anchors),
                      KIND = c(ifelse(shown == min(rows$LEVEL), "band", "cut"),
                               rep("band", length(anchors))))
    pts <- pts[order(pts$AT, pts$KIND == "band"), ]
    for (i in seq_len(nrow(pts) - 1L)) {
      if (all(pts$KIND[i:(i + 1L)] == "band")) next   # Check 3's own pair
      step <- cum(pts$AT[[i]]) - cum(pts$AT[[i + 1L]])
      expect_true(ok_step(step), info = paste(s, g, pts$AT[[i]]))
    }
  }
})

test_that("a band Check 3 cannot show is not given away by the margins", {
  # Check 3 shows every band's n but its share at the cut only when the band
  # holds min_cell or more. Here band 1.0-1.9 holds 5 students; the margins
  # minus the other four bands' shown shares would give their split (2 of
  # 5 proficient). So nothing can be shown.
  spec <- make_check4_spec()
  b <- make_check4_bundle(levels = c(1.5, 2.5, 3.5, 4.5, 5.5),
                          n = c(5L, 100L, 100L, 100L, 100L),
                          prof = c(2L, 10L, 20L, 30L, 40L), spec = spec)
  res <- run_check4(b, spec)
  expect_true(all(res$consistency_curve$SUPPRESSED))
  expect_true(all(is.na(res$consistency_curve$N_PROFICIENT)))
  expect_identical(res$j_optimal$NO_OPTIMUM, "suppressed")
  expect_true(is.na(res$j_optimal$BASE_RATE))
})

test_that("when suppression hides every separating cut, it says so", {
  # Only the lowest cut (everyone ready, J = 0) can be shown. That is the
  # suppression, not the data: J is clearly positive at the hidden cuts. The
  # thin levels share their bands, so no band is one Check 3 cannot show.
  spec <- make_check4_spec()
  b <- make_check4_bundle(levels = c(1.3, 1.6, 1.8, 2.3),
                          n = c(15L, 5L, 5L, 15L),
                          prof = c(2L, 2L, 3L, 5L), spec = spec)
  res <- run_check4(b, spec)
  o <- res$j_optimal
  expect_identical(sum(!res$consistency_curve$SUPPRESSED), 1L)
  expect_false(o$SUPPRESSED)
  expect_identical(o$NO_OPTIMUM, "suppressed")
  note <- check4_optimum_note(res)
  expect_match(note, "says nothing about")
  expect_false(grepl("No candidate cut separates", note))
})

test_that("a panel nobody (or everybody) passes has no optimum, and says why", {
  spec <- make_check4_spec()
  for (p in list(rep(0L, 5), rep(100L, 5))) {
    b <- make_check4_bundle(levels = make_check4_skewed_levels(),
                            n = rep(100L, 5), prof = p, spec = spec)
    res <- run_check4(b, spec)
    o <- res$j_optimal
    # Nothing is suppressed: the counts are all large or zero.
    expect_false(any(res$consistency_curve$SUPPRESSED))
    expect_false(o$SUPPRESSED)
    expect_identical(o$NO_OPTIMUM, "no_variation")
    expect_true(is.na(o$J_CUT))
    expect_true(is.na(o$THRESHOLD_SIDE))
    note <- check4_optimum_note(res)
    expect_match(note, "undefined")
    expect_false(grepl("suppression", note))

    # The tolerance note records an answer without placing it against an
    # optimum that does not exist.
    s <- spec
    s$policy$error_tolerance$costlier_error <- "premature_exit"
    tn <- check4_tolerance_note(run_check4(b, s))
    expect_match(tn, "No J optimum is reported")
    expect_false(grepl("NA", tn, fixed = TRUE))
  }
})

test_that("no cut separating the groups is not reported as an optimum", {
  # Proficiency falls as the level rises: J is zero or below everywhere, and
  # the lowest cut (everyone ready, J = 0) must not be called the optimum.
  spec <- make_check4_spec()
  b <- make_check4_bundle(levels = c(1.3, 2.3, 3.3, 4.3), n = rep(100L, 4),
                          prof = c(40L, 30L, 20L, 10L), spec = spec)
  res <- run_check4(b, spec)
  expect_identical(res$j_optimal$NO_OPTIMUM, "uninformative")
  expect_true(is.na(res$j_optimal$J_CUT))
  expect_match(check4_optimum_note(res), "separates")
})

test_that("ties go to the lowest cut, and say how many tied", {
  spec <- make_check4_spec()
  # Two adjacent cuts with identical cells-derived J: proficiency rate at the
  # middle level equals the base rate exactly, so moving past it leaves J
  # unchanged.
  b <- make_check4_bundle(levels = c(2.3, 3.3, 4.3), n = c(100L, 100L, 100L),
                          prof = c(10L, 20L, 30L), spec = spec)
  y <- compute_youden_j(compute_consistency_curve(b, spec))
  o <- compute_j_optimal(y, spec)
  expect_equal(y$J[[2]], y$J[[3]])
  expect_identical(o$N_J_TIED, 2L)
  expect_identical(o$J_CUT, 3.3)
})

test_that("whole-number levels against a fractional rule warn, and say so", {
  spec <- make_check4_spec()
  b <- make_check4_bundle(levels = c(2, 3, 4, 5), n = rep(50L, 4),
                          prof = c(10L, 10L, 15L, 20L), spec = spec)
  expect_warning(compute_consistency_curve(b, spec),
                 class = "check4_coarse_levels")
  expect_warning(run_check4(b, spec), class = "check4_coarse_levels")
  # The document carries it too, as a callout.
  res <- withCallingHandlers(
    run_check4(b, spec),
    check4_coarse_levels = function(w) invokeRestart("muffleWarning"))
  expect_match(res$meta$resolution_note, "whole number")
  # The decimal fixture never warns.
  expect_no_warning(compute_consistency_curve(make_check4_skewed_bundle(),
                                              spec))
})

# ---- the current rule --------------------------------------------------------

test_that("the current threshold is read from the spec's exit rule", {
  spec <- make_check4_spec()
  b <- make_check4_skewed_bundle(spec = spec)
  term <- spec$policy$exit$pathways[[1]]$terms[[1]]
  res <- run_check4(b, spec)
  o <- res$j_optimal
  expect_identical(o$THRESHOLD, term$value)
  expect_identical(o$THRESHOLD_OP, term$op)
  # Off the observed grid, the rule classifies as the next cut at or above it.
  lv <- make_check4_skewed_levels()
  expect_identical(o$THRESHOLD_CUT, min(lv[lv >= term$value]))
  expect_equal(o$J_AT_THRESHOLD,
               res$youden_j$J[res$youden_j$CUT == o$THRESHOLD_CUT])

  # Change the rule and the mark moves with it: nothing is typed.
  spec2 <- spec
  spec2$policy$exit$pathways[[1]]$terms[[1]]$value <- lv[[3]]
  o2 <- run_check4(b, spec2)$j_optimal
  expect_identical(o2$THRESHOLD, lv[[3]])
  expect_identical(o2$THRESHOLD_CUT, lv[[3]])

  # A rule above every observed level classifies nobody ready.
  spec3 <- spec
  spec3$policy$exit$pathways[[1]]$terms[[1]]$value <-
    spec$elp$level_scale$range[[2]]
  o3 <- run_check4(b, spec3)$j_optimal
  expect_true(is.na(o3$THRESHOLD_CUT))
  expect_identical(o3$J_AT_THRESHOLD, 0)
  expect_equal(o3$AGREE_AT_THRESHOLD, 1 - o3$BASE_RATE)
})

test_that("the rule's side is the cut it classifies as, not the typed value", {
  spec <- make_check4_spec()
  # Whole levels against a fractional rule: the rule classifies exactly as
  # the next whole level, which here is also the J optimum.
  b <- make_check4_bundle(levels = c(2, 3, 4, 5), n = rep(100L, 4),
                          prof = c(10L, 10L, 10L, 60L), spec = spec)
  res <- suppressWarnings(run_check4(b, spec))
  o <- res$j_optimal
  expect_identical(o$THRESHOLD_CUT, o$J_CUT)
  expect_false(o$THRESHOLD == o$J_CUT)
  expect_identical(o$THRESHOLD_SIDE, "same")
  expect_match(check4_optimum_note(res), "classifies exactly as the J optimum")

  # A strict rule on an observed level classifies as the next cut up.
  s2 <- spec
  lv <- make_check4_skewed_levels()
  s2$policy$exit$pathways[[1]]$terms[[1]]$value <- lv[[4]]
  s2$policy$exit$pathways[[1]]$terms[[1]]$op <- ">"
  r2 <- run_check4(make_check4_skewed_bundle(spec = spec), s2)
  expect_identical(r2$j_optimal$THRESHOLD_CUT, lv[[5]])
  expect_identical(r2$j_optimal$THRESHOLD_SIDE, "above")
  expect_match(check4_optimum_note(r2), "sits above the J optimum")
  expect_match(check4_optimum_note(r2), "> 4.3", fixed = TRUE)
})

test_that("unnamed pathways are named by their rule, and keep their own rule", {
  spec <- make_check4_spec()
  term <- spec$policy$exit$pathways[[1]]$terms[[1]]
  committee <- term
  committee$value <- term$value - 0.5
  spec$policy$exit$pathways <- list(
    list(rule = "Automatic on the composite", terms = list(term),
         combine = "all", discretionary = FALSE),
    list(rule = "Committee review", terms = list(committee),
         combine = "all", discretionary = TRUE))
  spec$policy$exit$types <- 5L
  res <- run_check4(make_check4_skewed_bundle(spec = make_check4_spec()), spec)
  expect_identical(res$meta$threshold, term$value)
  expect_identical(res$meta$threshold_rule, "Automatic on the composite")
  expect_match(res$meta$threshold_note, "Automatic on the composite")
  expect_false(grepl("'NA'", res$meta$threshold_note))

  # Alternative pathways are not a second criterion: nothing blocks anyone.
  # The marked term is one route of several, and is named so.
  expect_false(res$j_optimal$RULE_IS_TERM)
  expect_true(res$j_optimal$RULE_IS_ROUTE)
  expect_false(grepl("second criterion", res$meta$axis_note))
  expect_match(check4_optimum_note(res), "one route of several")

  # Who was asked is naturally plural, and renders.
  spec$policy$error_tolerance$costlier_error <- "premature_exit"
  spec$policy$error_tolerance$asked_of <- c("EL directors", "families")
  tn <- check4_tolerance_note(
    run_check4(make_check4_skewed_bundle(spec = make_check4_spec()), spec))
  expect_match(tn, "asked of EL directors, families", fixed = TRUE)
})

test_that("a categorical '>' rule is restated as '>=' the category it admits", {
  spec <- make_check4_elpa21_spec()
  lv <- spec$elp$level_scale$levels
  rule <- function(value) {
    s <- spec
    s$policy$exit$pathways[[1]]$terms <- list(list(
      component = spec_headline(s), measure = "level", op = ">",
      value = value))
    run_check4(make_check4_elpa21_bundle(spec), s)
  }
  r <- rule(lv[[length(lv) - 1L]])
  expect_identical(r$meta$threshold_op, ">=")
  expect_identical(r$meta$threshold_label, lv[[length(lv)]])
  expect_equal(r$j_optimal$THRESHOLD, length(lv))
  # Above the top category nothing is admitted, and nothing is marked.
  top <- rule(lv[[length(lv)]])
  expect_true(is.na(top$meta$threshold))
  expect_match(top$meta$threshold_note, "admits no category")
})

test_that("only a term on the reported component's own level is a threshold", {
  # ELPA21's rule is a domain profile: four floors, none on the
  # determination. A floor on a domain is not a cut on this axis.
  spec <- make_check4_elpa21_spec()
  res <- run_check4(make_check4_elpa21_bundle(spec), spec)
  expect_true(is.na(res$j_optimal$THRESHOLD))
  expect_true(is.na(res$meta$threshold))
  expect_match(res$meta$threshold_note, "no term on the")
  expect_true(is.na(res$meta$axis_note))
})

# ---- error tolerance ---------------------------------------------------------

test_that("Prep 3's error tolerance renders beside the J-optimal point", {
  # J weights the two error *rates* equally; a stated asymmetry is a
  # documented reason to land off it -- a values input, not a departure from
  # the evidence. The note says what J weights per student, because Prep 3
  # asks per student.
  spec <- make_check4_spec()
  b <- make_check4_skewed_bundle(spec = spec)
  res <- run_check4(b, spec)
  o <- res$j_optimal
  expect_identical(o$COSTLIER_ERROR, "unknown")
  expect_true(is.na(o$LEAN))
  expect_identical(res$meta$costlier_error, "unknown")
  expect_equal(o$IMPLIED_COST_RATIO, (1 - 0.16) / 0.16)
  note <- check4_tolerance_note(res)
  expect_match(note, "open question")
  expect_match(note, "as rates")
  expect_match(note, o$J_CUT_LABEL, fixed = TRUE)
  expect_match(note, formatC(o$IMPLIED_COST_RATIO, format = "f", digits = 1),
               fixed = TRUE)
})

test_that("the J optimum prices a hold at (1 - pi) / pi exits, and the lean follows", {
  # The identity behind every tolerance sentence: the J optimum is the cut
  # minimizing FP + R * FN with R = (1 - pi) / pi; equal per-student costs
  # (R = 1) instead pick the agreement maximum.
  spec <- make_check4_spec()
  b <- make_check4_skewed_bundle(spec = spec)
  res <- run_check4(b, spec)
  y <- res$youden_j[!res$youden_j$SUPPRESSED, ]
  o <- res$j_optimal
  r <- o$IMPLIED_COST_RATIO
  expect_identical(y$CUT[which.min(y$FP + r * y$FN)], o$J_CUT)
  expect_identical(y$CUT[which.min(y$FP + y$FN)], o$AGREE_CUT)

  lean <- function(bundle, answer) {
    s <- spec
    s$policy$error_tolerance$costlier_error <- answer
    r <- run_check4(bundle, s)
    list(lean = r$j_optimal$LEAN, note = check4_tolerance_note(r),
         ratio = r$j_optimal$IMPLIED_COST_RATIO)
  }
  # Base rate 16%, so J prices a hold at about 5.2 exits.
  pe <- lean(b, "premature_exit")
  expect_identical(pe$lean, "above")
  expect_match(pe$note, "above the J optimum")
  eq <- lean(b, "equal")
  expect_identical(eq$lean, "above")
  expect_match(eq$note, "above the J optimum")
  pr <- lean(b, "prolonged_retention")
  expect_true(is.na(pr$lean))     # depends on how much costlier
  expect_match(pr$note, formatC(pr$ratio, format = "f", digits = 1),
               fixed = TRUE)
  expect_match(pr$note, "how much costlier")

  # A majority-proficient panel (60%): J now prices a hold below one exit.
  maj <- make_check4_bundle(levels = make_check4_skewed_levels(),
                            n = rep(100L, 5), prof = c(40L, 50L, 60L, 70L, 80L),
                            spec = spec)
  expect_lt(lean(maj, "equal")$ratio, 1)
  expect_identical(lean(maj, "prolonged_retention")$lean, "below")
  expect_true(is.na(lean(maj, "premature_exit")$lean))
  # Agreement ties at 2.3 and 3.3 here, and the J optimum (3.3) is one of the
  # tied cuts: not a divergence, and equal costs land on it.
  rmaj <- run_check4(maj, spec)
  expect_false(rmaj$j_optimal$OPTIMA_DIVERGE)
  expect_identical(lean(maj, "equal")$lean, "at")

  # Agreement at the J optimum below the everyone-not-ready floor: equal
  # costs point above it, toward classifying nobody ready.
  flr <- make_check4_bundle(levels = c(2.3, 3.3, 4.3, 5.3), n = rep(200L, 4),
                            prof = c(10L, 10L, 10L, 50L), spec = spec)
  eq_flr <- lean(flr, "equal")
  expect_identical(eq_flr$lean, "above")
  expect_match(eq_flr$note, "classifying nobody ready")

  # A suppressed cut below the J optimum cannot hide a better equal-cost cut.
  low <- make_check4_bundle(levels = c(2.1, 2.5, 3.3, 4.3, 5.3),
                            n = c(100L, 5L, 100L, 100L, 100L),
                            prof = c(10L, 0L, 10L, 10L, 60L), spec = spec)
  expect_true(any(run_check4(low, spec)$consistency_curve$SUPPRESSED))
  expect_identical(lean(low, "equal")$lean, "at")

  # Equal costs land on the J optimum itself when agreement peaks there too.
  same <- make_check4_bundle(levels = c(2, 3, 4, 5), n = rep(100L, 4),
                             prof = c(10L, 10L, 10L, 60L), spec = spec)
  eq_same <- suppressWarnings(lean(same, "equal"))
  expect_identical(eq_same$lean, "at")
  expect_match(eq_same$note, "on the J optimum itself")

  # Near an even trade the ratio is printed finely enough to show its side.
  near <- make_check4_bundle(levels = make_check4_skewed_levels(),
                             n = rep(100L, 5),
                             prof = c(20L, 35L, 50L, 65L, 75L), spec = spec)
  nr <- lean(near, "prolonged_retention")
  expect_gt(nr$ratio, 1)
  expect_match(nr$note, formatC(nr$ratio, format = "f", digits = 2),
               fixed = TRUE)
})

# ---- slots this pass leaves empty --------------------------------------------

test_that("the output is a candidate region, not a single number", {
  skip("check4 not built: $candidate_region")
})

test_that("the region is annotated with per-grade reliability from Check 1", {
  skip("check4 not built: $candidate_region")
})

test_that("the dependence slot is NULL and the document says turnkey vs custom", {
  # The copula / Kendall's-tau analysis has no package support yet.
  spec <- make_check4_spec()
  res <- run_check4(make_check4_skewed_bundle(spec = spec), spec)
  expect_null(res$dependence)
  expect_true("dependence" %in% names(res))
  expect_match(res$meta$dependence_note, "turnkey")
  expect_match(res$meta$dependence_note, "custom")

  qmd <- system.file("templates", "check4_consistency.qmd",
                     package = "stateELP")
  expect_true(nzchar(qmd))
  txt <- paste(readLines(qmd, warn = FALSE), collapse = "\n")
  expect_match(txt, "turnkey")
  expect_match(txt, "custom")
})

test_that("the policy-as-whole slot is NULL under a Type 1 spec", {
  spec <- make_check4_spec()
  expect_false(spec_has_type(spec, 2:6))
  res <- run_check4(make_check4_skewed_bundle(spec = spec), spec)
  expect_null(res$policy_as_whole)
  expect_true("policy_as_whole" %in% names(res))
  expect_false(res$meta$type_gate_applies)
  expect_match(res$meta$policy_as_whole_note, "^Does not apply")
  expect_false("policy_as_whole" %in% res$meta$pending)

  # A Type 6 spec opens the gate; this pass names it pending rather than
  # pretending it does not apply.
  spec6 <- make_check4_elpa21_spec()
  res6 <- run_check4(make_check4_elpa21_bundle(spec6), spec6)
  expect_true(res6$meta$type_gate_applies)
  expect_match(res6$meta$policy_as_whole_note, "^Applies")
  expect_true("policy_as_whole" %in% res6$meta$pending)
})

test_that("policy_as_whole is computed under a Type 2-6 spec", {
  skip("check4 not built: $policy_as_whole (Types 2-6)")
})

test_that("under a Type 2-6 rule the marked line is one term, and says so", {
  # A composite term beside a domain floor: the composite term is still a cut
  # on this axis, but it is not the rule, and the top of the axis holds
  # students the floor is blocking.
  spec <- make_check3_type2_spec(make_check4_spec())
  res <- run_check4(make_check4_skewed_bundle(spec = make_check4_spec()), spec)
  expect_true(res$j_optimal$RULE_IS_TERM)
  expect_match(res$meta$axis_note, "second criterion")
  expect_match(check4_optimum_note(res), "term")
  expect_match(check4_optimum_note(res), "Additional check")

  res1 <- run_check4(make_check4_skewed_bundle(), make_check4_spec())
  expect_false(res1$j_optimal$RULE_IS_TERM)
  expect_false(grepl("second criterion", res1$meta$axis_note))
})

# ---- structure ---------------------------------------------------------------

test_that("run_check4() returns every named slot and a $meta", {
  spec <- make_check4_spec()
  res <- run_check4(make_check4_skewed_bundle(spec = spec), spec)
  expect_named(res, c("consistency_curve", "youden_j", "j_optimal",
                      "candidate_region", "dependence", "policy_as_whole",
                      "meta"))
  expect_null(res$candidate_region)
  expect_identical(res$meta$stage, "check4")
  expect_identical(res$meta$spec_id, spec$program$short_name)
  for (f in c("sample", "sim_contract_version", "package_version",
              "run_date", "pending")) {
    expect_true(f %in% names(res$meta), info = f)
  }
  expect_true("candidate_region" %in% res$meta$pending)
})

test_that("the Sample says it is Check 3's cross-section, not the cohort", {
  spec <- make_check4_spec()
  b <- make_check4_skewed_bundle(spec = spec)
  res <- run_check4(b, spec)
  p <- compute_panel(b, spec)
  s <- res$meta$sample
  expect_match(s, "[Cc]ross-sectional")
  expect_match(s, "Data Check 3")
  expect_match(s, "not the entry cohort")
  expect_match(s, paste0("grade-", p$grade), fixed = TRUE)
  expect_match(s, as.character(p$year), fixed = TRUE)
  expect_match(s, spec_headline_label(spec), fixed = TRUE)
  expect_match(s, "n = 500", fixed = TRUE)
  expect_match(s, "never pooled")
})

test_that("output is keyed by grade and never pooled", {
  spec <- make_check4_spec()
  b <- make_check4_sim_bundle()
  subj <- spec_academic_subjects(spec)[[1]]
  grades <- spec_academic_grades(spec, subj)
  a <- compute_consistency_curve(b, spec, compute_panel(b, spec,
                                                        grade = grades[[1]]))
  z <- compute_consistency_curve(b, spec, compute_panel(b, spec,
                                                        grade = grades[[2]]))
  expect_identical(unique(a$GRADE), grades[[1]])
  expect_identical(unique(z$GRADE), grades[[2]])
  # Two grades are two curves with their own N, never one curve over both.
  expect_false(identical(unique(a$N), unique(z$N)))

  # A panel is one grade: a multi-grade panel is refused, not pooled.
  bad <- compute_panel(b, spec)
  bad$grade <- grades[1:2]
  expect_error(compute_consistency_curve(b, spec, bad))

  # run_check4() renders one panel, and every slot carries one key.
  res <- run_check4(b, spec)
  for (s in c("consistency_curve", "youden_j", "j_optimal")) {
    k <- unique(paste(res[[s]]$SUBJECT, res[[s]]$GRADE, res[[s]]$YEAR))
    expect_length(k, 1L)
  }

  # And j_optimal keeps panels apart when handed more than one.
  both <- dplyr::bind_rows(compute_youden_j(a), compute_youden_j(z))
  o <- compute_j_optimal(both, spec)
  expect_identical(nrow(o), 2L)
  expect_setequal(o$GRADE, grades[1:2])
})

test_that("Check 4's sample and cut agree with Check 3's for the same panel", {
  # Check 4 rebuilds the paired rows because Check 3's builder is internal to
  # its prefix. Here is the proof they are the same students, band by band,
  # and the same proficiency cut and exit band.
  spec <- make_check4_spec()
  b <- make_check4_sim_bundle()
  p <- compute_panel(b, spec)
  rows <- .check4_panel_rows(b, spec, p)
  bd <- compute_box_data(b, spec, p)
  bands <- spec_bands(spec)$labels

  per_band <- as.integer(table(factor(assign_elp_band(rows$LEVEL, spec),
                                      levels = bands)))
  expect_identical(per_band, as.integer(bd$N_PLOTTED[match(bands, bd$GROUP)]))
  expect_identical(nrow(rows), as.integer(sum(bd$N_PLOTTED[bd$GROUP %in% bands])))

  curve <- compute_consistency_curve(b, spec, p)
  expect_identical(attr(curve, "cut"), attr(bd, "cut"))
  lg <- compute_logistic(b, spec, p)
  expect_identical(unique(curve$N), as.integer(lg$N))
  expect_identical(unique(curve$N_PROFICIENT[!is.na(curve$N_PROFICIENT)]),
                   as.integer(lg$N_ABOVE))

  # The rule Check 4 marks falls in the band Check 3 reads as the exit band.
  o <- compute_j_optimal(compute_youden_j(curve), spec)
  landing <- compute_landing(bd, lg, spec)
  expect_identical(assign_elp_band(o$THRESHOLD, spec), landing$EXIT_BAND)
})

test_that("the sim curve demonstrates the gap, and the J optimum is reported", {
  spec <- make_check4_spec()
  res <- run_check4(make_check4_sim_bundle(), spec)
  o <- res$j_optimal
  expect_false(o$SUPPRESSED)
  expect_lt(o$BASE_RATE, 0.5)
  expect_true(o$OPTIMA_DIVERGE)
  expect_lt(o$J_CUT, o$AGREE_CUT)
  y <- res$youden_j
  expect_identical(o$J_CUT, y$CUT[which.max(y$J)])
  expect_identical(o$J, max(y$J, na.rm = TRUE))
})

test_that("no academic table, or no academic subject, means no stage", {
  spec <- make_check4_spec()
  b <- make_check4_skewed_bundle(spec = spec)
  b$academic <- NULL
  res <- run_check4(b, spec)
  for (s in c("consistency_curve", "youden_j", "j_optimal")) {
    expect_null(res[[s]], info = s)
  }
  expect_match(res$meta$note, "No academic table")
  expect_match(res$meta$sample, "no panel is drawn")

  # The shipped ELPA21 spec points at a separate academic spec and declares no
  # subject: there is no cut to classify against, and no crash.
  spec21 <- read_el_spec("00_ELPA21_EL_Spec")
  b21 <- make_check4_elpa21_bundle()
  res21 <- run_check4(b21, spec21)
  expect_null(res21$consistency_curve)
  expect_match(res21$meta$note, "no academic subject")

  # An end-of-course subject declared first, which the delivery carries no
  # rows for: the panel cannot be derived, and that is a note, not a crash.
  eoc <- spec
  first <- spec$academic[[spec_academic_subjects(spec)[[1]]]]
  first$grades <- NULL
  eoc$academic <- c(list(EOC_SUBJECT = first), spec$academic)
  validate_el_spec(eoc)
  res_eoc <- run_check4(make_check4_skewed_bundle(spec = spec), eoc)
  expect_null(res_eoc$consistency_curve)
  expect_match(res_eoc$meta$note, "No panel could be derived")
  expect_match(res_eoc$meta$sample, "No panel could be derived for EOC_SUBJECT")
  # The R error is kept for the analyst, never printed in the document.
  expect_false(grepl("subscript", res_eoc$meta$note))
  expect_false(is.na(res_eoc$meta$panel_error))
})

test_that("a categorical, level-only scale runs through every function", {
  # ELPA21: the determination is a category with no scale score. Coarse --
  # one cut per category -- but neither NULL nor an error.
  spec <- make_check4_elpa21_spec()
  b <- make_check4_elpa21_bundle(spec)
  res <- run_check4(b, spec)
  expect_identical(res$consistency_curve$CUT, c(1, 2, 3))
  expect_identical(res$consistency_curve$CUT_LABEL,
                   spec$elp$level_scale$levels)
  expect_false(any(res$consistency_curve$SUPPRESSED))
  expect_false(is.na(res$j_optimal$J_CUT))
  expect_s3_class(plot_consistency_curve(res$youden_j, res$j_optimal, spec),
                  "ggplot")
  expect_s3_class(ft_check4_optimum(res), "flextable")
  expect_s3_class(ft_check4_curve(res), "flextable")
})

# ---- rendering helpers -------------------------------------------------------

test_that("the figure and tables build, and say what they mark", {
  spec <- make_check4_spec()
  res <- run_check4(make_check4_skewed_bundle(spec = spec), spec)
  p <- plot_consistency_curve(res$youden_j, res$j_optimal, spec)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  # Without the optimum, nothing is marked and nothing breaks.
  expect_s3_class(plot_consistency_curve(res$youden_j), "ggplot")
  expect_error(plot_consistency_curve(res$consistency_curve), "youden_j")

  d <- .check4_curve_display(res$youden_j, res$j_optimal)
  o <- res$j_optimal
  expect_identical(d$MARK[d$CUT == o$J_CUT_LABEL], "J optimum")
  expect_match(d$MARK[d$CUT == o$AGREE_CUT_LABEL],
               "highest reportable agreement")
  expect_match(d$MARK[d$CUT == o$AGREE_CUT_LABEL], "current rule")

  # A table of two years is two panels: the helpers take one, never both.
  spec <- make_check4_spec()
  b <- make_check4_sim_bundle()
  yr <- compute_panel(b, spec)$years
  two <- dplyr::bind_rows(
    compute_youden_j(compute_consistency_curve(
      b, spec, compute_panel(b, spec, year = yr[[length(yr)]]))),
    compute_youden_j(compute_consistency_curve(
      b, spec, compute_panel(b, spec, year = yr[[length(yr) - 1L]]))))
  one <- .c4_one_panel(two)
  expect_length(unique(one$YEAR), 1L)
  expect_identical(nrow(.check4_curve_display(two, compute_j_optimal(two,
                                                                      spec))),
                   nrow(one))

  od <- .check4_optimum_display(o)
  expect_identical(od$J_CUT, o$J_CUT_LABEL)
  expect_identical(od$BASE_RATE, "16%")

  note <- check4_optimum_note(res)
  expect_match(note, "16% of the 500")
  expect_match(note, paste0("highest at ", o$J_CUT_LABEL), fixed = TRUE)
  expect_match(note, "base-rate pull")
})

test_that("suppressed cells render as a dash", {
  spec <- make_check4_spec()
  b <- make_check4_bundle(levels = make_check4_skewed_levels(),
                          n = rep(100L, 5),
                          prof = c(spec_min_cell(spec) - 1L, 10L, 10L, 20L,
                                   30L), spec = spec)
  res <- run_check4(b, spec)
  d <- .check4_curve_display(res$youden_j, res$j_optimal)
  dash <- intToUtf8(0x2014)
  expect_true(all(d$TP[res$youden_j$SUPPRESSED] == dash))
  expect_true(all(d$J[res$youden_j$SUPPRESSED] == dash))
})

test_that("check4 sources type no year, band label or threshold", {
  # The panel, the bands and the rule all come from the spec and the data.
  files <- list.files(test_path("..", "..", "R"), pattern = "^check4_.*\\.R$",
                      full.names = TRUE)
  skip_if(!length(files), "R/ sources not available (installed package)")
  for (f in files) {
    code <- readLines(f, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    code <- sub("#.*$", "", code)
    expect_false(any(grepl("\\b(19|20)[0-9]{2}\\b", code)), info = f)
    expect_false(any(grepl("\"[0-9]\\.[0-9]-[0-9]\\.[0-9]\"", code)),
                 info = f)
    expect_false(any(grepl("\\b4\\.5\\b", code)), info = f)
  }
})
