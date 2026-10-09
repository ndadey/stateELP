# Check 4 -- Decision consistency and Youden's J. Plain data out, no
# plotting, no I/O.
#
# The sample is Check 3's paired cross-section, not Checks 1-2's cohort. The
# panel comes from Check 3's exported compute_panel(), so the two stages cannot
# disagree about which grade, subject and year they describe. The rows are
# rebuilt here because Check 3's row builder is internal to its prefix; a test
# proves the two agree on every band (CLAUDE.md 14).
#
# The axis is the ELP *level*, not the scale score (decided 2026-10-09; see
# internal_docs/NOTES-check4.md): the exit rule is written on the level, and
# only the level scale is declared in the spec.

# Alternate academic takers are on a different scale; "reached the cut" is not
# defined for them, so they are never in the sample.
.CHECK4_ALT <- "ACHIEVEMENT_LEVEL_ALT"

# Levels are delivered as decimals (or ordinal indices); comparisons against a
# value typed in a spec allow for representation error, never more.
.CHECK4_TOL <- 1e-9

#' Decision consistency across candidate cuts, for one panel
#'
#' At every candidate cut on the ELP level, each current EL in the panel is
#' classified ready to exit (level at or above the cut) or not, and that
#' classification is set against whether the same student reached proficiency
#' on the content test that year. One row per cut carries the four cells of
#' that two-by-two table and **decision consistency** -- the share of students
#' for whom the two classifications agree.
#'
#' **The sample is Check 3's paired cross-section.** The panel is
#' [compute_panel()]'s, unchanged. The rows are every current EL in that grade
#' and year with both a same-year annual level and a content score: the
#' academic table leads the join to the roster, alternate academic takers are
#' excluded (their scores are on another scale), and former and never-EL
#' students are not in it, because the classifications compared here exist
#' only for students still classified. Proficiency is the academic scale score
#' at or above the minimum of the level the spec calls proficient -- exactly
#' Check 3's definition.
#'
#' **Candidate cuts are the levels the delivery actually carries**, within the
#' spec's declared level scale -- never a hardcoded grid. A numeric scale gives
#' one cut per distinct observed level; a categorical scale (an ordinal index
#' into `level_scale$levels`) one per observed category, labelled with it. A
#' level outside the declared scale is a delivery problem, not a cut: it is
#' left out and counted in the `"n_out_of_scale"` attribute.
#'
#' When every observed level is whole but the spec draws on a fractional break
#' (a band boundary or the exit threshold), the curve is coarser than the rule
#' it is read against. A warning of class `check4_coarse_levels` says so.
#'
#' **Suppression, in two steps.** `[PROPOSED - CONFIRM]` A suppressed cut
#' keeps its row, its cut and `N`, with every count and rate blanked to `NA`
#' and `SUPPRESSED = TRUE`; a zero is shown, as everywhere in the package.
#'
#' 1. *Within a row.* Every statistic in a row is a function of all four cells
#'    together, so a small cell is recoverable from any of them: a cut with any
#'    non-zero cell below the spec's `min_cell` is suppressed.
#' 2. *Between rows.* The cells are cumulative -- every cut counts everyone at
#'    or above it -- so subtracting two reported cuts gives the students between
#'    them, by proficiency. Walking up from the lowest cut, a cut is reported
#'    only if the step from the last published point holds 0 or at least
#'    `min_cell` proficient students, and 0 or at least `min_cell` who are
#'    not. A thin level is merged into the next cut that clears it rather than
#'    exposed by difference. "Published" includes the band breaks, whose
#'    cumulative counts Check 3's band table already gives for the same
#'    students: a cut is also checked against the next break above it.
#'
#' The same cell standard as Check 2's crosstab, applied to the slices as well
#' as the rows.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param panel Output of [compute_panel()], or `NULL` to derive it.
#' @return A tibble, one row per candidate cut in ascending order: `SUBJECT`,
#'   `GRADE`, `YEAR`, `CUT`, `CUT_LABEL`, `N`, `N_PROFICIENT`, `TP`
#'   (proficient, at/above the cut), `FP` (not proficient, at/above), `FN`
#'   (proficient, below), `TN` (not proficient, below), `AGREE`
#'   (`(TP + TN) / N`), `SUPPRESSED`. `NULL` when the panel has no year, the
#'   spec no proficiency cut for the subject, or the panel no paired student.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_consistency_curve(b, spec)
compute_consistency_curve <- function(data, spec, panel = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  if (is.null(panel)) panel <- compute_panel(data, spec)
  stopifnot(is.list(panel), all(c("subject", "grade", "year") %in% names(panel)),
            length(panel$grade) == 1L, length(panel$subject) == 1L)
  if (is.na(panel$year)) return(NULL)      # [no academic year for this panel]

  cut_ac <- .check4_cut(spec, panel$subject)
  if (is.na(cut_ac)) return(NULL)          # [no proficiency cut in the spec]

  rows <- .check4_panel_rows(data, spec, panel)
  if (!nrow(rows)) return(NULL)            # [a missing year, not an empty one]

  prof <- rows$SCALE_SCORE >= cut_ac
  cuts <- sort(unique(rows$LEVEL))
  .check4_warn_coarse(cuts, spec)

  cells <- vapply(cuts, function(ct) .check4_cells(rows$LEVEL, prof, ct),
                  numeric(4))
  out <- tibble::tibble(
    SUBJECT      = panel$subject,
    GRADE        = panel$grade,
    YEAR         = as.integer(panel$year),
    CUT          = cuts,
    CUT_LABEL    = .check4_cut_label(cuts, spec),
    N            = nrow(rows),
    N_PROFICIENT = sum(prof),
    TP           = as.integer(cells[1, ]),
    FP           = as.integer(cells[2, ]),
    FN           = as.integer(cells[3, ]),
    TN           = as.integer(cells[4, ]))
  out$AGREE <- (out$TP + out$TN) / out$N

  # Two-step suppression (roxygen above). The threshold is the spec's
  # min_cell, never a literal; the blanking is done here rather than through
  # apply_suppression() because the second step depends on which earlier rows
  # survived. The margins need no check of their own: a margin below min_cell
  # forces a cell below it in every row.
  min_cell <- spec_min_cell(spec)
  hide <- !.check4_reportable(cells, min_cell,
                              anchors = .check4_band_anchors(cuts, spec))
  # Check 3's band table shows every band's n but its share at the cut only
  # where the band holds min_cell or more. Check 4's margins (N_PROFICIENT, on
  # every shown row) minus the bands whose shares are shown give the
  # proficiency split of the bands that are not -- so if that split is a
  # small group, no row can be shown at all.
  rest <- .check4_unshared_split(rows$LEVEL, prof, spec, min_cell)
  if (any(rest > 0 & rest < min_cell)) hide[] <- TRUE
  for (cl in c("N_PROFICIENT", "TP", "FP", "FN", "TN", "AGREE")) {
    out[[cl]][hide] <- NA
  }
  out$SUPPRESSED <- hide

  attr(out, "panel") <- panel
  attr(out, "cut") <- cut_ac
  attr(out, "n_out_of_scale") <- attr(rows, "n_out_of_scale")
  out
}

#' Youden's J across candidate cuts
#'
#' Adds sensitivity, specificity and **Youden's J** to a consistency curve.
#' Sensitivity is the share of proficient students the cut classifies ready,
#' `TP / (TP + FN)`; specificity the share of not-proficient students it
#' classifies not ready, `TN / (TN + FP)`; and `J = sensitivity + specificity
#' - 1`.
#'
#' **Why J and not agreement.** Agreement is `pi * sensitivity + (1 - pi) *
#' specificity`, where `pi` is the proficiency rate, so when `pi` is well below
#' one half -- typical for ELs -- the specificity term dominates it. J weights
#' the two error *rates* equally whatever the base rate. Raising the cut past a
#' level moves agreement up when most students at that level are not
#' proficient, but moves J up when they are proficient less often than the base
#' rate; so (for proficiency rates that rise with level) agreement peaks where
#' the rate among students at a level crosses one half, and J where it crosses
#' `pi`. Below one half, the J optimum sits at or below the agreement peak --
#' in practice usually well below it.
#'
#' Computed from the four cells, never from `AGREE`, so J cannot be the
#' consistency statistic relabelled. A suppressed row stays suppressed: its
#' cells are `NA`, and so is everything computed from them.
#'
#' @param curve Output of [compute_consistency_curve()].
#' @return `curve` with `SENSITIVITY`, `SPECIFICITY` and `J` added; `NULL` when
#'   `curve` is `NULL`. A rate whose denominator is zero is `NA`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_youden_j(compute_consistency_curve(b, spec))
compute_youden_j <- function(curve) {
  if (is.null(curve)) return(NULL)
  stopifnot(is.data.frame(curve),
            all(c("CUT", "TP", "FP", "FN", "TN") %in% names(curve)))
  out <- curve
  pos <- out$TP + out$FN
  neg <- out$TN + out$FP
  out$SENSITIVITY <- ifelse(!is.na(pos) & pos > 0, out$TP / pos, NA_real_)
  out$SPECIFICITY <- ifelse(!is.na(neg) & neg > 0, out$TN / neg, NA_real_)
  out$J <- out$SENSITIVITY + out$SPECIFICITY - 1
  out
}

#' The J optimum, the agreement peak, and the current rule, per panel
#'
#' One row per panel reading [compute_youden_j()]'s curve: the cut where J is
#' highest (**the stage's optimal-cut statistic**), the highest reportable
#' agreement beside it (so the gap between the two is visible), and both
#' statistics at the state's current exit threshold.
#'
#' **No optimum** is reported (`J_CUT` is `NA`, `NO_OPTIMUM` says why) when
#' every cut is suppressed, or every cut that could separate the groups is
#' (`"suppressed"`); when no student, or every student, is proficient, so J
#' is undefined (`"no_variation"`); or when no cut has J above zero and none
#' was suppressed, so no cut separates the two groups at all
#' (`"uninformative"`).
#'
#' **The current threshold is read from the spec, never typed**: the
#' `policy$exit` term on the reported ELP component's own level. When several
#' pathways carry such a term, the first non-discretionary one is used and the
#' rest are named in [run_check4()]'s `$meta$threshold_note`. When none does
#' -- a domain-profile rule, say -- every threshold column is `NA`. Because
#' students are ordered by level, a threshold that is not itself an observed
#' level classifies exactly as the next observed cut at or above it
#' (`THRESHOLD_CUT`), so the statistics at the rule are read from that row and
#' `THRESHOLD_SIDE` compares that cut, not the typed value, with the J optimum;
#' a rule above every observed level classifies nobody ready, and is read off
#' the margins. When the term's pathway also asks for another criterion (a
#' Type 2-6 rule with a conjunctive pathway), the term is one criterion of
#' several, and `RULE_IS_TERM` says so. On a categorical scale the rule is
#' restated as ">=" the first category it admits.
#'
#' **Ties.** The lowest cut among those tied for the maximum wins, for J and for
#' agreement alike, and `N_J_TIED` says how many tied. `[PROPOSED - CONFIRM]`
#' Only reportable (unsuppressed) cuts compete.
#'
#' **What J weights, and error tolerance** (Prep 3). J weights the two error
#' *rates* equally -- the share of proficient students held back and the share
#' of not-proficient students exited. Per student that is not an even trade:
#' the J optimum is the cut that minimizes `FP + R * FN` with
#' `R = (1 - pi) / pi` (`IMPLIED_COST_RATIO`), so at a 12% base rate it weighs
#' holding back one proficient student as heavily as exiting about seven who
#' are not. Prep 3 asks stakeholders per student, so `LEAN` compares their
#' answer with `R`: a premature exit judged costlier points `"above"` the J
#' optimum whenever `R >= 1`; equal per-student costs point `"above"` when
#' `R > 1` (toward the agreement maximum); prolonged retention judged costlier
#' points `"below"` only when `R <= 1` -- otherwise it depends on how much
#' costlier, and `LEAN` is `NA`. `NA` too while the question is open.
#' `[PROPOSED - CONFIRM]`
#'
#' @param youden_j Output of [compute_youden_j()]; one or more panels, keyed by
#'   `SUBJECT`, `GRADE`, `YEAR`.
#' @param spec An `el_spec`.
#' @return A tibble, one row per panel: the keys, `N`, `N_PROFICIENT`,
#'   `BASE_RATE`, `J_CUT`, `J_CUT_LABEL`, `J`, `SENSITIVITY`, `SPECIFICITY`,
#'   `AGREE_AT_J`, `N_J_TIED`, `J_MAX_HIDDEN` (a suppressed cut lies between
#'   the reported cuts either side of the J optimum, so it is located only to
#'   within that range; `FALSE` does not promise that no suppressed cut
#'   farther away holds the full-data maximum), `NO_OPTIMUM`, `AGREE_CUT`, `AGREE_CUT_LABEL`,
#'   `AGREE`, `AGREE_MAX_HIDDEN` (some cut is suppressed, so the true maximum
#'   may be hidden), `AGREE_FLOOR`
#'   (the agreement of classifying every student not ready,
#'   `1 - BASE_RATE`), `OPTIMA_DIVERGE`, `THRESHOLD`, `THRESHOLD_OP`,
#'   `THRESHOLD_LABEL`, `THRESHOLD_CUT`, `THRESHOLD_SIDE` (`"above"`,
#'   `"below"` or `"same"` relative to the J optimum, or `"within"` the
#'   suppressed range around it), `RULE_IS_TERM`, `RULE_IS_ROUTE` (the term is
#'   one route of several: students below it can exit another way),
#'   `J_SHOWN_BELOW`, `J_SHOWN_ABOVE` (the shown cuts bounding a hidden range
#'   around the optimum),
#'   `J_AT_THRESHOLD`, `AGREE_AT_THRESHOLD`, `IMPLIED_COST_RATIO`,
#'   `COSTLIER_ERROR`, `LEAN`, `SUPPRESSED` (every cut in the panel is
#'   suppressed). `NULL` when `youden_j` is `NULL`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_j_optimal(compute_youden_j(compute_consistency_curve(b, spec)), spec)
compute_j_optimal <- function(youden_j, spec) {
  stopifnot(inherits(spec, "el_spec"))
  if (is.null(youden_j)) return(NULL)
  need <- c("SUBJECT", "GRADE", "YEAR", "CUT", "CUT_LABEL", "N",
            "N_PROFICIENT", "AGREE", "SENSITIVITY", "SPECIFICITY", "J")
  stopifnot(is.data.frame(youden_j), all(need %in% names(youden_j)))

  thr <- .check4_exit_threshold(spec)
  tol <- .check4_tolerance(spec)
  is_term <- spec_has_type(spec, 2:6)
  key <- paste(youden_j$SUBJECT, youden_j$GRADE, youden_j$YEAR, sep = "\r")
  parts <- lapply(unique(key), function(k) {
    .check4_optimum_row(youden_j[key == k, , drop = FALSE], thr, tol, is_term)
  })
  dplyr::bind_rows(parts)
}

# One panel's optimum row. `d` is that panel's curve in ascending cut order.
.check4_optimum_row <- function(d, thr, tol, is_term) {
  d <- d[order(d$CUT), , drop = FALSE]
  best <- function(v) {
    ok <- !is.na(v)
    if (!any(ok)) return(NA_integer_)
    min(which(ok & v >= max(v[ok]) - 1e-12))
  }
  j_i <- best(d$J)
  a_i <- best(d$AGREE)

  n <- d$N[[1]]
  n_prof <- d$N_PROFICIENT[!is.na(d$N_PROFICIENT)][1]
  base_rate <- if (is.na(n_prof)) NA_real_ else n_prof / n
  all_hidden <- if ("SUPPRESSED" %in% names(d)) all(d$SUPPRESSED) else
    all(is.na(d$J))

  # Why there is no optimum, when there is none. A J of zero or less at every
  # reportable cut means no reportable cut separates the two groups: the
  # lowest cut (everyone ready) would otherwise "win" with J = 0. Whether that
  # is the data or the suppression depends on whether any cut was hidden --
  # "uninformative" is claimed only when nothing was.
  any_hidden <- "SUPPRESSED" %in% names(d) && any(d$SUPPRESSED)
  no_opt <- if (all_hidden) {
    "suppressed"
  } else if (!is.na(base_rate) && (base_rate == 0 || base_rate == 1)) {
    "no_variation"
  } else if (is.na(j_i) || d$J[[j_i]] <= 1e-12) {
    if (any_hidden) "suppressed" else "uninformative"
  } else {
    NA_character_
  }
  if (!is.na(no_opt)) j_i <- NA_integer_
  at <- function(col, i) if (is.na(i)) NA else d[[col]][[i]]

  # Whether a suppressed cut sits inside the merged range around the J optimum
  # -- between the reported cuts either side of it -- so the optimum is
  # located only to within that range.
  # It does not promise the reverse: J_MAX_HIDDEN = FALSE says nothing about
  # suppressed cuts farther away, any of which can hold the full-data maximum.
  j_hidden <- FALSE
  lo <- 0L
  hi <- nrow(d) + 1L
  if (!is.na(j_i) && any_hidden) {
    shown <- which(!d$SUPPRESSED)
    lo <- max(c(0L, shown[shown < j_i]))
    hi <- min(c(nrow(d) + 1L, shown[shown > j_i]))
    j_hidden <- any(d$SUPPRESSED[setdiff(seq.int(lo + 1L, hi - 1L), j_i)])
  }

  # Agreement's maximum, as a set: a tie that includes the J optimum is not a
  # divergence, whichever tied cut the lowest-cut rule happens to report.
  agree_top <- if (is.na(a_i)) logical(nrow(d)) else
    !is.na(d$AGREE) & d$AGREE >= d$AGREE[[a_i]] - 1e-12
  diverge <- !is.na(j_i) && !is.na(a_i) && !agree_top[[j_i]]

  # The rule's own row: the first observed cut the rule classifies the same
  # way. Above every observed level, nobody is ready -- read off the margins.
  thr_i <- NA_integer_
  j_thr <- NA_real_
  a_thr <- NA_real_
  above_all <- FALSE
  if (!is.na(thr$value)) {
    hit <- if (identical(thr$op, ">")) {
      which(d$CUT > thr$value + .CHECK4_TOL)
    } else {
      which(d$CUT >= thr$value - .CHECK4_TOL)
    }
    if (length(hit)) {
      thr_i <- hit[[1]]
      j_thr <- d$J[[thr_i]]
      a_thr <- d$AGREE[[thr_i]]
    } else {
      above_all <- TRUE
      if (!is.na(base_rate) && base_rate > 0 && base_rate < 1) {
        j_thr <- 0
        a_thr <- 1 - base_rate
      }
    }
  }

  # Which side of the J optimum the rule falls, by the cut it classifies as:
  # a rule between two observed levels classifies exactly as the higher one.
  # When suppressed cuts surround the optimum and the rule's cut lies among
  # them, the rule is inside the range the optimum is only known to within:
  # no side can be stated.
  in_hidden <- j_hidden && !is.na(thr$value) &&
    ((above_all && hi == nrow(d) + 1L) ||
       (!is.na(thr_i) && thr_i != j_i && thr_i > lo && thr_i < hi))
  side <- if (is.na(thr$value) || is.na(j_i)) {
    NA_character_
  } else if (in_hidden) {
    "within"
  } else if (above_all || thr_i > j_i) {
    "above"
  } else if (thr_i < j_i) {
    "below"
  } else {
    "same"
  }

  ratio <- if (is.na(base_rate) || base_rate <= 0 || base_rate >= 1) {
    NA_real_
  } else {
    (1 - base_rate) / base_rate
  }

  tibble::tibble(
    SUBJECT            = d$SUBJECT[[1]],
    GRADE              = d$GRADE[[1]],
    YEAR               = d$YEAR[[1]],
    N                  = n,
    N_PROFICIENT       = n_prof,
    BASE_RATE          = base_rate,
    J_CUT              = as.numeric(at("CUT", j_i)),
    J_CUT_LABEL        = as.character(at("CUT_LABEL", j_i)),
    J                  = as.numeric(at("J", j_i)),
    SENSITIVITY        = as.numeric(at("SENSITIVITY", j_i)),
    SPECIFICITY        = as.numeric(at("SPECIFICITY", j_i)),
    AGREE_AT_J         = as.numeric(at("AGREE", j_i)),
    N_J_TIED           = if (is.na(j_i)) NA_integer_ else
                           sum(!is.na(d$J) & d$J >= d$J[[j_i]] - 1e-12),
    J_MAX_HIDDEN       = j_hidden,
    J_SHOWN_BELOW      = if (j_hidden && lo > 0L) d$CUT_LABEL[[lo]] else
                           NA_character_,
    J_SHOWN_ABOVE      = if (j_hidden && hi <= nrow(d)) d$CUT_LABEL[[hi]] else
                           NA_character_,
    NO_OPTIMUM         = no_opt,
    AGREE_CUT          = as.numeric(at("CUT", a_i)),
    AGREE_CUT_LABEL    = as.character(at("CUT_LABEL", a_i)),
    AGREE              = as.numeric(at("AGREE", a_i)),
    AGREE_MAX_HIDDEN   = !is.na(a_i) && any_hidden,
    AGREE_FLOOR        = 1 - base_rate,
    OPTIMA_DIVERGE     = diverge,
    THRESHOLD          = thr$value,
    THRESHOLD_OP       = thr$op,
    THRESHOLD_LABEL    = thr$label,
    THRESHOLD_CUT      = as.numeric(at("CUT", thr_i)),
    THRESHOLD_SIDE     = side,
    RULE_IS_TERM       = !is.na(thr$value) && is_term &&
                           isTRUE(thr$conjunctive),
    RULE_IS_ROUTE      = !is.na(thr$value) && is_term &&
                           !isTRUE(thr$conjunctive) &&
                           isTRUE(thr$alternative),
    J_AT_THRESHOLD     = as.numeric(j_thr),
    AGREE_AT_THRESHOLD = as.numeric(a_thr),
    IMPLIED_COST_RATIO = ratio,
    COSTLIER_ERROR     = tol$costlier_error,
    LEAN               = if (is.na(j_i)) NA_character_ else
                           .check4_lean(tol$costlier_error, ratio,
                                        coincide = .check4_equal_at_j(
                                          j_i, agree_top, d, base_rate)),
    SUPPRESSED         = all_hidden)
}

# Whether equal per-student costs land exactly on the J optimum: the
# cost-minimizing cut under equal costs is agreement's maximum (or nobody
# ready, at the floor), and it coincides with the J optimum when the J
# optimum is among agreement's tied maxima, beats the floor, and no
# suppressed cut lies above it (for pi below one half every equal-cost
# optimum is at or above the J optimum, so only a hidden cut above it could
# hold a better one).
.check4_equal_at_j <- function(j_i, agree_top, d, base_rate) {
  !is.na(j_i) && isTRUE(agree_top[[j_i]]) &&
    !is.na(base_rate) && d$AGREE[[j_i]] > 1 - base_rate + .CHECK4_TOL &&
    !("SUPPRESSED" %in% names(d) && any(d$SUPPRESSED[-seq_len(j_i)]))
}

# ---- internal helpers --------------------------------------------------------

# One row per current EL in the panel with both scores: the academic table
# leads, roster status and the annual level are attached by key, so no row is
# ever added or duplicated. Check 3's join, restated (its builder is internal
# to its prefix); test-check4.R proves the two agree.
.check4_panel_rows <- function(data, spec, panel) {
  ac <- el_academic(data)
  ac <- ac[ac$SUBJECT %in% panel$subject &
             as.character(ac$GRADE) == panel$grade &
             ac$YEAR == panel$year, , drop = FALSE]

  key <- paste(ac$STUDENT_ID, ac$YEAR)
  roster <- el_roster(data)
  status <- roster$EL_STATUS[match(key, paste(roster$STUDENT_ID, roster$YEAR))]
  h <- elp_headline(data, spec)
  level <- h$ACHIEVEMENT_LEVEL[match(key, paste(h$STUDENT_ID, h$YEAR))]
  alternate <- if (.CHECK4_ALT %in% names(ac)) {
    !is.na(ac[[.CHECK4_ALT]])
  } else {
    rep(FALSE, nrow(ac))
  }

  paired <- status %in% "current" & !alternate & !is.na(ac$SCALE_SCORE) &
    !is.na(level)
  in_scale <- .check4_in_scale(level, spec)
  keep <- paired & in_scale

  out <- tibble::tibble(STUDENT_ID = ac$STUDENT_ID[keep],
                        YEAR = ac$YEAR[keep],
                        LEVEL = as.numeric(level[keep]),
                        SCALE_SCORE = ac$SCALE_SCORE[keep])
  attr(out, "n_out_of_scale") <- sum(paired & !in_scale)
  out
}

# The four cells at one cut, in TP / FP / FN / TN order. Ready means at or
# above the cut.
.check4_cells <- function(level, proficient, cut) {
  ready <- level >= cut - .CHECK4_TOL
  c(sum(ready & proficient), sum(ready & !proficient),
    sum(!ready & proficient), sum(!ready & !proficient))
}

# Which cuts can be reported, given the 4 x K matrix of cells in ascending cut
# order. A cut is reported when (1) none of its own cells is a small non-zero
# count, and (2) the slice between it and every neighbouring *published*
# cumulative point -- the last reported cut below it, and the next anchor
# above it -- holds 0 or at least `min_cell` proficient students and likewise
# not proficient. A cut failing (2) is merged into its neighbours.
#
# `anchors` are cut positions whose cumulative counts are published whatever
# this stage does: Check 3's band table gives, for the same students, each
# band's n and share at the cut, so the counts at or above every band break
# are known to anyone holding both documents. A reported cut next to one is
# checked against it as well as against the cuts Check 4 reports.
.check4_reportable <- function(cells, min_cell, anchors = integer()) {
  small <- function(x) x > 0 & x < min_cell
  step_bad <- function(a, b) {
    small(cells[1, a] - cells[1, b]) || small(cells[2, a] - cells[2, b])
  }
  n_cut <- ncol(cells)
  anchors <- sort(unique(anchors[anchors >= 1L & anchors <= n_cut]))
  keep <- !apply(cells, 2, function(cl) any(small(cl)))
  prev <- NA_integer_
  for (k in seq_len(n_cut)) {
    if (keep[[k]] && !is.na(prev) && step_bad(prev, k)) keep[[k]] <- FALSE
    # The lowest cut classifies everyone ready: its cells are the margins,
    # public anyway in every row's N and N_PROFICIENT, so blanking it would
    # protect nothing. Every other reported cut is checked against the next
    # published band break above it.
    if (keep[[k]] && k > 1L) {
      nxt <- anchors[anchors > k][1]
      if (!is.na(nxt) && step_bad(k, nxt)) keep[[k]] <- FALSE
    }
    # A published point -- reported here, or published by Check 3 -- is what
    # the next slice is measured from.
    if (keep[[k]] || k %in% anchors) prev <- k
  }
  keep
}

# The proficient / not-proficient split, summed over the bands whose share at
# the cut Check 3 does not show (0 < n < min_cell). Anyone with Check 4's
# margins and Check 3's shown shares can recover exactly this.
.check4_unshared_split <- function(level, proficient, spec, min_cell) {
  band <- assign_elp_band(level, spec)
  n_band <- table(band)
  thin <- names(n_band)[n_band > 0 & n_band < min_cell]
  in_thin <- band %in% thin
  c(sum(in_thin & proficient), sum(in_thin & !proficient))
}

# The cut positions whose cumulative counts Check 3's band table publishes:
# for each band break inside the observed range, the first cut at or above it.
# Below the lowest cut the counts are the margins, which every row already
# checks; above the highest they are zero, which the top row's own cells cover.
.check4_band_anchors <- function(cuts, spec) {
  bands <- spec_bands(spec)
  if (!length(cuts)) return(integer())
  # No declared bands means one band per level (assign_elp_band()'s default):
  # every observed level is its own published group.
  if (is.null(bands)) return(seq_along(cuts))
  idx <- vapply(bands$breaks, function(b) {
    hit <- which(cuts >= b - .CHECK4_TOL)
    if (length(hit)) hit[[1]] else NA_integer_
  }, integer(1))
  sort(unique(idx[!is.na(idx) & idx > 1L]))
}

# The academic proficiency cut: the minimum score of the level the spec calls
# proficient (`cuts` are the minima of levels 2..n). Check 3's definition,
# restated because its helper is internal to its prefix; a test asserts they
# agree.
.check4_cut <- function(spec, subject) {
  lvl <- spec$policy$academic_benchmark$proficient_level
  cuts <- spec$academic[[subject]]$cuts
  if (is.null(lvl) || length(lvl) != 1L || is.na(lvl) || is.null(cuts) ||
      lvl < 2L || lvl - 1L > length(cuts)) {
    return(NA_real_)
  }
  as.numeric(cuts[[lvl - 1L]])
}

# The declared level scale, in one shape for both kinds. A categorical scale is
# stored as an ordinal index into its levels (sim_contract.md 4.2).
.check4_scale <- function(spec) {
  ls <- spec$elp$level_scale
  if (identical(ls$type, "categorical")) {
    return(list(type = "categorical", lo = 1, hi = length(ls$levels),
                labels = ls$levels))
  }
  list(type = "numeric", lo = ls$range[[1]], hi = ls$range[[2]],
       labels = NULL)
}

.check4_in_scale <- function(level, spec) {
  sc <- .check4_scale(spec)
  ok <- !is.na(level) & level >= sc$lo - .CHECK4_TOL &
    level <= sc$hi + .CHECK4_TOL
  if (identical(sc$type, "categorical")) {
    ok <- ok & abs(level - round(level)) < .CHECK4_TOL
  }
  ok
}

# How a cut reads: the category's own name on a categorical scale, the level
# as delivered (at least one decimal) on a numeric one.
.check4_cut_label <- function(cut, spec) {
  sc <- .check4_scale(spec)
  if (identical(sc$type, "categorical")) {
    return(as.character(sc$labels[round(cut)]))
  }
  vapply(cut, function(x) {
    if (is.na(x)) NA_character_ else format(x, nsmall = 1, trim = TRUE)
  }, character(1))
}

# The current exit threshold on the reported component's own level, from the
# exit rule's structured terms. Unlike Check 3's band lookup, the term must be
# on the component the curves are drawn on: a domain floor is not a cut on
# this axis. Only ">=" and ">" make sense as an exit threshold on a level.
.check4_exit_threshold <- function(spec) {
  hl <- spec_headline(spec)
  sc <- .check4_scale(spec)
  found <- list()
  pathways <- spec$policy$exit$pathways
  for (i in seq_along(pathways)) {
    p <- pathways[[i]]
    for (tm in p$terms) {
      from <- tm$from %||% "elp"
      if (identical(from, "elp") && identical(tm$component, hl) &&
          identical(tm$measure, "level")) {
        # The pathway travels with its term: name and rule are optional, so a
        # second lookup by name could land on the wrong pathway.
        # Conjunctive: the pathway asks for this term *and* something else,
        # so students at or above the term can still be held by the other.
        found[[length(found) + 1L]] <- list(
          term = tm, index = i,
          pathway = p$name %||% NA_character_,
          rule = p$rule %||% NA_character_,
          discretionary = isTRUE(p$discretionary),
          conjunctive = length(p$terms) > 1L &&
            !identical(p$combine %||% "all", "any"),
          # Alternative: a student below the term can still exit some other
          # way -- another pathway, or another term of an "any" pathway.
          alternative = length(pathways) > 1L ||
            (length(p$terms) > 1L && identical(p$combine, "any")))
      }
    }
  }
  component <- spec_headline_label(spec)
  none <- list(value = NA_real_, op = NA_character_, label = NA_character_,
               pathway = NA_character_, rule = NA_character_,
               conjunctive = FALSE, alternative = FALSE)
  if (!length(found)) {
    none$note <- paste0(
      "The exit rule has no term on the ", component, " level itself -- ",
      "its criteria sit on other components -- so no current threshold is ",
      "marked on the curves.")
    return(none)
  }

  first <- which(!vapply(found, function(f) f$discretionary, TRUE))
  pick <- found[[if (length(first)) first[[1]] else 1L]]
  tm <- pick$term
  value <- tm$value
  if (is.character(value) && identical(sc$type, "categorical")) {
    value <- match(value, sc$labels)
  }
  op <- tm$op %||% ">="
  if (!is.numeric(value) || is.na(value) || !op %in% c(">=", ">")) {
    none$note <- paste0(
      "The exit rule's term on the ", component, " level (", op, " ",
      tm$value, ") cannot be read as a cut on this axis, so no current ",
      "threshold is marked on the curves.")
    return(none)
  }

  which_one <- if (!is.na(pick$pathway)) {
    paste0("the '", pick$pathway, "' pathway")
  } else if (!is.na(pick$rule)) {
    paste0("the pathway '", pick$rule, "'")
  } else {
    paste("pathway", pick$index)
  }
  note <- if (length(found) > 1L) {
    paste0("The exit rule carries ", length(found), " terms on the ",
           component, " level; the curves mark the one in ", which_one, ".")
  } else {
    NA_character_
  }

  # On a categorical scale the categories are whole positions, so a rule is
  # restated as ">=" the first category it admits: "> Progressing" is
  # ">= Proficient", and a fractional position rounds up. A rule no category
  # satisfies is not a cut on this axis.
  value <- as.numeric(value)
  if (identical(sc$type, "categorical")) {
    value <- if (identical(op, ">")) floor(value + .CHECK4_TOL) + 1 else
      ceiling(value - .CHECK4_TOL)
    op <- ">="
    if (value > sc$hi) {
      none$note <- paste0(
        "The exit rule's term on the ", component, " level (", tm$op, " ",
        tm$value, ") admits no category, so no current threshold is marked ",
        "on the curves.")
      return(none)
    }
  }
  list(value = value, op = op, label = .check4_cut_label(value, spec),
       pathway = pick$pathway, rule = pick$rule, note = note,
       conjunctive = isTRUE(pick$conjunctive),
       alternative = isTRUE(pick$alternative))
}

# Prep 3's elicited answer, read from the spec while Prep 3 is unbuilt (the
# instruction file allows it). Absent is the same as asked-and-unknown.
.check4_tolerance <- function(spec) {
  et <- spec$policy$error_tolerance
  list(costlier_error = et$costlier_error %||% "unknown",
       asked_of = et$asked_of %||% NA_character_,
       asked_date = et$asked_date %||% NA_character_,
       rationale = et$rationale %||% NA_character_)
}

# Which side of the J optimum a stakeholder answer licenses. A premature exit
# is a false "ready" (FP); a prolonged hold a false "not ready" (FN). The J
# optimum minimizes FP + ratio * FN, with ratio = (1 - pi) / pi: it already
# prices a hold at `ratio` exits. Prep 3's answer is per student, so it points
# above the optimum when it prices a hold below `ratio` exits and below when
# above. Without a magnitude, "prolonged retention is costlier" settles the
# side only when ratio <= 1; otherwise it depends on how much costlier.
.check4_lean <- function(costlier_error, ratio, coincide = FALSE) {
  if (is.null(costlier_error) || is.na(costlier_error) || is.na(ratio)) {
    return(NA_character_)
  }
  switch(costlier_error,
         premature_exit = if (ratio >= 1) "above" else NA_character_,
         prolonged_retention = if (ratio <= 1) "below" else NA_character_,
         equal = if (coincide || abs(ratio - 1) < .CHECK4_TOL) "at" else
           if (ratio > 1) "above" else "below",
         NA_character_)
}

# A numeric scale read from whole-number levels while the spec draws on a
# fraction: the same condition the SGP adapter warns on, restated for this
# stage's axis (the adapter's helper is internal to el_data.R).
.check4_warn_coarse <- function(cuts, spec) {
  sc <- .check4_scale(spec)
  if (!identical(sc$type, "numeric") || !length(cuts)) return(invisible(FALSE))
  frac <- function(x) any(abs(x - round(x)) > .CHECK4_TOL)
  whole <- !frac(cuts)
  bands <- spec_bands(spec)
  thr <- .check4_exit_threshold(spec)$value
  fractional_spec <- (!is.null(bands) && frac(bands$breaks)) ||
    (!is.na(thr) && frac(thr))
  if (!(whole && fractional_spec)) return(invisible(FALSE))

  msg <- paste0(
    "Every delivered ", spec_headline_label(spec), " level in this panel is ",
    "a whole number, but the spec draws on a fractional break (a band ",
    "boundary or the exit threshold). The curves have only ", length(cuts),
    " points, and a cut between whole levels -- the current rule's ",
    "included -- classifies exactly as the next whole level above it. The ",
    "raw file's decimal level is needed for a finer curve.")
  warning(structure(class = c("check4_coarse_levels", "warning", "condition"),
                    list(message = msg, call = NULL)))
  invisible(TRUE)
}
