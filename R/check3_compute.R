# Check 3 -- Academic patterns. Plain data out, no plotting, no I/O.
#
# The sample is NOT Checks 1-2's cohort. It is cross-sectional: all ELs in one
# grade and year, beside two reference groups. Each grade is its own panel and
# grades are never pooled, so every function here is keyed by grade x subject
# x year and returns one panel at a time.

# The x-axis of the slide-73 box plot, in display order: the ELP bands, then
# the two reference groups. Former and never-EL students have no ELP rows at
# all (contract 4.4), so they cannot be banded and are their own columns.
.CHECK3_FORMER <- "Former ELs"
.CHECK3_NEVER <- "English-only"

# Current ELs with no annual headline that year -- untested, or on the
# alternate form, whose levels are a different scale. They have an academic
# score and belong to the grade, so dropping them would quietly shrink the
# panel; they get their own column instead, exactly as Check 1's band-split
# curve does. The wording matches, so a reader meets the same group twice
# under the same name.
.CHECK3_NO_BAND <- "No annual ELP score"

# Students on an alternate academic assessment are counted -- they are part of
# the population -- but their scores are on a different scale and never enter
# a distribution (instruction file, slide-71 rules).
.CHECK3_ALT <- "ACHIEVEMENT_LEVEL_ALT"

#' The panel Check 3 renders
#'
#' Which grade, subject and year the stage's figure is drawn for. All three
#' are **derived, never chosen**: the first subject the spec's `academic`
#' block declares, the first grade that subject is tested in, and the most
#' recent year the delivery actually carries academic rows for that pairing.
#'
#' The shell renders one panel. Picking it by hand would hard-code a grade
#' that is wrong for the next state, and picking "the grade with the most
#' data" would move the figure whenever a delivery changed. Deriving it from
#' the spec's own declaration keeps it stable and correct for any state, and
#' the later pass that loops every grade reuses the same accessors.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param subject,grade,year Optional overrides, each defaulting to the
#'   derivation above. The later per-grade pass supplies them.
#' @return A list: `subject`, `grade`, `year`, and `years` (every year the
#'   pairing is available, so the document can say what was not drawn).
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_panel(b, spec)
compute_panel <- function(data, spec, subject = NULL, grade = NULL,
                          year = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  ac <- el_academic(data)
  if (is.null(ac)) {
    stop("Check 3 needs the academic table; without it every slot is NULL.",
         call. = FALSE)
  }

  subjects <- spec_academic_subjects(spec)
  if (is.null(subject)) subject <- subjects[[1]]
  if (!subject %in% subjects) {
    stop("Subject '", subject, "' is not in the spec's academic block.",
         call. = FALSE)
  }

  # The spec says which grades the subject is tested in, not the data. A
  # delivery missing a grade is a coverage fact the coverage table reports;
  # it must not silently redefine the analysis.
  grades <- spec_academic_grades(spec, subject)
  if (is.null(grades)) {
    # An EOC subject carries its tested grade on the row instead.
    grades <- sort(unique(as.character(ac$GRADE[ac$SUBJECT %in% subject])))
  }
  if (is.null(grade)) grade <- grades[[1]]
  if (!as.character(grade) %in% as.character(grades)) {
    stop("Grade '", grade, "' is not a tested grade for ", subject,
         " in this spec.", call. = FALSE)
  }

  rows <- ac[ac$SUBJECT %in% subject &
               as.character(ac$GRADE) == as.character(grade), , drop = FALSE]
  years <- sort(unique(rows$YEAR))
  if (is.null(year)) year <- if (length(years)) max(years) else NA_integer_

  list(subject = subject, grade = as.character(grade),
       year = as.integer(year), years = as.integer(years))
}

#' Box-plot statistics for one grade x subject x year panel
#'
#' The slide-73 figure's data: one row per x-axis group -- each ELP band, then
#' former ELs, then the English-only reference group -- carrying the five
#' numbers a box is drawn from plus the mean and the counts.
#'
#' **Summary statistics, not student rows.** The plot function receives only
#' this, so no student-level data is ever in scope where a figure is built
#' (CLAUDE.md 8). It also makes suppression meaningful: a group below the
#' spec's `min_cell` keeps its row with `SUPPRESSED = TRUE` and blanked
#' statistics, rather than vanishing from the axis as though it had no
#' students.
#'
#' **The universe is the academic table left-joined to the roster**, never the
#' reverse: former and never-EL students have no ELP rows at all, so starting
#' from the ELP side would drop both reference groups silently. A test guards
#' the direction.
#'
#' **Who is counted, and who is plotted.** Students on an alternate academic
#' assessment are in `N` -- they are part of the grade -- but their scores are
#' on a different scale and are excluded from the distribution, so `N_PLOTTED`
#' is the number the box is actually drawn from. Never-EL students are a 10%
#' sample, so their `N_WEIGHTED` is reported and the unweighted count is never
#' presented as a population figure.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param panel Output of [compute_panel()], or `NULL` to derive it.
#' @return A tibble, one row per group: `GROUP`, `N`, `N_PLOTTED`,
#'   `N_WEIGHTED`, `N_ALTERNATE`, `Y_MIN`, `Y_P25`, `Y_MEDIAN`, `Y_MEAN`,
#'   `Y_P75`, `Y_MAX`, `SUPPRESSED`; `NULL` when the panel's year carries no
#'   academic rows at all.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_box_data(b, spec)
compute_box_data <- function(data, spec, panel = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  if (is.null(panel)) panel <- compute_panel(data, spec)
  if (is.na(panel$year)) return(NULL)   # [no academic year for this panel]

  rows <- .check3_panel_rows(data, spec, panel)
  if (!nrow(rows)) return(NULL)         # [a missing year, not an empty one]

  levels <- .check3_group_levels(spec)
  rows$GROUP <- factor(rows$GROUP, levels = levels)

  out <- rows |>
    dplyr::group_by(.data$GROUP, .drop = FALSE) |>
    dplyr::summarise(
      N           = dplyr::n(),
      N_ALTERNATE = sum(.data$ALTERNATE),
      N_PLOTTED   = sum(!.data$ALTERNATE & !is.na(.data$SCALE_SCORE)),
      N_WEIGHTED  = sum(.data$SAMPLE_WEIGHT),
      Y_MIN       = .check3_q(.data$SCALE_SCORE[!.data$ALTERNATE], 0),
      Y_P25       = .check3_q(.data$SCALE_SCORE[!.data$ALTERNATE], 0.25),
      Y_MEDIAN    = .check3_q(.data$SCALE_SCORE[!.data$ALTERNATE], 0.5),
      Y_MEAN      = .check3_mean(.data$SCALE_SCORE[!.data$ALTERNATE]),
      Y_P75       = .check3_q(.data$SCALE_SCORE[!.data$ALTERNATE], 0.75),
      Y_MAX       = .check3_q(.data$SCALE_SCORE[!.data$ALTERNATE], 1),
      .groups = "drop")
  out$GROUP <- as.character(out$GROUP)

  # Suppression runs on the number the box is drawn from, not on N: a group
  # of 40 students with 3 scores discloses those 3.
  out <- apply_suppression(
    out, spec, n_col = "N_PLOTTED",
    cols = c("Y_MIN", "Y_P25", "Y_MEDIAN", "Y_MEAN", "Y_P75", "Y_MAX"))

  # A group nobody is in is not suppressed, it is empty, and those read
  # differently: "too few to show" invites a reader to imagine students
  # behind the gap. The same rule Check 1 applies to a zero outcome.
  out$SUPPRESSED[out$N == 0L] <- FALSE

  attr(out, "panel") <- panel
  attr(out, "cut") <- .check3_cut(spec, panel$subject)
  out
}

# One row per student in the panel, with the group they sit in. The academic
# table leads the join (instruction file); roster status is attached to it.
.check3_panel_rows <- function(data, spec, panel) {
  ac <- el_academic(data)
  ac <- ac[ac$SUBJECT %in% panel$subject &
             as.character(ac$GRADE) == panel$grade &
             ac$YEAR == panel$year, , drop = FALSE]
  if (!nrow(ac)) return(ac[0, , drop = FALSE])

  roster <- el_roster(data)
  at <- match(paste(ac$STUDENT_ID, ac$YEAR),
              paste(roster$STUDENT_ID, roster$YEAR))
  ac$EL_STATUS <- roster$EL_STATUS[at]
  ac$SAMPLE_WEIGHT <- if ("SAMPLE_WEIGHT" %in% names(roster)) {
    ifelse(is.na(roster$SAMPLE_WEIGHT[at]), 1, roster$SAMPLE_WEIGHT[at])
  } else 1

  h <- elp_headline(data, spec)
  ac$ELP_BAND <- h$ELP_BAND[match(paste(ac$STUDENT_ID, ac$YEAR),
                                  paste(h$STUDENT_ID, h$YEAR))]
  ac$ALTERNATE <- if (.CHECK3_ALT %in% names(ac)) {
    !is.na(ac[[.CHECK3_ALT]])
  } else FALSE

  # Current ELs sit in their band; the two reference groups are their own
  # columns because they have no ELP rows to band (contract 4.4). A current
  # EL with no annual score that year is not dropped -- it gets the no-band
  # column, so the panel keeps every academic row it started with.
  ac$GROUP <- ifelse(ac$EL_STATUS %in% "never", .CHECK3_NEVER,
                     ifelse(ac$EL_STATUS %in% "former", .CHECK3_FORMER,
                            ifelse(is.na(ac$ELP_BAND), .CHECK3_NO_BAND,
                                   ac$ELP_BAND)))
  # What remains unassigned is a student with an academic score and no roster
  # row for that year, which is a delivery integrity problem rather than a
  # group. The coverage table is where that shows up.
  ac[!is.na(ac$EL_STATUS), , drop = FALSE]
}

# Display order: the spec's own band labels, the no-band column, then the two
# reference groups. Taken from the spec so a non-WIDA instrument's bands order
# correctly and no band label is ever typed here.
.check3_group_levels <- function(spec) {
  c(spec_bands(spec)$labels, .CHECK3_NO_BAND, .CHECK3_FORMER, .CHECK3_NEVER)
}

# The proficiency cut: the minimum score of the level the spec calls
# proficient. `cuts` are the minima of levels 2..n, so level k is cuts[k - 1].
.check3_cut <- function(spec, subject) {
  lvl <- spec$policy$academic_benchmark$proficient_level
  cuts <- spec$academic[[subject]]$cuts
  if (is.null(lvl) || is.null(cuts) || lvl < 2L || lvl - 1L > length(cuts)) {
    return(NA_real_)
  }
  as.numeric(cuts[[lvl - 1L]])
}

.check3_q <- function(x, p) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, probs = p, names = FALSE, type = 7))
}

.check3_mean <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}
