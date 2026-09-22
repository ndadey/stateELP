# Check 3 -- figures. Takes the matching compute_*() output and returns a
# ggplot without recomputing (CLAUDE.md 8). Only summary statistics arrive
# here; no student-level data is ever in scope.
#
# The palette matches Prep 2's and Check 1's. The three stages hold their own
# copies because a stage session may not edit another prefix or R/utils.R; a
# shared palette helper is `[PROPOSED - CONFIRM]`.

.C3_BLUE <- "#2C7FB8"
.C3_GREY <- "grey60"
.C3_CUT <- "#C0392B"     # the slide's red reference line

.c3_theme <- function() ggplot2::theme_minimal(base_size = 11)

#' The slide-73 box plot for one panel
#'
#' Academic scale score by ELP band, with former ELs and the English-only
#' group as reference columns, and the proficiency cut drawn as a labeled red
#' line. The interpretive question the figure exists for: at what ELP level
#' does the distribution reach the cut, and how does that sit against the
#' exit threshold?
#'
#' Boxes are drawn from [compute_box_data()]'s precomputed five numbers
#' (`stat = "identity"`), so the figure and any table of the same panel cannot
#' disagree, and a suppressed group leaves a visible gap on the axis rather
#' than silently closing it up.
#'
#' **The band axis is not a proficiency gradient across a stable population.**
#' Its top end is constituted by the exit rule: a student observed as a
#' current EL at or above the exit threshold is, in most states, in their
#' final year. The caption in the stage document says so; see
#' `internal_docs/NOTES-check3.md`.
#'
#' @param x Output of [compute_box_data()], or `$box_data` from
#'   [run_check3()], which carries every panel.
#' @param spec An `el_spec`, optional. Supplies the band display order and the
#'   cut when `x` has lost its attributes (e.g. after a round-trip through
#'   RDS).
#' @param subject,grade Which panel to draw when `x` holds more than one.
#'   Defaults to the first, which [run_check3()] orders to be the focal panel.
#'   A selector matching no panel is an error rather than a silent fallback to
#'   a different grade.
#' @return A `ggplot`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' plot_box_panel(compute_box_data(b, spec), spec)
plot_box_panel <- function(x, spec = NULL, subject = NULL, grade = NULL) {
  need <- c("GROUP", "Y_MIN", "Y_P25", "Y_MEDIAN", "Y_P75", "Y_MAX")
  if (!is.data.frame(x) || !all(need %in% names(x))) {
    stop("plot_box_panel() needs a data frame with ",
         paste(need, collapse = ", "), "; pass $box_data from run_check3().",
         call. = FALSE)
  }
  stopifnot(is.null(spec) || inherits(spec, "el_spec"))

  # `$box_data` holds every panel. A figure is one of them, so the selector
  # defaults to the first -- which run_check3() orders to be the focal panel.
  x <- .c3_one_panel(x, subject, grade)

  cut <- attr(x, "cut")
  if ((is.null(cut) || is.na(cut)) && !is.null(spec)) {
    panel <- attr(x, "panel")
    subj <- if (!is.null(panel)) panel$subject else
      if ("SUBJECT" %in% names(x)) x$SUBJECT[[1]] else NULL
    if (!is.null(subj)) cut <- .check3_cut(spec, subj)
  }

  d <- x[!is.na(x$Y_MEDIAN), , drop = FALSE]
  levels <- if (!is.null(spec)) .check3_group_levels(spec) else x$GROUP

  # A group with students but a suppressed box keeps its place, so the gap is
  # visible; a group nobody is in leaves the axis entirely, because an empty
  # column invites a reader to wonder who is hiding in it.
  if ("N" %in% names(x)) levels <- intersect(levels, x$GROUP[x$N > 0L])
  d$GROUP <- factor(d$GROUP, levels = levels)

  # The reference groups are not bands, and colouring them the same invites
  # reading the axis as one continuous scale.
  d$KIND <- ifelse(d$GROUP %in% c(.CHECK3_FORMER, .CHECK3_NEVER),
                   "reference", "band")

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$GROUP))

  if (!is.null(cut) && !is.na(cut)) {
    p <- p +
      ggplot2::geom_hline(yintercept = cut, colour = .C3_CUT,
                          linetype = "dashed") +
      ggplot2::annotate("text", x = Inf, y = cut, hjust = 1.02, vjust = -0.6,
                        size = 3, colour = .C3_CUT,
                        label = paste("proficiency cut:", round(cut)))
  }

  p +
    ggplot2::geom_boxplot(
      ggplot2::aes(ymin = .data$Y_MIN, lower = .data$Y_P25,
                   middle = .data$Y_MEDIAN, upper = .data$Y_P75,
                   ymax = .data$Y_MAX, fill = .data$KIND),
      stat = "identity", width = 0.6, colour = "grey30", na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = .data$Y_MEAN), shape = 18, size = 2.4,
                        colour = "grey20", na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c(band = .C3_BLUE,
                                          reference = .C3_GREY),
                               guide = "none") +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = NULL, y = "Academic scale score") +
    .c3_theme()
}

# One panel out of a long, keyed box table. Without keys the frame already is
# one panel and comes back untouched, which is what a hand-built fixture or a
# single compute_box_data() call produces.
.c3_one_panel <- function(x, subject = NULL, grade = NULL) {
  if (!all(c("SUBJECT", "GRADE") %in% names(x))) return(x)
  keep <- rep(TRUE, nrow(x))
  if (!is.null(subject)) keep <- keep & x$SUBJECT %in% subject
  if (!is.null(grade)) keep <- keep & as.character(x$GRADE) %in%
    as.character(grade)
  if (!any(keep)) {
    stop("No panel in this box data for ",
         if (is.null(subject)) "" else paste0("subject ", subject, " "),
         if (is.null(grade)) "" else paste0("grade ", grade),
         ".", call. = FALSE)
  }
  x <- x[keep, , drop = FALSE]
  # Still more than one panel: take the first, which run_check3() has ordered
  # to be the focal one. Drawing several on one axis would silently pool them.
  first <- x$SUBJECT[[1]]
  first_g <- as.character(x$GRADE[[1]])
  x[x$SUBJECT == first & as.character(x$GRADE) == first_g, , drop = FALSE]
}

#' Where each grade lands, read together
#'
#' The stage's across-grade view: one point per grade for the first ELP band
#' that reaches the proficiency cut, with grades where no band reaches it
#' shown on their own line at the top. Convergence across grades corroborates
#' a landing; scatter says the answer is grade-dependent and has to be read
#' against Prep 2's composition and Check 1's attrition.
#'
#' Points at or above the band the exit threshold falls in are marked: those
#' bands are constituted by the exit rule, so a landing there rests on a
#' population the rule itself selects.
#'
#' @param x `$landing` from [run_check3()].
#' @param spec An `el_spec`, for the band order.
#' @return A `ggplot`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' plot_landing(run_check3(b, spec)$landing, spec)
plot_landing <- function(x, spec) {
  if (!is.data.frame(x) || !all(c("GRADE", "LANDING_BAND") %in% names(x))) {
    stop("plot_landing() needs $landing from run_check3().", call. = FALSE)
  }
  stopifnot(inherits(spec, "el_spec"))

  none <- "No band reached half"
  bands <- c(spec_bands(spec)$labels, none)
  d <- x
  d$BAND <- ifelse(is.na(d$LANDING_BAND), none, d$LANDING_BAND)
  d$BAND <- factor(d$BAND, levels = bands)
  d$GRADE <- factor(as.character(d$GRADE),
                    levels = unique(as.character(d$GRADE))[
                      order(suppressWarnings(as.numeric(
                        unique(as.character(d$GRADE)))))])
  d$AT_EXIT <- ifelse(d$LANDING_AT_OR_ABOVE_EXIT %in% TRUE,
                      "at or above the exit band", "below the exit band")

  ggplot2::ggplot(d, ggplot2::aes(x = .data$GRADE, y = .data$BAND)) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$AT_EXIT), size = 2.6,
                        colour = .C3_BLUE, na.rm = TRUE) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::scale_shape_manual(values = c("below the exit band" = 16,
                                           "at or above the exit band" = 17)) +
    ggplot2::facet_wrap(~ SUBJECT) +
    ggplot2::labs(x = "Grade", y = NULL, shape = NULL) +
    .c3_theme() +
    ggplot2::theme(legend.position = "bottom")
}
