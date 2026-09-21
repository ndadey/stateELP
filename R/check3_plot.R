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
#' @param x Output of [compute_box_data()].
#' @param spec An `el_spec`, optional. Supplies the band display order and the
#'   cut when `x` has lost its attributes (e.g. after a round-trip through
#'   RDS).
#' @return A `ggplot`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' plot_box_panel(compute_box_data(b, spec), spec)
plot_box_panel <- function(x, spec = NULL) {
  need <- c("GROUP", "Y_MIN", "Y_P25", "Y_MEDIAN", "Y_P75", "Y_MAX")
  if (!is.data.frame(x) || !all(need %in% names(x))) {
    stop("plot_box_panel() needs a data frame with ",
         paste(need, collapse = ", "), "; pass $box_data from run_check3().",
         call. = FALSE)
  }
  stopifnot(is.null(spec) || inherits(spec, "el_spec"))

  cut <- attr(x, "cut")
  if ((is.null(cut) || is.na(cut)) && !is.null(spec)) {
    panel <- attr(x, "panel")
    if (!is.null(panel)) cut <- .check3_cut(spec, panel$subject)
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
