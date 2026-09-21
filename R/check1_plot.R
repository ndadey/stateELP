# Check 1 -- figures. Each takes the matching compute_*() output (a slot of
# run_check1()) and returns a ggplot without recomputing (CLAUDE.md 8). No
# student-level data is ever in scope here.
#
# Suppressed cells are NA in the slot and are simply not drawn.
#
# The palette matches Prep 2's (R/prep2_plot.R). The two stages hold their own
# copies because a stage session may not edit another prefix or R/utils.R; a
# shared palette helper is `[PROPOSED - CONFIRM]`.

.C1_BLUE <- "#2C7FB8"
.C1_GREY <- "grey60"

.c1_theme <- function() ggplot2::theme_minimal(base_size = 11)
.c1_label_pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")

.c1_stop_slot <- function(x, cols, what) {
  if (!is.data.frame(x) || !all(cols %in% names(x))) {
    stop(what, " needs a data frame with ", paste(cols, collapse = ", "),
         "; pass the matching slot of run_check1().", call. = FALSE)
  }
}

#' The cumulative exit curve
#'
#' Plots [compute_exit_curve()]'s output: the share of the entry cohort
#' observed to have exited by each year in status. One line when the slot
#' holds the whole cohort, one line per group when it is split by initial ELP
#' band.
#'
#' The curve is a lower bound -- censored students are not exiters -- and the
#' caption in the stage document says so. When a spec is given, the maximum
#' timeline is marked, because "exited within the timeline" is read off this
#' curve at that year.
#'
#' @param x `$exit_curve` or `$exit_curve_by_initial_band` from
#'   [run_check1()].
#' @param spec An `el_spec`, optional. Supplies the maximum-timeline marker
#'   and the band display order.
#' @return A `ggplot`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' res <- run_check1(bundle, spec)
#' plot_exit_curve(res$exit_curve, spec)
plot_exit_curve <- function(x, spec = NULL) {
  .c1_stop_slot(x, c("GROUP", "YEAR_IN_STATUS", "CUM_EXIT_RATE"),
                "plot_exit_curve()")
  stopifnot(is.null(spec) || inherits(spec, "el_spec"))

  groups <- unique(x$GROUP)
  split <- length(groups) > 1L
  d <- x
  d$GROUP <- factor(d$GROUP, levels = if (is.null(spec)) groups
                                      else .check1_group_order(groups, spec))

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$YEAR_IN_STATUS,
                                       .data$CUM_EXIT_RATE))

  # The maximum timeline, drawn before the data so the lines sit on top.
  max_years <- if (is.null(spec)) NA_integer_ else spec_max_years(spec)
  if (!is.na(max_years) && max_years <= max(d$YEAR_IN_STATUS)) {
    p <- p +
      ggplot2::geom_vline(xintercept = max_years, linetype = "dashed",
                          colour = .C1_GREY) +
      ggplot2::annotate("text", x = max_years, y = 0, hjust = -0.1, vjust = 0,
                        size = 3, colour = .C1_GREY,
                        label = sprintf("maximum timeline: %d years", max_years))
  }

  p <- if (split) {
    p + ggplot2::geom_line(ggplot2::aes(colour = .data$GROUP), na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$GROUP), size = 1,
                          na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = .c1_group_colours(levels(d$GROUP)),
                                   drop = FALSE)
  } else {
    p + ggplot2::geom_line(colour = .C1_BLUE, na.rm = TRUE) +
      ggplot2::geom_point(colour = .C1_BLUE, size = 1, na.rm = TRUE)
  }

  p +
    ggplot2::scale_y_continuous(labels = .c1_label_pct, limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$YEAR_IN_STATUS))) +
    ggplot2::labs(x = "Year in status", y = "Exited, cumulative",
                  colour = if (split) "Initial ELP band" else NULL) +
    .c1_theme()
}

# Light-to-dark for the ordered bands; grey for the group that has no band,
# so it reads as "not on this scale" rather than as an extra band.
.c1_group_colours <- function(levels) {
  banded <- setdiff(levels, .CHECK1_NO_BAND)
  cols <- grDevices::colorRampPalette(c("#A6BDDB", "#08519C"))(max(length(banded), 1L))
  stats::setNames(c(cols[seq_along(banded)], .C1_GREY),
                  c(banded, .CHECK1_NO_BAND))
}
