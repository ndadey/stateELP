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
#' Passing `survival_est` draws the at-risk estimate and its confidence band
#' alongside the observed curve, which is the honest way to show what
#' censoring costs: the two answer different questions, so they are labeled
#' and never merged. It applies to the whole-cohort curve only. The estimate
#' is computed for the cohort as a whole, and drawing one band behind several
#' band-specific curves would attach it to each of them.
#'
#' @param x `$exit_curve` or `$exit_curve_by_initial_band` from
#'   [run_check1()].
#' @param spec An `el_spec`, optional. Supplies the maximum-timeline marker
#'   and the band display order.
#' @param survival_est `$survival_est` from [run_check1()], optional. Ignored
#'   when `x` is split by group.
#' @return A `ggplot`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' res <- run_check1(bundle, spec)
#' plot_exit_curve(res$exit_curve, spec)
#' plot_exit_curve(res$exit_curve, spec, survival_est = res$survival_est)
plot_exit_curve <- function(x, spec = NULL, survival_est = NULL) {
  .c1_stop_slot(x, c("GROUP", "YEAR_IN_STATUS", "CUM_EXIT_RATE"),
                "plot_exit_curve()")
  stopifnot(is.null(spec) || inherits(spec, "el_spec"))
  if (!is.null(survival_est)) {
    .c1_stop_slot(survival_est,
                  c("YEAR_IN_STATUS", "CUM_EXIT_EST", "CI_LO", "CI_HI"),
                  "plot_exit_curve(survival_est = )")
  }

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

  # The estimate goes underneath: it sits above the observed curve by
  # construction, and drawing it first keeps the observed line -- what
  # actually happened -- reading as the foreground.
  est <- if (!split && !is.null(survival_est)) {
    .c1_est_frame(survival_est)
  } else NULL
  if (!is.null(est) && nrow(est)) {
    p <- p +
      ggplot2::geom_ribbon(
        data = est,
        ggplot2::aes(x = .data$YEAR_IN_STATUS, ymin = .data$CI_LO,
                     ymax = .data$CI_HI),
        inherit.aes = FALSE, fill = .C1_GREY, alpha = 0.18) +
      ggplot2::geom_line(
        data = est,
        ggplot2::aes(x = .data$YEAR_IN_STATUS, y = .data$CUM_EXIT_EST,
                     linetype = "At-risk estimate (95% CI)"),
        inherit.aes = FALSE, colour = .C1_GREY, na.rm = TRUE)
  }

  p <- if (split) {
    p + ggplot2::geom_line(ggplot2::aes(colour = .data$GROUP), na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$GROUP), size = 1,
                          na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = .c1_group_colours(levels(d$GROUP)),
                                   drop = FALSE)
  } else {
    p + ggplot2::geom_line(ggplot2::aes(linetype = "Observed, against the starting cohort"),
                           colour = .C1_BLUE, na.rm = TRUE) +
      ggplot2::geom_point(colour = .C1_BLUE, size = 1, na.rm = TRUE)
  }

  # One line and no legend reads better than one line labeled "observed";
  # the legend earns its place only once there are two series to tell apart.
  p <- p + if (!is.null(est) && nrow(est)) {
    ggplot2::scale_linetype_manual(
      values = stats::setNames(c("solid", "dashed"),
                               c("Observed, against the starting cohort",
                                 "At-risk estimate (95% CI)")))
  } else {
    ggplot2::guides(linetype = "none")
  }

  p +
    ggplot2::scale_y_continuous(labels = .c1_label_pct, limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$YEAR_IN_STATUS))) +
    ggplot2::labs(x = "Year in status", y = "Exited, cumulative",
                  colour = if (split) "Initial ELP band" else NULL,
                  linetype = NULL) +
    .c1_theme() +
    ggplot2::theme(legend.position = if (split || is.null(est)) "right"
                   else "bottom")
}

# Rows whose band is missing -- suppressed, or past the point where the risk
# set ran out -- would break the ribbon into pieces at exactly the years a
# reader is least entitled to a confident line, so they are dropped and the
# ribbon simply stops.
.c1_est_frame <- function(survival_est) {
  keep <- !is.na(survival_est$CUM_EXIT_EST) & !is.na(survival_est$CI_LO) &
    !is.na(survival_est$CI_HI)
  survival_est[keep, c("YEAR_IN_STATUS", "CUM_EXIT_EST", "CI_LO", "CI_HI"),
               drop = FALSE]
}

# Light-to-dark for the ordered bands; grey for the group that has no band,
# so it reads as "not on this scale" rather than as an extra band.
.c1_group_colours <- function(levels) {
  banded <- setdiff(levels, .CHECK1_NO_BAND)
  cols <- grDevices::colorRampPalette(c("#A6BDDB", "#08519C"))(max(length(banded), 1L))
  stats::setNames(c(cols[seq_along(banded)], .C1_GREY),
                  c(banded, .CHECK1_NO_BAND))
}
