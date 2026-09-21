# Check 2 -- figures. Each takes the matching compute_*() output (a slot of
# run_check2()) and returns a ggplot without recomputing (CLAUDE.md 8). No
# student-level data is ever in scope here.
#
# Suppressed cells are NA in the slot and are simply not drawn.
#
# The palette matches Prep 2's and Check 1's (R/prep2_plot.R, R/check1_plot.R).
# Each stage holds its own copy because a stage session may not edit another
# prefix or R/utils.R; a shared palette helper is `[PROPOSED - CONFIRM]`.

.C2_GREY <- "grey60"

# One colour per exit group, fixed so the same group is the same colour in
# every figure: on-time dark, prolonged mid, disappeared grey (it is an
# unobserved outcome, not a third result).
.c2_group_colours <- function() {
  stats::setNames(c("#08519C", "#F16913", .C2_GREY),
                  .check2_group_labels(.CHECK2_GROUPS))
}

.c2_theme <- function() ggplot2::theme_minimal(base_size = 11)
.c2_label_pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")

.c2_stop_slot <- function(x, cols, what) {
  if (!is.data.frame(x) || !all(cols %in% names(x))) {
    stop(what, " needs a data frame with ", paste(cols, collapse = ", "),
         "; pass the matching slot of run_check2().", call. = FALSE)
  }
}

# Coded group values become their display labels for the legend, keeping the
# stage's display order. The labels come from inst/lookups/Display_Labels.R
# (see R/check2_render.R), so no legend text is typed here.
.c2_group_factor <- function(x) {
  keys <- intersect(.CHECK2_GROUPS, unique(x))
  factor(.check2_group_labels(x), levels = .check2_group_labels(keys))
}

#' Academic achievement by exit group
#'
#' Plots [compute_academic_summary()]'s output: the content-proficiency rate of
#' each exit group at each year in status. A group with no students left to
#' measure -- an on-time group that has exited and left the monitoring window --
#' simply has no point there, which is the honest rendering of its absence.
#'
#' @param x `$academic_summary` from [run_check2()].
#' @param spec An `el_spec`, optional; supplies the on-time timeline marker.
#' @return A `ggplot`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                      academic = academic_long_sim, spec = spec)
#' res <- run_check2(bundle, spec)
#' plot_academic_summary(res$academic_summary, spec)
plot_academic_summary <- function(x, spec = NULL) {
  .c2_stop_slot(x, c("EXIT_GROUP", "YEAR_IN_STATUS", "PCT_PROFICIENT"),
                "plot_academic_summary()")
  stopifnot(is.null(spec) || inherits(spec, "el_spec"))
  d <- x
  d$EXIT_GROUP <- .c2_group_factor(d$EXIT_GROUP)

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$YEAR_IN_STATUS,
                                       .data$PCT_PROFICIENT,
                                       colour = .data$EXIT_GROUP))
  p <- .c2_add_ontime_line(p, spec, d$YEAR_IN_STATUS)
  p +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(size = 1.4, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = .c2_group_colours(), drop = FALSE) +
    ggplot2::scale_y_continuous(labels = .c2_label_pct, limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$YEAR_IN_STATUS))) +
    ggplot2::labs(x = "Year in status", y = "At or above the proficiency cut",
                  colour = NULL) +
    .c2_theme()
}

#' ELP trajectories by exit group
#'
#' Plots [compute_trajectories()]'s `levels`: the reported ELP level by year in
#' status, one line per exit group. There is deliberately **no pooled line**.
#' A single cohort mean over time would fall for two reasons it cannot separate
#' -- students growing less, and the fastest-growing students leaving -- and
#' drawing it is the slide-81 error this view exists to prevent. The function
#' refuses a frame without `EXIT_GROUP` for that reason.
#'
#' @param x `$trajectories$levels` from [run_check2()].
#' @param spec An `el_spec`, optional; supplies the on-time timeline marker.
#' @param measure `"mean"` or `"median"` level.
#' @return A `ggplot`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' res <- run_check2(bundle, spec)
#' plot_trajectories(res$trajectories$levels, spec)
plot_trajectories <- function(x, spec = NULL, measure = c("mean", "median")) {
  measure <- match.arg(measure)
  col <- if (measure == "mean") "MEAN_LEVEL" else "MEDIAN_LEVEL"
  # EXIT_GROUP is required, not optional: see the note above.
  .c2_stop_slot(x, c("EXIT_GROUP", "YEAR_IN_STATUS", col),
                "plot_trajectories()")
  stopifnot(is.null(spec) || inherits(spec, "el_spec"))
  d <- x
  d$EXIT_GROUP <- .c2_group_factor(d$EXIT_GROUP)

  p <- ggplot2::ggplot(d, ggplot2::aes(.data$YEAR_IN_STATUS, .data[[col]],
                                       colour = .data$EXIT_GROUP))
  p <- .c2_add_ontime_line(p, spec, d$YEAR_IN_STATUS)
  p +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(size = 1.4, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = .c2_group_colours(), drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$YEAR_IN_STATUS))) +
    ggplot2::labs(x = "Year in status",
                  y = paste(if (measure == "mean") "Mean" else "Median",
                            "reported ELP level"),
                  colour = NULL) +
    .c2_theme()
}

# The on-time timeline, drawn before the data so the lines sit on top. It is
# where the two groups are defined apart, so every trajectory figure marks it.
.c2_add_ontime_line <- function(p, spec, years) {
  if (is.null(spec)) return(p)
  yrs <- spec$policy$timelines$on_time_rule$default_years %||%
    spec_essa_years(spec)
  if (is.null(yrs) || is.na(yrs) || yrs >= max(years, na.rm = TRUE)) return(p)
  p +
    ggplot2::geom_vline(xintercept = yrs, linetype = "dashed",
                        colour = .C2_GREY) +
    ggplot2::annotate("text", x = yrs, y = Inf, hjust = -0.05, vjust = 1.4,
                      size = 3, colour = .C2_GREY,
                      label = sprintf("on-time timeline: %d years", yrs))
}
