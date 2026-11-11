# Prep 2 -- figures for the context deck (decks/context.qmd). Each takes the
# matching compute_*() output and returns a ggplot without recomputing
# (CLAUDE.md 8). Styled for slides -- large type, value labels on the bars,
# the Center for Assessment green and grey -- rather than for the stage report, whose
# figures are in prep2_plot.R. A suppressed (NA) value is drawn as no bar and
# labelled "*".

.P2C_GREEN <- "#00A84F"
.P2C_GREY <- "#B5B5B5"
.P2C_DARKGREY <- "#8C8C8C"
.P2C_INK <- "#262626"
.P2C_MUTED <- "#595959"
.P2C_GRID <- "#E0E0E0"
.P2C_ORANGE <- "#F36E21"
.P2C_ORANGE_TEXT <- "#B84A0E"

.p2c_theme <- function(base_size = 18) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = .P2C_INK),
      axis.text = ggplot2::element_text(colour = .P2C_INK),
      axis.title = ggplot2::element_text(colour = .P2C_INK),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = .P2C_GRID, linewidth = 0.4),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = ggplot2::rel(1)),
      plot.title = ggplot2::element_text(size = ggplot2::rel(0.95), face = "bold"),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(colour = .P2C_MUTED, hjust = 0,
                                           size = ggplot2::rel(0.7)),
      plot.caption.position = "plot")
}

# 0.212 -> "21%", 0.003 -> "<1%", 0 -> "0%", NA -> "*".
.p2c_pct_label <- function(p) {
  ifelse(is.na(p), "*",
         ifelse(p > 0 & p < 0.005, "<1%", paste0(round(100 * p), "%")))
}

.p2c_suppressed_caption <- function(v) {
  if (any(is.na(v))) "* Too few students to report." else NULL
}

# Exit-rate labels and caption: "*" for a suppressed rate, "n/a" for one that
# cannot be observed (nobody followed into the next year, e.g. grade 12).
.p2c_exit_label <- function(p, suppressed) {
  ifelse(is.na(p) & !suppressed, "n/a", .p2c_pct_label(p))
}
.p2c_exit_caption <- function(p, suppressed) {
  out <- c(if (any(is.na(p) & suppressed)) "* Too few students to report.",
           if (any(is.na(p) & !suppressed))
             "n/a: no next-year record, so an exit cannot be observed.")
  if (length(out)) paste(out, collapse = "   ") else NULL
}

#' Context-deck figures
#'
#' Slide-styled plot builders over [run_prep2_context()]'s slots. Each takes
#' the matching slot and returns a ggplot; nothing is recomputed. A suppressed
#' value draws no bar and is labelled `*`.
#'
#' * `plot_context_exit_rates()` -- `$exit_by_grade` or `$exit_by_years`: the
#'   latest year's exit rate, beside the pooled rate when `series = "both"`.
#' * `plot_context_by_grade()` -- `$by_grade`: current ELs by grade.
#' * `plot_context_district_share()` -- `$district_share`: districts by EL
#'   share of enrollment.
#' * `plot_context_concentration()` -- `$concentration` or
#'   `$concentration_by_mean`: share of districts against share of EL
#'   students, by district EL enrollment, with the average district marked
#'   when the bands were split there.
#'
#' @param x The matching slot.
#' @param series `"both"` (pooled and latest year) or `"latest"`.
#' @param year The year to show; defaults to the latest in `x`.
#' @param base_size Base font size, in points.
#' @return A `ggplot`.
#' @name prep2_context_plots
NULL

#' @rdname prep2_context_plots
#' @export
plot_context_exit_rates <- function(x, series = c("both", "latest"), base_size = 18) {
  series <- match.arg(series)
  .p2_stop_slot(x, c("FIRST_YEAR", "POOLED", "PCT_EXITED", "SUPPRESSED"),
                "plot_context_exit_rates()")
  by <- attr(x, "by") %||% "grade"
  grp <- c(grade = "GRADE", years_in_status = "YEARS_IN_STATUS",
           district = "DISTRICT_ID", state = "STATE")[[by]]
  latest <- attr(x, "latest") %||% max(x$FIRST_YEAR[!x$POOLED])
  pooled <- attr(x, "pooled")

  lat <- x[!x$POOLED & x$FIRST_YEAR == latest, , drop = FALSE]
  lat$SERIES <- .p2c_school_year(latest)
  d <- lat
  if (series == "both" && !is.null(pooled) && pooled[[1]] != pooled[[2]]) {
    po <- x[x$POOLED, , drop = FALSE]
    po$SERIES <- sprintf("Pooled rate (%s to %s)", .p2c_school_year(pooled[[1]]),
                         .p2c_school_year(pooled[[2]]))
    d <- rbind(po, lat)
  }
  d$SERIES <- factor(d$SERIES, levels = unique(d$SERIES))
  fills <- if (nlevels(d$SERIES) == 2L) c(.P2C_GREY, .P2C_GREEN) else .P2C_GREEN
  names(fills) <- levels(d$SERIES)

  lv <- unique(x[[grp]])
  d$G <- factor(d[[grp]], levels = lv)
  xlab <- if (by == "years_in_status") paste("Year", lv) else lv
  d$Y <- ifelse(is.na(d$PCT_EXITED), 0, d$PCT_EXITED)
  dodge <- ggplot2::position_dodge(width = 0.82)

  ggplot2::ggplot(d, ggplot2::aes(.data$G, .data$Y, fill = .data$SERIES)) +
    ggplot2::geom_col(position = dodge, width = 0.78) +
    ggplot2::geom_text(ggplot2::aes(label = .p2c_exit_label(.data$PCT_EXITED,
                                                            .data$SUPPRESSED),
                                    group = .data$SERIES),
                       position = dodge, vjust = -0.45, fontface = "bold",
                       size = base_size / ggplot2::.pt * 0.85, colour = .P2C_INK) +
    ggplot2::scale_fill_manual(values = fills) +
    ggplot2::scale_x_discrete(labels = stats::setNames(xlab, lv)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::labs(x = if (by == "years_in_status") "Years in EL status" else "Grade",
                  y = NULL, caption = .p2c_exit_caption(d$PCT_EXITED, d$SUPPRESSED)) +
    .p2c_theme(base_size) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   legend.position = if (nlevels(d$SERIES) > 1L) "top" else "none")
}

#' @rdname prep2_context_plots
#' @export
plot_context_by_grade <- function(x, year = NULL, base_size = 18) {
  .p2_stop_slot(x, c("YEAR", "GRADE", "N"), "plot_context_by_grade()")
  if (is.null(year)) year <- max(x$YEAR)
  d <- x[x$YEAR == year, , drop = FALSE]
  d$GRADE <- factor(d$GRADE, levels = .P2_GRADES)
  # The largest grade is drawn darker, so the slide's callout has a mark.
  d$TOP <- !is.na(d$N) & d$N == max(d$N, na.rm = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(.data$GRADE, .data$N, fill = .data$TOP)) +
    ggplot2::geom_col(width = 0.7, na.rm = TRUE, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = .P2C_GREEN, `TRUE` = "#00733A")) +
    ggplot2::scale_y_continuous(labels = .p2_label_n,
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Grade", y = "Number of ELs",
                  caption = .p2c_suppressed_caption(d$N)) +
    .p2c_theme(base_size)
}

#' @rdname prep2_context_plots
#' @export
plot_context_district_share <- function(x, base_size = 18) {
  .p2_stop_slot(x, c("SHARE_BAND", "N_DISTRICTS"), "plot_context_district_share()")
  d <- x
  d$SHARE_BAND <- factor(d$SHARE_BAND, levels = d$SHARE_BAND)
  ggplot2::ggplot(d, ggplot2::aes(.data$SHARE_BAND, .data$N_DISTRICTS)) +
    ggplot2::geom_col(fill = .P2C_GREEN, width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = .p2_label_n(.data$N_DISTRICTS)),
                       vjust = -0.45, fontface = "bold",
                       size = base_size / ggplot2::.pt * 0.85, colour = .P2C_INK) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "ELs as a share of district enrollment",
                  y = "Number of districts") +
    .p2c_theme(base_size)
}

#' @rdname prep2_context_plots
#' @export
plot_context_concentration <- function(x, base_size = 18) {
  .p2_stop_slot(x, c("SIZE_BAND", "PCT_DISTRICTS", "PCT"), "plot_context_concentration()")
  n <- nrow(x)
  s_d <- "Share of districts"
  s_e <- "Share of EL students"
  # Bars placed by hand rather than dodged: band i sits at y = n - i + 1 (the
  # first band on top), its districts bar just above and its EL bar just below.
  pos <- n - seq_len(n) + 1
  d <- rbind(
    data.frame(Y = pos + 0.2, SERIES = s_d, P = x$PCT_DISTRICTS),
    data.frame(Y = pos - 0.2, SERIES = s_e, P = x$PCT))
  d$SERIES <- factor(d$SERIES, levels = c(s_d, s_e))
  d$X <- ifelse(is.na(d$P), 0, d$P)
  top <- max(d$X, na.rm = TRUE)
  if (!is.finite(top) || top <= 0) top <- 1
  mb <- attr(x, "mean_break")
  split <- !is.null(mb) && !is.na(mb) && "ABOVE_MEAN" %in% names(x)
  # Room to the right of the longest bar: for its label, and when split, for
  # the average's label, which would otherwise sit on the bar labels.
  xmax <- top * if (split) 1.75 else 1.18

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = .data$X,
                                    ymin = .data$Y - 0.18, ymax = .data$Y + 0.18,
                                    fill = .data$SERIES)) +
    ggplot2::geom_text(ggplot2::aes(x = .data$X, y = .data$Y,
                                    label = .p2c_pct_label(.data$P)),
                       hjust = -0.15, fontface = "bold",
                       size = base_size / ggplot2::.pt * 0.8, colour = .P2C_INK) +
    ggplot2::scale_fill_manual(values = stats::setNames(c(.P2C_DARKGREY, .P2C_GREEN),
                                                        c(s_d, s_e))) +
    ggplot2::scale_x_continuous(limits = c(0, xmax),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::scale_y_continuous(breaks = pos, labels = x$SIZE_BAND,
                                expand = ggplot2::expansion(add = 0.15)) +
    ggplot2::labs(x = NULL, y = NULL, title = "EL students in district",
                  caption = .p2c_suppressed_caption(d$P)) +
    .p2c_theme(base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_line(colour = .P2C_DARKGREY),
                   legend.position = "top", legend.justification = "left")

  if (split) {
    k <- sum(!x$ABOVE_MEAN)
    if (k > 0 && k < n) {
      yy <- n - k + 0.5
      p <- p +
        ggplot2::geom_hline(yintercept = yy, colour = .P2C_ORANGE, linewidth = 1.3) +
        ggplot2::annotate("text", x = xmax, y = yy, hjust = 1, vjust = -0.5,
                          colour = .P2C_ORANGE_TEXT, fontface = "bold",
                          size = base_size / ggplot2::.pt * 0.85,
                          label = sprintf("Average: %s EL students",
                                          .p2_label_n(round(attr(x, "mean_n")))))
    }
  }
  p
}
