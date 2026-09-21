# Prep 2 -- figures. Each takes the matching compute_*() output (a slot of
# run_prep2()) and returns a ggplot without recomputing (CLAUDE.md 8). The only
# arithmetic here is a reference line drawn through what is already displayed:
# the mean of the bars shown, never a statistic about students.
#
# Suppressed cells are NA in the slot and are simply not drawn.

.P2_BLUE <- "#2C7FB8"
.P2_LIGHT <- "#A6BDDB"

.p2_theme <- function() ggplot2::theme_minimal(base_size = 11)
.p2_label_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
.p2_label_pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")

.p2_stop_slot <- function(x, cols, what) {
  if (!is.data.frame(x) || !all(cols %in% names(x))) {
    stop(what, " needs a data frame with ", paste(cols, collapse = ", "),
         "; pass the matching slot of run_prep2().", call. = FALSE)
  }
}

#' Prep 2 figures
#'
#' Plot builders over [run_prep2()]'s slots. Each takes the matching slot and
#' returns a ggplot; nothing is recomputed. A suppressed (`NA`) cell is not
#' drawn.
#'
#' @param x The matching slot: `$counts_by_year`, `$el_share`, `$by_grade_span`,
#'   `$by_grade`, `$by_district`, `$entry_grade`, `$time_in_status` or
#'   `$initial_elp`.
#' @param type `"percent"` (the deck's 100% stacked bars) or `"count"`.
#' @param year The year to show; defaults to the latest in `x`.
#' @return A `ggplot`.
#' @name prep2_plots
NULL

#' @rdname prep2_plots
#' @export
plot_counts_by_year <- function(x) {
  .p2_stop_slot(x, c("YEAR", "N_CURRENT"), "plot_counts_by_year()")
  avg <- mean(x$N_CURRENT, na.rm = TRUE)
  ggplot2::ggplot(x, ggplot2::aes(factor(.data$YEAR), .data$N_CURRENT)) +
    ggplot2::geom_col(fill = .P2_BLUE, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = avg, linetype = "dashed") +
    ggplot2::scale_y_continuous(labels = .p2_label_n) +
    ggplot2::labs(x = NULL, y = "Current ELs",
                  subtitle = sprintf("%d-year average: %s", nrow(x),
                                     .p2_label_n(round(avg)))) +
    .p2_theme()
}

#' @rdname prep2_plots
#' @export
plot_el_share <- function(x) {
  .p2_stop_slot(x, c("YEAR", "PCT_EL"), "plot_el_share()")
  ggplot2::ggplot(x, ggplot2::aes(.data$YEAR, .data$PCT_EL)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = .p2_label_pct, limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = sort(unique(x$YEAR))) +
    ggplot2::labs(x = NULL, y = "EL share of enrollment") +
    .p2_theme()
}

#' @rdname prep2_plots
#' @export
plot_by_grade_span <- function(x, type = c("percent", "count")) {
  type <- match.arg(type)
  .p2_stop_slot(x, c("YEAR", "SPAN", "N", "PCT"), "plot_by_grade_span()")
  y <- if (type == "percent") "PCT" else "N"
  ggplot2::ggplot(x, ggplot2::aes(factor(.data$YEAR), .data[[y]],
                                  fill = factor(.data$SPAN, levels = c("6-12", "K-5")))) +
    ggplot2::geom_col(na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = if (type == "percent") .p2_label_pct
                                         else .p2_label_n) +
    ggplot2::scale_fill_manual(values = c("K-5" = .P2_BLUE, "6-12" = .P2_LIGHT),
                               breaks = c("K-5", "6-12")) +
    ggplot2::labs(x = NULL, fill = NULL,
                  y = if (type == "percent") "Share of current ELs" else "Current ELs") +
    .p2_theme()
}

#' @rdname prep2_plots
#' @export
plot_by_grade <- function(x, year = NULL) {
  .p2_stop_slot(x, c("YEAR", "GRADE", "N"), "plot_by_grade()")
  if (is.null(year)) year <- max(x$YEAR)
  d <- x[x$YEAR == year, , drop = FALSE]
  d$GRADE <- factor(d$GRADE, levels = .P2_GRADES)
  ggplot2::ggplot(d, ggplot2::aes(.data$GRADE, .data$N)) +
    ggplot2::geom_col(fill = .P2_BLUE, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = mean(d$N, na.rm = TRUE), linetype = "dashed") +
    ggplot2::scale_y_continuous(labels = .p2_label_n) +
    ggplot2::labs(x = "Grade", y = "Current ELs", subtitle = as.character(year)) +
    .p2_theme()
}

#' @rdname prep2_plots
#' @export
plot_by_district <- function(x) {
  .p2_stop_slot(x, c("RANK", "N", "TWO_THIRDS"), "plot_by_district()")
  k <- sum(x$TWO_THIRDS)
  small <- attr(x, "small_n")
  year <- attr(x, "year")
  p <- ggplot2::ggplot(x, ggplot2::aes(.data$RANK, .data$N)) +
    ggplot2::geom_col(fill = .P2_BLUE, width = 1, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = k + 0.5, linetype = "dashed") +
    ggplot2::annotate("text", x = k + 1, y = Inf, hjust = 0, vjust = 1.5,
                      label = sprintf("%d of %d districts hold two-thirds of ELs",
                                      k, nrow(x)))
  if (!is.null(small) && !is.na(small)) {
    p <- p + ggplot2::geom_hline(yintercept = small, linetype = "dotted")
  }
  p +
    ggplot2::scale_x_continuous(breaks = function(lim) {
      b <- pretty(lim)
      b[b >= 1 & b == round(b)]
    }) +
    ggplot2::scale_y_continuous(labels = .p2_label_n) +
    ggplot2::labs(x = "District rank", y = "Current ELs",
                  subtitle = if (is.null(year)) NULL else as.character(year)) +
    .p2_theme()
}

#' @rdname prep2_plots
#' @export
plot_entry_grade <- function(x) {
  .p2_stop_slot(x, c("ENTRY_GRADE", "PCT"), "plot_entry_grade()")
  d <- x
  d$ENTRY_GRADE <- factor(d$ENTRY_GRADE, levels = .P2_GRADES)
  cn <- attr(x, "counts")
  yrs <- attr(x, "years")
  sub <- if (is.null(cn)) NULL else sprintf(
    "%s entrants observed at entry%s; %s entered before the panel (dated); %s entry unknown",
    .p2_fmt_n(cn[["OBSERVED"]]),
    if (is.null(yrs)) "" else sprintf(", %d%s%d", yrs[[1]], .P2_ENDASH, yrs[[2]]),
    .p2_fmt_n(cn[["BEFORE_PANEL"]]), .p2_fmt_n(cn[["UNKNOWN"]]))
  ggplot2::ggplot(d, ggplot2::aes(.data$ENTRY_GRADE, .data$PCT)) +
    ggplot2::geom_col(fill = .P2_BLUE, na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = .p2_label_pct) +
    ggplot2::labs(x = "Entry grade", y = "Share of observed entrants", subtitle = sub) +
    .p2_theme()
}

#' @rdname prep2_plots
#' @export
plot_time_in_status <- function(x) {
  d <- if (is.list(x) && !is.data.frame(x)) x$distribution else x
  .p2_stop_slot(d, c("GRADE", "DUR_BIN", "PCT"), "plot_time_in_status()")
  lv <- unique(d$DUR_BIN)
  n_known <- sum(lv != .P2_DUR_UNKNOWN)
  fills <- c(grDevices::colorRampPalette(c("#DEEBF7", "#08519C"))(n_known), "grey75")
  names(fills) <- c(lv[lv != .P2_DUR_UNKNOWN], .P2_DUR_UNKNOWN)
  d$GRADE <- factor(d$GRADE, levels = .P2_GRADES)
  d$DUR_BIN <- factor(d$DUR_BIN, levels = rev(names(fills)))
  ggplot2::ggplot(d, ggplot2::aes(.data$GRADE, .data$PCT, fill = .data$DUR_BIN)) +
    ggplot2::geom_col(na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = .p2_label_pct) +
    ggplot2::scale_fill_manual(values = fills, breaks = names(fills), drop = FALSE) +
    ggplot2::labs(x = "Grade", y = "Share of current ELs", fill = "Years in status") +
    .p2_theme()
}

#' @rdname prep2_plots
#' @export
plot_initial_elp <- function(x) {
  .p2_stop_slot(x, c("INITIAL", "PCT", "IN_BANDS"), "plot_initial_elp()")
  d <- x[x$IN_BANDS, , drop = FALSE]
  d$INITIAL <- factor(d$INITIAL, levels = d$INITIAL)
  ggplot2::ggplot(d, ggplot2::aes(.data$INITIAL, .data$PCT)) +
    ggplot2::geom_col(fill = .P2_BLUE, na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = .p2_label_pct) +
    ggplot2::labs(x = "Initial ELP band", y = "Share of observed entrants") +
    .p2_theme()
}
