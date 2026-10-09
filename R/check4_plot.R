# Check 4 -- figures. Takes the matching compute_*() output and returns a
# ggplot without recomputing (CLAUDE.md 8). Only per-cut summaries arrive
# here; no student-level data is ever in scope.
#
# The palette matches Check 3's. Each stage holds its own copy because a stage
# session may not edit another prefix or R/utils.R; a shared palette helper is
# `[PROPOSED - CONFIRM]`.

.C4_BLUE <- "#2C7FB8"
.C4_GREY <- "grey40"
.C4_CUT <- "#C0392B"     # the current rule, in the red the other stages use

.c4_theme <- function() ggplot2::theme_minimal(base_size = 11)

#' Decision consistency and Youden's J across candidate cuts
#'
#' The stage's figure: the two statistics for one panel, drawn one above the
#' other on a shared candidate-cut axis, with **the J optimum marked** and the
#' state's **current exit threshold** marked beside it. Each statistic gets its
#' own vertical scale, because agreement and J sit in very different ranges and
#' on one scale the J curve's peak would be flattened out of sight; the two
#' vertical lines run through both.
#'
#' The agreement peak is circled on its own curve. When the proficiency rate is
#' well below one half, it sits well to the right of the J optimum -- often at
#' the top of the observed scale -- which is the base-rate distortion the stage
#' exists to show.
#'
#' @param x `$youden_j` from [run_check4()], or [compute_youden_j()] output.
#' @param optimum `$j_optimal` from [run_check4()], or [compute_j_optimal()]
#'   output; optional. Without it nothing is marked.
#' @param spec An `el_spec`, optional. Names the axis after the reported ELP
#'   component and labels a categorical scale's cuts with its categories.
#' @param subject,grade Which panel to draw when `x` holds more than one.
#'   Defaults to the first. A selector matching no panel is an error rather
#'   than a silent fallback to another grade.
#' @return A `ggplot`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' res <- run_check4(b, spec)
#' plot_consistency_curve(res$youden_j, res$j_optimal, spec)
plot_consistency_curve <- function(x, optimum = NULL, spec = NULL,
                                   subject = NULL, grade = NULL) {
  need <- c("CUT", "CUT_LABEL", "AGREE", "J")
  if (!is.data.frame(x) || !all(need %in% names(x))) {
    stop("plot_consistency_curve() needs a data frame with ",
         paste(need, collapse = ", "), "; pass $youden_j from run_check4().",
         call. = FALSE)
  }
  stopifnot(is.null(optimum) || is.data.frame(optimum),
            is.null(spec) || inherits(spec, "el_spec"))

  x <- .c4_one_panel(x, subject, grade)
  if (!nrow(x)) stop("No candidate cuts to draw.", call. = FALSE)
  opt <- .c4_panel_optimum(optimum, x)

  stats <- c("Decision consistency", "Youden's J")
  d <- rbind(
    data.frame(CUT = x$CUT, VALUE = x$AGREE, STAT = stats[[1]]),
    data.frame(CUT = x$CUT, VALUE = x$J, STAT = stats[[2]]))
  d$STAT <- factor(d$STAT, levels = stats)
  # Suppressed cuts are not drawn; the curve joins the reportable ones, so a
  # cut merged into its neighbour for suppression leaves no gap to misread.
  d <- d[!is.na(d$VALUE), , drop = FALSE]
  # Every cut suppressed: keep one empty row per panel so the facets still
  # exist (facet_wrap() cannot build from zero rows).
  if (!nrow(d)) {
    d <- data.frame(CUT = x$CUT[[1]], VALUE = NA_real_,
                    STAT = factor(stats, levels = stats))
  }

  # Each curve is drawn in its own fixed colour (the panel strip names it), so
  # the colour scale is free for the marks, which a legend names: text labels
  # on the plot collide with the curves wherever the two lines sit close.
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = .data$CUT, y = .data$VALUE))
  for (i in seq_along(stats)) {
    di <- d[d$STAT == stats[[i]], , drop = FALSE]
    colour <- c(.C4_GREY, .C4_BLUE)[[i]]
    p <- p +
      ggplot2::geom_line(data = di, colour = colour, na.rm = TRUE) +
      ggplot2::geom_point(data = di, colour = colour, size = 1.1,
                          na.rm = TRUE)
  }

  # The two vertical marks, each in both panels. A layer without the facet
  # variable repeats across facets, which is what a cut position should do.
  marks <- data.frame(CUT = numeric(), MARK = character(), COLOUR = character(),
                      LINETYPE = character())
  if (!is.null(opt) && !is.na(opt$J_CUT)) {
    marks <- rbind(marks, data.frame(
      CUT = opt$J_CUT, MARK = paste("J optimum:", opt$J_CUT_LABEL),
      COLOUR = .C4_BLUE, LINETYPE = "dashed"))
  }
  if (!is.null(opt) && !is.na(opt$THRESHOLD)) {
    marks <- rbind(marks, data.frame(
      CUT = opt$THRESHOLD,
      MARK = paste(if (isTRUE(opt$RULE_IS_TERM)) "Current rule's term:" else
                     if (isTRUE(opt$RULE_IS_ROUTE)) "Current rule's route:" else
                       "Current rule:", .check4_op_text(opt$THRESHOLD_OP),
                   opt$THRESHOLD_LABEL),
      COLOUR = .C4_CUT, LINETYPE = "dotted"))
  }
  if (nrow(marks)) {
    marks$MARK <- factor(marks$MARK, levels = marks$MARK)
    p <- p +
      ggplot2::geom_vline(
        data = marks,
        ggplot2::aes(xintercept = .data$CUT, colour = .data$MARK,
                     linetype = .data$MARK), linewidth = 0.7) +
      ggplot2::scale_colour_manual(
        values = stats::setNames(marks$COLOUR, levels(marks$MARK)),
        name = NULL) +
      ggplot2::scale_linetype_manual(
        values = stats::setNames(marks$LINETYPE, levels(marks$MARK)),
        name = NULL)
  }
  if (!is.null(opt) && !is.na(opt$AGREE_CUT)) {
    p <- p + ggplot2::geom_point(
      data = data.frame(CUT = opt$AGREE_CUT, VALUE = opt$AGREE,
                        STAT = factor(stats[[1]], levels = stats)),
      shape = 21, size = 3.2, colour = .C4_GREY, fill = NA, stroke = 0.9)
  }

  x_lab <- if (is.null(spec)) "Candidate cut (ELP level)" else
    paste0("Candidate cut: ", spec_headline_label(spec), " level")
  if (!is.null(spec) && identical(.check4_scale(spec)$type, "categorical")) {
    p <- p + ggplot2::scale_x_continuous(breaks = x$CUT, labels = x$CUT_LABEL)
  }

  p +
    ggplot2::facet_wrap(~ STAT, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = x_lab, y = NULL) +
    .c4_theme() +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0,
                                                      face = "bold"),
                   legend.position = "bottom")
}

# One panel out of a long, keyed curve. A frame without keys already is one
# panel and comes back untouched.
.c4_one_panel <- function(x, subject = NULL, grade = NULL) {
  if (!all(c("SUBJECT", "GRADE") %in% names(x))) return(x)
  keep <- rep(TRUE, nrow(x))
  if (!is.null(subject)) keep <- keep & x$SUBJECT %in% subject
  if (!is.null(grade)) keep <- keep & as.character(x$GRADE) %in%
    as.character(grade)
  if (!any(keep)) {
    stop("No panel in this curve for ",
         if (is.null(subject)) "" else paste0("subject ", subject, " "),
         if (is.null(grade)) "" else paste0("grade ", grade),
         ".", call. = FALSE)
  }
  x <- x[keep, , drop = FALSE]
  # Still more than one panel: take the first -- subject, grade and year
  # together. Drawing several on one axis would silently pool them.
  x[.c4_key(x) == .c4_key(x)[[1]], , drop = FALSE]
}

# The optimum row matching the curve's panel, or NULL.
.c4_panel_optimum <- function(optimum, x) {
  if (is.null(optimum) || !nrow(optimum)) return(NULL)
  if (!all(c("SUBJECT", "GRADE") %in% names(optimum)) ||
      !all(c("SUBJECT", "GRADE") %in% names(x))) {
    return(optimum[1, , drop = FALSE])
  }
  hit <- .c4_key(optimum) == .c4_key(x)[[1]]
  if (!any(hit)) return(NULL)
  optimum[which(hit)[[1]], , drop = FALSE]
}

# A panel's key: subject, grade and, where the frame carries it, year.
.c4_key <- function(x) {
  paste(x$SUBJECT, as.character(x$GRADE),
        if ("YEAR" %in% names(x)) x$YEAR else "", sep = "\r")
}
