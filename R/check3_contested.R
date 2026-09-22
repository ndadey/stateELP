# Check 3 -- the Type-gated additional check: what happens inside the range
# where a multi-criterion exit rule disagrees with itself.
#
# WHERE THE CRITERION OUTCOMES COME FROM (decided 2026-09-22). Prep 2 computes
# a per-student criterion pass matrix inside compute_criteria_crosswalk() and
# returns only aggregates -- counts by pass/fail pattern, disagreement by
# level, and the students blocked by exactly one criterion near the cut. None
# of those supports the two-group split this check needs, and Prep 2 is
# another prefix.
#
# So the outcomes are derived here, from the same spec terms, and the risk
# that two derivations drift apart is answered by a test rather than a
# comment: test-check3.R asserts this function reproduces Prep 2's own
# `blocked_groups` exactly. If the two ever disagree about whether a student
# cleared a criterion, that test fails.
#
# The derivation lives in R/, never in the .qmd; the document states which
# route was taken.

#' Criterion-by-criterion pass/fail for a multi-criterion exit rule
#'
#' One row per current student-year with an annual ELP record, and one logical
#' column per criterion in the first exit pathway that carries two or more ELP
#' terms. `CLEARED_OTHERS` is whether every criterion **except** the headline
#' one was met -- the split the contested-zone check is built on.
#'
#' A term whose component is missing for a student counts as passed when the
#' term says `exempt_if_absent`, and failed otherwise, which is the same rule
#' Prep 2 applies.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @return A tibble, or `NULL` when no pathway carries two or more ELP terms.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")   # Type 1: returns NULL
#' b <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' compute_criterion_pass(b, spec)
compute_criterion_pass <- function(data, spec) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  crit <- .check3_criteria(spec)
  if (is.null(crit)) return(NULL)      # [no pathway with two ELP terms]

  roster <- el_roster(data)
  cur <- roster[roster$EL_STATUS %in% "current", c("STUDENT_ID", "YEAR")]
  ann <- el_base(data)
  ann <- ann[ann$ELP_FORM %in% "annual", , drop = FALSE]
  ann <- dplyr::semi_join(ann, cur, by = c("STUDENT_ID", "YEAR"))
  spine <- dplyr::distinct(ann[c("STUDENT_ID", "YEAR")])
  if (!nrow(spine)) return(NULL)

  keys <- paste(spine$STUDENT_ID, spine$YEAR)
  out <- spine
  headline <- spec_headline(spec)
  is_head <- logical(length(crit$terms))

  for (i in seq_along(crit$terms)) {
    k <- crit$terms[[i]]
    col <- if (identical(k$measure, "level")) "ACHIEVEMENT_LEVEL" else
      "SCALE_SCORE"
    sub <- ann[ann$COMPONENT == k$component, c("STUDENT_ID", "YEAR", col)]
    x <- sub[[col]][match(keys, paste(sub$STUDENT_ID, sub$YEAR))]
    pass <- do.call(k$op, list(x, k$value))
    # An absent component is a pass only where the term says so; otherwise a
    # student cannot be shown to have met it.
    pass[is.na(x)] <- isTRUE(k$exempt_if_absent)
    out[[crit$names[[i]]]] <- pass
    is_head[[i]] <- identical(k$component, headline) &&
      identical(k$measure, "level")
  }

  passm <- as.matrix(out[, crit$names])
  out$CLEARED_ALL <- rowSums(!passm) == 0L
  # Everything except the headline term: the "second criterion" of the
  # instruction file, generalised to however many there are.
  others <- crit$names[!is_head]
  out$CLEARED_OTHERS <- if (!length(others)) NA else
    rowSums(!as.matrix(out[, others])) == 0L
  out$N_FAILED <- rowSums(!passm)
  attr(out, "criteria") <- crit$names
  attr(out, "headline_criterion") <- crit$names[is_head]
  out
}

# The first pathway carrying two or more ELP terms, and stable names for its
# criteria. Mirrors Prep 2's selection so the two agree on WHICH rule is being
# evaluated as well as on the outcome; the test compares the outcomes.
.check3_criteria <- function(spec) {
  paths <- spec$policy$exit$pathways
  for (i in seq_along(paths)) {
    terms <- Filter(function(t) identical(t$from %||% "elp", "elp"),
                    paths[[i]]$terms)
    if (length(terms) >= 2L) {
      names_ <- vapply(terms, function(k) {
        paste0(k$component, " ", k$measure, " ", k$op, " ", k$value)
      }, "")
      return(list(pathway = paths[[i]]$name %||% paste0("pathway_", i),
                  combine = paths[[i]]$combine %||% "all",
                  terms = terms, names = names_))
    }
  }
  NULL
}

#' The contested zone, split by the second criterion
#'
#' **Type-gated (2, 4, 5, 6): `NULL` under a single-criterion rule**, which is
#' what the shipped SIM spec is. Where a state's rule carries more than one
#' criterion there is a range of ELP levels in which the criteria disagree --
#' Prep 2 computes it -- and inside that range the question is whether the
#' second criterion is doing independent work or merely restating the first.
#'
#' Two outputs, both restricted to the contested range checked slightly wider
#' (the instruction file's wording; `widen` sets how much):
#'
#' * `$boxes` -- the academic distribution by band, split by whether the
#'   student cleared the other criteria. A visible shift between the two says
#'   the second criterion separates students the headline score does not.
#' * `$model` -- one logistic with ELP, a cleared-others indicator and their
#'   interaction, plus `$curves`, the two predicted curves that model implies.
#'   Presenting them as separate curves rather than a coefficient is what
#'   makes a difference in slope legible.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param panel Output of [compute_panel()], or `NULL` to derive it.
#' @param widen How far past the contested range to look, in ELP levels.
#' @return A list, or `NULL` when the spec is not a gated Type, carries no
#'   multi-criterion pathway, or the zone holds too few students to report.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")   # Type 1: returns NULL
#' b <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                 academic = academic_long_sim, spec = spec)
#' compute_contested_models(b, spec)
compute_contested_models <- function(data, spec, panel = NULL, widen = 0.5) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"))
  # [Type-gated: 2, 4, 5 and 6. A Type 1 rule has one criterion, so nothing
  #  can disagree with it and there is no contested zone to model.]
  if (!spec_has_type(spec, c(2L, 4L, 5L, 6L))) return(NULL)

  pass <- compute_criterion_pass(data, spec)
  if (is.null(pass) || all(is.na(pass$CLEARED_OTHERS))) return(NULL)

  zone <- .check3_zone(data, spec)
  if (is.null(zone)) return(NULL)

  if (is.null(panel)) panel <- compute_panel(data, spec)
  if (is.na(panel$year)) return(NULL)
  rows <- .check3_panel_rows(data, spec, panel)
  rows <- rows[rows$EL_STATUS %in% "current" & !rows$ALTERNATE, , drop = FALSE]
  if (!nrow(rows)) return(NULL)

  at <- match(paste(rows$STUDENT_ID, rows$YEAR),
              paste(pass$STUDENT_ID, pass$YEAR))
  rows$CLEARED_OTHERS <- pass$CLEARED_OTHERS[at]
  h <- elp_headline(data, spec)
  rows$LEVEL <- h$ACHIEVEMENT_LEVEL[match(paste(rows$STUDENT_ID, rows$YEAR),
                                          paste(h$STUDENT_ID, h$YEAR))]

  inside <- !is.na(rows$LEVEL) & !is.na(rows$CLEARED_OTHERS) &
    rows$LEVEL >= zone$lo - widen & rows$LEVEL <= zone$hi + widen
  zr <- rows[inside, , drop = FALSE]
  if (nrow(zr) < spec_min_cell(spec)) return(NULL)   # [zone too thin to report]

  list(zone = zone, widen = widen,
       n = nrow(zr), n_cleared = sum(zr$CLEARED_OTHERS),
       criteria = attr(pass, "criteria"),
       boxes = .check3_zone_boxes(zr, spec, panel),
       model = .check3_zone_model(zr, spec, panel),
       source = paste0(
         "Criterion outcomes derived in Check 3 from the spec's exit terms. ",
         "Prep 2 computes the same outcomes and reports only aggregates, so ",
         "they are not read across from it; a test asserts the two agree."))
}

# Prep 2 owns the contested zone, and Check 3 reads it rather than recomputing
# it: it is a property of the rule and the population, not of this panel.
.check3_zone <- function(data, spec) {
  cw <- compute_criteria_crosswalk(data, spec)
  if (is.null(cw) || is.null(cw$contested_zone)) return(NULL)
  z <- cw$contested_zone
  rng <- z$core_90
  if (any(is.na(rng))) rng <- z$range
  if (any(is.na(rng))) return(NULL)
  list(lo = min(rng), hi = max(rng), cut = z$cut,
       n_disagree = z$n_disagree, n_total = z$n_total)
}

# The academic distribution inside the zone, by band and by whether the other
# criteria were cleared. Same summary arithmetic as the main panel.
.check3_zone_boxes <- function(zr, spec, panel) {
  cut <- .check3_cut(spec, panel$subject)
  zr$CLEARED <- ifelse(zr$CLEARED_OTHERS, "cleared the other criteria",
                       "blocked by another criterion")
  out <- zr |>
    dplyr::group_by(.data$GROUP, .data$CLEARED) |>
    dplyr::summarise(
      N        = dplyr::n(),
      Y_MIN    = .check3_q(.data$SCALE_SCORE, 0),
      Y_P25    = .check3_q(.data$SCALE_SCORE, 0.25),
      Y_MEDIAN = .check3_q(.data$SCALE_SCORE, 0.5),
      Y_MEAN   = .check3_mean(.data$SCALE_SCORE),
      Y_P75    = .check3_q(.data$SCALE_SCORE, 0.75),
      Y_MAX    = .check3_q(.data$SCALE_SCORE, 1),
      PCT_AT_CUT = .check3_share_at(.data$SCALE_SCORE, .env$cut),
      .groups = "drop")
  apply_suppression(out, spec, n_col = "N",
                    cols = c("Y_MIN", "Y_P25", "Y_MEDIAN", "Y_MEAN", "Y_P75",
                             "Y_MAX", "PCT_AT_CUT"))
}

# One logistic with the interaction, and the two curves it implies. The curves
# are the output that matters: a coefficient on an interaction term is not
# something a policy reader can act on, and two curves that separate are.
.check3_zone_model <- function(zr, spec, panel) {
  cut <- .check3_cut(spec, panel$subject)
  fr <- zr[!is.na(zr$ELP_SCALE) & !is.na(zr$SCALE_SCORE), , drop = FALSE]
  fr$ABOVE <- as.integer(fr$SCALE_SCORE >= cut)
  fr$CLEARED <- as.integer(fr$CLEARED_OTHERS)

  ok <- nrow(fr) >= spec_min_cell(spec) &&
    length(unique(fr$ABOVE)) > 1L && length(unique(fr$CLEARED)) > 1L
  if (!ok) {
    return(list(coefficients = NULL, curves = NULL,
                note = paste0(
                  "Too few students in the contested zone, or no variation in ",
                  "either the outcome or the second criterion, to fit the ",
                  "interaction.")))
  }

  fit <- stats::glm(ABOVE ~ ELP_SCALE * CLEARED, data = fr,
                    family = stats::binomial())
  cf <- summary(fit)$coefficients
  coefficients <- tibble::tibble(
    TERM = rownames(cf), ESTIMATE = unname(cf[, 1]), SE = unname(cf[, 2]),
    P_VALUE = unname(cf[, 4]))

  grid <- expand.grid(
    ELP_SCALE = seq(min(fr$ELP_SCALE), max(fr$ELP_SCALE), length.out = 60),
    CLEARED = c(0L, 1L))
  grid$FITTED <- stats::predict(fit, newdata = grid, type = "response")
  grid$CLEARED_LABEL <- ifelse(grid$CLEARED == 1L,
                               "cleared the other criteria",
                               "blocked by another criterion")

  list(coefficients = coefficients, curves = tibble::as_tibble(grid),
       n = nrow(fr), n_cleared = sum(fr$CLEARED),
       note = paste0(
         "A gap between the two curves means the second criterion separates ",
         "students the headline score does not. Curves that lie on top of ",
         "each other mean it is restating the first."))
}
