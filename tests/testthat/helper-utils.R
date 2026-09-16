# Fixtures for the shared-derivations module.
#
# Module-scoped names (make_utils_*): testthat sources every helper into one
# shared environment, so a generic name would clobber another module's builder.

# The deck's A-J worked example, slides 43-51, as a spine.
#
#   Student | K  1  2  3  4  5  6  7  8  9 10 11 12
#   A       | EL EL EL EL EL EL R
#   B       | EL EL EL EL EL EL EL  ?  ?  ?  ?  ?  ?
#   C       | EL EL EL EL EL R
#   D       | EL EL EL EL EL EL EL EL EL  ?  ?  ?  ?
#   E       | EL EL EL R
#   F       | EL EL EL EL EL R
#   G       | EL EL EL EL EL EL EL EL EL  ?  ?  ?  ?
#   H       | EL EL EL EL R
#   I       | EL EL EL  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?
#   J       | EL EL EL EL EL EL EL EL R
#
# "EL" is a current row, "R" a former row, "?" no row at all. Grade K is
# school year `base_year`; year in status is grade index + 1, so student E --
# EL in K, 1 and 2 and R in grade 3 -- exits in YEAR 4 under first-former
# dating. sim_contract.md 3.3 names E explicitly as the check on that.
make_utils_aj_spine <- function(base_year = 2015L) {
  n_el <- c(A = 6L, B = 7L, C = 5L, D = 9L, E = 3L,
            F = 5L, G = 9L, H = 4L, I = 3L, J = 8L)
  exits <- c(A = TRUE, B = FALSE, C = TRUE, D = FALSE, E = TRUE,
             F = TRUE, G = FALSE, H = TRUE, I = FALSE, J = TRUE)

  grade_label <- function(i) if (i == 0L) "K" else as.character(i)

  rows <- list()
  for (id in names(n_el)) {
    idx <- seq_len(n_el[[id]]) - 1L              # grade indices, K = 0
    status <- rep("current", length(idx))
    if (exits[[id]]) {                            # the R cell follows the ELs
      idx <- c(idx, n_el[[id]])
      status <- c(status, "former")
    }
    rows[[id]] <- tibble::tibble(
      student_id  = id,
      school_year = base_year + idx,
      grade       = vapply(idx, grade_label, ""),
      el_status   = status
    )
  }
  out <- dplyr::bind_rows(rows)
  # The deck's grid runs to grade 12, so the observation window does too.
  # Without that, D and G -- last seen in grade 8, the last year anyone is
  # seen -- would count as still-enrolled rather than disappeared.
  attr(out, "window_last_year") <- base_year + 12L
  out
}

make_utils_window <- function(spine) attr(spine, "window_last_year")

# The same cohort dated the other way: exit on the LAST CURRENT row, which is
# what the simulator flags and what the package must not do. Used as the
# counter-test, so the convention is proved rather than asserted.
make_utils_aj_last_current <- function(spine) {
  spine |>
    dplyr::filter(.data$el_status == "current") |>
    dplyr::group_by(.data$student_id) |>
    dplyr::summarise(first_current_year = min(.data$school_year),
                     last_current_year = max(.data$school_year),
                     .groups = "drop") |>
    dplyr::left_join(
      spine |>
        dplyr::filter(.data$el_status == "former") |>
        dplyr::group_by(.data$student_id) |>
        dplyr::summarise(has_exit = TRUE, .groups = "drop"),
      by = "student_id"
    ) |>
    dplyr::mutate(
      exited = !is.na(.data$has_exit),
      exit_year_in_status = dplyr::if_else(
        .data$exited,
        as.integer(.data$last_current_year - .data$first_current_year + 1L),
        NA_integer_)
    )
}
