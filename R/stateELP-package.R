#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data .env
#'
#' @details
#' `.data` and `.env` are imported package-wide rather than being referenced
#' bare. Without the import, every `dplyr` verb that refers to a column through
#' `.data$x` is reported by `R CMD check` as an undefined global, and `rlang`
#' reads as a declared-but-unused dependency.
#'
#' The version of R recorded as this package's reference is **4.3.3**
#' (`sim_contract.md`), but `Depends` carries `R (>= 4.3.0)`: `R CMD check`
#' requires a version dependency at patchlevel 0, and a `Depends` field cannot
#' express "built and verified against 4.3.3". The build machine's own version
#' and the determinism evidence for it live in `sim_contract.md` section 2.1.
NULL
