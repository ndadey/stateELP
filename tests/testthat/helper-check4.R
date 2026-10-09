# Fixtures for Check 4.
#
# Module-scoped names (make_check4_*): testthat sources every helper into one
# shared environment, so a generic name would clobber another module's
# builder.

make_check4_spec <- function() read_el_spec("00_SIM_EL_Spec")

# The shipped simulation. Check 3's memoized bundle is the same object built
# from the same spec, so it is borrowed rather than built a second time (about
# ten seconds each) -- the pattern Check 2 uses with Check 1's fixtures.
make_check4_sim_bundle <- function() make_check3_sim_bundle()

# The academic cut the package derives, restated for fixtures so a score can
# be placed on either side of it without a test typing the number.
make_check4_cut <- function(spec, subject = spec_academic_subjects(spec)[[1]]) {
  lvl <- spec$policy$academic_benchmark$proficient_level
  as.numeric(spec$academic[[subject]]$cuts[[lvl - 1L]])
}

# A hand-built paired panel: one grade, one year, one subject, current ELs
# only unless asked. `levels[i]` is held by `n[i]` students, `prof[i]` of whom
# score at or above the proficiency cut. Extra groups test what must be left
# out of the sample: an alternate academic taker with an annual level, a
# current EL with no annual level, and former / never-EL students.
make_check4_bundle <- function(levels, n, prof, spec = make_check4_spec(),
                               year = 2024L, grade = NULL,
                               n_alternate = 0L, n_no_level = 0L,
                               n_former = 0L, n_never = 0L) {
  stopifnot(length(levels) == length(n), length(n) == length(prof),
            all(prof <= n))
  subject <- spec_academic_subjects(spec)[[1]]
  if (is.null(grade)) grade <- spec_academic_grades(spec, subject)[[1]]
  cut <- make_check4_cut(spec, subject)

  ids <- function(prefix, k) sprintf("%s%04d", prefix, seq_len(k))
  cur_level <- rep(levels, n)
  cur_prof <- unlist(Map(function(k, p) c(rep(TRUE, p), rep(FALSE, k - p)),
                         n, prof))
  cur <- tibble::tibble(STUDENT_ID = ids("C", sum(n)), LEVEL = cur_level,
                        PROF = cur_prof, STATUS = "current", ALT = FALSE)

  extra <- function(prefix, k, status, level, alt) {
    if (!k) return(NULL)
    tibble::tibble(STUDENT_ID = ids(prefix, k), LEVEL = level,
                   PROF = rep_len(c(TRUE, FALSE), k), STATUS = status,
                   ALT = alt)
  }
  everyone <- dplyr::bind_rows(
    cur,
    extra("A", n_alternate, "current", levels[[1]], TRUE),
    extra("U", n_no_level, "current", NA_real_, FALSE),
    extra("F", n_former, "former", NA_real_, FALSE),
    extra("N", n_never, "never", NA_real_, FALSE))

  roster <- tibble::tibble(
    STUDENT_ID = everyone$STUDENT_ID, YEAR = as.integer(year), GRADE = grade,
    EL_STATUS = everyone$STATUS,
    ELP_FORM = ifelse(everyone$STATUS == "current", "annual", NA_character_),
    SAMPLE_WEIGHT = ifelse(everyone$STATUS == "never", 10, 1))

  # Only current ELs with a level carry an ELP row (contract 4.4). The scale
  # score is present only where the reported component measures one.
  hl <- spec_headline(spec)
  has_scale <- "scale" %in% spec$elp$components[[hl]]$measures
  with_level <- everyone[!is.na(everyone$LEVEL), , drop = FALSE]
  base <- tibble::tibble(
    STUDENT_ID = with_level$STUDENT_ID, YEAR = as.integer(year),
    COMPONENT = hl, ELP_FORM = "annual",
    SCALE_SCORE = if (has_scale) 400 else NA_real_,
    ACHIEVEMENT_LEVEL = with_level$LEVEL)

  academic <- tibble::tibble(
    STUDENT_ID = everyone$STUDENT_ID, YEAR = as.integer(year), GRADE = grade,
    SUBJECT = subject,
    SCALE_SCORE = ifelse(everyone$ALT, NA_real_,
                         ifelse(everyone$PROF, cut + 10, cut - 10)),
    ACHIEVEMENT_LEVEL = NA_integer_,
    ACHIEVEMENT_LEVEL_ALT = ifelse(everyone$ALT, 1L, NA_integer_))

  as_el_data(base, roster = roster, academic = academic, spec = spec)
}

# THE skewed fixture (instruction file, Fixture): 500 current ELs, 80
# proficient -- a base rate of 16%, well below one half. The proficiency rate
# by level never reaches one half, so raw agreement keeps rising to the top
# cut, while J peaks one cut lower. Worked by hand:
#
#   cut   TP  FP  FN   TN   agreement   sens    spec    J
#   1.3   80 420   0    0   .16         1       0       0
#   2.3   70 330  10   90   .32         .875    .2143   .0893
#   3.3   60 240  20  180   .48         .75     .4286   .1786
#   4.3   50 150  30  270   .64         .625    .6429   .2679  <- J optimum
#   5.3   30  70  50  350   .76         .375    .8333   .2083  <- agree peak
#
# Every non-zero cell is at least 10, so nothing is suppressed. Levels sit off
# the whole numbers so the coarse-level warning never fires, and off the SIM
# exit threshold so the rule's row is a mapping, not a lookup.
make_check4_skewed_levels <- function() c(1.3, 2.3, 3.3, 4.3, 5.3)
make_check4_skewed_bundle <- function(spec = make_check4_spec(), ...) {
  make_check4_bundle(levels = make_check4_skewed_levels(),
                     n = rep(100L, 5), prof = c(10L, 10L, 10L, 20L, 30L),
                     spec = spec, ...)
}

# ELPA21 with an academic block: the shipped spec declares none (its academic
# spec is a pointer), so the SIM block is grafted on -- the pattern Check 2
# and Check 3 use for a Type-varied spec.
make_check4_elpa21_spec <- function() {
  spec <- read_el_spec("00_ELPA21_EL_Spec")
  spec$academic <- make_check4_spec()$academic
  validate_el_spec(spec)
  spec
}

# A categorical, level-only panel: the three determinations stored as their
# ordinal index, 40 students each.
make_check4_elpa21_bundle <- function(spec = make_check4_elpa21_spec()) {
  k <- length(spec$elp$level_scale$levels)
  make_check4_bundle(levels = seq_len(k), n = rep(40L, k),
                     prof = round(seq(10, 30, length.out = k)), spec = spec)
}
