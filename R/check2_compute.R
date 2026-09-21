# Check 2 -- on-time vs prolonged exiters. Plain data out, no plotting, no I/O.
#
# Check 2 does not select a cohort and does not date an exit. The cohort is
# Check 1's (the caller passes the label from the exported compute_cohorts()),
# and the exit dating, the nested indicators and the on-time / prolonged split
# are R/utils.R's build_student_spans() and add_exit_flags(), called unchanged
# (CLAUDE.md 14). What is new here is the crosstab built on them.

# The crosstab's columns, in the deck's order (slides 63-64). ALL is every
# cohort student; the rest are the indicator columns add_exit_flags() returns.
# EXIT_EVER >= EXIT_MAX >= EXIT_ONTIME are nested, so those three do not add
# up; what adds up is EXIT_EVER + NO_EXIT + DISAPPEARED = ALL.
.CHECK2_FLAG_COLUMNS <- c("EXIT_EVER", "EXIT_MAX", "EXIT_ONTIME",
                          "NO_EXIT", "DISAPPEARED")
.CHECK2_COLUMNS <- c("ALL", .CHECK2_FLAG_COLUMNS)

# The three exit groups the academic summary and the trajectories condition on,
# in display order. Disappeared is its own group, never folded into either
# (open decision 11).
#
# These are the coded values of `exit_category` in
# inst/lookups/Display_Labels.R, which names them as this stage's vocabulary;
# only *values* keep their own case (CLAUDE.md 4), and the display strings come
# from that lookup at render time, never from here.
.CHECK2_GROUPS <- c("on_time", "prolonged", "disappeared")

# Typography, from code points: R sources must be ASCII (see check1_render.R).
.CHECK2_EM_DASH <- intToUtf8(0x2014)

.CHECK2_MISSING <- "(not reported)"

# Tokens a delivery may use for an affirmative flag. A logical column needs
# none of this; a character or 0/1 column is matched case-insensitively, so a
# state sending "Yes" or 1 is read the same as one sending "Y".
.CHECK2_YES <- c("Y", "YES", "TRUE", "T", "1")

.check2_is_yes <- function(x) {
  if (is.logical(x)) return(x)
  out <- toupper(trimws(as.character(x))) %in% .CHECK2_YES
  out[is.na(x)] <- NA
  out
}

.check2_is_value <- function(x, values) {
  out <- toupper(trimws(as.character(x))) %in% toupper(values)
  out[is.na(x)] <- NA
  out
}

#' The exit indicators for one cohort
#'
#' One row per student in the entry cohort, carrying the nested exit
#' indicators and the on-time / prolonged split. Membership is
#' [build_cohort()]'s and the flags are [add_exit_flags()]'s; this function
#' only joins them for one cohort. The cohort is normally the one Check 1
#' analyzes, taken from [compute_cohorts()] rather than selected again.
#'
#' The indicators are **window-relative**: `NO_EXIT` means not observed
#' exiting by the panel's last year, and a student exiting in year 7 of a
#' six-year timeline is `EXIT_EVER` but not `EXIT_MAX`. `PROLONGED` is late
#' exiters plus `NO_EXIT`. Students who `DISAPPEARED` are in neither group:
#' their outcome is unobserved, and censored is not did-not-exit.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param cohort A single `COHORT_LABEL`, normally the selected row of
#'   [compute_cohorts()].
#' @param start_grade The entry grade those cohorts were built on.
#' @return A tibble, one row per student: `STUDENT_ID`, `COHORT_LABEL`,
#'   `COHORT_START_YEAR`, `EXIT_YEAR_IN_STATUS`, `LAST_YEAR_IN_STATUS`,
#'   `ON_TIME_YEARS`, `EXIT_EVER`, `EXIT_MAX`, `EXIT_ONTIME`, `NO_EXIT`,
#'   `DISAPPEARED`, `ON_TIME`, `PROLONGED`, `EXIT_GROUP` (the three-way
#'   grouping the academic summary and the trajectories condition on).
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' flags <- compute_exit_flags(bundle, spec, co$COHORT_LABEL[co$SELECTED])
#' colSums(flags[c("EXIT_EVER", "EXIT_ONTIME", "NO_EXIT", "DISAPPEARED")])
compute_exit_flags <- function(data, spec, cohort, start_grade = "K") {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.character(cohort), length(cohort) == 1L, !is.na(cohort),
            is.character(start_grade), length(start_grade) == 1L)
  roster <- el_roster(data)

  members <- build_cohort(roster, spec, start_grade = start_grade)
  members <- members[members$COHORT_LABEL == cohort, , drop = FALSE]
  if (!nrow(members)) {
    stop("No cohort labelled '", cohort, "' among the ", start_grade,
         " entry cohorts this panel supports.", call. = FALSE)
  }

  # The window is the panel's, not the cohort's: a student last seen in the
  # panel's final year is still enrolled (NO_EXIT), not disappeared.
  rows <- roster[roster$STUDENT_ID %in% members$STUDENT_ID, , drop = FALSE]
  spans <- build_student_spans(rows, last_year = max(roster$YEAR, na.rm = TRUE))
  flags <- add_exit_flags(spans, spec)
  at <- match(flags$STUDENT_ID, members$STUDENT_ID)
  flags$COHORT_LABEL <- members$COHORT_LABEL[at]
  flags$COHORT_START_YEAR <- members$COHORT_START_YEAR[at]

  # The three-way grouping, disjoint and exhaustive by construction: every
  # student is an on-time exiter, a prolonged case, or unobserved.
  flags$EXIT_GROUP <- ifelse(flags$DISAPPEARED, .CHECK2_GROUPS[[3]],
                             ifelse(flags$ON_TIME, .CHECK2_GROUPS[[1]],
                                    .CHECK2_GROUPS[[2]]))

  flags[c("STUDENT_ID", "COHORT_LABEL", "COHORT_START_YEAR",
          "EXIT_YEAR_IN_STATUS", "LAST_YEAR_IN_STATUS", "ON_TIME_YEARS",
          "EXIT_EVER", "EXIT_MAX", "EXIT_ONTIME", "NO_EXIT", "DISAPPEARED",
          "ON_TIME", "PROLONGED", "EXIT_GROUP")]
}

# ---- the cohort's characteristics at entry -----------------------------------

# The crosstab's row blocks, in the deck's order (slides 63-64). One entry per
# block; the FIELD is a column of the profile frame, never of a raw delivery,
# so the two derived fields (district size band, initial ELP band) sit here
# beside the delivered ones.
#
# TYPE is the load-bearing distinction the deck's blank template hides:
#   "distribution" -- the rows partition the column, so their shares sum to 1;
#   "indicator"    -- each row is an independent share of the column, and the
#                     rows do not sum to anything.
# Slide 63's "Other Demographics" block is three indicator rows (Male, Low
# SES, Special education), not a fourth distribution.
# VALUES is `NA` for a yes/no flag, matched through .check2_is_yes(); otherwise
# a comma-separated list of the values that count as the row, for a categorical
# field where "positive" is one category rather than an affirmative.
.check2_block_declaration <- function() {
  tibble::tribble(
    ~BLOCK,                ~TYPE,          ~FIELD,               ~ROW,                ~VALUES,
    "Primary language",    "distribution", "HOME_LANGUAGE",      NA_character_,       NA_character_,
    "District size",       "distribution", "DISTRICT_SIZE_BAND", NA_character_,       NA_character_,
    "Other demographics",  "indicator",    "GENDER",             "Male",              "M,MALE",
    "Other demographics",  "indicator",    "FRL",                "Low SES",           NA_character_,
    "Other demographics",  "indicator",    "IEP",                "Special education", NA_character_,
    "Type of EL services", "distribution", "SERVICE_MODEL",      NA_character_,       NA_character_,
    "Initial ELP",         "distribution", "ELP_BAND",           NA_character_,       NA_character_
  )
}

#' The cohort's characteristics at entry
#'
#' One row per cohort student, carrying the disaggregation fields the crosstab
#' breaks out. Values are read **at entry** -- the cohort's own start year, the
#' one year every member is current by construction -- so a block describes who
#' the students were when they entered, not a characteristic that moves with
#' them. `[PROPOSED - CONFIRM]`
#'
#' Two fields are derived rather than delivered:
#' * `DISTRICT_SIZE_BAND` -- the student's entry-year district, banded by that
#'   district's current-EL count in that year under
#'   `policy$district_size_bands`. **Never** a delivered `DISTRICT_SIZE_BAND`
#'   column, which in the shipped simulation is a tertile split of something
#'   else (the same rule Prep 2 follows).
#' * `ELP_BAND` -- the initial ELP band, the banded first annual headline
#'   level, taken from the same derivation Check 1's band-split curve uses so
#'   the two stages cannot disagree about a student's band.
#'
#' A field the delivery does not carry is absent from the result, and its block
#' is then absent from the crosstab rather than rendered empty.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param flags Output of [compute_exit_flags()].
#' @return A tibble, one row per student in `flags`: `STUDENT_ID` plus one
#'   column per available field of [compute_crosstab()]'s block declaration.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' flags <- compute_exit_flags(bundle, spec, co$COHORT_LABEL[co$SELECTED])
#' compute_cohort_profile(bundle, spec, flags)
compute_cohort_profile <- function(data, spec, flags) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.data.frame(flags), "STUDENT_ID" %in% names(flags),
            "COHORT_START_YEAR" %in% names(flags))
  roster <- el_roster(data)
  entry_year <- flags$COHORT_START_YEAR[[1]]

  at_entry <- roster[roster$YEAR == entry_year &
                       roster$STUDENT_ID %in% flags$STUDENT_ID, , drop = FALSE]
  at_entry <- at_entry[!duplicated(at_entry$STUDENT_ID), , drop = FALSE]
  at <- match(flags$STUDENT_ID, at_entry$STUDENT_ID)

  out <- tibble::tibble(STUDENT_ID = flags$STUDENT_ID)
  delivered <- c("HOME_LANGUAGE", "GENDER", "FRL", "IEP", "SERVICE_MODEL")
  for (cl in intersect(delivered, names(at_entry))) {
    out[[cl]] <- as.character(at_entry[[cl]])[at]
  }

  # District size band, from the entry year's own district counts.
  bands <- spec$policy$district_size_bands
  if ("DISTRICT_ID" %in% names(at_entry) && !is.null(bands)) {
    cur <- roster[roster$YEAR == entry_year &
                    roster$EL_STATUS %in% "current", , drop = FALSE]
    counts <- dplyr::count(cur[!is.na(cur$DISTRICT_ID), ], .data$DISTRICT_ID,
                           name = "N")
    band <- as.character(cut(counts$N, breaks = bands$breaks,
                             labels = bands$labels, right = FALSE,
                             include.lowest = TRUE))
    out$DISTRICT_SIZE_BAND <- band[match(at_entry$DISTRICT_ID[at],
                                         counts$DISTRICT_ID)]
  }

  # The initial ELP band, from Check 1's derivation rather than a second one.
  band <- .check1_initial_band(data, spec)
  if (nrow(band)) {
    out$ELP_BAND <- band$ELP_BAND[match(flags$STUDENT_ID, band$STUDENT_ID)]
  }
  out
}

# One block's rows for every column, long. `hit` is a logical matrix of
# students x rows; the denominators differ by block type and travel as N_BASE,
# so PCT = N / N_BASE is auditable from the table itself.
.check2_block_rows <- function(block, type, rows, hit, base, flags, spec,
                               fields = NA_character_) {
  cols <- lapply(.CHECK2_COLUMNS, function(cl) {
    if (identical(cl, "ALL")) rep(TRUE, nrow(flags)) else flags[[cl]] %in% TRUE
  })
  names(cols) <- .CHECK2_COLUMNS

  grid <- expand.grid(ROW = seq_along(rows), COLUMN = .CHECK2_COLUMNS,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  n <- vapply(seq_len(nrow(grid)), function(i) {
    sum(cols[[grid$COLUMN[[i]]]] & hit[, grid$ROW[[i]]], na.rm = TRUE)
  }, 0L)
  n_base <- vapply(seq_len(nrow(grid)), function(i) {
    sum(cols[[grid$COLUMN[[i]]]] & base[, grid$ROW[[i]]], na.rm = TRUE)
  }, 0L)

  tibble::tibble(
    BLOCK      = block,
    BLOCK_TYPE = type,
    # The roster column a row's value came from, carried so the document can
    # label coded values without guessing which lookup to use.
    FIELD      = rep_len(fields, length(rows))[grid$ROW],
    ROW        = rows[grid$ROW],
    COLUMN     = grid$COLUMN,
    N          = as.integer(n),
    N_BASE     = as.integer(n_base),
    PCT        = ifelse(n_base > 0L, n / n_base, NA_real_)
  )
}

# Suppression with the zero exception, used by every slot in this stage so they
# cannot disagree about what a blank cell means.
#
# A zero is not a small cell. apply_suppression() blanks it, which reads as
# "withheld" for a cell that is simply empty -- and in this stage an empty cell
# is usually a finding in its own right: the on-time group has no ELP rows once
# it has exited, and a reader has to be able to tell "nobody left to measure"
# from "too few to report". Prep 2 makes the same exception
# (R/prep2_compute.R).
.check2_suppress_zero_ok <- function(x, spec, n_col, cols) {
  raw <- x
  out <- apply_suppression(x, spec, n_col = n_col, cols = cols, blank_n = TRUE)
  empty <- !is.na(raw[[n_col]]) & raw[[n_col]] == 0L
  out[[n_col]][empty] <- raw[[n_col]][empty]
  for (cl in intersect(cols, names(out))) out[[cl]][empty] <- raw[[cl]][empty]
  out$SUPPRESSED[empty] <- FALSE
  out
}

.check2_suppress <- function(out, spec) {
  out <- .check2_suppress_zero_ok(out, spec, n_col = "N", cols = "PCT")
  out[c("BLOCK", "BLOCK_TYPE", "FIELD", "ROW", "COLUMN", "N", "N_BASE", "PCT",
        "SUPPRESSED")]
}

#' The slide-63/64 crosstab
#'
#' Columns **All / Exit_Ever / Exit_Max / Exit_OnTime / No_Exit /
#' Disappeared**, in long form: one row per block, row and column. The
#' `"Total"` block is the count of students in each column (`N`) and that count
#' as a share of the whole cohort (`PCT`, the deck's "% of all EL students"
#' row). Passing a `profile` adds the characteristic blocks -- primary
#' language, district size band, the Male / low-SES / special-education
#' indicators, type of EL services, initial ELP band -- as **column
#' percentages**: read down a block within one column.
#'
#' `BLOCK_TYPE` says how a block's rows relate to each other, which the deck's
#' blank template leaves implicit. A `"distribution"` block's rows partition
#' the column and sum to 1 (a category for students whose value is not
#' reported keeps that true); an `"indicator"` block's rows are independent
#' shares and sum to nothing. `N_BASE` carries the denominator each `PCT` was
#' taken over, so no share in the table has to be taken on trust: for a
#' distribution it is the column total, for an indicator the students in that
#' column whose value is reported.
#'
#' Suppression follows the spec's `min_cell`: a nonzero count below it is
#' `NA`, along with its share, and the row is kept. A count of **zero** is
#' shown as zero rather than suppressed: it is not a small cell, and blanking
#' it would tell the reader a column was hidden when it is simply empty.
#' `[PROPOSED - CONFIRM]`
#'
#' @param flags Output of [compute_exit_flags()].
#' @param spec An `el_spec`.
#' @param profile Output of [compute_cohort_profile()], or `NULL` for the
#'   `"Total"` block alone.
#' Row values stay in the delivery's own codes; `FIELD` names the roster column
#' each came from, so the document can label them through
#' `inst/lookups/Display_Labels.R` without guessing.
#'
#' @return A tibble: `BLOCK`, `BLOCK_TYPE`, `FIELD`, `ROW`, `COLUMN` (`ALL`,
#'   `EXIT_EVER`, `EXIT_MAX`, `EXIT_ONTIME`, `NO_EXIT`, `DISAPPEARED`), `N`,
#'   `N_BASE`, `PCT`, `SUPPRESSED`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' flags <- compute_exit_flags(bundle, spec, co$COHORT_LABEL[co$SELECTED])
#' compute_crosstab(flags, spec, compute_cohort_profile(bundle, spec, flags))
compute_crosstab <- function(flags, spec, profile = NULL) {
  stopifnot(is.data.frame(flags), inherits(spec, "el_spec"),
            all(.CHECK2_FLAG_COLUMNS %in% names(flags)),
            is.null(profile) || is.data.frame(profile))

  n_all <- nrow(flags)
  n <- c(n_all, vapply(.CHECK2_FLAG_COLUMNS,
                       function(cl) sum(flags[[cl]] %in% TRUE), 0L))

  total <- tibble::tibble(
    BLOCK      = "Total",
    BLOCK_TYPE = "total",
    FIELD      = NA_character_,
    ROW        = "Total students",
    COLUMN     = .CHECK2_COLUMNS,
    N          = as.integer(n),
    N_BASE     = as.integer(n_all),
    PCT        = if (n_all > 0L) as.numeric(n) / n_all else NA_real_
  )
  blocks <- list(.check2_suppress(total, spec))

  if (!is.null(profile)) {
    profile <- profile[match(flags$STUDENT_ID, profile$STUDENT_ID), ,
                       drop = FALSE]
    decl <- .check2_block_declaration()
    for (b in unique(decl$BLOCK)) {
      part <- decl[decl$BLOCK == b, , drop = FALSE]
      part <- part[part$FIELD %in% names(profile), , drop = FALSE]
      if (!nrow(part)) next                # [field not delivered: block omitted]
      type <- part$TYPE[[1]]

      if (identical(type, "distribution")) {
        x <- profile[[part$FIELD[[1]]]]
        x <- ifelse(is.na(x), .CHECK2_MISSING, as.character(x))
        rows <- .check2_row_order(x, part$FIELD[[1]], spec)
        hit <- vapply(rows, function(r) x == r, logical(length(x)))
        # Every student counts toward a distribution's denominator, which is
        # what makes the shares sum to one.
        base <- matrix(TRUE, nrow = length(x), ncol = length(rows))
      } else {
        rows <- part$ROW
        hit <- vapply(seq_len(nrow(part)), function(i) {
          v <- profile[[part$FIELD[[i]]]]
          vals <- part$VALUES[[i]]
          if (is.na(vals)) .check2_is_yes(v)
          else .check2_is_value(v, strsplit(vals, ",", fixed = TRUE)[[1]])
        }, logical(nrow(profile)))
        # A student whose value is not reported is not counted as a "no": they
        # leave that row's denominator instead.
        base <- vapply(seq_len(nrow(part)), function(i) {
          !is.na(profile[[part$FIELD[[i]]]])
        }, logical(nrow(profile)))
      }
      if (!is.matrix(hit)) hit <- matrix(hit, nrow = nrow(profile))
      if (!is.matrix(base)) base <- matrix(base, nrow = nrow(profile))
      hit[is.na(hit)] <- FALSE

      blocks[[length(blocks) + 1L]] <- .check2_suppress(
        .check2_block_rows(b, type, rows, hit, base, flags, spec,
                           fields = part$FIELD), spec)
    }
  }
  dplyr::bind_rows(blocks)
}

# Display order within a distribution block: the spec's own band order where
# the field has one, otherwise by size, with the not-reported row last. Band
# labels and language codes are never typed here.
.check2_row_order <- function(x, field, spec) {
  present <- unique(x)
  known <- if (identical(field, "ELP_BAND")) {
    spec_bands(spec)$labels %||% spec$elp$level_scale$levels
  } else if (identical(field, "DISTRICT_SIZE_BAND")) {
    spec$policy$district_size_bands$labels
  } else NULL

  ordered <- if (!is.null(known)) {
    c(intersect(known, present),
      sort(setdiff(present, c(known, .CHECK2_MISSING))))
  } else {
    tab <- sort(table(x[x != .CHECK2_MISSING]), decreasing = TRUE)
    names(tab)
  }
  c(ordered, intersect(.CHECK2_MISSING, present))
}

# ---- comparable points in each group's trajectory ----------------------------

# Year in status for a cohort student-year. Every member entered in the same
# year by construction, so this is arithmetic on the cohort's start year and
# needs no per-student dating -- and it matches the life table's rows.
.check2_year_in_status <- function(year, entry_year) {
  as.integer(year - entry_year + 1L)
}

# One row per group and year in status, groups in display order and empty
# combinations kept, so a group that stops being observed reads as a gap rather
# than dropping out of the table.
.check2_group_grid <- function(groups, years) {
  present <- intersect(.CHECK2_GROUPS, unique(groups))
  grid <- expand.grid(YEAR_IN_STATUS = years, EXIT_GROUP = present,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  tibble::tibble(EXIT_GROUP = grid$EXIT_GROUP,
                 YEAR_IN_STATUS = as.integer(grid$YEAR_IN_STATUS))
}

# The most common value, ties to the first sorted: a label for a row, not a
# statistic about students.
.check2_mode <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

#' Academic achievement at comparable points in each group's trajectory
#'
#' The descriptive companion to the crosstab: how on-time and prolonged
#' students were doing on the content assessment at the **same year in
#' status** -- the comparable point this package uses (open decision 12).
#' Pooling by calendar year instead would compare students at different stages
#' of their own trajectories.
#'
#' The headline is the proficiency rate, from `ACHIEVEMENT_LEVEL` against
#' `policy$academic_benchmark$proficient_level`: it means the same thing in
#' every grade, which a scale score does not. The median scale score has the
#' modal grade beside it so a reader can see whether a row is a within-grade
#' comparison -- for a single-entry-grade cohort each year in status is mostly
#' one grade, which is what makes the row readable at all. Students with no
#' level that year, which includes alternate-academic takers, are in `N` and out
#' of `N_SCORED` and the rate.
#'
#' A year in status the content assessment does not reach -- grades below the
#' spec's tested span, and a missing assessment year -- has no rows and is
#' simply absent.
#'
#' **Read this as description, not as cause.** On-time exiters left because they
#' cleared an English proficiency bar, so finding them academically stronger is
#' partly definitional; what the stage asks is whether the gap is bigger than
#' that explains. On simulated data the gap is a parameter of the generator
#' (`sim_contract.md` 5.2-5.3), never a finding.
#'
#' @param data An `el_data` bundle with an academic table.
#' @param spec An `el_spec`.
#' @param flags Output of [compute_exit_flags()].
#' @param subject Academic subject. Defaults to the spec's first.
#' @return A tibble: `EXIT_GROUP`, `YEAR_IN_STATUS`, `GRADE_MODAL`, `N`,
#'   `N_SCORED`, `PCT_PROFICIENT`, `MEDIAN_SCALE`, `SUPPRESSED`; attributes
#'   `subject` and `proficient_level`. `NULL` without an academic table.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim,
#'                      academic = academic_long_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' flags <- compute_exit_flags(bundle, spec, co$COHORT_LABEL[co$SELECTED])
#' compute_academic_summary(bundle, spec, flags)
compute_academic_summary <- function(data, spec, flags, subject = NULL) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.data.frame(flags), "EXIT_GROUP" %in% names(flags))
  ac <- el_academic(data)
  if (is.null(ac)) return(NULL)            # [REQUIRES the academic table]

  subjects <- spec_academic_subjects(spec)
  if (is.null(subject)) subject <- subjects[[1]]
  if (!subject %in% subjects) {
    stop("Subject '", subject, "' is not in the spec's academic block.",
         call. = FALSE)
  }
  proficient <- spec$policy$academic_benchmark$proficient_level

  entry_year <- flags$COHORT_START_YEAR[[1]]
  d <- ac[ac$SUBJECT %in% subject & ac$STUDENT_ID %in% flags$STUDENT_ID, ,
          drop = FALSE]
  if (!nrow(d)) return(NULL)               # [no academic rows for this cohort]
  d$YEAR_IN_STATUS <- .check2_year_in_status(d$YEAR, entry_year)
  d$EXIT_GROUP <- flags$EXIT_GROUP[match(d$STUDENT_ID, flags$STUDENT_ID)]
  d <- d[d$YEAR_IN_STATUS >= 1L, , drop = FALSE]
  if (!nrow(d)) return(NULL)               # [no rows inside the cohort's window]

  out <- d |>
    dplyr::group_by(.data$EXIT_GROUP, .data$YEAR_IN_STATUS) |>
    dplyr::summarise(
      GRADE_MODAL    = .check2_mode(.data$GRADE),
      N              = dplyr::n(),
      N_SCORED       = sum(!is.na(.data$ACHIEVEMENT_LEVEL)),
      PCT_PROFICIENT = if (is.null(proficient)) NA_real_ else {
        mean(.data$ACHIEVEMENT_LEVEL[!is.na(.data$ACHIEVEMENT_LEVEL)] >=
               proficient)
      },
      MEDIAN_SCALE   = stats::median(.data$SCALE_SCORE, na.rm = TRUE),
      .groups = "drop")

  grid <- .check2_group_grid(d$EXIT_GROUP, sort(unique(d$YEAR_IN_STATUS)))
  out <- dplyr::left_join(grid, out, by = c("EXIT_GROUP", "YEAR_IN_STATUS"))
  out$N <- dplyr::coalesce(out$N, 0L)
  out$N_SCORED <- dplyr::coalesce(out$N_SCORED, 0L)

  out <- .check2_suppress_zero_ok(out, spec, n_col = "N_SCORED",
                                  cols = c("PCT_PROFICIENT", "MEDIAN_SCALE"))
  out$N[out$SUPPRESSED] <- NA
  attr(out, "subject") <- subject
  attr(out, "proficient_level") <- proficient
  out[c("EXIT_GROUP", "YEAR_IN_STATUS", "GRADE_MODAL", "N", "N_SCORED",
        "PCT_PROFICIENT", "MEDIAN_SCALE", "SUPPRESSED")]
}

#' ELP trajectories, conditioned on exit group
#'
#' The reported ELP level by year in status, **always split by exit group**.
#' That split is the point of the view, and the reason this function has no
#' unconditioned form: a single cohort mean over time falls for two reasons it
#' cannot tell apart. It is not necessarily that the same students are growing
#' less; it is also that the students who grow fastest exit, so the mean is
#' taken over a progressively slower-growing remainder (deck slide 81).
#' `$composition` carries that mechanism as data -- each group's share of the
#' students still measured -- so the caution can be shown rather than asserted.
#'
#' Students hold ELP rows only while they are current: formers carry no ELP at
#' all (`sim_contract.md` 4.4). An on-time group's line therefore stops near its
#' exit years, and `N` is what says so.
#'
#' `NULL` when no annual headline level resolves for the cohort, and for a
#' categorical level scale, which has no mean.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param flags Output of [compute_exit_flags()].
#' @return A list: `levels` (`EXIT_GROUP`, `YEAR_IN_STATUS`, `N`, `MEAN_LEVEL`,
#'   `MEDIAN_LEVEL`, `SUPPRESSED`) and `composition` (`EXIT_GROUP`,
#'   `YEAR_IN_STATUS`, `N`, `PCT_OF_MEASURED`); or `NULL`.
#' @export
#' @examples
#' spec <- read_el_spec("00_SIM_EL_Spec")
#' bundle <- as_el_data(el_long_sim, roster = el_roster_sim, spec = spec)
#' co <- compute_cohorts(bundle, spec)
#' flags <- compute_exit_flags(bundle, spec, co$COHORT_LABEL[co$SELECTED])
#' compute_trajectories(bundle, spec, flags)$levels
compute_trajectories <- function(data, spec, flags) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.data.frame(flags), "EXIT_GROUP" %in% names(flags))
  # A categorical level scale is stored as the ordinal index into its labels
  # (sim_contract.md 4.2), so it arrives numeric and would average without
  # complaint -- but the average of "Emerging" and "Proficient" is not a level.
  # The spec, not the column's storage type, is what says which scale this is.
  if (identical(spec$elp$level_scale$type, "categorical")) return(NULL)

  h <- elp_headline(data, spec)
  h <- h[h$STUDENT_ID %in% flags$STUDENT_ID & !is.na(h$ACHIEVEMENT_LEVEL), ,
         drop = FALSE]
  if (!nrow(h)) return(NULL)               # [no annual headline for this cohort]
  if (!is.numeric(h$ACHIEVEMENT_LEVEL)) return(NULL)

  entry_year <- flags$COHORT_START_YEAR[[1]]
  h$YEAR_IN_STATUS <- .check2_year_in_status(h$YEAR, entry_year)
  h$EXIT_GROUP <- flags$EXIT_GROUP[match(h$STUDENT_ID, flags$STUDENT_ID)]
  h <- h[h$YEAR_IN_STATUS >= 1L, , drop = FALSE]
  if (!nrow(h)) return(NULL)               # [no rows inside the cohort's window]

  lv <- h |>
    dplyr::group_by(.data$EXIT_GROUP, .data$YEAR_IN_STATUS) |>
    dplyr::summarise(N = dplyr::n(),
                     MEAN_LEVEL = mean(.data$ACHIEVEMENT_LEVEL),
                     MEDIAN_LEVEL = stats::median(.data$ACHIEVEMENT_LEVEL),
                     .groups = "drop")
  grid <- .check2_group_grid(h$EXIT_GROUP, sort(unique(h$YEAR_IN_STATUS)))
  lv <- dplyr::left_join(grid, lv, by = c("EXIT_GROUP", "YEAR_IN_STATUS"))
  lv$N <- dplyr::coalesce(lv$N, 0L)
  lv <- .check2_suppress_zero_ok(lv, spec, n_col = "N",
                                 cols = c("MEAN_LEVEL", "MEDIAN_LEVEL"))

  # The mechanism behind the caution: who is still being measured each year.
  comp <- lv[c("EXIT_GROUP", "YEAR_IN_STATUS", "N")] |>
    dplyr::group_by(.data$YEAR_IN_STATUS) |>
    dplyr::mutate(PCT_OF_MEASURED = if (sum(.data$N, na.rm = TRUE) > 0) {
      .data$N / sum(.data$N, na.rm = TRUE)
    } else NA_real_) |>
    dplyr::ungroup()

  list(levels = lv[c("EXIT_GROUP", "YEAR_IN_STATUS", "N", "MEAN_LEVEL",
                     "MEDIAN_LEVEL", "SUPPRESSED")],
       composition = comp[c("EXIT_GROUP", "YEAR_IN_STATUS", "N",
                            "PCT_OF_MEASURED")])
}

# ---- Additional check (Types 2-6): Prep 2's blocked groups -------------------

#' Blocked-by-one-criterion groups as extra comparison columns
#'
#' **Type-gated** (Types 2-6; `NULL` for a Type 1 rule). Prep 2's
#' blocked-by-exactly-one-criterion student-years, narrowed to this cohort and
#' resolved to one group per student, then crossed with the exit outcomes. A
#' group that looks systematically different from the cohort as a whole means
#' that criterion is screening on something other than what the main score
#' screens on.
#'
#' The groups come from [compute_criteria_crosswalk()] -- Prep 2's own
#' computation, called rather than repeated, so the two stages cannot disagree
#' about who is blocked by what. A student blocked by different criteria in
#' different years takes the one that blocked them **most often**, ties going to
#' the first alphabetically; a student never blocked by exactly one criterion is
#' in no group.
#'
#' @param data An `el_data` bundle.
#' @param spec An `el_spec`.
#' @param flags Output of [compute_exit_flags()].
#' @return `NULL` unless a Type 2-6 rule yields blocked groups inside this
#'   cohort; otherwise a tibble: `BLOCKED_BY`, `COLUMN`, `N`, `N_BASE`, `PCT`,
#'   `SUPPRESSED`, where `PCT` is the group's share of that exit column.
#' @export
compute_blocked_groups <- function(data, spec, flags) {
  stopifnot(inherits(data, "el_data"), inherits(spec, "el_spec"),
            is.data.frame(flags), "STUDENT_ID" %in% names(flags))
  if (!spec_has_type(spec, 2:6)) return(NULL)   # [Type-gated: Types 2-6 only]

  cw <- compute_criteria_crosswalk(data, spec)
  if (is.null(cw) || !nrow(cw$blocked_groups)) return(NULL)  # [no blocked groups]

  bg <- cw$blocked_groups
  bg <- bg[bg$STUDENT_ID %in% flags$STUDENT_ID, , drop = FALSE]
  if (!nrow(bg)) return(NULL)              # [none of them are in this cohort]

  # One group per student: the criterion that blocked them most often.
  tab <- dplyr::count(bg, .data$STUDENT_ID, .data$BLOCKED_BY, name = "N")
  tab <- tab[order(tab$STUDENT_ID, -tab$N, tab$BLOCKED_BY), ]
  tab <- tab[!duplicated(tab$STUDENT_ID), , drop = FALSE]

  groups <- sort(unique(tab$BLOCKED_BY))
  member <- tab$BLOCKED_BY[match(flags$STUDENT_ID, tab$STUDENT_ID)]
  hit <- vapply(groups, function(g) !is.na(member) & member == g,
                logical(nrow(flags)))
  if (!is.matrix(hit)) hit <- matrix(hit, nrow = nrow(flags))
  base <- matrix(TRUE, nrow = nrow(flags), ncol = length(groups))

  out <- .check2_block_rows("Blocked by one criterion", "indicator",
                            paste0(groups, " only"), hit, base, flags, spec)
  out <- .check2_suppress(out, spec)
  out$BLOCKED_BY <- out$ROW
  out[c("BLOCKED_BY", "COLUMN", "N", "N_BASE", "PCT", "SUPPRESSED")]
}
