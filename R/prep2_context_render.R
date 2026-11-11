# Prep 2 -- sentence builders for the context deck (decks/context.qmd).
# Each takes computed results and returns display text; nothing is computed
# here beyond choosing which already-computed values a sentence names
# (CLAUDE.md 8). The deck holds the layout and the authored wording around
# these.
#
# Coded roster values (`spa`, `esl_pullout`) are labelled through
# inst/lookups/Display_Labels.R. The reader is restated from check2_render.R
# rather than called, because it is internal to that prefix (CLAUDE.md 14); a
# test asserts the two agree. A value absent from the lookup renders as itself.

.p2c_labels <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    path <- system.file("lookups", "Display_Labels.R", package = "stateELP")
    cache <<- if (nzchar(path) && file.exists(path)) {
      env <- new.env(parent = baseenv())
      ok <- tryCatch({ sys.source(path, envir = env); TRUE },
                     error = function(e) FALSE)
      if (ok && !is.null(env$Display_Labels)) env$Display_Labels else list()
    } else list()
    cache
  }
})

.p2c_label <- function(x, key) {
  lk <- .p2c_labels()[[key]]
  out <- as.character(x)
  if (is.null(lk)) return(out)
  hit <- !is.na(out) & out %in% names(lk)
  out[hit] <- unname(lk[out[hit]])
  out
}

.P2C_ARROW <- intToUtf8(0x2192)

.p2c_n <- function(n) {
  ifelse(is.na(n), "*", format(round(n), big.mark = ",", scientific = FALSE, trim = TRUE))
}
.p2c_pct1 <- function(p) ifelse(is.na(p), "*", sprintf("%.1f%%", 100 * p))
.p2c_pct0 <- function(p) .p2c_pct_label(p)

# A proportional change as words: 0.39 -> "up 39%", -0.05 -> "down 5%".
.p2c_change <- function(r) {
  if (is.na(r)) return("change not reportable")
  if (r >= 2) return("more than tripled")
  if (r >= 1) return("more than doubled")
  k <- round(100 * abs(r))
  if (k == 0) return("about unchanged")
  paste0(if (r > 0) "up " else "down ", k, "%")
}

.p2c_cap_first <- function(s) paste0(toupper(substr(s, 1, 1)), substring(s, 2))
.p2c_low_first <- function(s) paste0(tolower(substr(s, 1, 1)), substring(s, 2))

# "grades 4-5" when the two are next to each other in `lv`, else
# "grades 4 and 9"; the pair is put in `lv` order first.
.p2c_pair <- function(unit, g, lv) {
  g <- g[order(match(g, lv))]
  joint <- if (diff(match(g, lv)) == 1L && !any(grepl("+", g, fixed = TRUE))) {
    paste0(g[[1]], .P2_ENDASH, g[[2]])
  } else {
    paste(g[[1]], "and", g[[2]])
  }
  paste0(unit, "s ", joint)
}

# The rows of an exit-rate slot a sentence reads: pooled when the pool spans
# more than one year (and `prefer_pooled`), else the latest year.
.p2c_exit_rows <- function(x, prefer_pooled) {
  pooled <- attr(x, "pooled")
  use_pool <- prefer_pooled && !is.null(pooled) && pooled[[1]] != pooled[[2]]
  rows <- if (use_pool) x[x$POOLED, , drop = FALSE]
          else x[!x$POOLED & x$FIRST_YEAR == attr(x, "latest"), , drop = FALSE]
  period <- if (use_pool) {
    paste0("pooled, ", .p2c_school_year(pooled[[1]]), " to ",
           .p2c_school_year(pooled[[2]]))
  } else .p2c_school_year(attr(x, "latest"))
  list(rows = rows[!is.na(rows$PCT_EXITED), , drop = FALSE], period = period)
}

#' Sentences for the context deck
#'
#' The deck's callouts that have to *choose* something from a computed table
#' -- which grades have the highest exit rates, which grade is largest -- are
#' built here, from [run_prep2_context()]'s results, so the choice is code
#' and not prose. Each element is markdown (a bold lead, then the rest), or
#' `NULL` when its slot is absent or too thin to rank (fewer than three
#' reportable groups).
#'
#' The district callouts are returned as pieces for the deck's cards: a short
#' bold lead and the sentence that follows it.
#'
#' @param res The list [run_prep2_context()] returns.
#' @param state Display name for the state.
#' @return A named list: `exit_grade`, `exit_years` (one string each);
#'   `grade` (three strings: the total and the largest and smallest grades);
#'   `share` (`state`, `high`, `low`: the statewide EL share and the districts
#'   in the top and bottom share bands); `small` and `top` (each `lead` and
#'   `body`: districts under the spec's small-district size, and how few
#'   districts hold two-thirds of ELs); `below` and `above` (each `lead` and
#'   `body`: the districts on either side of the average).
#' @export
prep2_context_notes <- function(res, state = "the state's") {
  stopifnot(is.list(res), !is.null(res$meta))
  out <- list(exit_grade = NULL, exit_years = NULL, grade = NULL, share = NULL,
              small = NULL, top = NULL, below = NULL, above = NULL)
  dn <- .p2c_district_notes(res, state)
  out[names(dn)] <- dn

  x <- res$exit_by_grade
  if (!is.null(x)) {
    r <- .p2c_exit_rows(x, prefer_pooled = TRUE)
    d <- r$rows
    if (nrow(d) >= 3L) {
      o <- order(-d$PCT_EXITED)
      top <- d[o[1:2], , drop = FALSE]
      top <- top[order(match(top$GRADE, .P2_GRADES)), , drop = FALSE]
      rest <- max(d$PCT_EXITED[o[-(1:2)]])
      out$exit_grade <- sprintf(
        "**Exit rates peak in %s** (%s and %s; %s) and are %s or lower in every other grade.",
        .p2c_pair("grade", top$GRADE, .P2_GRADES),
        .p2c_pct0(top$PCT_EXITED[[1]]), .p2c_pct0(top$PCT_EXITED[[2]]), r$period,
        .p2c_pct0(rest))
    }
  }

  x <- res$exit_by_years
  if (!is.null(x)) {
    r <- .p2c_exit_rows(x, prefer_pooled = FALSE)
    d <- r$rows
    lv <- unique(x$YEARS_IN_STATUS)
    if (nrow(d) >= 4L) {
      o <- order(-d$PCT_EXITED)
      hi <- d[o[1:2], , drop = FALSE]
      lo <- d[rev(o)[1:2], , drop = FALSE]
      hi <- hi[order(match(hi$YEARS_IN_STATUS, lv)), , drop = FALSE]
      lo <- lo[order(match(lo$YEARS_IN_STATUS, lv)), , drop = FALSE]
      out$exit_years <- sprintf(
        "**Exit rates peak in %s in status** (%s and %s) and are lowest in %s (%s and %s), %s.",
        .p2c_pair("year", hi$YEARS_IN_STATUS, lv),
        .p2c_pct0(hi$PCT_EXITED[[1]]), .p2c_pct0(hi$PCT_EXITED[[2]]),
        .p2c_pair("year", lo$YEARS_IN_STATUS, lv),
        .p2c_pct0(lo$PCT_EXITED[[1]]), .p2c_pct0(lo$PCT_EXITED[[2]]), r$period)
    }
  }

  g <- res$by_grade
  if (!is.null(g)) {
    yr <- res$meta$latest_year
    d <- g[g$YEAR == yr & !is.na(g$N), , drop = FALSE]
    total <- res$meta$n_current_latest
    off <- res$meta$n_current_off_grid
    if (nrow(d) && !is.null(total) && !is.na(total)) {
      big <- d[which.max(d$N), ]
      small <- d[which.min(d$N), ]
      gl <- function(z) if (z == "K") "kindergarten" else paste("grade", z)
      out$grade <- c(
        sprintf("**%s EL students** in %s%s", .p2c_n(total), .p2c_school_year(yr),
                if (is.null(off) || is.na(off)) sprintf(", grades K%s12", .P2_ENDASH)
                else if (off > 0)
                  sprintf(" (%s outside grades K%s12)", .p2c_n(off), .P2_ENDASH)
                else ""),
        sprintf("Largest grade = **%s** (%s EL students)", gl(big$GRADE), .p2c_n(big$N)),
        sprintf("Smallest grade = **%s** (%s EL students)", gl(small$GRADE), .p2c_n(small$N)))
    }
  }
  out
}

# The district cards. Wording turns on zero: "no district" rather than "0
# districts", and "0% of districts" never appears.
.p2c_district_notes <- function(res, state) {
  out <- list()
  nd <- function(k, what) {
    if (k == 0L) paste("no district", sub("^have", "has", what))
    else if (k == 1L) paste("1 district", sub("^have", "has", what))
    else paste(.p2c_n(k), "districts", what)
  }

  ds <- res$district_share
  if (!is.null(ds)) {
    k <- nrow(ds)
    pct_state <- attr(ds, "pct_state")
    out$share <- list(
      state = if (is.null(pct_state) || is.na(pct_state)) NULL else
        sprintf("**%s** of %s students are EL students", .p2c_pct1(pct_state), state),
      high = nd(ds$N_DISTRICTS[[k]], paste("have", ds$SHARE_BAND[[k]], "EL students")),
      low = nd(ds$N_DISTRICTS[[1]],
               paste("have", .p2c_low_first(sub("^Less than", "under", ds$SHARE_BAND[[1]])),
                     "EL students")))
  }

  cc <- res$concentration
  if (!is.null(cc)) {
    sm <- attr(cc, "small")
    if (!is.null(sm)) {
      n_small <- .p2c_n(sm$N)
      out$small <- if (sm$PCT_DISTRICTS == 0) {
        list(lead = "No district",
             body = sprintf("has fewer than %s EL students", n_small))
      } else {
        list(lead = sprintf("%s of districts", .p2c_pct0(sm$PCT_DISTRICTS)),
             body = sprintf("have fewer than %s EL students, and enroll %s of EL students together",
                            n_small, if (is.na(sm$PCT)) "*" else .p2c_pct1(sm$PCT)))
      }
    }
    tp <- attr(cc, "top")
    if (!is.null(tp) && !is.na(tp$N_DISTRICTS)) {
      share_word <- if (isTRUE(all.equal(tp$SHARE, 2 / 3))) "Two-thirds"
                    else .p2c_pct0(tp$SHARE)
      out$top <- list(
        lead = paste(share_word, "of EL students"),
        body = sprintf("attend just %s of %s districts, each with %s or more EL students",
                       tp$N_DISTRICTS, attr(cc, "n_districts"), .p2c_n(tp$MIN_N)))
    }
  }

  cm <- res$concentration_by_mean
  sp <- if (is.null(cm)) NULL else attr(cm, "split")
  if (!is.null(sp)) {
    side <- function(z) sprintf("%s of districts, %s of EL students",
                                .p2c_pct0(z$PCT_DISTRICTS),
                                if (is.na(z$PCT)) "*" else .p2c_pct0(z$PCT))
    out$below <- list(lead = "Below average", body = side(sp$below))
    out$above <- list(lead = "Above average", body = side(sp$above))
  }
  out
}

#' Display text for one district profile
#'
#' The strings one district's profile slide shows, from
#' [compute_district_profiles()]: a title, three headline cards (EL share of
#' enrollment, EL count over the panel, exit rate against the state's) and the
#' detail rows (languages, service models, schools, former ELs). A value that
#' is suppressed or unavailable prints as `*` or drops its clause.
#'
#' @param profiles The list [compute_district_profiles()] returns (the
#'   `$profiles` slot of [run_prep2_context()]).
#' @param id A `DISTRICT_ID` in `profiles$summary`.
#' @param label Display name for the district; defaults to `id`.
#' @param state Display name for the state, for the exit-rate comparison.
#' @param population The exit-rate population the profiles were computed on
#'   (`res$meta$population`), so the card says whom the rate counts.
#' @return A list: `title`, `cards` (three lists of `big` and `small`), and
#'   `rows` (a named character vector).
#' @export
prep2_profile_text <- function(profiles, id, label = NULL, state = "the state",
                               population = "tested") {
  stopifnot(is.list(profiles), is.data.frame(profiles$summary))
  s <- profiles$summary[profiles$summary$DISTRICT_ID == id, , drop = FALSE]
  if (nrow(s) != 1L) stop("No profile for district '", id, "'.", call. = FALSE)
  if (is.null(label) || !nzchar(label)) label <- id
  sy <- .p2c_school_year
  setting <- if (is.na(s$SETTING)) "" else paste0(.p2c_low_first(s$SETTING), ", ")
  title <- sprintf("%s: %s%s ELs", label, setting, .p2c_n(s$N_EL))

  trend <- function(now, then) {
    if (is.na(now) || is.na(then)) return("")
    word <- if (round(1000 * now) > round(1000 * then)) "up from"
            else if (round(1000 * now) < round(1000 * then)) "down from" else "the same as"
    sprintf(", %s %s in %s", word, .p2c_pct1(then), sy(s$FIRST_YEAR))
  }
  card1 <- if (is.na(s$PCT_EL)) {
    list(big = "*", small = "EL share of enrollment not reportable")
  } else {
    list(big = .p2c_pct1(s$PCT_EL),
         small = sprintf("of students are ELs in %s%s", sy(s$YEAR),
                         trend(s$PCT_EL, s$PCT_EL_FIRST)))
  }
  card2 <- list(
    big = sprintf("%s %s %s", .p2c_n(s$N_EL_FIRST), .P2C_ARROW, .p2c_n(s$N_EL)),
    small = paste0(
      sprintf("ELs, %s to %s (%s)", sy(s$FIRST_YEAR), sy(s$YEAR), .p2c_change(s$EL_CHANGE)),
      if (!is.na(s$ENROLLMENT_CHANGE))
        sprintf("; all students %s", .p2c_change(s$ENROLLMENT_CHANGE)) else "",
      if (!is.na(s$PCT_OF_STATE_EL))
        sprintf("; %s of the state's ELs", .p2c_pct1(s$PCT_OF_STATE_EL)) else ""))
  who <- if (identical(population, "tested")) "tested ELs" else "ELs"
  card3 <- list(
    big = .p2c_pct1(s$PCT_EXITED),
    small = paste0(
      sprintf("of %s in %s exited the next year", who, sy(s$EXIT_YEAR)),
      if (!is.na(s$PCT_EXITED_FIRST) && !identical(s$EXIT_FIRST_YEAR, s$EXIT_YEAR))
        sprintf(" (%s in %s)", .p2c_pct1(s$PCT_EXITED_FIRST), sy(s$EXIT_FIRST_YEAR)) else "",
      if (!is.na(s$STATE_PCT_EXITED))
        sprintf("; %s %s", state, .p2c_pct1(s$STATE_PCT_EXITED)) else ""))

  rows <- character()
  top3 <- function(tab, field) {
    if (is.null(tab)) return(NULL)
    t <- tab[tab$DISTRICT_ID == id & !is.na(tab$PCT), , drop = FALSE]
    t <- utils::head(t[order(-t$PCT), , drop = FALSE], 3L)
    if (!nrow(t)) return(NULL)
    sprintf("%s (%s)", .p2c_label(t[[field]], field), .p2c_pct0(t$PCT))
  }
  l3 <- top3(profiles$languages, "HOME_LANGUAGE")
  if (length(l3)) {
    rows[["Languages"]] <- sprintf(
      "%s %s among %s home languages of current ELs",
      if (length(l3) > 1L) paste(paste(utils::head(l3, -1L), collapse = ", "), "and",
                                 utils::tail(l3, 1L)) else l3,
      if (length(l3) > 1L) "lead" else "leads",
      s$N_LANGUAGES)
  }
  m3 <- top3(profiles$service_models, "SERVICE_MODEL")
  if (length(m3)) {
    n_models <- sum(profiles$service_models$DISTRICT_ID == id)
    rows[["Service models"]] <- sprintf("Most common of %d: %s", n_models,
                                        paste(m3, collapse = "; "))
  }
  if (!is.na(s$N_SCHOOLS)) {
    rows[["Schools"]] <- if (is.na(s$N_SCHOOLS_SHOWN) || s$N_SCHOOLS_SHOWN == 0L) {
      sprintf("%s; no school-level EL share is shown (too few ELs to report)",
              if (s$N_SCHOOLS == 1L) "One school" else paste(s$N_SCHOOLS, "schools"))
    } else if (s$N_SCHOOLS_SHOWN == 1L) {
      sprintf("%s of %s schools shows an EL share: %s", s$N_SCHOOLS_SHOWN, s$N_SCHOOLS,
              .p2c_pct1(s$SCHOOL_PCT_MID))
    } else {
      sprintf("%s show an EL share, from %s to %s; the median school is %s",
              if (s$N_SCHOOLS_SHOWN < s$N_SCHOOLS)
                paste(s$N_SCHOOLS_SHOWN, "of", s$N_SCHOOLS, "schools")
              else if (s$N_SCHOOLS == 2L) "Both schools"
              else paste("All", s$N_SCHOOLS, "schools"),
              .p2c_pct1(s$SCHOOL_PCT_MIN), .p2c_pct1(s$SCHOOL_PCT_MAX),
              .p2c_pct1(s$SCHOOL_PCT_MID))
    }
  }
  if (!is.na(s$N_FORMER) || !is.na(s$N_FORMER_FIRST)) {
    rows[["Former ELs"]] <- if (!is.na(s$PCT_FORMER) && !is.na(s$PCT_FORMER_FIRST)) {
      sprintf("%s of students in %s and %s in %s (%s to %s students)",
              .p2c_pct1(s$PCT_FORMER_FIRST), sy(s$FIRST_YEAR), .p2c_pct1(s$PCT_FORMER),
              sy(s$YEAR), .p2c_n(s$N_FORMER_FIRST), .p2c_n(s$N_FORMER))
    } else {
      sprintf("%s former ELs in %s", .p2c_n(s$N_FORMER), sy(s$YEAR))
    }
  }
  list(title = title, cards = list(card1, card2, card3), rows = rows)
}
