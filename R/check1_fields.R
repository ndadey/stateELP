# Check 1 -- the "Data fields used" box (D2, 2026-09-20).
#
# ONE declaration, two consumers: the compute functions validate their inputs
# against it, and the document renders it. That is the point -- a box that is
# hand-written drifts from the code the first time a slot changes, and a
# reader cannot tell which columns actually did any work.
#
# Declare only the fields the slots built so far consume. Each later pass adds
# its own rows (Model 2's academic fields, the curves' INITIAL_ELP, ...) and
# the box grows with them, because `$meta$data_fields` keeps only the rows
# whose slots were actually computed in the run.

# FIELD     canonical ALL-CAPS name (CLAUDE.md 4)
# TABLE     roster / academic / derived
# KIND      required (a slot cannot run without it) / optional (a slot
#           degrades) / derived (the package computes it; a delivered column
#           of the same name is not used)
# SLOTS     comma-separated results slots that consume the field
# USED_FOR  one plain-language line, for the reader of the document
# FALLBACK  optional fields only: what happens when it is absent
.check1_field_declaration <- function() {
  tibble::tribble(
    ~FIELD,            ~TABLE,    ~KIND,       ~SLOTS,                 ~USED_FOR, ~FALLBACK,

    "STUDENT_ID",      "roster",  "required",  "cohorts,life_table",
    "Follows one student across years, and counts the cohort once per student.",
    NA_character_,

    "YEAR",            "roster",  "required",  "cohorts,life_table",
    "Places each row in the panel: sets the follow-up window, and each student's year in status.",
    NA_character_,

    "GRADE",           "roster",  "required",  "cohorts,life_table",
    "Fixes the entry grade that defines the cohort, and labels each life-table row.",
    NA_character_,

    "EL_STATUS",       "roster",  "required",  "cohorts,life_table",
    "The current / former transitions that date entry, exit and censoring.",
    NA_character_,

    "LEFT_TRUNCATED",  "roster",  "optional",  "cohorts,life_table",
    "Excludes students already in EL status when the panel opened, whose time in status cannot be measured.",
    "entrants in the panel's first year are treated as left-truncated and excluded",

    "COHORT_LABEL",    "derived", "derived",   "cohorts,life_table",
    "Entry grade and entry year, the cohort key.",
    NA_character_,

    "YEAR_IN_STATUS",  "derived", "derived",   "life_table",
    "Years since the first year observed as an EL, counting the exit year.",
    NA_character_,

    "EXIT_YEAR",       "derived", "derived",   "life_table",
    "The first year observed as a former EL; exit is absorbing.",
    NA_character_,

    "CENSOR_TYPE",     "derived", "derived",   "life_table",
    "Disappeared (first year absent) or still classified at the window's edge.",
    NA_character_,

    "A_TO_F",          "derived", "derived",   "life_table",
    "The life table's own columns: at risk, exited, disappeared, and the three rates.",
    NA_character_
  )
}

# Rows whose SLOTS list any of `slots`.
.check1_fields_for <- function(slots, declaration = .check1_field_declaration()) {
  keep <- vapply(strsplit(declaration$SLOTS, ",", fixed = TRUE),
                 function(s) any(trimws(s) %in% slots), TRUE)
  declaration[keep, , drop = FALSE]
}

# The validation half of the declaration. A slot that names a required field
# cannot run without it, and the error says which field and which slot.
.check1_require_fields <- function(roster, slots) {
  need <- .check1_fields_for(slots)
  need <- need[need$TABLE == "roster" & need$KIND == "required", ]
  miss <- setdiff(need$FIELD, names(roster))
  if (length(miss)) {
    stop("Check 1 cannot run: the roster is missing ",
         paste(miss, collapse = ", "), ", required by ",
         paste(slots, collapse = " / "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

# The reporting half: the rows whose slots were computed, resolved against
# this delivery. YOUR_COLUMN is what the state actually sent (the canonical
# name when the crosswalk does not rename it); PRESENT answers the optional
# fields.
.check1_data_fields <- function(roster, spec, slots) {
  out <- .check1_fields_for(slots)
  if (!nrow(out)) return(out)

  cross <- spec$data$columns$roster
  out$YOUR_COLUMN <- vapply(seq_len(nrow(out)), function(i) {
    if (!identical(out$TABLE[i], "roster")) return(NA_character_)
    # A field the crosswalk does not mention arrives under its canonical
    # name; `[[` on an absent name would error rather than say so.
    if (!is.null(cross) && out$FIELD[i] %in% names(cross)) {
      as.character(cross[[out$FIELD[i]]])
    } else {
      out$FIELD[i]
    }
  }, "")
  out$PRESENT <- ifelse(out$TABLE == "roster", out$FIELD %in% names(roster), NA)
  out
}
