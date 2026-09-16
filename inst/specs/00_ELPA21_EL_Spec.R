# =============================================================================
# 00_ELPA21_EL_Spec.R  --  no data; the anti-WIDA guardrail
# -----------------------------------------------------------------------------
# This spec exists to break WIDA assumptions that would otherwise leak into
# R/spec.R or any compute_*(). It differs from the SIM spec on every axis the
# package abstracts over:
#
#   * the headline is a CATEGORICAL determination, not a numeric composite;
#   * `determination` reports a level and no scale score;
#   * `overall` reports a scale score and no level (the mirror case);
#   * no bands are declared, so the package default of one band per level
#     applies rather than the WIDA 4.0-4.4 / 4.5-4.9 scheme;
#   * exit is a DOMAIN PROFILE (Type 6), not one composite -- so the Type-gated
#     Additional checks populate structurally here and are NULL under SIM.
#
# Values are illustrative of the consortium's published structure, not any one
# member state's rule. No data ships for it. verification$status = "unverified".
# =============================================================================

ELPA21_EL_Spec <- stateELP::el_spec(

  schema_version = "1.0.0",

  program = list(
    name                = "ELPA21 Summative",
    short_name          = "ELPA21",
    state               = "ELPA21",
    department          = "ELPA21 consortium (illustrative)",
    assessment_type     = "elp",
    administration_year = 2025L,
    vendor              = "ELPA21"
  ),

  elp = list(
    instrument            = "ELPA21 Summative",
    administration_window = "Feb-Mar",

    components = list(
      listening = list(kind = "domain", measures = c("scale", "level")),
      speaking  = list(kind = "domain", measures = c("scale", "level")),
      reading   = list(kind = "domain", measures = c("scale", "level")),
      writing   = list(kind = "domain", measures = c("scale", "level")),

      # Scale without a level: the mirror of the level-without-scale case.
      overall = list(
        kind     = "composite",
        measures = "scale",
        from     = c(listening = 0.25, speaking = 0.25,
                     reading = 0.25, writing = 0.25)
      ),

      # Level without a scale. This is the headline: a profile rule over the
      # four domains, reported by the vendor as a category. The package never
      # derives it -- vendor-reported levels only.
      determination = list(
        kind        = "composite",
        measures    = "level",
        from        = c(listening = 0.25, speaking = 0.25,
                        reading = 0.25, writing = 0.25),
        native_name = "Overall Performance Determination"
      )
    ),

    headline = "determination",

    level_scale = list(
      type   = "categorical",
      levels = c("Emerging", "Progressing", "Proficient")
    ),

    # No bands declared: the package default is one band per level. This is the
    # branch the WIDA-shaped SIM spec never exercises.
    bands = NULL,

    grade_clusters = c("K", "1", "2-3", "4-5", "6-8", "9-12"),
    cuts           = NULL,
    alternate      = NULL
  ),

  policy = list(

    # Type 6: a minimum on EVERY domain, rather than one composite driving
    # exit. `exempt_if_absent` is load-bearing here -- a waived domain is an
    # absent row, and an absent row must not be read as a failed criterion.
    exit = list(
      pathways = list(
        list(
          name  = "domain_profile",
          rule  = "Level 4 or higher in each of listening, speaking, reading and writing",
          terms = list(
            list(component = "listening", measure = "level",
                 op = ">=", value = 4, exempt_if_absent = TRUE),
            list(component = "speaking", measure = "level",
                 op = ">=", value = 4, exempt_if_absent = TRUE),
            list(component = "reading", measure = "level",
                 op = ">=", value = 4, exempt_if_absent = TRUE),
            list(component = "writing", measure = "level",
                 op = ">=", value = 4, exempt_if_absent = TRUE)
          ),
          combine       = "all",
          authority     = "Automatic on the summative determination.",
          discretionary = FALSE
        )
      ),
      combination = "any_pathway",
      types       = 6L,
      set_date    = NA_character_,
      set_by      = NA_character_,
      source      = "provisional"
    ),

    timelines = list(
      max_years_to_exit   = 5L,
      essa_timeline_years = 5L,
      on_time_rule = list(
        default_years   = 5L,
        by_start_grade  = NULL,
        by_initial_band = NULL
      ),
      lt_el_definition   = "Unverified.",
      exit_dating        = "first_former_year",
      initial_elp_source = "first_annual",
      source             = "provisional"
    ),

    suppression = list(min_cell = 10L, source = "provisional"),

    academic_benchmark = list(
      proficient_level = 3L,
      description      = "Illustrative.",
      source           = "provisional"
    ),

    error_tolerance = list(
      costlier_error = "unknown",
      asked_of       = NA_character_,
      asked_date     = NA_character_,
      rationale      = NA_character_,
      source         = "provisional"
    )
  ),

  academic = list(spec_id = NA_character_),

  data = list(
    columns = list(
      spine = c(
        student_id  = "student_id",
        school_year = "school_year",
        grade       = "grade",
        el_status   = "el_status",
        elp_level   = "elp_level"
      )
    )
  ),

  verification = list(
    status        = "unverified",
    verified_by   = NA_character_,
    verified_date = NA_character_,
    method        = NA_character_,
    notes = paste(
      "Illustrative of the ELPA21 structure; not any member state's rule.",
      "Ships without data. Its purpose is to keep WIDA assumptions out of the",
      "package: any compute_*() that hardcodes a component name, assumes a",
      "numeric headline, or assumes declared bands fails against this spec."
    )
  ),

  source_documents = list(
    list(title = "Process Overview (v1.8), Type 6 -- domain profile",
         url = NA_character_)
  )
)
