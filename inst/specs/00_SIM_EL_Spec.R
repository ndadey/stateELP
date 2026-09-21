# =============================================================================
# 00_SIM_EL_Spec.R  --  the spec for the shipped simulated data
# -----------------------------------------------------------------------------
# WIDA-shaped, Type 1 (single overall composite). Matches data-raw/el_long_sim.R
# and sim_contract.md v0.4.0 field for field: the component model of 4.1/4.3,
# the bands of 4.5, the exit rule of 4.7, min_cell and max_years from the P
# block of 2.2, and the academic cuts and >= convention of 5.4.
#
# This spec doubles as the TEMPLATE a state copies (open decision 3). Every
# value that is a modelling choice rather than a published rule carries
# source = "provisional", which renders as [DERIVED - VERIFY].
#
# verification$status is "synthetic": these values describe a fixture, and no
# number produced from them may be reported as a finding about any real state.
# =============================================================================

SIM_EL_Spec <- stateELP::el_spec(

  schema_version = "1.0.0",

  program = list(
    name                = "Simulated ELP Program",
    short_name          = "SIM",
    state               = "SIM",
    department          = "Simulated Department of Education",
    assessment_type     = "elp",
    administration_year = 2025L,
    vendor              = "synthetic"
  ),

  # --- measurement: vendor changes the test ----------------------------------
  elp = list(
    instrument            = "WIDA ACCESS",
    administration_window = "Jan-Mar",

    # from-weights are informational: Prep 1 renders them and a QC check may
    # recompute against them, but reported values are always the vendor's.
    # [DERIVED - VERIFY] against the WIDA Interpretive Guide.
    components = list(
      listening     = list(kind = "domain",    measures = c("scale", "level")),
      speaking      = list(kind = "domain",    measures = c("scale", "level")),
      reading       = list(kind = "domain",    measures = c("scale", "level")),
      writing       = list(kind = "domain",    measures = c("scale", "level")),
      oral          = list(kind = "composite", measures = c("scale", "level"),
                           from = c(listening = 0.50, speaking = 0.50)),
      written       = list(kind = "composite", measures = c("scale", "level"),
                           from = c(reading = 0.50, writing = 0.50),
                           native_name = "Literacy"),
      comprehension = list(kind = "composite", measures = c("scale", "level"),
                           from = c(listening = 0.30, reading = 0.70)),
      overall       = list(kind = "composite", measures = c("scale", "level"),
                           from = c(listening = 0.15, speaking = 0.15,
                                    reading = 0.35, writing = 0.35))
    ),

    headline    = "overall",
    # Numeric in tenths; the labels name the whole levels, and are what a
    # labeled SGP LONG_DATA delivery is parsed against (index = level).
    level_scale = list(
      type   = "numeric",
      range  = c(1, 6),
      labels = c("Entering", "Emerging", "Developing",
                 "Expanding", "Bridging", "Reaching")
    ),

    # sim_contract 4.5. The 4.5 boundary IS the exit threshold, so the top two
    # bands hold only students in their exit year.
    bands = list(
      breaks = c(1, 2, 3, 4, 4.5, 5, 7),
      labels = c("1.0-1.9", "2.0-2.9", "3.0-3.9", "4.0-4.4", "4.5-4.9", "5.0+")
    ),

    grade_clusters = c("K", "1", "2-3", "4-5", "6-8", "9-12"),

    # Scale-score -> level cut tables. Never used to derive a level: levels are
    # vendor-reported. NULL here.
    cuts = NULL,

    # No alternate level scale is declared, so alternate-form students are
    # excluded from band distributions and reported as their own group
    # (sim_contract 4.4).
    alternate = NULL
  ),

  # --- policy: an agency or legislature changes a rule ------------------------
  policy = list(

    exit = list(
      pathways = list(
        list(
          name          = "overall_composite",
          rule          = "Overall composite proficiency level >= 4.5",
          terms         = list(
            list(component = "overall", measure = "level",
                 op = ">=", value = 4.5)
          ),
          combine       = "all",
          authority     = "Automatic on the annual score report; no committee.",
          discretionary = FALSE
        )
      ),
      combination = "any_pathway",
      types       = 1L,         # single overall composite; no Additional checks
      set_date    = NA_character_,
      set_by      = "synthetic",
      source      = "provisional"
    ),

    timelines = list(
      max_years_to_exit   = 6L,   # = P$max_years in the generator
      # The ESSA timeline that defines Check 2's on-time split. The contract
      # reports both <=5 and <=6 year exit shares and fixes no on-time rule, so
      # 5 is a choice, not a source. [PROPOSED - CONFIRM]
      essa_timeline_years = 5L,
      on_time_rule = list(
        default_years   = 5L,
        by_start_grade  = NULL,
        by_initial_band = NULL
      ),
      lt_el_definition = "Current EL in year 6 or later of status.",
      # The structured form of the definition above, which is what LT_EL is
      # derived from. A spec with only the text leaves LT_EL NA.
      # [PROPOSED - CONFIRM]
      lt_el_years      = 6L,
      exit_dating      = "first_former_year",
      initial_elp_source = "first_annual",
      source           = "provisional"
    ),

    growth = list(
      annual_target_levels = 0.4,
      met_indicator        = "ELP_GROWTH_TARGET_MET",
      how_set              = "Modelling choice in the generator; no policy basis.",
      source               = "provisional"
    ),

    accountability_indicator = list(
      name        = "Progress toward English language proficiency",
      description = "Out of scope for the eight stages; recorded for Prep 1.",
      source      = "provisional"
    ),

    suppression = list(
      min_cell = 10L,           # = P$min_cell in the generator
      source   = "provisional"
    ),

    academic_benchmark = list(
      proficient_level = 4L,    # level 4 of 5 is proficient (sim_contract 5.4)
      description      = "Check 3's red reference line.",
      source           = "provisional"
    ),

    # Deck default (slides 33-34). Never the sim's own tertile column.
    district_size_bands = list(
      breaks = c(0, 100, 1000, Inf),
      labels = c("<100", "101-999", "1,000+"),
      source = "provisional"
    ),

    # Prep 3's inventory: what EL status actually gates on the ground. Unknown
    # for a synthetic state; Prep 3 renders each as an open confirm item.
    program = list(
      models            = NA_character_,
      course_access     = NA_character_,
      credentialing     = NA_character_,
      gated_by_el_status = NA_character_,
      source            = "provisional"
    ),

    # Prep 3's elicited values judgment. "unknown" renders as an open confirm
    # item, and Check 4 and the Decision stage caption their outputs to say the
    # J-optimal point assumes the two errors are equally costly.
    error_tolerance = list(
      costlier_error = "unknown",
      asked_of       = NA_character_,
      asked_date     = NA_character_,
      rationale      = NA_character_,
      source         = "provisional"
    )
  ),

  # --- academic assessment ----------------------------------------------------
  # Inline for SIM. Check 3 drives its grade loop from $grades, never from the
  # observed grade span, or it renders empty panels for K-2 and 12.
  academic = list(
    ELA = list(
      instrument   = "Simulated summative ELA",
      grades       = as.character(3:11),
      cuts         = c(700, 735, 750, 790),  # minimum score of levels 2-5
      op           = ">=",
      level_labels = c("1", "2", "3", "4", "5"),
      source       = "provisional"
    ),
    Math = list(
      instrument   = "Simulated summative Mathematics",
      grades       = as.character(3:11),
      cuts         = c(700, 735, 750, 790),
      op           = ">=",
      level_labels = c("1", "2", "3", "4", "5"),
      source       = "provisional"
    )
  ),

  # --- delivery ---------------------------------------------------------------
  # Identity crosswalk: the sim already emits the canonical ALL-CAPS names
  # (sim_contract.md 3.5 / 4.2 / 5.1). Each entry is canonical = delivered.
  data = list(
    # Prep 2's entry dating. The sim stamps LEP_PROGRAM_START_DATE as 15
    # September of the SPRING-year label (sim_contract.md 3.5), so its calendar
    # year IS the entry year; the package default reads a date on the academic
    # calendar, where a September date belongs to the next spring year.
    entry_date_rule = "calendar_year",

    columns = list(
      roster = c(
        STUDENT_ID    = "STUDENT_ID",
        YEAR          = "YEAR",
        GRADE         = "GRADE",
        EL_STATUS     = "EL_STATUS",
        ELP_FORM      = "ELP_FORM",
        SAMPLE_WEIGHT = "SAMPLE_WEIGHT"
      ),
      base = c(
        STUDENT_ID        = "STUDENT_ID",
        YEAR              = "YEAR",
        COMPONENT         = "COMPONENT",
        ELP_FORM          = "ELP_FORM",
        SCALE_SCORE       = "SCALE_SCORE",
        ACHIEVEMENT_LEVEL = "ACHIEVEMENT_LEVEL"
      ),
      academic = c(
        STUDENT_ID            = "STUDENT_ID",
        YEAR                  = "YEAR",
        GRADE                 = "GRADE",
        SUBJECT               = "SUBJECT",
        SCALE_SCORE           = "SCALE_SCORE",
        ACHIEVEMENT_LEVEL     = "ACHIEVEMENT_LEVEL",
        ACHIEVEMENT_LEVEL_ALT = "ACHIEVEMENT_LEVEL_ALT"
      ),
      # SGP LONG_DATA delivery: CONTENT_AREA *values* map to component and
      # subject keys, never by label -- READING is the overall composite in
      # one state's WIDA file and the reading test in another's. Unused by the
      # sim's own separate-objects call.
      value_maps = list(
        CONTENT_AREA = list(
          base     = c(READING = "overall"),
          academic = c(ELA = "ELA", MATHEMATICS = "Math")
        ),
        GRADE = c("0" = "K")
      )
    )
  ),

  verification = list(
    status        = "synthetic",
    verified_by   = NA_character_,
    verified_date = NA_character_,
    method        = "Synthetic fixture; nothing here describes a real state.",
    notes = paste(
      "Mirrors data-raw/el_long_sim.R under sim_contract.md v0.4.0.",
      "Every policy value is provisional by construction."
    )
  ),

  source_documents = list(
    list(title = "sim_contract.md v0.4.0", url = NA_character_),
    list(title = "Process Overview: Evaluating and Setting English Learner Exit Thresholds (v1.8)",
         url = NA_character_)
  )
)
