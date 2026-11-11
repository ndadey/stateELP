# =============================================================================
# 00_TEMPLATE_EL_Spec.R  --  a WIDA-shaped template. NO STATE'S POLICY.
# -----------------------------------------------------------------------------
# EVERY POLICY VALUE HERE IS A PLACEHOLDER. The exit threshold, the timelines,
# the growth definition and the academic benchmark describe no state, and must
# not be read as any state's exit criteria. What is real is the *shape*: the
# published WIDA ACCESS component model, the six-level scale, and a
# non-identity `data$columns` crosswalk.
#
# Two jobs:
#
#   1. The second shape in the test suite. The SIM spec's crosswalk is
#      identity and its component keys are already canonical, so it cannot
#      exercise either renaming or crosswalk resolution. This one does both.
#   2. A starting point for a real state spec: copy it, replace every
#      `source = "provisional"` value with a sourced one, and set
#      `verification$status`. Since 2026-11 it is complete on its own: every
#      field a stage reads is here, including an inline academic block with
#      cut scores BY GRADE, `lt_el_years` and `value_maps`. Fill it in and
#      you have a whole spec; nothing needs copying from the SIM spec.
#      (`build_el_spec()` asks the same questions at the R console and
#      writes the file for you.)
#
# Two rules that change results, both now enforced or harmless:
#   * Check 3 reads the exit threshold from the rule's term on the headline
#     component, wherever it sits in `terms` -- term order no longer matters.
#   * `policy$academic_benchmark$proficient_level` must be a whole number
#     once `academic` names a subject; the validator says so at read time.
#
# It is laid out field for field per the CLAUDE.md 5 diff table, so a
# migration from the older `assessment_spec` shape reads as a diff rather than
# a rewrite:
#
#   subjects                -> elp$components (+ kind, measures, from, native_name)
#   subjects$*$grades       -> elp$grade_clusters
#   achievement_levels      -> elp$level_scale (policy_benchmark dropped: it is
#                              an exit rule, not a level label -- it moved into
#                              policy$exit); data_column -> data$columns
#   scale_scores            -> elp$cuts (NULL in the shell; never fills a level)
#   elp$band_scheme         -> elp$bands (labels -> breaks + labels)
#   instrument/domains/composites/composite_weights -> elp$components + headline
#   exit_criteria           -> policy$exit (+ terms, + types)
#   growth_targets/timelines-> policy$growth / policy$timelines
#                              (+ exit_dating, + initial_elp_source)
#   years$cohort_anchor_grade -> dropped; the starting cohort is the
#                              start_grade argument on run_check1()/run_check2()
#   data$columns            -> data$columns, extended
#   as_assessment_spec()    -> el_spec()
#
# =============================================================================

TEMPLATE_EL_Spec <- stateELP::el_spec(

  schema_version = "1.0.0",

  program = list(
    name                = "ACCESS for ELLs",
    short_name          = "ACCESS",
    state               = "XX",
    department          = "Example State Department of Education",
    assessment_type     = "elp",
    administration_year = 2023L,
    vendor              = "WIDA / DRC"
  ),

  elp = list(
    instrument            = "ACCESS for ELLs (WIDA)",
    administration_window = "Jan-Feb",

    # Was `subjects`. The four domains plus three composites plus overall; the
    # old `composite_overall` is the vocabulary's `overall`, and the old
    # `literacy` is `written`, carrying its vendor label as native_name.
    components = list(
      listening     = list(kind = "domain",    measures = c("scale", "level")),
      speaking      = list(kind = "domain",    measures = c("scale", "level")),
      reading       = list(kind = "domain",    measures = c("scale", "level")),
      writing       = list(kind = "domain",    measures = c("scale", "level")),
      oral          = list(kind = "composite", measures = c("scale", "level"),
                           from = c(listening = 0.50, speaking = 0.50),
                           native_name = "Oral (Listening + Speaking)"),
      written       = list(kind = "composite", measures = c("scale", "level"),
                           from = c(reading = 0.50, writing = 0.50),
                           native_name = "Literacy (Reading + Writing)"),
      comprehension = list(kind = "composite", measures = c("scale", "level"),
                           from = c(listening = 0.30, reading = 0.70),
                           native_name = "Comprehension (Listening + Reading)"),
      overall       = list(kind = "composite", measures = c("scale", "level"),
                           from = c(listening = 0.15, speaking = 0.15,
                                    reading = 0.35, writing = 0.35),
                           native_name = "Overall Composite")
    ),

    headline = "overall",

    # Was `achievement_levels`. `policy_benchmark = "5 Bridging"` is NOT here:
    # it is an exit rule, and lives under policy$exit.
    level_scale = list(
      type   = "numeric",
      range  = c(1, 6),
      labels = c("1 Entering", "2 Emerging", "3 Developing",
                 "4 Expanding", "5 Bridging", "6 Reaching")
    ),

    # Was `elp$band_scheme` (labels only); breaks added.
    bands = list(
      breaks = c(1, 2, 3, 4, 4.5, 5, 7),
      labels = c("1.0-1.9", "2.0-2.9", "3.0-3.9", "4.0-4.4", "4.5-4.9", "5.0+")
    ),

    # Was `subjects$<domain>$grades`.
    grade_clusters = c("K", "1", "2-3", "4-5", "6-8", "9-12"),

    # Was `scale_scores`, one illustrative domain x cluster entry. Dropped in
    # the shell: cuts are optional and never fill a level. Restore from the
    # WIDA cut tables keyed <component>_<cluster> when a scale-level crosswalk
    # is actually needed.
    cuts      = NULL,
    # The state's alternate ELP test (e.g. WIDA Alternate ACCESS), recorded
    # for Prep 1's policy documentation: instrument, eligibility, exit
    # criteria. NO STAGE READS IT. Alternate-test students are handled through
    # ELP_FORM = "alternate" in the data (see mark_alternate_elp()), and a
    # level_scale given here changes no result.
    alternate = NULL
  ),

  policy = list(

    # Was `elp$exit_criteria`, moved across the measurement/policy boundary.
    # `terms` and `types` are new: the display `rule` is what Prep 1 renders,
    # `terms` is what the validator checks -- and never evaluates.
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
          authority     = "TODO: confirm who holds decision authority.",
          discretionary = FALSE
        )
      ),
      combination     = "any_pathway",
      types           = 1L,   # TODO: confirm whether a second criterion applies
      varies_by_grade = FALSE,
      set_date        = NA_character_,
      set_by          = "TODO: the citation that sets the rule",
      source          = "provisional"
    ),

    timelines = list(
      max_years_to_exit   = 6L,   # PLACEHOLDER, carried from the source file
      essa_timeline_years = 5L,   # PLACEHOLDER
      on_time_rule = list(
        default_years   = 5L,
        by_start_grade  = NULL,   # "function of start grade and/or baseline ELP"
        by_initial_band = NULL
      ),
      lt_el_definition   = "TODO: the state long-term EL definition",
      # PLACEHOLDER. The year in status from which a current EL counts as
      # long-term -- the structured form of lt_el_definition. Without it the
      # LT_EL flag is NA and Prep 2's time-in-status view is NULL.
      lt_el_years        = 6L,
      exit_dating        = "first_former_year",
      initial_elp_source = "first_annual",
      source             = "provisional"
    ),

    growth = list(
      annual_target_levels = NA_real_,
      met_indicator        = "ELP_GROWTH_TARGET_MET",
      how_set              = "TODO: expected annual growth toward exit",
      source               = "provisional"
    ),

    suppression = list(min_cell = 10L, source = "provisional"),

    # Which academic performance level counts as proficient: its position in
    # each subject's level_labels, lowest = 1. PLACEHOLDER: level 3 of 4.
    # It must be a whole number once `academic` below names a subject.
    academic_benchmark = list(
      proficient_level = 3L,
      description      = "TODO: the lowest performance level that counts as proficient.",
      source           = "provisional"
    ),

    district_size_bands = list(
      breaks = c(0, 100, 1000, Inf),
      labels = c("<100", "101-999", "1,000+"),
      source = "provisional"
    ),

    program = list(
      models             = NA_character_,
      course_access      = NA_character_,
      credentialing      = NA_character_,
      gated_by_el_status = NA_character_,
      source             = "provisional"
    ),

    error_tolerance = list(
      costlier_error = "unknown",
      asked_of       = NA_character_,
      asked_date     = NA_character_,
      rationale      = NA_character_,
      source         = "provisional"
    )
  ),

  # --- academic assessment: the state's general reading and math tests -------
  # Inline, one entry per subject. The subject keys (ELA, Math) are what the
  # academic file's SUBJECT column must hold after the value map below.
  #
  #   grades       the tested grades, as character; NULL for an end-of-course
  #                test (the tested grade then comes from the academic row).
  #   level_labels the performance levels, lowest first, EXACTLY as they
  #                appear in the academic file's ACHIEVEMENT_LEVEL (labels are
  #                translated to 1, 2, ...; a file already holding 1, 2, ...
  #                needs c("1", "2", ...)).
  #   cuts         the LOWEST scale score of each level above the first, read
  #                with op ">=" (a score exactly at a cut is in the higher
  #                level). Two shapes:
  #                  * one vector, when the cuts are the same in every grade
  #                    (a vertical scale): cuts = c(450, 500, 550);
  #                  * a list named by grade, when they differ -- the usual
  #                    case, shown here: cuts = list("3" = c(...), ...).
  #                Leave a grade out, or set cuts = NULL, and Checks 3 and 4
  #                read proficiency from the file's own ACHIEVEMENT_LEVEL for
  #                that grade instead, and draw no cut line.
  #
  # EVERY NUMBER BELOW IS A PLACEHOLDER describing no state's test.
  academic = list(
    ELA = list(
      instrument   = "TODO: the state reading / ELA test",
      grades       = as.character(3:8),
      level_labels = c("Level 1", "Level 2", "Level 3", "Level 4"),
      cuts = list(
        "3" = c(400, 450, 500),
        "4" = c(405, 455, 505),
        "5" = c(410, 460, 510),
        "6" = c(415, 465, 515),
        "7" = c(420, 470, 520),
        "8" = c(425, 475, 525)
      ),
      op           = ">=",
      source       = "provisional"
    ),
    Math = list(
      instrument   = "TODO: the state mathematics test",
      grades       = as.character(3:8),
      level_labels = c("Level 1", "Level 2", "Level 3", "Level 4"),
      cuts = list(
        "3" = c(400, 450, 500),
        "4" = c(405, 455, 505),
        "5" = c(410, 460, 510),
        "6" = c(415, 465, 515),
        "7" = c(420, 470, 520),
        "8" = c(425, 475, 525)
      ),
      op           = ">=",
      source       = "provisional"
    )
  ),

  data = list(
    # A DELIBERATELY NON-IDENTITY crosswalk, which is most of this spec's
    # reason to exist. The SIM spec's crosswalk is identity, so it cannot
    # exercise the resolution path at all: a canonical ALL-CAPS name on the
    # left, whatever the state actually sends on the right. The names below
    # are invented in the idiom real deliveries use -- mixed case, a vendor
    # id, run-together component columns -- and are not any state's file.
    #
    # Only some fields are renamed on purpose: GRADE and EL_STATUS are absent
    # here so the "a field the crosswalk does not mention keeps its canonical
    # name" path is exercised too.
    columns = list(
      roster = c(
        STUDENT_ID = "Student_UID",
        YEAR       = "school_year"
      ),
      base = c(
        STUDENT_ID = "Student_UID",
        YEAR       = "school_year"
      ),
      components = list(
        overall   = c(SCALE_SCORE       = "compositeoverallscalescore",
                      ACHIEVEMENT_LEVEL = "compositeoverallproficiencylevel"),
        listening = c(SCALE_SCORE       = "listeningscalescore",
                      ACHIEVEMENT_LEVEL = "listeningproficiencylevel"),
        speaking  = c(SCALE_SCORE       = "speakingscalescore",
                      ACHIEVEMENT_LEVEL = "speakingproficiencylevel"),
        reading   = c(SCALE_SCORE       = "readingscalescore",
                      ACHIEVEMENT_LEVEL = "readingproficiencylevel"),
        writing   = c(SCALE_SCORE       = "writingscalescore",
                      ACHIEVEMENT_LEVEL = "writingproficiencylevel")
      ),
      # Delivered VALUES that differ from the package's, mapped by value.
      #   GRADE: kindergarten must be "K" -- a file coding it "0" needs this,
      #          or Check 1 finds no kindergarten cohort.
      #   CONTENT_AREA$academic: the academic file's subject codes -> the
      #          subject keys above.
      # PLACEHOLDERS: replace with the codes your files actually use.
      value_maps = list(
        GRADE = c("0" = "K"),
        CONTENT_AREA = list(
          academic = c(READING = "ELA", MATHEMATICS = "Math")
        )
      )
    )
  ),

  verification = list(
    status        = "unverified",
    verified_by   = NA_character_,
    verified_date = NA_character_,
    method        = NA_character_,
    notes = paste(
      "SHAPE ONLY. The component model and the six-level scale follow the",
      "published WIDA ACCESS structure; every policy value -- exit threshold,",
      "growth definition, maximum timeline, academic benchmark -- is a",
      "placeholder and describes no state. `status = \"unverified\"` is",
      "therefore permanent for this spec rather than pending. Ships without",
      "data. Its job is to be the second shape in the test suite: a",
      "non-identity crosswalk and renamed components, which the SIM spec",
      "cannot provide."
    )
  ),

  source_documents = list(
    list(title = "WIDA ACCESS Interpretive Guide for Score Reports",
         url   = "https://wida.wisc.edu/resources/wida-access-interpretive-guide-score-reports")
  )
)
