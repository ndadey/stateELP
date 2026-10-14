# =============================================================================
# 00_MA_EL_Spec.R  --  Massachusetts, ACCESS for ELLs (WIDA), 2024-25.
# -----------------------------------------------------------------------------
# ONE POLICY VALUE IN THIS FILE IS SOURCED: the exit rule. Massachusetts DESE,
# "Understanding and Implementing ACCESS Tests" (14 August 2024), states on
# slide 3, as a footnote to the participation requirements:
#
#     "Exit criteria for ACCESS is 4.2 overall and 3.9 literacy.
#      WIDA Alternate ACCESS exit criteria will be announced this fall."
#
# That is a Type 2 rule -- composite plus floor -- and it is the only thing
# here a reader may take as Massachusetts policy.
#
# EVERYTHING ELSE UNDER `policy` IS A PLACEHOLDER, because the source is an
# operational test-administration deck (windows, modes, accommodations,
# training) and not a policy document. The maximum timeline, the on-time
# rule, the long-term-EL definition, the growth targets, the academic
# benchmark, the suppression minimum and the district-size bands are NOT
# Massachusetts values. Each carries `source = "provisional"`, which renders
# as [DERIVED - VERIFY], and `verification$status` is "provisional" for the
# spec as a whole: part sourced, part not. Replace them from the department's
# own regulations and reporting rules before any Massachusetts number is
# reported. This is the failure mode that retired the former NJ spec
# (CLAUDE.md 5): real attribution over unverified values.
#
# WHAT IS ALSO TAKEN FROM THE SOURCE, as description rather than policy: the
# 2025 test window (6 January - 14 February 2025), grades K-12, the paper /
# online / Alternate ACCESS modes, WIDA Alternate ACCESS as the alternate
# form with its exit criteria not yet announced, and the existence of SGPA
# (Student Growth Percentile on ACCESS), progress and attainment indicators
# in the Round 2 results file.
#
# NO DELIVERY HAS BEEN INSPECTED, so there is no `data$columns` crosswalk and
# no academic block: see the comments on both below for what to add and when.
# =============================================================================

MA_EL_Spec <- stateELP::el_spec(

  schema_version = "1.0.0",

  program = list(
    name = "ACCESS for ELLs",
    # The instrument program, not the state -- the convention every
    # $meta$spec_id follows. It does not distinguish Massachusetts from the
    # TEMPLATE spec, which is also WIDA-shaped; `state` below is what does.
    # Whether short_name should carry the state is [PROPOSED - CONFIRM].
    short_name = "ACCESS",
    state      = "MA",
    department = "Massachusetts Department of Elementary and Secondary Education",
    assessment_type = "elp",
    # The administration the source document describes: the January-February
    # 2025 window, announced August 2024.
    administration_year = 2025L,
    vendor              = "WIDA / DRC"
  ),

  elp = list(
    instrument = "ACCESS for ELLs (WIDA)",
    # Source, slide 2: test window 6 January - 14 February 2025.
    administration_window = "Jan-Feb",

    # The published WIDA ACCESS component model: four domains, three
    # composites, and the overall composite. `native_name` carries the
    # vendor's and the state's own words -- Massachusetts says "literacy" in
    # the exit footnote, which is the Reading + Writing composite, the
    # vocabulary's `written`. Reader-facing text resolves these through
    # spec_headline_label(), never the key (CLAUDE.md 9).
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
                           native_name = "Literacy"),
      comprehension = list(kind = "composite", measures = c("scale", "level"),
                           from = c(listening = 0.30, reading = 0.70),
                           native_name = "Comprehension (Listening + Reading)"),
      overall       = list(kind = "composite", measures = c("scale", "level"),
                           from = c(listening = 0.15, speaking = 0.15,
                                    reading = 0.35, writing = 0.35),
                           native_name = "Overall Composite")
    ),

    # The component the exit rule's main criterion sits on, so every stage
    # runs on it. The Literacy floor is the second term of the same pathway.
    headline = "overall",

    level_scale = list(
      type   = "numeric",
      range  = c(1, 6),
      labels = c("1 Entering", "2 Emerging", "3 Developing",
                 "4 Expanding", "5 Bridging", "6 Reaching")
    ),

    # The deck's band scheme (CLAUDE.md 1, template authority), unchanged, so
    # Massachusetts figures read against the same bands as every other view.
    #
    # KNOWN TENSION, deliberately left for the user [PROPOSED - CONFIRM]:
    # Massachusetts exits at 4.2, which sits INSIDE the "4.0-4.4" band, so
    # that band mixes students below the cut with students at or above it.
    # Check 3 reports "4.0-4.4" as the exit band and Check 2's crosstab rows
    # split on these labels, so both are coarser than the rule. A break at
    # the state's own cut -- breaks c(1, 2, 3, 4, 4.2, 4.5, 5, 7), labels
    # c("1.0-1.9", "2.0-2.9", "3.0-3.9", "4.0-4.1", "4.2-4.4", "4.5-4.9",
    # "5.0+") -- would align the views with the rule, but it changes every
    # Check 2 and Check 3 figure for Massachusetts, so it is proposed here
    # and not taken. Check 4 is unaffected: it runs on the level itself.
    bands = list(
      breaks = c(1, 2, 3, 4, 4.5, 5, 7),
      labels = c("1.0-1.9", "2.0-2.9", "3.0-3.9", "4.0-4.4", "4.5-4.9", "5.0+")
    ),

    # The published ACCESS grade clusters; the source confirms K-12 testing.
    grade_clusters = c("K", "1", "2-3", "4-5", "6-8", "9-12"),

    # Scale-to-level cut tables are not needed: no level is ever derived from
    # a scale score (CLAUDE.md 5). Vendor-reported levels only.
    cuts = NULL,

    # Source, slides 3-4: WIDA Alternate ACCESS, grades K-12, paper, 1:1,
    # locally scored, for students with the most significant cognitive
    # disabilities, and it must be in the IEP. No `level_scale` is declared
    # because the document states its exit criteria were not yet announced --
    # so alternate-form students stay out of the headline, out of the exit
    # rule and out of band distributions, and are reported as a group
    # (CLAUDE.md 4). Declare a `level_scale` here once the department
    # announces them.
    alternate = list(
      instrument  = "WIDA Alternate ACCESS",
      grades      = c("K", "1", "2-3", "4-5", "6-8", "9-12"),
      eligibility = paste(
        "Students with the most significant cognitive disabilities who meet",
        "the WIDA participation guidelines; must be listed in the IEP."),
      exit_criteria = paste(
        "Not announced as of the source document (14 August 2024):",
        "WIDA Alternate ACCESS exit criteria will be announced this fall."),
      level_scale = NULL
    )
  ),

  policy = list(

    # ---- THE ONE SOURCED BLOCK ----------------------------------------------
    # Type 2: an overall composite threshold plus a Literacy floor, both on
    # the vendor-reported proficiency LEVEL (ACCESS composite levels are
    # reported to one decimal, which is why 4.2 and 3.9 are levels and not
    # scale scores). Conjunctive: a student needs both.
    #
    # TERM ORDER IS LOAD-BEARING. Check 3's `.check3_exit_band()` takes the
    # FIRST term with measure = "level" whatever component it names
    # (R/check3_models.R:210-223), so the headline term must come first or
    # Check 3 would report the Literacy floor as the exit threshold. For the
    # same reason neither term sets `from` explicitly: that reader requires
    # `is.null(tm$from)`, and "elp" is the default. Check 4 is not order
    # dependent -- it matches the headline component by name and flags the
    # pathway conjunctive (R/check4_compute.R:613-640). A test in
    # test-spec.R pins the order.
    exit = list(
      pathways = list(
        list(
          name  = "access_overall_plus_literacy",
          rule  = paste("Overall Composite proficiency level >= 4.2",
                        "AND Literacy proficiency level >= 3.9"),
          terms = list(
            list(component = "overall", measure = "level",
                 op = ">=", value = 4.2),
            list(component = "written", measure = "level",
                 op = ">=", value = 3.9)
          ),
          combine = "all",
          # Not stated in the source. The footnote's framing -- students
          # opted out of EL services are still tested "until they meet exit
          # criteria" -- reads as automatic on the annual score report rather
          # than a committee decision, but that is an inference, not a cite.
          authority = paste(
            "Not stated in the source document. The exit footnote implies the",
            "criteria apply on the annual score report rather than through a",
            "committee; confirm with the department. [DERIVED - VERIFY]"),
          discretionary = FALSE
        )
      ),
      # One pathway, so the combination is trivially satisfied.
      combination     = "any_pathway",
      types           = 2L,
      varies_by_grade = FALSE,
      set_date        = NA_character_,
      set_by = paste(
        "Massachusetts Department of Elementary and Secondary Education,",
        "Student Assessment Services."),
      source = paste(
        "MA DESE, Understanding and Implementing ACCESS Tests",
        "(14 August 2024), slide 3 footnote: Exit criteria for ACCESS is",
        "4.2 overall and 3.9 literacy.")
    ),

    # ---- EVERYTHING BELOW IS A PLACEHOLDER ----------------------------------

    # NOT MASSACHUSETTS POLICY. The source document says nothing about
    # timelines. `max_years_to_exit` is present only because the validator
    # requires a whole number, and it is deliberately the same value the
    # TEMPLATE spec carries so that it reads as the package's placeholder
    # rather than as a plausible Massachusetts figure.
    #
    # `essa_timeline_years`, `on_time_rule` and `lt_el_years` are OMITTED on
    # purpose: an absent field is unambiguous, where a placeholder can be
    # mistaken for policy. The consequence is that Check 2's on-time split
    # and Check 1's within-timeline row both fall back to max_years_to_exit
    # (R/utils.R:329-330), i.e. to this placeholder, and LT_EL is NA
    # (CLAUDE.md 5) -- so neither reads as a Massachusetts result until the
    # real values land here.
    timelines = list(
      max_years_to_exit = 6L,
      lt_el_definition  = paste(
        "Not in the source document. Supply the department's long-term-EL",
        "definition and its structured `lt_el_years` together."),
      exit_dating        = "first_former_year",
      initial_elp_source = "first_annual",
      source             = "provisional"
    ),

    # The source reports that growth exists and is published -- Round 2
    # results carry SGPA (Student Growth Percentile on ACCESS), a progress
    # indicator, attainment and the following year's targets (slide 12). No
    # target VALUE is given, so the value is NA and the description records
    # only what the document actually says.
    growth = list(
      annual_target_levels = NA_real_,
      met_indicator        = NA_character_,
      how_set = paste(
        "Massachusetts publishes SGPA, a progress indicator, attainment and",
        "next-year targets in the Round 2 ACCESS results file; the source",
        "document gives no target values. Supply them from the department."),
      source = "provisional"
    ),

    accountability_indicator = list(
      name = "Progress toward English language proficiency",
      description = paste(
        "The source names a progress indicator and an attainment indicator",
        "in the ACCESS results files (slide 12) without defining either.",
        "Out of scope for the eight stages; recorded for Prep 1."),
      source = "provisional"
    ),

    # The package default, not a departmental reporting rule. Never lowered
    # to make a cell appear (CLAUDE.md 4).
    suppression = list(min_cell = 10L, source = "provisional"),

    # NA, and that is safe only while the academic block stays a pointer.
    # LANDMINE for whoever inlines MCAS below: `.check3_cut()`
    # (R/check3_compute.R:245) errors on an NA `proficient_level` once a spec
    # declares subjects -- today the pointer's NULL cuts short-circuit it.
    # Set this in the same edit that inlines the subjects.
    academic_benchmark = list(
      proficient_level = NA_integer_,
      description = paste(
        "Check 3's red reference line. Not in the source document. The",
        "obvious candidate is the MCAS Meeting Expectations level, but it is",
        "not cited here and must come from the department's own",
        "achievement-level tables."),
      source = "provisional"
    ),

    # The deck's default bins (CLAUDE.md 5), not a Massachusetts structure.
    district_size_bands = list(
      breaks = c(0, 100, 1000, Inf),
      labels = c("<100", "101-999", "1,000+"),
      source = "provisional"
    ),

    # Prep 3's inventory: what EL status gates on the ground. The source is a
    # testing deck and says nothing about service models, course access or
    # credentialing, so Prep 3 renders each as an open confirm item.
    program = list(
      models             = NA_character_,
      course_access      = NA_character_,
      credentialing      = NA_character_,
      gated_by_el_status = NA_character_,
      source             = "provisional"
    ),

    # Prep 3's elicited values judgment -- which error stakeholders find
    # costlier. Not a question a document can answer; it is asked of people.
    # "unknown" renders as an open confirm item and Check 4 prints its
    # implied cost ratio beside the J optimum rather than assuming an answer.
    error_tolerance = list(
      costlier_error = "unknown",
      asked_of       = NA_character_,
      asked_date     = NA_character_,
      rationale      = NA_character_,
      source         = "provisional"
    )
  ),

  # POINTER FORM, AND IT RESOLVES TO NOTHING TODAY (CLAUDE.md 5): no code
  # fetches a general spec, so this spec declares NO academic subjects.
  #
  # That is deliberate. The Massachusetts content assessment is MCAS, and the
  # source document carries no MCAS subjects, tested grades or scaled-score
  # cuts. Inventing them would attribute unverified cuts to the state, which
  # is exactly what retired the NJ spec.
  #
  # CONSEQUENCE, and it is sharp: with an academic table delivered and no
  # subjects declared, Check 4 degrades to NULL with a note, but Check 3,
  # Check 1's Model 2 and Check 2's academic summary ERROR with "subscript
  # out of bounds" (CLAUDE.md 3, Known issues). Nothing runs today because
  # no Massachusetts delivery exists; the moment one does, inline the
  # subjects FIRST.
  #
  # TO INLINE: replace this with one entry per subject in the shape the SIM
  # spec uses -- `instrument`, `grades` (character, or NULL for an
  # end-of-course subject with the real grade on the academic row), `cuts`
  # (strictly increasing level minimums), `op = ">="`, `level_labels`,
  # `source` -- and set `policy$academic_benchmark$proficient_level` in the
  # same edit.
  academic = list(spec_id = "MA_MCAS"),

  # NO CROSSWALK: no Massachusetts delivery has been inspected, so the
  # canonical ALL-CAPS column names of the three tables (sim_contract.md
  # 3.5 / 4.2 / 5.1) are assumed as-is. The source does say EL status is
  # taken from the October and March SIMS collections (slide 3), which is
  # where the roster's EL_STATUS will come from.
  #
  # When the first files arrive, add `columns` here -- `roster` / `base` /
  # `academic` as canonical = delivered, plus `value_maps` for CONTENT_AREA
  # and GRADE. `00_TEMPLATE_EL_Spec.R` has the shape, including the
  # vendor-style ACCESS column names, and `00_SIM_EL_Spec.R` has a worked
  # `value_maps`.
  data = list(),

  verification = list(
    status        = "provisional",
    verified_by   = NA_character_,
    verified_date = NA_character_,
    method = paste(
      "Read from one public source document (below). Verified against it:",
      "the exit rule, the test window, the K-12 grade range and the",
      "alternate form. Not verified, because the document does not carry",
      "them: timelines, growth targets, the long-term-EL definition, the",
      "academic benchmark, suppression, district-size bands, the program",
      "inventory and error tolerance."),
    notes = paste(
      "provisional rather than verified is the point: one policy field is",
      "sourced (policy$exit) and the rest are placeholders. Every",
      "placeholder carries source = provisional, so print.el_spec() and",
      "Prep 1 list them as [DERIVED - VERIFY]. Massachusetts is a Type 2",
      "state -- the first shipped spec whose Type gate opens on a real rule,",
      "so Prep 2's criteria crosswalk and contested zone, Check 2's",
      "blocked-by-one-criterion groups and Check 3's contested-zone models",
      "all apply here and are skipped under the Type 1 SIM spec. Ships",
      "without data.")
  ),

  source_documents = list(
    list(title = paste(
           "MA DESE, Understanding and Implementing ACCESS Tests",
           "(14 August 2024)"),
         url = paste0("https://wida.wisc.edu/sites/default/files/Website/",
                      "State%20Pages/Massachusetts/",
                      "MA-Understanding-and-Implementing-ACCESS-Tests-",
                      "August-2024.pdf")),
    list(title = "Massachusetts state page, WIDA consortium",
         url   = "https://wida.wisc.edu/memberships/consortium/ma"),
    list(title = "DESE ACCESS for ELLs",
         url   = "https://www.doe.mass.edu/mcas/access/")
  )
)
