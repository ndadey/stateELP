# =============================================================================
# 00_TEMPLATE_EL_Spec.R  --  a WIDA-shaped template. NO STATE'S POLICY.
# -----------------------------------------------------------------------------
# EVERY VALUE HERE IS A PLACEHOLDER: the exit cut, the timelines, the academic
# cut scores and the file layout describe no state, and must not be read as
# any state's exit criteria. What is real is the shape.
#
# To start a real state spec, copy this file (outside inst/specs/ is fine:
# read_el_spec() takes a path), replace every value, say where each sourced
# value is written down in `sources`, and set `status`. build_el_spec() asks
# the same questions at the R console and writes the file for you.
#
# It shows every optional field, so it is also the suite's test of them: cut
# scores by grade, the performance-level names, and a `delivery` block with a
# non-identity crosswalk (the SIM spec's file needs none).
#
# What is NOT a field, because the package fixes it for every state: the exit
# rule compares each score's level with >=, and a student must meet every
# criterion; exit is dated to the first year observed as former.
# =============================================================================

# PLACEHOLDER cut scores: the lowest proficient scale score in each grade.
template_cuts <- c("3" = 450, "4" = 455, "5" = 460, "6" = 465, "7" = 470,
                   "8" = 475)

TEMPLATE_EL_Spec <- stateELP::el_spec(
  id     = "TEMPLATE",
  name   = "TODO: state name",
  status = "unverified",               # verified / unverified / provisional

  # ---- the ELP test ----------------------------------------------------------
  elp = list(
    test   = "ACCESS for ELLs (WIDA)",
    score  = "overall",                # WIDA's Literacy score is "written"
    label  = "Overall Composite",
    levels = c(1, 6),
    bands  = c(1, 2, 3, 4, 4.5, 5),    # 1.0-1.9 ... 4.0-4.4, 4.5-4.9, 5.0+
    # Only needed when the file reports levels as names:
    level_labels = c("Entering", "Emerging", "Developing",
                     "Expanding", "Bridging", "Reaching")
  ),

  # ---- the exit rule ---------------------------------------------------------
  # Minimum level on each score. A floor is a second entry, e.g.
  # c(overall = 4.5, written = 4.0).
  exit = c(overall = 4.5),             # PLACEHOLDER

  # ---- timelines, in years in EL status --------------------------------------
  timelines = list(
    max     = 6,                       # PLACEHOLDER: maximum years to exit
    on_time = 5,                       # PLACEHOLDER: on-time (ESSA) timeline
    lt_el   = 6                        # PLACEHOLDER: long-term EL from year 6
  ),

  # ---- the state academic tests ----------------------------------------------
  # proficient_cut: one number for every grade, or one per grade named by
  # grade. proficient_level: the lowest proficient performance level (lowest
  # = 1); Check 2 reads proficiency off the file's own levels with it, and a
  # grade with no cut falls back to it in Checks 3 and 4.
  academic = list(
    ELA = list(
      grades           = 3:8,
      proficient_cut   = template_cuts,
      proficient_level = 3,
      level_labels     = c("Level 1", "Level 2", "Level 3", "Level 4")
    ),
    Math = list(
      grades           = 3:8,
      proficient_cut   = template_cuts,
      proficient_level = 3,
      level_labels     = c("Level 1", "Level 2", "Level 3", "Level 4")
    )
  ),

  min_cell = 10,                       # PLACEHOLDER: the state's suppression rule

  # Where each sourced value is written down; anything not named reads
  # [DERIVED - VERIFY]. e.g. sources = c(exit = "State regulation 1.23, 2019")
  sources = NULL,

  # ---- how the state's files differ from the canonical tables ----------------
  # Crosswalks are canonical name = the file's own column name; a column not
  # listed keeps its name. The names below are invented, in the idiom real
  # deliveries use. GRADE and EL_STATUS are left out on purpose.
  delivery = list(
    roster = c(STUDENT_ID = "Student_UID", YEAR = "school_year"),
    base   = c(STUDENT_ID = "Student_UID", YEAR = "school_year"),
    value_maps = list(
      GRADE = c("0" = "K"),            # kindergarten must arrive as "K"
      CONTENT_AREA = list(
        academic = c(READING = "ELA", MATHEMATICS = "Math")
      )
    )
  )
)
