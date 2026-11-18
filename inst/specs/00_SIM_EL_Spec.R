# =============================================================================
# 00_SIM_EL_Spec.R  --  the spec for the shipped simulated data
# -----------------------------------------------------------------------------
# Matches data-raw/el_long_sim.R and sim_contract.md v0.4.0: the bands of 4.5
# (the 4.5 edge IS the exit cut, so the top two bands hold only students in
# their exit year), the exit rule of 4.7, min_cell and the maximum timeline
# from the P block of 2.2, and the academic cuts of 5.4 (level 4 of 5 is
# proficient; its minimum is 750). The 5-year on-time timeline is a choice,
# not a source: the contract reports both <=5 and <=6 year exit shares.
#
# status = "synthetic": these values describe a fixture, and no number
# produced from them may be reported as a finding about any real state.
# Nothing is sourced, so every field reads [DERIVED - VERIFY].
#
# A state starts from 00_TEMPLATE_EL_Spec.R, not from this file.
# =============================================================================

SIM_EL_Spec <- stateELP::el_spec(
  id     = "SIM",
  name   = "Simulated state",
  status = "synthetic",

  elp = list(
    test   = "WIDA ACCESS",
    score  = "overall",               # the score every stage runs on
    label  = "Overall Composite",     # what the reports call it
    levels = c(1, 6),                 # proficiency-level range
    bands  = c(1, 2, 3, 4, 4.5, 5)    # 1.0-1.9, 2.0-2.9, ... 4.5-4.9, 5.0+
  ),

  exit = c(overall = 4.5),            # minimum level; every criterion must be met

  timelines = list(
    max     = 6,                      # maximum years to exit
    on_time = 5,                      # on-time (ESSA) timeline
    lt_el   = 6                       # long-term EL from year 6 in status
  ),

  # The proficient cut: the lowest proficient scale score, every grade alike.
  # The proficient level: level 4 of the five, which Check 2 reads off the
  # file's own performance levels. Check 3 drives its grade loop from
  # `grades`, never from the observed span.
  academic = list(
    ELA  = list(grades = 3:11, proficient_cut = 750, proficient_level = 4),
    Math = list(grades = 3:11, proficient_cut = 750, proficient_level = 4)
  ),

  min_cell = 10,

  # The sim stamps LEP_PROGRAM_START_DATE on 15 September of the SPRING-year
  # label (sim_contract.md 3.5), so its calendar year is the entry year.
  delivery = list(entry_date_rule = "calendar_year")
)
