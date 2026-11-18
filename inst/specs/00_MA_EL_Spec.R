# =============================================================================
# 00_MA_EL_Spec.R  --  Massachusetts, ACCESS for ELLs (WIDA), 2024-25.
# -----------------------------------------------------------------------------
# ONE VALUE IN THIS FILE IS SOURCED: the exit rule. Massachusetts DESE,
# "Understanding and Implementing ACCESS Tests" (14 August 2024), states on
# slide 3, as a footnote to the participation requirements:
#
#     "Exit criteria for ACCESS is 4.2 overall and 3.9 literacy.
#      WIDA Alternate ACCESS exit criteria will be announced this fall."
#
# A composite plus a floor (Type 2): Overall Composite level 4.2 AND Literacy
# (the Reading + Writing composite, the vocabulary's `written`) level 3.9.
#
# EVERYTHING ELSE IS A PLACEHOLDER, because the source is an operational
# test-administration deck, not a policy document. The maximum timeline and
# the suppression minimum are NOT Massachusetts values; the on-time timeline
# and the long-term-EL year are left out so that their absence reads as
# unknown rather than as policy. Only `exit` is named in `sources`, so every
# other field reads [DERIVED - VERIFY], and `status` is "provisional": part
# sourced, part not. Replace them from the department's own regulations and
# reporting rules before any Massachusetts number is reported.
#
# NO MCAS BLOCK: the source carries no academic cut scores, and inventing
# them is how the former NJ spec failed (CLAUDE.md 5). Without `academic`,
# Check 4 returns NULL with a note; Check 3, Check 1's Model 2 and Check 2's
# academic summary still error on a delivery that carries an academic table
# (CLAUDE.md 3, Known issues). Add `academic` -- grades and proficient cuts
# per subject -- once the department's cuts are in hand.
#
# NO DELIVERY HAS BEEN INSPECTED, so there is no `delivery` crosswalk.
#
# BANDS, a known tension left for the user [PROPOSED - CONFIRM]: the deck's
# bands put the 4.2 cut INSIDE "4.0-4.4", so Check 3's exit band and Check 2's
# crosstab rows are coarser than the rule. Splitting at the cut --
# bands = c(1, 2, 3, 4, 4.2, 4.5, 5) -- would align them with it, but changes
# every Check 2 and Check 3 figure for Massachusetts, so it is not taken.
# =============================================================================

MA_EL_Spec <- stateELP::el_spec(
  id     = "MA",
  name   = "Massachusetts",
  status = "provisional",

  elp = list(
    test   = "ACCESS for ELLs (WIDA)",
    score  = "overall",
    label  = c(overall = "Overall Composite", written = "Literacy"),
    levels = c(1, 6),
    bands  = c(1, 2, 3, 4, 4.5, 5)
  ),

  exit = c(overall = 4.2, written = 3.9),   # Overall 4.2 AND Literacy 3.9

  timelines = list(max = 6),                # PLACEHOLDER, not Massachusetts policy

  min_cell = 10,                            # PLACEHOLDER

  sources = c(
    exit = paste(
      "MA DESE, Understanding and Implementing ACCESS Tests",
      "(14 August 2024), slide 3 footnote: Exit criteria for ACCESS is",
      "4.2 overall and 3.9 literacy.")
  )
)
