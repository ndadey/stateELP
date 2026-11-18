# =============================================================================
# 00_ELPA21_EL_Spec.R  --  no data; the anti-WIDA guardrail
# -----------------------------------------------------------------------------
# This spec exists to break WIDA assumptions that would otherwise leak into
# R/spec.R or any compute_*(). It differs from the SIM spec on the axes the
# package abstracts over:
#
#   * the score is a CATEGORICAL determination, not a numeric composite, and
#     it reports a level and no scale score;
#   * no bands are declared, so the package default of one band per level
#     applies rather than the WIDA 4.0-4.4 / 4.5-4.9 scheme.
#
# Exit is the "Proficient" determination, level 3 of 3. ELPA21 reports
# Proficient when every domain is at level 4 or 5, so this is the consortium's
# domain profile as the vendor computes it, written as one threshold. The
# profile itself is also writable, as a Type 6 rule on the domains:
#   exit = c(listening = 4, speaking = 4, reading = 4, writing = 4)
#
# Values are illustrative of the consortium's published structure, not any one
# member state's rule. No data ships for it. status = "unverified".
# =============================================================================

ELPA21_EL_Spec <- stateELP::el_spec(
  id     = "ELPA21",
  name   = "ELPA21 (illustrative)",
  status = "unverified",

  elp = list(
    test   = "ELPA21 Summative",
    score  = "determination",
    label  = "Overall Performance Determination",
    levels = c("Emerging", "Progressing", "Proficient")   # lowest first
  ),

  exit = c(determination = 3),          # "Proficient"

  timelines = list(max = 5, on_time = 5),

  min_cell = 10
)
