# =============================================================================
# Display_Labels.R  --  display labels for coded roster and analysis values
# -----------------------------------------------------------------------------
# Presentation labels only: nothing here is a policy or measurement parameter,
# and no el_spec points at this file. It was formerly
# inst/specs/00_Demographics_Lookups.R, reached through data$demographics_spec;
# that pointer is gone from every spec, and the file moved out of inst/specs/
# because el_spec_files() lists everything in that directory and this file is
# not a spec -- it had to be skipped by name in test-spec.R.
#
# Renamed because the old name described only half the contents. Two kinds of
# thing live here:
#
#   1. Demographic labels -- HOME_LANGUAGE, SERVICE_MODEL, SPED_CLASSIFICATION,
#      GENDER, FRL_DETAIL. Presentation only; no stage requires them.
#
#   2. Analytic vocabulary the stages DO depend on -- EL_STATUS,
#      EL_STATUS_DETAIL, ELP_FORM, exit_category. `exit_category` is Check 2's
#      crosstab column set; EL_STATUS carries the locked three-level status and
#      EL_STATUS_DETAIL the four federal monitoring years; ELP_FORM is the
#      annual/alternate split behind the "alternate forms never enter headline"
#      rule. Do not drop these along with the demographics.
#
# Every lookup is keyed by the ALL-CAPS roster column it labels
# (sim_contract.md 3.5) and is a named character vector: names are the VALUES as
# delivered (lowercase codes stay lowercase -- only column names are ALL CAPS),
# elements are what a table or axis should show. A value absent from a lookup
# renders as itself -- a lookup never silently drops a category, because a
# dropped category is a suppressed group nobody can see.
#
# RACE_ETHNICITY needs no lookup: its values are already the federal labels.
# `yes_no` and `exit_category` label values, not a single column.
#
# Row order is display order. "Other" and "Unknown" sort last by convention.
# =============================================================================

Display_Labels <- list(

  HOME_LANGUAGE = c(
    spa = "Spanish",
    por = "Portuguese",
    ara = "Arabic",
    guj = "Gujarati",
    hat = "Haitian Creole",
    cmn = "Mandarin",
    kor = "Korean",
    urd = "Urdu",
    tgl = "Tagalog",
    eng = "English",
    oth = "Other"
  ),

  SERVICE_MODEL = c(
    esl_pullout       = "ESL, pull-out",
    esl_pushin        = "ESL, push-in",
    sheltered         = "Sheltered content instruction",
    bilingual_tw      = "Dual language / two-way bilingual",
    bilingual_trans   = "Transitional bilingual",
    newcomer_program  = "Newcomer program",
    refused_services  = "Services refused"
  ),

  SPED_CLASSIFICATION = c(
    SLD = "Specific learning disability",
    SLI = "Speech or language impairment",
    OHI = "Other health impairment",
    AUT = "Autism",
    MD  = "Multiple disabilities",
    OTH = "Other classification"
  ),

  GENDER = c(M = "Male", F = "Female", X = "Non-binary / not reported"),

  FRL_DETAIL = c(F = "Free lunch", R = "Reduced-price lunch", N = "Neither"),

  yes_no = c(Y = "Yes", N = "No"),

  EL_STATUS = c(
    current = "Current EL",
    former  = "Former EL",
    never   = "Never EL (English-only)"
  ),

  # Monitoring years are sub-states of `former`, not a fourth status.
  EL_STATUS_DETAIL = c(
    current      = "Current EL",
    monitoring_1 = "Former EL, monitoring year 1",
    monitoring_2 = "Former EL, monitoring year 2",
    monitoring_3 = "Former EL, monitoring year 3",
    monitoring_4 = "Former EL, monitoring year 4",
    never        = "Never EL (English-only)"
  ),

  ELP_FORM = c(annual = "Annual form", alternate = "Alternate form"),

  exit_category = c(
    on_time     = "Exited on time",
    prolonged   = "Prolonged classification",
    disappeared = "Disappeared (censored)"
  )
)
