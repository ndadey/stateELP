# =============================================================================
# 00_Demographics_Lookups.R  --  display labels for coded demographic values
# -----------------------------------------------------------------------------
# Kept out of the el_spec files on purpose: these are presentation labels, not
# policy or measurement parameters, and several states share them. A spec points
# here through data$demographics_spec.
#
# Every lookup is a named character vector: names are the values as delivered,
# elements are what a table or axis should show. A value absent from a lookup
# renders as itself -- a lookup never silently drops a category, because a
# dropped category is a suppressed group nobody can see.
#
# Row order is display order. "Other" and "Unknown" sort last by convention.
# =============================================================================

Demographics_Lookups <- list(

  home_language = c(
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

  service_model = c(
    esl_pullout       = "ESL, pull-out",
    esl_pushin        = "ESL, push-in",
    sheltered         = "Sheltered content instruction",
    bilingual_tw      = "Dual language / two-way bilingual",
    bilingual_trans   = "Transitional bilingual",
    newcomer_program  = "Newcomer program",
    refused_services  = "Services refused"
  ),

  sped_classification = c(
    SLD = "Specific learning disability",
    SLI = "Speech or language impairment",
    OHI = "Other health impairment",
    AUT = "Autism",
    MD  = "Multiple disabilities",
    OTH = "Other classification"
  ),

  sex = c(M = "Male", F = "Female", X = "Non-binary / not reported"),

  frpl_status = c(F = "Free lunch", R = "Reduced-price lunch", N = "Neither"),

  yes_no = c(Y = "Yes", N = "No"),

  el_status = c(
    current = "Current EL",
    former  = "Former EL",
    never   = "Never EL (English-only)"
  ),

  # Monitoring years are sub-states of `former`, not a fourth status.
  el_status_detail = c(
    current      = "Current EL",
    monitoring_1 = "Former EL, monitoring year 1",
    monitoring_2 = "Former EL, monitoring year 2",
    monitoring_3 = "Former EL, monitoring year 3",
    monitoring_4 = "Former EL, monitoring year 4",
    never        = "Never EL (English-only)"
  ),

  elp_form = c(annual = "Annual form", alternate = "Alternate form"),

  exit_category = c(
    on_time     = "Exited on time",
    prolonged   = "Prolonged classification",
    disappeared = "Disappeared (censored)"
  )
)
