# Non-standard-evaluation column names used across the package.
#
# Package code refers to columns through `.data$X` (imported from rlang in
# R/stateELP-package.R), which needs no declaration. Any name a dplyr verb
# refers to BARE must be declared here or R CMD check reports it as an
# undefined global. Grouped by the table it belongs to, in the ALL-CAPS names
# of sim_contract.md 0.4.0, so a stage session can see at a glance whether a
# name is already available.

utils::globalVariables(c(
  # --- keys, shared by all three tables --------------------------------------
  "STUDENT_ID", "YEAR", "GRADE",

  # --- roster: status and cohort ---------------------------------------------
  "EL_STATUS", "EL_STATUS_DETAIL", "EL_MONITOR_YEAR", "EL", "EVER_EL",
  "EXIT_OBSERVED", "YEAR_IN_STATUS", "ENTRY_GRADE", "COHORT_START_YEAR",
  "COHORT_LABEL", "LEFT_TRUNCATED", "INITIAL_ELP", "ELP_FORM",
  "SAMPLE_WEIGHT",

  # --- roster: demographics and program --------------------------------------
  "DISTRICT_ID", "SCHOOL_ID", "GENDER", "RACE_ETHNICITY", "FRL", "FRL_DETAIL",
  "IEP", "SPED_CLASSIFICATION", "HOME_LANGUAGE", "SERVICE_MODEL",
  "IMMIGRANT_STATUS", "NEWCOMER_FLAG", "SIFE_FLAG", "DISTRICT_SIZE_BAND",

  # --- base table ------------------------------------------------------------
  "COMPONENT", "SCALE_SCORE", "ACHIEVEMENT_LEVEL",

  # --- academic table --------------------------------------------------------
  "SUBJECT", "ACHIEVEMENT_LEVEL_ALT",

  # --- derived: elp_headline() and R/utils.R ---------------------------------
  "ELP_BAND", "LT_EL", "START_GRADE", "FIRST_CURRENT_YEAR",
  "FIRST_FORMER_YEAR", "EXIT_YEAR", "EXIT_YEAR_IN_STATUS",
  "LAST_OBSERVED_YEAR", "LAST_YEAR_IN_STATUS", "CENSOR_TYPE", "CENSOR_YEAR",
  "CENSORED", "EXITED", "ON_TIME_YEARS", "EXIT_EVER", "EXIT_MAX",
  "EXIT_ONTIME", "NO_EXIT", "DISAPPEARED", "ON_TIME", "PROLONGED",
  "SUPPRESSED",

  # --- Check 1 ---------------------------------------------------------------
  "N_ENTRY", "FOLLOWUP_YEARS", "SELECTED",
  "GROUP", "N_AT_ENTRY", "N_EXITED_CUM", "CUM_EXIT_RATE",

  # --- generic summary names -------------------------------------------------
  "N", "N_WEIGHTED", "PCT"
))
