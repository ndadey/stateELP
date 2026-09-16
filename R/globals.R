# Non-standard-evaluation column names used across the package.
#
# Every name a dplyr verb refers to bare must be declared here or R CMD check
# reports it as an undefined global. Grouped by the table it belongs to so a
# stage session can see at a glance whether a name is already available.

utils::globalVariables(c(
  # --- spine keys and status -------------------------------------------------
  "student_id", "school_year", "grade", "el_status", "el_status_detail",
  "ever_el", "exit_observed", "left_truncated", "cohort_label",
  "cohort_start_year", "entry_grade", "year_in_status", "sample_weight",

  # --- spine: headline ELP ---------------------------------------------------
  "elp_form", "elp_level", "elp_scale", "elp_band", "initial_elp",

  # --- spine: demographics and program ---------------------------------------
  "district_id", "school_id", "sex", "frpl_status", "econ_disadv",
  "sped_classification", "swd_flag", "home_language", "service_model",
  "immigrant_status", "newcomer_flag", "sife_flag", "district_size_band",

  # --- components table ------------------------------------------------------
  "component", "form", "scale_score", "level",

  # --- academic table --------------------------------------------------------
  "subject", "perf_level", "alt_perf_level",

  # --- derived by R/utils.R --------------------------------------------------
  "first_current_year", "last_el_year", "last_observed_year", "exit_year",
  "exit_year_in_status", "years_in_status", "censored", "censor_year",
  "exit_ever", "exit_max", "exit_ontime", "no_exit", "on_time", "prolonged",
  "lt_el", "start_grade", "cohort_n",

  # --- generic summary names -------------------------------------------------
  "n", "n_students", "n_weighted", "pct", "value"
))
