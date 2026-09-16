# ==============================================================================
# data-raw/el_long_sim.R  --  stateELP
#
# Contract: sim_contract.md v0.3.0. Bump the contract when a column, a grain,
# or a distribution changes here.
#
# Synthetic EL data in the THREE-TABLE component form.
# ------------------------------------------------------------------------------
# COMPONENT is a row key, not a set of columns. Domains, composites, and the
# overall score are rows in one long table; which of them is the *headline* is a
# single spec field. Same atomic-layer-plus-resolver split as stateSpecs'
# tests + map, and the same move stateSummative made with subject.
#
# Produces three tables, all keyed on student_id x school_year:
#
#   el_roster_sim       ROSTER      one row per student-year: the enrollment /
#                                   status frame (grade, el_status, demographics,
#                                   elp_form). Counts students; carries NO
#                                   measurement columns. Untested years and
#                                   former / never students exist ONLY here.
#   el_long_sim         BASE        the base table for all analysis. One row per
#                                   student-year-component: component, form,
#                                   scale_score, level. WIDA fills eight rows per
#                                   tested student-year. The headline is resolved
#                                   FROM this table via spec$elp$headline; the
#                                   roster caches nothing. Rows exist only for
#                                   CURRENT student-years: formers do not test.
#   academic_long_sim   ACADEMIC    one row per student-year-subject:
#                                   scale_score, perf_level.
#
# Nothing below hardcodes a component name outside `elp_spec$components`.
# Composites are computed by iterating each component's `from` weights, so
# adding "expressive" (writing + speaking) is a spec edit, not a code edit.
# assert_no_hardcoded_components() at the bottom enforces this.
#
# Span 2015-2025. Spring 2020 produced no state summative anywhere; spring 2021
# ran at reduced participation for both the ELP and content assessments.
#
# [DERIVED - VERIFY] marks every quantity that is a modelling choice rather
# than a value taken from an official source.
#
# Reproduce with:  Rscript data-raw/el_long_sim.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

set.seed(20260727)

# ==============================================================================
# 0. SPEC -- the structure declaration. Code reads this; it never assumes.
# ==============================================================================
# WIDA-shaped, matching what inst/specs/00_SIM_EL_Spec.R will declare. WIDA's
# native "Literacy" maps onto the controlled-vocabulary name `written`; the
# mapping lives in the spec, not in the data.

elp_spec <- list(
  instrument = "WIDA ACCESS",
  components = list(
    listening     = list(kind = "domain",    measures = c("scale", "level")),
    speaking      = list(kind = "domain",    measures = c("scale", "level")),
    reading       = list(kind = "domain",    measures = c("scale", "level")),
    writing       = list(kind = "domain",    measures = c("scale", "level")),
    oral          = list(kind = "composite", measures = c("scale", "level"),
                         from = c(listening = .5,  speaking = .5)),
    written       = list(kind = "composite", measures = c("scale", "level"),
                         from = c(reading   = .5,  writing  = .5),
                         native_name = "Literacy"),
    comprehension = list(kind = "composite", measures = c("scale", "level"),
                         from = c(listening = .3,  reading  = .7)),
    overall       = list(kind = "composite", measures = c("scale", "level"),
                         from = c(listening = .15, speaking = .15,
                                  reading   = .35, writing  = .35))
  ),
  headline    = "overall",
  level_scale = list(type = "numeric", range = c(1, 6)),
  bands       = list(breaks = c(1, 2, 3, 4, 4.5, 5, 7),
                     labels = c("1.0-1.9","2.0-2.9","3.0-3.9",
                                "4.0-4.4","4.5-4.9","5.0+")),
  initial_elp_source = "first_annual"
)

exit_spec <- list(rule = "overall level >= 4.5",
                  component = "overall", threshold = 4.5)

# Derived from the spec, never typed out again.
COMPONENTS <- names(elp_spec$components)
kinds      <- vapply(elp_spec$components, `[[`, "", "kind")
DOMAINS    <- COMPONENTS[kinds == "domain"]
COMPOSITES <- COMPONENTS[kinds == "composite"]
HEADLINE   <- elp_spec$headline

stopifnot(HEADLINE %in% COMPONENTS,
          exit_spec$component %in% COMPONENTS,
          all(unlist(lapply(elp_spec$components[COMPOSITES],
                            function(x) names(x$from))) %in% COMPONENTS))

# ---- Simulation parameters ---------------------------------------------------
# Env overrides give the fixture-sized build without editing this file:
#   STATEELP_K_COHORT_N=300 STATEELP_NEVER_EL_N=4250  (keeps the never/ever ratio)
env_int <- function(name, default) {
  v <- Sys.getenv(name, unset = "")
  if (nzchar(v)) as.integer(v) else default
}
P <- list(
  years        = 2015:2025,
  k_cohort_n   = env_int("STATEELP_K_COHORT_N", 2400L),  # NEW EL students entering at grade K each year
  never_el_n   = env_int("STATEELP_NEVER_EL_N", 34000L), # never-EL reference students (10% SAMPLE)
  never_sample_frac = 0.10,
  n_districts  = 12,
  min_cell     = 10,
  max_years    = 6,
  # [DERIVED - VERIFY] minimum scale score of academic levels 2-5; a score AT a
  # cut belongs to the upper level (sim_contract.md 5.4). Level 4 is proficient.
  academic_cuts = c(700, 735, 750, 790),
  prof_cut     = 750          # = academic_cuts[3]; the score model's center
)
stopifnot(P$prof_cut == P$academic_cuts[3])

# [DERIVED - VERIFY] Entry-grade shape follows the "When do students enter EL
# status?" exhibit in the EL Data Routines outline.
entry_grade_p <- c("K"=.550,"1"=.072,"2"=.060,"3"=.050,"4"=.062,"5"=.038,
                   "6"=.050,"7"=.026,"8"=.034,"9"=.028,"10"=.017,"11"=.013)
entry_grade_p <- entry_grade_p / sum(entry_grade_p)

# [DERIVED - VERIFY] NJ-like mix; the presence of Spanish, Portuguese, Arabic,
# Gujarati, Haitian Creole and Chinese follows OELA's most-spoken-language
# exhibits. Percentages chosen so each clears min_cell within one K cohort.
lang_p <- c(spa=.55, por=.08, ara=.07, guj=.05, hat=.05,
            cmn=.05, kor=.04, urd=.04, tgl=.03, oth=.04)

# LIEP categories follow the OELA Exhibit 4.1 taxonomy.
service_p <- c(esl_pullout=.30, esl_pushin=.22, sheltered=.13, bilingual_tw=.09,
               bilingual_trans=.11, newcomer_program=.09, refused_services=.06)

sped_p <- c(SLD=.34, SLI=.24, OHI=.14, AUT=.12, MD=.09, OTH=.07)
p_swd  <- .15   # [DERIVED - VERIFY] per OELA ELs-with-disabilities exhibits

# OELA reports ELP participation ~81% in SY 2020-21 and ~97% in SY 2021-22.
elp_participation <- c("2015"=.97,"2016"=.97,"2017"=.97,"2018"=.97,"2019"=.97,
                       "2020"=.93,"2021"=.81,"2022"=.97,"2023"=.97,
                       "2024"=.97,"2025"=.97)
academic_participation <- c("2015"=.96,"2016"=.96,"2017"=.96,"2018"=.96,"2019"=.96,
                            "2020"=.00,"2021"=.72,"2022"=.94,"2023"=.96,
                            "2024"=.96,"2025"=.96)

tested_grades_academic <- as.character(3:11)
YR <- P$years; Y0 <- min(YR); Y1 <- max(YR)
grade_num <- function(g) ifelse(g == "K", 0L, suppressWarnings(as.integer(g)))
num_grade <- function(n) ifelse(n == 0L, "K", as.character(n))

# Banding reads breaks and labels from the spec, never a literal.
band_of <- function(level) {
  as.character(cut(level, breaks = elp_spec$bands$breaks,
                   labels = elp_spec$bands$labels,
                   right = FALSE, include.lowest = TRUE))
}
# Waiver sets are built from DOMAINS in spec order: the first pair, the second
# pair, and a single domain. A sensory disability typically removes a pair.
waiver_pool <- c(paste(DOMAINS[1:2], collapse = ","),
                 paste(DOMAINS[3:4], collapse = ","),
                 DOMAINS[1])

# [DERIVED - VERIFY] synthetic monotone level -> scale map, ACCESS-like range
lvl_to_scale <- function(lvl, grade_n) {
  round(180 + 42 * lvl + 7.5 * grade_n + rnorm(length(lvl), 0, 9))
}

# ==============================================================================
# 1. Districts and schools
# ==============================================================================
districts <- tibble(
  district_id = sprintf("D%02d", 1:P$n_districts),
  d_weight    = c(.20,.16,.11,.09,.08,.07,.06,.06,.05,.05,.04,.03),
  n_schools   = c(14, 11, 8, 7, 6, 5, 4, 4, 3, 3, 2, 2)
)
schools <- districts %>% rowwise() %>%
  do(tibble(district_id = .$district_id,
            school_code = sprintf("S%02d", seq_len(.$n_schools)))) %>%
  ungroup()

# ==============================================================================
# 2. EL roster
# ==============================================================================
new_per_year <- round(P$k_cohort_n / entry_grade_p[["K"]])

make_entrants <- function(entry_year, n, stock = FALSE) {
  if (n <= 0) return(NULL)
  eg <- sample(names(entry_grade_p), n, replace = TRUE, prob = entry_grade_p)
  if (stock) {
    yrs_in <- sample(1:6, n, replace = TRUE, prob = c(.26,.22,.18,.14,.11,.09))
    entry_year <- Y0 - yrs_in
  }
  tibble(entry_year = entry_year, entry_grade = eg)
}

roster <- bind_rows(
  make_entrants(NA, round(new_per_year * 3.6), stock = TRUE),
  bind_rows(lapply(YR, function(y) make_entrants(y, new_per_year)))
) %>%
  filter(!is.na(entry_year)) %>%
  mutate(student_id = sprintf("S%06d", row_number()))

n_el <- nrow(roster)

roster <- roster %>%
  mutate(
    home_language = sample(names(lang_p), n_el, TRUE, lang_p),
    sex           = sample(c("M","F","X"), n_el, TRUE, c(.508,.489,.003)),
    hispanic      = ifelse(home_language == "spa",
                           sample(c("Y","N"), n_el, TRUE, c(.94,.06)),
                           sample(c("Y","N"), n_el, TRUE, c(.07,.93))),
    district_id   = sample(districts$district_id, n_el, TRUE, districts$d_weight),
    frpl_status   = sample(c("F","R","N"), n_el, TRUE, c(.62,.11,.27)),
    gifted_flag   = sample(c("Y","N"), n_el, TRUE, c(.03,.97)),
    migrant_status= sample(c("Y","N"), n_el, TRUE, c(.04,.96)),
    swd_flag      = sample(c("Y","N"), n_el, TRUE, c(p_swd, 1 - p_swd)),
    sped_classification = ifelse(swd_flag == "Y",
                                 sample(names(sped_p), n_el, TRUE, sped_p),
                                 NA_character_),
    service_model = sample(names(service_p), n_el, TRUE, service_p),
    entry_grade_n = grade_num(entry_grade),
    race_asian = ifelse(home_language %in% c("cmn","kor","tgl","urd","guj"),
                        sample(c("Y","N"), n_el, TRUE, c(.88,.12)),
                        sample(c("Y","N"), n_el, TRUE, c(.04,.96))),
    race_black = ifelse(home_language == "hat",
                        sample(c("Y","N"), n_el, TRUE, c(.92,.08)),
                        sample(c("Y","N"), n_el, TRUE, c(.07,.93))),
    race_white = ifelse(home_language %in% c("spa","por","ara"),
                        sample(c("Y","N"), n_el, TRUE, c(.55,.45)),
                        sample(c("Y","N"), n_el, TRUE, c(.12,.88))),
    race_american_indian = sample(c("Y","N"), n_el, TRUE, c(.006,.994)),
    race_pacific         = sample(c("Y","N"), n_el, TRUE, c(.005,.995))
  )

cob_map <- c(spa="MEX",por="BRA",ara="EGY",guj="IND",hat="HTI",
             cmn="CHN",kor="KOR",urd="PAK",tgl="PHL",oth="XXX")
roster <- roster %>%
  mutate(
    us_born = entry_grade_n <= 1 & runif(n_el) < .72,
    country_of_birth = ifelse(us_born, "USA", unname(cob_map[home_language])),
    immigrant_status = ifelse(!us_born, "Y", "N"),
    first_entry_us_school_date = as.Date(sprintf("%d-09-05", entry_year)) +
      sample(0:60, n_el, TRUE),
    # [DERIVED - VERIFY] SIFE among secondary newcomers
    sife_flag = ifelse(!us_born & entry_grade_n >= 4 & runif(n_el) < .28, "Y", "N")
  )

# ==============================================================================
# 3. Latent acquisition -> DOMAIN levels
# ==============================================================================
# Component-first: the model generates the four domain levels. Composites are
# built up from them in section 4 using the spec's `from` weights. Nothing here
# generates an overall score directly.
#
# [DERIVED - VERIFY] Latent curve theta_t = 6 - (6 - theta_1) * exp(-r_i(t-1)).
# A low-rate `plateau` subgroup supplies the long-term-EL tail. Tuned so roughly
# 70% of K entrants exit within six years, the direction implied by New Jersey's
# OELA figure of 21.8% of ELs not proficient within five years (SY 2021-22).
# OELA's denominator includes students with fewer than five years in status, so
# that is a plausibility anchor, not a target to reproduce.

roster <- roster %>%
  mutate(
    # capped below the exit threshold: a student screening at or above it would
    # not be identified as an EL at all
    theta_1 = pmin(3.9, pmax(1.0, rnorm(n_el, 1.9 + .075 * entry_grade_n, .62))),
    plateau = runif(n_el) < (.14 + .13*(swd_flag=="Y") + .10*(sife_flag=="Y") +
                               .05*(theta_1 < 1.8)),
    rate = ifelse(plateau, rlnorm(n_el, log(.055), .45),
                  rlnorm(n_el, log(.335), .42)),
    # ACCESS Alternate: ELs with the most significant cognitive disabilities.
    # [DERIVED - VERIFY] ~1% of ELs, concentrated in MD/AUT.
    elp_form = ifelse(swd_flag == "Y" & sped_classification %in% c("MD","AUT") &
                        runif(n_el) < .12, "alternate", "annual"),
    # Domain waivers: a documented sensory disability means one or more domains
    # are not administered. Any composite drawing on a waived domain is then
    # non-computable -- the case that breaks naive composite recalculation.
    # [DERIVED - VERIFY] ~2% of ELs.
    waived = ifelse(swd_flag == "Y" & runif(n_el) < .14,
                    sample(waiver_pool, n_el, TRUE, c(.5,.25,.25)), NA_character_)
  )

max_span <- 14L
traj <- roster %>%
  select(student_id, entry_year, entry_grade_n, theta_1, rate, elp_form, waived) %>%
  tidyr::crossing(t = 1:max_span) %>%
  mutate(school_year = entry_year + t - 1L,
         grade_n     = entry_grade_n + t - 1L) %>%
  filter(grade_n <= 12, school_year <= Y1) %>%
  arrange(student_id, school_year) %>%
  mutate(
    theta = pmin(6, pmax(1, 6 - (6 - theta_1) * exp(-rate * (t - 1)) +
                           rnorm(n(), 0, .17))),
    part_p = coalesce(unname(elp_participation[as.character(school_year)]), .97),
    tested = runif(n()) < part_p
  )

# Domain levels around the latent curve.
# [DERIVED - VERIFY] offsets reflect the usual receptive-before-productive order.
dom_offset <- c(listening = .35, speaking = .20, reading = -.15, writing = -.30)
for (d in DOMAINS) {
  traj[[d]] <- pmin(6, pmax(1, round(traj$theta +
                                       rnorm(nrow(traj), dom_offset[[d]], .45), 1)))
  traj[[d]] <- ifelse(grepl(d, traj$waived), NA_real_, traj[[d]])
}

# ==============================================================================
# 4. Composites from the spec's `from` weights, then the headline
# ==============================================================================
# Iterates elp_spec$components. A composite is NA whenever any contributing
# component is NA, which generalises the waiver rule without naming a domain.

for (cp in COMPOSITES) {
  w     <- elp_spec$components[[cp]]$from
  parts <- as.matrix(traj[, names(w), drop = FALSE])
  val   <- as.vector(parts %*% as.numeric(w))
  val[apply(is.na(parts), 1, any)] <- NA_real_
  traj[[cp]] <- round(val, 1)
}

traj$headline_level <- traj[[HEADLINE]]

# ---- Exit determination, read from exit_spec ---------------------------------
traj <- traj %>%
  mutate(exit_eligible = tested & elp_form == "annual" &
           !is.na(.data[[exit_spec$component]]) &
           .data[[exit_spec$component]] >= exit_spec$threshold)

first_exit <- traj %>% filter(exit_eligible) %>%
  group_by(student_id) %>%
  summarise(exit_year = min(school_year), .groups = "drop")

traj <- traj %>%
  left_join(first_exit, by = "student_id") %>%
  filter(is.na(exit_year) | school_year <= exit_year + 4L) %>%   # 4 monitoring yrs
  filter(school_year >= Y0) %>%                                  # clip to panel
  mutate(left_truncated = entry_year < Y0)

# ==============================================================================
# 5. Never-EL reference students
# ==============================================================================
never <- tibble(student_id = sprintf("N%06d", seq_len(P$never_el_n))) %>%
  mutate(
    start_grade_n = sample(0:12, P$never_el_n, TRUE),
    start_year    = sample((Y0 - 12):Y1, P$never_el_n, TRUE),
    home_language = sample(c("eng","spa","oth"), P$never_el_n, TRUE, c(.90,.07,.03)),
    sex           = sample(c("M","F","X"), P$never_el_n, TRUE, c(.509,.488,.003)),
    hispanic      = sample(c("Y","N"), P$never_el_n, TRUE, c(.14,.86)),
    district_id   = sample(districts$district_id, P$never_el_n, TRUE, districts$d_weight),
    frpl_status   = sample(c("F","R","N"), P$never_el_n, TRUE, c(.30,.08,.62)),
    gifted_flag   = sample(c("Y","N"), P$never_el_n, TRUE, c(.09,.91)),
    migrant_status= sample(c("Y","N"), P$never_el_n, TRUE, c(.004,.996)),
    swd_flag      = sample(c("Y","N"), P$never_el_n, TRUE, c(.13,.87)),
    sped_classification = ifelse(swd_flag == "Y",
                                 sample(names(sped_p), P$never_el_n, TRUE, sped_p),
                                 NA_character_),
    race_asian = sample(c("Y","N"), P$never_el_n, TRUE, c(.09,.91)),
    race_black = sample(c("Y","N"), P$never_el_n, TRUE, c(.15,.85)),
    race_white = sample(c("Y","N"), P$never_el_n, TRUE, c(.52,.48)),
    race_american_indian = sample(c("Y","N"), P$never_el_n, TRUE, c(.005,.995)),
    race_pacific         = sample(c("Y","N"), P$never_el_n, TRUE, c(.004,.996)),
    country_of_birth = "USA", immigrant_status = "N", sife_flag = "N",
    elp_form = NA_character_, service_model = NA_character_,
    first_entry_us_school_date = as.Date(NA)
  ) %>%
  tidyr::crossing(t = 1:max_span) %>%
  mutate(school_year = start_year + t - 1L, grade_n = start_grade_n + t - 1L) %>%
  filter(grade_n <= 12, school_year >= Y0, school_year <= Y1) %>%
  select(-t, -start_grade_n, -start_year)

# ==============================================================================
# 6. SPINE -- one row per student-year
# ==============================================================================
el_sy <- traj %>%
  select(student_id, school_year, grade_n, t, exit_year, left_truncated,
         elp_form, tested, waived, headline_level) %>%
  left_join(roster %>% select(-entry_grade_n, -theta_1, -rate, -plateau,
                              -elp_form, -waived, -us_born),
            by = "student_id")

never_sy <- never %>%
  mutate(exit_year = NA_integer_, t = NA_integer_, left_truncated = FALSE,
         tested = FALSE, waived = NA_character_, headline_level = NA_real_,
         entry_year = NA_integer_, entry_grade = NA_character_) %>%
  select(any_of(names(el_sy)))

spine <- bind_rows(el_sy, never_sy) %>% mutate(grade = num_grade(grade_n))

sch <- schools %>% group_by(district_id) %>%
  summarise(codes = list(school_code), .groups = "drop")
spine <- spine %>% left_join(sch, by = "district_id") %>%
  group_by(student_id) %>% mutate(school_code = sample(codes[[1]], 1)) %>%
  ungroup() %>% select(-codes) %>%
  mutate(school_id = paste(district_id, school_code, sep = "-"))

# ---- EL status and monitoring years ------------------------------------------
# el_status keeps three levels so status_coding downstream stays valid; the four
# federal monitoring years (OELA Exhibit 3.12) ride alongside.
spine <- spine %>%
  mutate(
    is_el = !is.na(entry_year),
    el_status = case_when(!is_el ~ "never",
                          is.na(exit_year) ~ "current",
                          school_year <= exit_year ~ "current",
                          TRUE ~ "former"),
    el_monitor_year = ifelse(el_status == "former",
                             pmin(4L, school_year - exit_year), NA_integer_),
    el_status_detail = ifelse(el_status == "former",
                              paste0("monitoring_", el_monitor_year), el_status),
    ever_el = ifelse(is_el, "Y", "N"),
    exit_observed = ifelse(!is.na(exit_year) & school_year == exit_year, 1L, 0L),
    title3_lep_flag = ifelse(el_status == "current" &
                               service_model != "refused_services", "Y", "N"),
    newcomer_flag = ifelse(is_el & immigrant_status == "Y" &
                             school_year - entry_year <= 1, "Y", "N"),
    year_in_status = ifelse(el_status == "current", t, NA_integer_),
    lep_program_start_date = if_else(is_el,
                                     as.Date(sprintf("%d-09-15", entry_year)), as.Date(NA)),
    lep_program_completion_date = if_else(!is.na(exit_year),
                                          as.Date(sprintf("%d-06-15", exit_year)), as.Date(NA)),
    econ_disadv = ifelse(frpl_status %in% c("F","R"), "Y", "N"),
    # Never-ELs are a SAMPLE, not the full non-EL population; weight for shares.
    sample_weight = ifelse(el_status == "never", 1 / P$never_sample_frac, 1)
  )

# ==============================================================================
# 7. COMPONENTS -- one row per student-year-component
# ==============================================================================
comp_annual <- traj %>%
  filter(tested, elp_form == "annual") %>%
  select(student_id, school_year, grade_n, all_of(COMPONENTS)) %>%
  tidyr::pivot_longer(all_of(COMPONENTS), names_to = "component",
                      values_to = "level") %>%
  filter(!is.na(level)) %>%
  mutate(scale_score = lvl_to_scale(level, grade_n), form = "annual")

# Alternate ACCESS reports P1-P3 / A1-A3; stored as the ordinal index 1-6 with
# labels living in the spec, and no scale score. Exercises the level-only case.
comp_alt <- traj %>%
  filter(tested, elp_form == "alternate") %>%
  select(student_id, school_year, grade_n) %>%
  tidyr::crossing(component = c(DOMAINS, HEADLINE)) %>%
  mutate(level = sample(1:6, n(), TRUE, c(.10,.14,.18,.24,.20,.14)),
         scale_score = NA_real_, form = "alternate")

# Population rule: component rows exist only for CURRENT student-years. Formers
# do not test, so the four monitoring years carry no ELP measurement. Applied
# AFTER every random draw above, so the RNG stream is unchanged from v0.1.0
# (sim_contract.md 2.1 / 4.4). `component` is character: the spec supplies
# order and labels, and a factor would coerce on bind_rows() with real data.
# Column names conform to ReportR's student_results_long conventions
# (internal_docs/NOTES-student_results_long.md): ALL CAPS, STUDENT_ID x YEAR
# keys, ACHIEVEMENT_LEVEL for the reported level. Generator internals stay
# lowercase; the rename happens once, here at the output boundary.
el_long_sim <- bind_rows(comp_annual, comp_alt) %>%
  semi_join(spine %>% filter(el_status == "current") %>%
              select(student_id, school_year),
            by = c("student_id", "school_year")) %>%
  transmute(STUDENT_ID = student_id, YEAR = school_year,
            COMPONENT = as.character(component), ELP_FORM = form,
            SCALE_SCORE = as.numeric(scale_score),
            ACHIEVEMENT_LEVEL = as.numeric(level)) %>%
  arrange(STUDENT_ID, YEAR, match(COMPONENT, COMPONENTS))

# ---- Headline resolution -----------------------------------------------------
# The spine's elp_* columns ARE the headline component. One join, one spec field.
headline <- el_long_sim %>%
  filter(COMPONENT == HEADLINE, ELP_FORM == "annual") %>%
  transmute(student_id = STUDENT_ID, school_year = YEAR,
            elp_level = ACHIEVEMENT_LEVEL, elp_scale = SCALE_SCORE)

spine <- spine %>%
  select(-headline_level) %>%
  left_join(headline, by = c("student_id", "school_year")) %>%
  mutate(elp_band = band_of(elp_level))

# Growth target needs a valid prior-year headline.
spine <- spine %>% arrange(student_id, school_year) %>%
  group_by(student_id) %>%
  mutate(prior = lag(elp_level),
         elp_growth_target_met = case_when(
           el_status != "current" ~ NA_character_,
           is.na(prior) | is.na(elp_level) ~ NA_character_,
           elp_level - prior >= 0.4 ~ "Y", TRUE ~ "N")) %>%
  ungroup() %>% select(-prior)

stopifnot(elp_spec$initial_elp_source == "first_annual")
init <- spine %>% filter(el_status == "current", !is.na(elp_band)) %>%
  group_by(student_id) %>% slice_min(school_year, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(student_id, initial_elp = elp_band)
spine <- spine %>% left_join(init, by = "student_id")

# ==============================================================================
# 8. ACADEMIC -- one row per student-year-subject
# ==============================================================================
academic_long_sim <- spine %>%
  filter(grade %in% tested_grades_academic) %>%
  select(student_id, school_year, grade_n, el_status, elp_level, swd_flag, elp_form) %>%
  tidyr::crossing(subject = c("ELA", "Math")) %>%
  mutate(part_p = unname(academic_participation[as.character(school_year)]),
         took = runif(n()) < part_p) %>%
  filter(took) %>%
  mutate(
    status_shift = case_when(el_status == "current" ~ -34,
                             el_status == "former"  ~   6, TRUE ~ 0),
    elp_shift  = ifelse(is.na(elp_level), 0, (elp_level - 3.0) * 9),
    subj_shift = ifelse(subject == "Math", 3, 0),
    swd_shift  = ifelse(swd_flag == "Y", -18, 0),
    on_alt = !is.na(elp_form) & elp_form == "alternate",
    scale_score = ifelse(on_alt, NA_real_,
                         round(P$prof_cut + 4*(grade_n - 6) + status_shift +
                                 elp_shift + subj_shift + swd_shift +
                                 rnorm(n(), 0, 33))),
    # Cuts are level MINIMUMS: findInterval() puts a score exactly at a cut in
    # the upper level, so prof_cut is the lowest proficient score (contract 5.4).
    perf_level = ifelse(on_alt, NA_integer_,
                        findInterval(scale_score, P$academic_cuts) + 1L),
    alt_perf_level = ifelse(on_alt,
                            sample(1:4, n(), TRUE, c(.34,.31,.24,.11)),
                            NA_integer_)
  ) %>%
  # grade rides on the academic row: student_results_long carries it, EOC
  # deliveries report a real tested grade, and the agreement check needs it.
  transmute(STUDENT_ID = student_id, YEAR = school_year,
            GRADE = num_grade(grade_n), SUBJECT = subject,
            SCALE_SCORE = scale_score, ACHIEVEMENT_LEVEL = perf_level,
            ACHIEVEMENT_LEVEL_ALT = alt_perf_level) %>%
  arrange(STUDENT_ID, YEAR, SUBJECT)

# ==============================================================================
# 9. Graduation / Seal, district size, final spine columns
# ==============================================================================
grad <- spine %>% filter(grade == "12") %>%
  distinct(student_id, school_year, ever_el, home_language) %>%
  mutate(
    grad_4yr_status = sample(c("Y","N"), n(), TRUE, c(.84,.16)),
    grad_5yr_status = ifelse(grad_4yr_status == "N",
                             sample(c("Y","N"), n(), TRUE, c(.42,.58)), "Y"),
    ela_grad_pathway  = sample(c("assessment","portfolio","substitute"),
                               n(), TRUE, c(.72,.11,.17)),
    math_grad_pathway = sample(c("assessment","portfolio","substitute"),
                               n(), TRUE, c(.68,.13,.19)),
    school_exit_date = as.Date(sprintf("%d-06-20", school_year)),
    # [DERIVED - VERIFY] ~14% of eligible ever-ELs
    seal_of_biliteracy = ifelse(ever_el == "Y" & runif(n()) < .14, "Y", "N"),
    seal_languages = ifelse(seal_of_biliteracy == "Y", home_language, NA_character_)
  ) %>%
  select(student_id, school_year, grad_4yr_status, grad_5yr_status,
         ela_grad_pathway, math_grad_pathway, school_exit_date,
         seal_of_biliteracy, seal_languages)

dsize <- spine %>% filter(el_status == "current") %>%
  count(district_id, school_year) %>% group_by(district_id) %>%
  summarise(mean_el = mean(n), .groups = "drop") %>%
  mutate(district_size_band = as.character(cut(mean_el,
           breaks = quantile(mean_el, c(0,.34,.67,1)),
           labels = c("small","medium","large"), include.lowest = TRUE)))

el_roster_sim <- spine %>%
  left_join(grad, by = c("student_id", "school_year")) %>%
  left_join(dsize %>% select(district_id, district_size_band), by = "district_id") %>%
  mutate(cohort_start_year = entry_year,
         cohort_label = ifelse(!is.na(entry_grade) & !left_truncated,
                               paste0(entry_grade, "-", entry_year), NA_character_)) %>%
  mutate(n_races = (race_american_indian == "Y") + (race_asian == "Y") +
                   (race_black == "Y") + (race_pacific == "Y") +
                   (race_white == "Y"),
         RACE_ETHNICITY = dplyr::case_when(       # ReportR canonical categorical
           hispanic == "Y" ~ "Hispanic or Latino",
           n_races > 1     ~ "Two or More Races",
           race_american_indian == "Y" ~ "American Indian or Alaska Native",
           race_asian   == "Y" ~ "Asian",
           race_black   == "Y" ~ "Black or African American",
           race_pacific == "Y" ~ "Native Hawaiian or Other Pacific Islander",
           race_white   == "Y" ~ "White",
           TRUE ~ "Not Reported")) %>%
  transmute(
    STUDENT_ID = student_id, YEAR = school_year, GRADE = grade,
    DISTRICT_ID = district_id, SCHOOL_ID = school_id,   # SCHOOL_CODE dropped: a
                                        # ReportR alias of SCHOOL_ID (NOTES map)
    GENDER = sex, RACE_ETHNICITY, HISPANIC = hispanic,
    RACE_AMERICAN_INDIAN = race_american_indian, RACE_ASIAN = race_asian,
    RACE_BLACK = race_black, RACE_PACIFIC = race_pacific,
    RACE_WHITE = race_white,
    COUNTRY_OF_BIRTH = country_of_birth, IMMIGRANT_STATUS = immigrant_status,
    FIRST_ENTRY_US_SCHOOL_DATE = first_entry_us_school_date,
    FRL = econ_disadv, FRL_DETAIL = frpl_status,   # FRL_STATUS is an alias of
                                                   # FRL; detail gets a non-alias name
    IEP = swd_flag, SPED_CLASSIFICATION = sped_classification,
    GIFTED_FLAG = gifted_flag, MIGRANT_STATUS = migrant_status,
    HOME_LANGUAGE = home_language, SERVICE_MODEL = service_model,
    LEP_PROGRAM_START_DATE = lep_program_start_date,
    LEP_PROGRAM_COMPLETION_DATE = lep_program_completion_date,
    TITLE3_LEP_FLAG = title3_lep_flag,
    EL = ifelse(el_status == "current", "Y", "N"),  # ReportR canonical binary
    EVER_EL = ever_el, EL_STATUS = el_status,
    EL_STATUS_DETAIL = el_status_detail, EL_MONITOR_YEAR = el_monitor_year,
    EXIT_OBSERVED = exit_observed, YEAR_IN_STATUS = year_in_status,
    ENTRY_GRADE = entry_grade, COHORT_START_YEAR = cohort_start_year,
    COHORT_LABEL = cohort_label, LEFT_TRUNCATED = left_truncated,
    INITIAL_ELP = initial_elp, NEWCOMER_FLAG = newcomer_flag,
    SIFE_FLAG = sife_flag, ELP_FORM = elp_form,
    ELP_GROWTH_TARGET_MET = elp_growth_target_met,
    GRAD_4YR_STATUS = grad_4yr_status, GRAD_5YR_STATUS = grad_5yr_status,
    ELA_GRAD_PATHWAY = ela_grad_pathway, MATH_GRAD_PATHWAY = math_grad_pathway,
    SCHOOL_EXIT_DATE = school_exit_date,
    SEAL_OF_BILITERACY = seal_of_biliteracy, SEAL_LANGUAGES = seal_languages,
    DISTRICT_SIZE_BAND = district_size_band, SAMPLE_WEIGHT = sample_weight
  ) %>%
  arrange(STUDENT_ID, YEAR)

# ==============================================================================
# 10. VALIDATION -- guardrails as executable checks
# ==============================================================================
cat("\n============== stateELP three-table sim ==============\n")
cat(sprintf("instrument : %s   headline: %s\n", elp_spec$instrument, HEADLINE))
cat(sprintf("components : %s\n", paste(COMPONENTS, collapse = ", ")))
cat(sprintf("el_roster_sim         %10s rows x %2d cols  (roster / enrollment frame)\n",
            format(nrow(el_roster_sim), big.mark=","), ncol(el_roster_sim)))
cat(sprintf("el_long_sim  %10s rows x %2d cols\n",
            format(nrow(el_long_sim), big.mark=","), ncol(el_long_sim)))
cat(sprintf("academic_long_sim   %10s rows x %2d cols\n",
            format(nrow(academic_long_sim), big.mark=","), ncol(academic_long_sim)))
cat(sprintf("students %s | years %d-%d\n",
            format(n_distinct(el_roster_sim$STUDENT_ID), big.mark=","), Y0, Y1))

ck <- function(label, ok) cat(sprintf("  %-52s %s\n", label, ifelse(ok, "PASS", "FAIL")))

cat("\n-- Key uniqueness --\n")
ck("spine unique on student x year",
   !any(duplicated(el_roster_sim[c("STUDENT_ID","YEAR")])))
ck("components unique on student x year x component",
   !any(duplicated(el_long_sim[c("STUDENT_ID","YEAR","COMPONENT")])))
ck("academic unique on student x year x subject",
   !any(duplicated(academic_long_sim[c("STUDENT_ID","YEAR","SUBJECT")])))

cat("\n-- Headline resolution --\n")
hd <- el_long_sim %>% filter(COMPONENT == HEADLINE, ELP_FORM == "annual") %>%
  inner_join(spine %>% select(student_id, school_year, elp_level),
             by = c("STUDENT_ID" = "student_id", "YEAR" = "school_year"))
ck(sprintf("internal headline equals the '%s' component row", HEADLINE),
   isTRUE(all.equal(hd$ACHIEVEMENT_LEVEL, hd$elp_level)))
ck("every non-NA internal headline has a component row",
   sum(!is.na(spine$elp_level)) == nrow(hd))
ck("roster ships no headline columns (elp_level / elp_scale / elp_band)",
   !any(c("elp_level","elp_scale","elp_band","ELP_LEVEL","ELP_SCALE","ELP_BAND") %in% names(el_roster_sim)))

cat("\n-- Component vocabulary --\n")
print(el_long_sim %>% count(COMPONENT, .drop = FALSE) %>% as.data.frame())

cat("\n-- Composite integrity: spec `from` weights and NA propagation --\n")
wide <- el_long_sim %>% filter(!is.na(SCALE_SCORE)) %>%
  select(STUDENT_ID, YEAR, COMPONENT, ACHIEVEMENT_LEVEL) %>%
  tidyr::pivot_wider(names_from = COMPONENT, values_from = ACHIEVEMENT_LEVEL,
                     names_expand = TRUE)
bad <- 0L
for (cp in COMPOSITES) {
  w <- elp_spec$components[[cp]]$from
  parts  <- as.matrix(wide[, names(w), drop = FALSE])
  expect <- round(as.vector(parts %*% as.numeric(w)), 1)
  got    <- wide[[cp]]
  bad <- bad + sum(!is.na(got) & !is.na(expect) & abs(got - expect) > 1e-6)
  bad <- bad + sum(!is.na(got) & apply(is.na(parts), 1, any))
}
ck("composites reproduce spec weights; NA propagates", bad == 0)

cat("\n-- Form separation (alternate levels never enter the headline) --\n")
ck("no alternate-form student has an internal headline",
   !any(spine$elp_form == "alternate" & !is.na(spine$elp_level),
        na.rm = TRUE))
print(el_long_sim %>% count(form) %>% as.data.frame())

cat("\n-- Level-only components --\n")
cat(sprintf("  rows with level and no scale_score: %s\n",
            format(sum(is.na(el_long_sim$SCALE_SCORE)), big.mark=",")))

cat("\n-- Bands come from the spec --\n")
print(spine %>% filter(!is.na(elp_band)) %>% count(elp_band) %>% as.data.frame())
ck("no band outside spec labels",
   all(na.omit(unique(spine$elp_band)) %in% elp_spec$bands$labels))

cat("\n-- Population rule: components and headline only on CURRENT rows --\n")
ck("no component row for a former or never student-year",
   nrow(anti_join(el_long_sim,
                  el_roster_sim %>% filter(EL_STATUS == "current") %>%
                    select(STUDENT_ID, YEAR),
                  by = c("STUDENT_ID", "YEAR"))) == 0)
ck("internal headline is NA on every non-current student-year",
   all(is.na(spine$elp_level[spine$el_status != "current"])))
ck("base ELP_FORM equals roster ELP_FORM on every row", {
  f <- el_long_sim %>%
    left_join(el_roster_sim %>% select(STUDENT_ID, YEAR, R_FORM = ELP_FORM),
              by = c("STUDENT_ID", "YEAR"))
  !any(is.na(f$R_FORM)) && all(f$ELP_FORM == f$R_FORM)
})
ck("COMPONENT is character and within the vocabulary",
   is.character(el_long_sim$COMPONENT) &&
     all(el_long_sim$COMPONENT %in% COMPONENTS))

cat("\n-- Academic table --\n")
ck("every academic row has a roster row with the same grade", {
  a <- academic_long_sim %>%
    left_join(el_roster_sim %>% select(STUDENT_ID, YEAR, ROSTER_GRADE = GRADE),
              by = c("STUDENT_ID", "YEAR"))
  !any(is.na(a$ROSTER_GRADE)) && all(a$GRADE == a$ROSTER_GRADE)
})
ck("no academic rows in 2020", !any(academic_long_sim$YEAR == 2020L))
ck("ACHIEVEMENT_LEVEL is integer and a score at prof_cut is level 4",
   is.integer(academic_long_sim$ACHIEVEMENT_LEVEL) &&
     findInterval(P$prof_cut, P$academic_cuts) + 1L == 4L)

cat("\n-- COVID: academic rows by year (2020 absent) --\n")
print(academic_long_sim %>% count(YEAR) %>% as.data.frame())

cat("\n-- Monitoring years (OELA Exhibit 3.12 structure) --\n")
print(el_roster_sim %>% filter(EL_STATUS == "former") %>%
        count(EL_STATUS_DETAIL) %>% as.data.frame())

cat("\n-- Flows suppression, cohort K-2017 --\n")
coh <- el_roster_sim %>% filter(COHORT_LABEL == "K-2017") %>%
  group_by(STUDENT_ID) %>%
  summarise(exited = any(EL_STATUS == "former") | any(EXIT_OBSERVED == 1L),
            .groups = "drop")
chars <- el_roster_sim %>% filter(COHORT_LABEL == "K-2017") %>%
  group_by(STUDENT_ID) %>% slice_min(YEAR, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(STUDENT_ID, INITIAL_ELP, HOME_LANGUAGE, SERVICE_MODEL,
         DISTRICT_SIZE_BAND, GENDER, FRL, SPED_CLASSIFICATION) %>%
  left_join(coh, by = "STUDENT_ID")
cat(sprintf("  cohort n = %d\n", nrow(chars)))
fail <- 0L
for (v in c("INITIAL_ELP","HOME_LANGUAGE","SERVICE_MODEL","DISTRICT_SIZE_BAND",
            "GENDER","FRL","SPED_CLASSIFICATION")) {
  tab <- chars %>%
    mutate(cat = tidyr::replace_na(as.character(.data[[v]]), "(none)")) %>%
    count(cat, exited) %>%
    tidyr::pivot_wider(names_from = exited, values_from = n, values_fill = 0)
  n_ex <- if ("TRUE"  %in% names(tab)) tab[["TRUE"]]  else rep(0, nrow(tab))
  n_st <- if ("FALSE" %in% names(tab)) tab[["FALSE"]] else rep(0, nrow(tab))
  # Expected suppression, not sizing failure: sex "X" (~0.3%), and the initial
  # bands at or above the exit threshold, which are empty by construction.
  keep <- tab$cat != "(none)" &                       # residual missing bucket
          !(v == "GENDER" & tab$cat == "X") &         # ~0.3% category
          !(v == "INITIAL_ELP" &
            tab$cat %in% elp_spec$bands$labels[
              elp_spec$bands$breaks[-length(elp_spec$bands$breaks)] >= 4])
  b <- sum((n_ex < P$min_cell | n_st < P$min_cell) & keep)
  cat(sprintf("  %-20s categories=%d  unexpected suppressed=%d\n", v, nrow(tab), b))
  fail <- fail + b
}
ck("all disaggregations clear min_cell", fail == 0)

cat("\n-- Duration: K entrants, full-follow-up cohorts --\n")
kd <- el_roster_sim %>% filter(ENTRY_GRADE == "K", !LEFT_TRUNCATED,
                             COHORT_START_YEAR + P$max_years <= Y1) %>%
  group_by(STUDENT_ID, COHORT_START_YEAR) %>%
  summarise(exit_t = suppressWarnings(min(YEAR_IN_STATUS[EXIT_OBSERVED == 1L],
                                          na.rm = TRUE)), .groups = "drop") %>%
  mutate(exit_t = ifelse(is.finite(exit_t), exit_t, NA_integer_))
print(kd %>% group_by(COHORT_START_YEAR) %>%
        summarise(n = n(),
                  pct_by_5  = round(mean(!is.na(exit_t) & exit_t <= 5)*100, 1),
                  pct_by_6  = round(mean(!is.na(exit_t) & exit_t <= P$max_years)*100, 1),
                  pct_never = round(mean(is.na(exit_t))*100, 1), .groups = "drop") %>%
        as.data.frame())

# ---- The abstraction guardrail -----------------------------------------------
# No component name may appear as a string literal in code below the spec block.
# This is what stops "elp_literacy_level" creeping back in.
assert_no_hardcoded_components <- function(path) {
  src  <- readLines(path, warn = FALSE)
  start <- grep("^# 1\\. Districts and schools", src)
  if (!length(start)) return(character())
  body <- src[seq(start[1], length(src))]
  body <- body[!grepl("^\\s*#", body)]
  hits <- unlist(lapply(COMPONENTS, function(cp)
    grep(sprintf('"%s"', cp), body, value = TRUE, fixed = FALSE)))
  hits[!grepl("dom_offset|native_name", hits)]
}
script_path <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) f[1]
  else if (file.exists("data-raw/el_long_sim.R")) "data-raw/el_long_sim.R"
  else "el_long_sim.R"
})
h <- tryCatch(assert_no_hardcoded_components(script_path),
              error = function(e) character())
cat("\n-- Abstraction guardrail --\n")
ck("guardrail source file located (otherwise the next check is vacuous)",
   file.exists(script_path))
ck("no component named as a string literal below the spec", length(h) == 0)
if (length(h)) print(h)

cat("\nNOTE: no mean time-in-status is computed anywhere in this script.\n")
cat("======================================================\n")

# ==============================================================================
# 11. Save
# ==============================================================================
SIM_CONTRACT_VERSION <- "0.3.0"
attr(el_roster_sim,        "sim_contract_version") <- SIM_CONTRACT_VERSION
attr(el_long_sim, "sim_contract_version") <- SIM_CONTRACT_VERSION
attr(academic_long_sim,  "sim_contract_version") <- SIM_CONTRACT_VERSION

if (dir.exists("data")) {
  save(el_roster_sim,        file = "data/el_roster_sim.rda",        compress = "xz")
  save(el_long_sim, file = "data/el_long_sim.rda", compress = "xz")
  save(academic_long_sim,  file = "data/academic_long_sim.rda",  compress = "xz")
  cat("Wrote data/el_roster_sim.rda, el_long_sim.rda, academic_long_sim.rda\n")
}
