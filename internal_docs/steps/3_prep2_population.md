# Stage instruction — Prep 2: Population analysis

**Question.** What is the shape of the EL population — where it
concentrates, how it enters, who is in it — and which cohorts/subgroups are
large and stable enough to analyze? This stage is the interpretive backdrop
every later stage is read against.

**Sample.** Full statewide EL population (current, and former where the data
allows), observed across ≥5 years — not a single cohort. Never-EL students
enter only as the enrollment denominator.

**Data needs.** Spine: `student_id`, `school_year`, `grade`, `el_status`,
`sample_weight`, `district_id`, `entry_grade`, `initial_elp`/`elp_band`,
`elp_form`, and the disaggregation set (`home_language`, `service_model`,
`sped_classification`/`swd_flag`, `sex`, `frpl_status`/`econ_disadv`,
`immigrant_status`/`newcomer_flag`, `el_status_detail`). Spec:
`policy$district_size_bands`, `policy$suppression`, `elp$bands`,
`policy$timelines$lt_el_definition`.

**Method / figure list** (deck templates govern): EL counts by year + average
(line/bar); EL % of total enrollment — **weighted**, `NULL` + note when no
never-ELs; K–5 vs 6–12 100% stacked bars by year (slide 35); per-grade bars
over ≥5 years + average; ranked district bars, descending, reference lines
at the two-thirds cumulative-share rank and the `<100` count (slides 33–34);
binned district counts from `policy$district_size_bands`; entry-grade
first-appearance counts; initial-ELP distribution (banded; alternate-form
excluded with a note); profile shares (home language, service model,
disability, sex, SES, alt-assessment participation; newcomer / LTEL;
heritage → `NULL` in sim); monitoring-year counts `[PROPOSED — CONFIRM]`.

**Additional check (Types 2–6, gated; `NULL` for the Type 1 sim).** Criteria
crosswalk: pass/fail cross-tab per criterion near the overall cut;
blocked-by-one-criterion counts; the **contested zone** carried to Checks
2–3. Slots `$criteria_crosswalk`, `$contested_zone`.

**Output slots.** `$counts_by_year`, `$el_share`, `$by_grade_span`,
`$by_grade`, `$by_district`, `$district_bins`, `$entry_grade`,
`$initial_elp`, `$profile` (named list), `$monitoring`,
`$analytic_sample_memo`, `$criteria_crosswalk`, `$contested_zone`, `$meta`.

**Static text.** Process Overview Prep 2 block, including both "looking for"
framings (the population's own shape; the compositional facts that make
later stages downweight grades or split new vs long-term ELs) and the
Decide box (clean K cohort? where would pooling obscure?). Deck slide-38
caution ("resist the urge to average") renders.

**Guardrails (tests).** Statewide shares weighted, within-EL statistics not;
no unweighted never-EL total in any slot; district bins from spec, never the
sim's `district_size_band` column; suppression `NA` not dropped;
alternate-form exclusion noted where initial ELP renders; heritage `NULL`
does not error.

**Done-when (shell).** `$counts_by_year` computes and renders as the real
element; every other slot present as `NULL` or placeholder; `.qmd` renders
both formats with Sample line; triple tests for the built compute.

**Later passes.** One figure per session in the order above; the weighted
share early (it carries the §2.3 rule); crosswalk/contested zone only when
a Type 2–6 spec exists to exercise it.
