# Stage instruction — Check 1: Cohort survival analysis

**Question.** How long do students who start EL in the same grade and year
take to exit — and once ELP is accounted for, is academic (ELA) performance
doing selection work that should belong to the language test alone?

**Sample.** A stable entry cohort — same starting grade (default K), same
entry year — tracked forward. Cohort set derived from the panel range and
`policy$timelines$max_years_to_exit`, **never year literals** (sim: K-2015…
K-2019). `left_truncated` excluded by construction. Checkpoint: cohort large
and stable (no pandemic-era enrollment anomaly) before trusting the curve.

**Data needs.** Spine for Model 1 (`student_id`, `school_year`, `grade`,
`el_status`, `entry_grade`, `cohort_label`, `left_truncated`,
`initial_elp`). Academic table for Model 2 (lagged ELA) — slot `NULL`
without it. Spec: `policy$timelines` (max timeline, on-time rule,
`exit_dating`), `elp$bands`, `policy$suppression`. Shared derivations from
`R/utils.R` (exit dating first-former, absorbing; censored; flags) — call,
never re-derive locally.

**Method.**
- *Model 1 — unconditional survival.* The slide-52 life table: rows = years
  in status; A enrolled at year's beginning, B exited, C disappeared,
  D = B/A (at-risk annual rate), E = C/A, F = ΣB/A_K (original-cohort
  cumulative — the headline, a lower bound); recurrence A_{t+1} = A_t − B_t
  − C_t; last year B empty, C = A. Plus: cumulative exit curve overall and
  by initial ELP band; discrete-time survival estimate 1 − Π(1 − D_t) as a
  labeled companion column; median years to exit; % within the ESSA
  timeline; % still classified at the window's edge.
- *Model 2 — ELA-augmented survival.* Person-period discrete-time hazard
  model (frame derived on demand, never stored) adding lagged ELA alongside
  ELP. Headline: the hazard ratio on lagged ELA (≈1 ⇒ exit driven by ELP as
  intended; away from 1 ⇒ skimming caught at the source). 2021 person-period
  rows drop (no 2020 lag exists) with a rendered note — never imputed.
- *Grade weights (experimental).* Retention share (Model 1 curve) as a hard
  floor; selection strength (|HR − 1|) as a continuous downweight.
  Render only with an experimental label; no mechanical use until validated
  (Indiana pipeline). `[PROPOSED — CONFIRM]`

**Additional check (Type 3, gated; `NULL` in sim).** The non-ELP criterion
as a Model 2 covariate; if it is an achievement measure also in the exit
rule, caption Checks 3–4 "consistency" with it as partly circular.

**Output slots.** `$cohorts`, `$life_table`, `$exit_curve`,
`$exit_curve_by_initial_band`, `$survival_est`, `$years_to_50`,
`$at_max_timeline`, `$model2`, `$retention_share`, `$selection_strength`,
`$grade_weights`, `$type3_covariate`, `$meta`.

**Fixtures (in test file, from the foundation session's utils where shared).** A–J
(slides 43–51): 50% at year 7 (grade 6), 40% within 6 years under
first-former dating; last-current dating demonstrably fails. Slide-52
worked numbers: D/E/F to two decimals against the slide's own A column;
recurrence at **±1 tolerance** (two slide A cells are off by one); test the
cells, not the callouts. Sim-vs-package exit-year `+1` assertion.

**Guardrails (tests).** No mean/average time-in-status; censored ≠
non-exiter; exit absorbing (F never decreases); cohorts from spec + panel,
no literals; on sim data the Model 2 HR is a generator parameter —
machinery demo caption, never a finding.

**Static text.** Process Overview Check 1 block including both models'
interpretations and the Decide box; deck slide-49 caution (later entrants
excluded; overall exit % differs from K-cohort figures).

**Done-when (shell).** The slide-52 life table renders for one K cohort as
the real element; A–J and slide-52 fixtures pass; all other slots
placeholders with `skip()`ped tests; `.qmd` renders both formats with the
Sample line.

**Later passes.** Curves → survival column → years-to-50 / at-max → Model 2
→ experimental weights (last, and only with the label).
