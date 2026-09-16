# Stage instruction — Check 2: On-time vs. prolonged exiters

**Question.** Who exits on time and who doesn't — and does prolonged
classification associate with identifiable characteristics (disability,
language, district/program), such that a threshold change alone wouldn't
address it?

**Sample.** The same cohort as Check 1, headline-split into **on-time**
(exited within the ESSA timeline from `policy$timelines`) vs **prolonged**
(late exiters + never-exited within the window). Disappeared/censored
students are their own column, never silently folded into either group
`[PROPOSED — CONFIRM]`.

**Data needs.** Spine + the disaggregation set (`home_language`,
`district_id` → size band from spec, `sex`, `econ_disadv`,
`sped_classification`, `service_model`, `initial_elp`). Academic table for
the descriptive summary — `NULL` without it. Spec: `policy$timelines`,
`policy$district_size_bands`, `policy$suppression`, `elp$bands`. Shared
flags from `R/utils.R` (`exit_ever ⊇ exit_max ⊇ exit_ontime`, `no_exit`,
on-time/prolonged) — call, never re-derive.

**Method.** The slide-63/64 crosstab: columns All / Exit_Ever / Exit_Max /
Exit_OnTime / No_Exit / Disappeared; rows Total, % of all, then
column-percent blocks for primary language, district size band, sex / low
SES / special education, service type, initial ELP band (bands ≥ 4.0
near-empty by construction — expected). Window-relative captions
("never exited *in the years observed*"). The **descriptive academic
summary**: median ELA percentile or proficiency rate for on-time vs
prolonged at comparable trajectory points (comparable = same
`year_in_status` `[PROPOSED — CONFIRM]`). "Trajectories" view: headline
level × year-in-status faceted by exit category `[PROPOSED — CONFIRM]`.

**Additional check (Types 2–6, gated; `NULL` in sim).** Prep 2's
blocked-by-one-criterion groups as extra comparison columns — a group that
looks systematically different means that criterion screens on something
other than what the main score screens on.

**Output slots.** `$flags`, `$crosstab`, `$academic_summary`,
`$trajectories`, `$blocked_groups`, `$meta`.

**Static text.** Process Overview Check 2 block (the human face of the
stuck population; Decide: which subgroups get separate treatment in Checks
3–4, which equity considerations the recommendation must address); deck
slide-67 reminder; slide-81 caution.

**Guardrails (tests).** Never plot a cohort's mean ELP over time without
conditioning on exit group (slide 81: the slowest growers are the ones who
remain); columns sum to All (Disappeared included); nesting holds
(Ever ⊇ Max ⊇ OnTime); suppression `NA` not dropped; size bands from spec.

**Done-when (shell).** The crosstab's Total and %-of-all rows render across
all six columns as the real element; triple tests for the built compute;
other slots placeholders with `skip()`ped tests; `.qmd` renders both
formats with the Sample line.

**Later passes.** Row blocks one at a time → academic summary →
trajectories (with its guardrail test first).
