# Stage instruction — Check 3: Academic patterns — box plots + logistic

**Question.** At what ELP level does language stop being the dominant
constraint on content-test performance — and is the implied bar one that
English-only peers themselves clear?

**Sample.** **Different from Checks 1–2 — state it first.** Cross-sectional:
all ELs per grade × year (not the K cohort), restricted to grades with
summative academic data (`spec$academic$<subject>$grades`; 3–11 in sim; EOC
panels by subject when `grades = NULL`). **Each grade analyzed separately,
never pooled** (structure-tested). Plus two EO comparison groups
(EO = `el_status == "never"`; former ELs are never EO): SES-filtered or
matched EO for the fairness box plots; full EO for the average-score
benchmark.

**Data needs.** Academic table ⟕ spine status (**this join direction;
row-count test guards it**). Spine: `elp_level`/`elp_band`/`elp_scale`
(scale is load-bearing here), `elp_form`, `econ_disadv`, `sample_weight`.
Spec: `academic` cuts + `policy$academic_benchmark`, `elp$bands`,
`elp$administration_window`, `policy$suppression`. Same-year pairing only;
2020 panels `NULL` (missing year, not empty).

**Method.**
- *Box plots* in the slide-73 grammar: bands + Former + EO on x, scale
  score on y, red proficiency-cut line labeled with its value, median line,
  mean marker; the slide-71 summary table (n, min, p25, median, mean, p75,
  max, IQR); alternate-academic takers in `n`, excluded from distributions;
  weighted EO n; spec-gated "screened but not classified" slot (`NULL` in
  sim).
- *Logistic regression* (core): P(academic proficiency) as a function of
  headline `elp_scale`; report the 50%-crossing scale score.
- *Landing statistics per grade, examined together across grades*: the band
  where the share at/above the cut first reaches 50% (equivalently the
  first band whose median clears the cut); the logistic 50%-crossing.
  Convergence across methods/grades corroborates; divergence is read
  against Prep 2 composition and Check 1 attrition/selection — never
  resolved by picking one grade arbitrarily.
- *Fairness checks*: (a) box plots re-run on the SES-filtered EO group
  (crude filter in shell — sim has `econ_disadv`, no parental education;
  matching is a later addition); (b) identical logistic with the
  binarization threshold swapped to the EO group's average ELA score, read
  at its 50% crossing. A persistent gap (fairness threshold below the
  standard one) is a construct-validity signal surfaced to the Decision
  stage — never resolved here.

**Additional check (Types 2/4/5/6, gated; `NULL` in sim).** Within Prep 2's
contested zone (checked slightly wider): box plots split by the second
criterion within band; one logistic with ELP + cleared-criterion indicator
+ interaction, presented as separate predicted curves (dual-curve support:
later addition).

**Output slots.** `$summary_table`, `$box_data`, `$logistic`, `$landing`,
`$fairness_boxes`, `$fairness_logistic`, `$contested_zone_models`,
`$coverage`, `$meta`.

**Checkpoint (rendered + tested).** Per-grade estimates never pooled; EO
group large enough at every grade after any SES exclusion.

**Static text.** Process Overview Check 3 block; deck slide-77 caution;
the pairing caption (`administration_window` vs academic window); coverage
table as the data-considerations block.

**Guardrails (tests).** Join direction; per-grade keying; bands,
reference-group labels, and cuts from spec, never literals; suppression;
weighted EO n; **on sim data every EL/EO gap is a generator parameter —
fairness outputs caption as machinery demo, never findings**.

**Done-when (shell).** One box-plot panel (one grade × subject) in the
slide-73 grammar renders as the real element; triple tests; other slots
placeholders with `skip()`ped tests; `.qmd` renders both formats with the
Sample line.

**Later passes.** Summary table → logistic + landing → fairness (a) then
(b) → contested-zone models (needs a Type 2/4/5/6 spec and dual-curve
support).
