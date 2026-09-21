# NOTES — Check 1

Build notes for the Check 1 prefix: decisions that go beyond what
`internal_docs/steps/5_check1_survival.md` specifies, or that resolve
something it left open. Everything here is `[PROPOSED — CONFIRM]` unless it
cites the instruction file or CLAUDE.md.

D1 (one cohort, the most complete) and D2 (the Data fields used box) are not
repeated here: both are recorded in the instruction file and CLAUDE.md §6,
and in the commit that introduced them.

## Model 1

**The survival companion is computed from unsuppressed counts.** Suppression
blanks `D`, and `1 - prod(1 - D)` would read a blank as "no exits" and
flatten the curve silently. `.check1_life_counts()` returns A, B and C before
suppression, and both the life table and the estimate build on it. A test
sets `min_cell` above every row and shows the estimate still rises.

**Median years to exit is converted to exit-year terms.** A life-table row
*t* accumulates exits dated *t + 1* (CLAUDE.md §6: "exited at year *t*'s end"
means observed former in *t + 1*), so the row index where the cumulative
column first reaches half is one less than the answer a reader wants. The
conversion is in `compute_years_to_50()`, and a test ties the result to the
year the exit curve crosses half, so the two slots cannot drift.

**Two bases are reported, never one.** The observed share (a lower bound,
possibly never reaching half) and the at-risk estimate. Reporting a single
"median" would hide which one it is.

**A zero outcome is shown, not suppressed** (`compute_at_max_timeline()`).
`apply_suppression()` blanks anything below `min_cell`, including 0. A count
of none discloses no student, and blanking it reads as "hidden" when the
finding is "this did not happen" — which is exactly the case for
disappearances in the shipped sim.

**The timeline table mixes nested rows with a partition.** The first three
rows are nested (on time ⊆ within maximum ⊆ exited at all); the last three
partition the cohort. The `NESTED` column marks which is which, the caption
says so, and a test asserts both properties, because a reader who adds the
column up gets a wrong number.

## Model 2

**Specification** — not given in the instruction file beyond "person-period
discrete-time hazard model … adding lagged ELA alongside ELP":

- Outcome is the life table's **B** in student form: exited at the end of
  this year, observed as former in the next. The model and the table
  therefore count the same events.
- Baseline hazard is `factor(YEAR_IN_STATUS)`.
- Both covariates are **standardised**, so each ratio is per standard
  deviation. A ratio per raw scale point would be indistinguishable from 1
  on a 650–850 scale and would read as "no effect" whatever the truth.
- Strictly this is an odds ratio on the discrete-time hazard; the Process
  Overview calls it a hazard ratio. The roxygen says both.

**Subject is spec-driven**: `spec_academic_subjects(spec)[[1]]`, overridable
with the `subject` argument. Nothing types a subject name.

**Years in status with no exits are dropped.** A stratum with zero events
gives its own dummy an infinite coefficient and contributes nothing to the
covariates. Dropping it is standard and leaves the other estimates
unchanged; the count is reported in `$dropped`.

**The two reasons a lag is missing are reported separately**: the calendar
(a year with no assessment anywhere — 2021 in the sim, whose 2020 lag does
not exist) and the structural one (content testing starts in grade 3, so a K
cohort has no prior-year score for its first years). Lumping them would make
a design feature look like data loss. **Nothing is imputed**, and a test
asserts the dropped counts plus the fitted rows equal the frame.

**Quasi-separation is detected and captioned.** When fitted probabilities
reach 0 and 1, a covariate is deciding the outcome almost perfectly — which
is what the sim does, since exit there is a rule applied to the ELP score.
`$quasi_separated` carries the flag and `check1_model2_note()` says the ELP
ratio is not interpretable while the achievement ratio, the question the
model exists to answer, still is.

## The Type 3 additional check

Implemented rather than stubbed: when `policy$exit$types` declares Type 3 and
a pathway carries a term with `from = "academic"`, that subject's **same-year**
score is added to Model 2 as an extra covariate, and the result carries the
caption about circularity that the instruction file asks for. The slot stays
`NULL` when the spec is not Type 3, or when the delivery does not carry the
named subject. Exercised by a Type 3 spec built in `helper-check1.R` from the
SIM spec.

## The experimental index

**Selection strength is one number for the cohort**, computed as
`|ratio - 1|` on lagged achievement. The Process Overview asks for it at each
grade, via an interaction or a refit through each grade; neither is built.
The document says so in "What is not here", and the single number must not be
read as if it were per grade.

**The weight is `1 / (1 + |ratio - 1|)` above a hard floor of `min_cell`
students still at risk**, and 0 below it. The floor is the instruction file's
"retention share as a hard floor"; the functional form of the downweight is
mine, and arbitrary. Everything renders under an explicit experimental
warning, and nothing in the package consumes it.

## Verification, and what was not run

Built under the session's lighter loop (CLAUDE.md §3, directed 2026-09-21):
`document()` → `load_all()` → `test(filter = "check1")`, one full
`devtools::test()`, and a **Word-only** render. **`devtools::check()` was not
run for this pass, and the HTML render was not produced.** Both should run
before the shell is called done.

## Model 1's fitted half, added 2026-09-21

**Why it was missing.** The Process Overview defines Model 1 as a model
"using ELP level/trajectory as the only predictor" and Model 2 as "the same
discrete-time setup" with lagged ELA added -- so Model 1 is the baseline
Model 2 is nested in. CLAUDE.md 6 recast Model 1 as the life-table backbone,
the first pass built that, and the stage document went on reproducing the
Process Overview's sentence about two models being *estimated* with ELP as a
predictor. Nothing was estimated and ELP was not a predictor: it entered only
as a way of splitting the exit curve. The divergence was a spec one, not a
bug, but the rendered text overclaimed.

**What Model 1 is now.** Two halves, named as such in the document. The
descriptive half is unchanged -- life table, curves, product-limit estimate,
counts only. The fitted half is `compute_model1()`: the same person-period
frame as Model 2, `factor(YEAR_IN_STATUS)` plus the standardised headline
scale score, nothing else.

**One engine, two right-hand sides.** `.check1_fit_hazard()` holds the fit,
the coefficient tibble and the separation check; `.check1_drop_eventless()`
holds the rule both models apply to years with no exits. Model 2 was rewired
onto them rather than the code being copied, because the comparison between
the two only means anything if the dropped-row accounting and the
standardisation mean the same thing in both. Model 2's existing tests are the
guard that the refactor changed nothing.

**The frames deliberately differ.** Model 1 keeps every person-year with an
ELP score; Model 2 additionally needs a prior-year content score and keeps
about a fifth as many on the sim. Fitting Model 1 on Model 2's frame would
have made the two comparable at the cost of throwing away rows for a
covariate Model 1 does not use. `check1_nested_note()` therefore states the
row counts beside the ratios and says to read the movement as a direction,
not a measurement.

**Separation.** Exit in the sim is a rule applied to the ELP score, so
Model 1 separates hard -- the ratio comes out in the thousands. It is
rendered as it is, with `check1_model1_note()` saying it is not interpretable
and what remains usable. Hiding the number would be worse. `glm`'s own
warning is muffled in `.check1_fit_hazard()` *only* for the "fitted
probabilities numerically 0 or 1" message, because that fact is carried
structurally in `$quasi_separated`; every other warning still surfaces.

**Unfittable is a condition, not a crash.** A cohort with no observed exits
has no model in it. `.check1_unfittable()` signals class
`check1_unfittable`, `run_check1()` catches that one class and degrades the
slot to `NULL` with the message as `$meta$model1_note`. A blanket
`tryCatch()` would have hidden real errors.

**Level-only headlines.** ELPA21 reports a determination with no scale, so
there is no covariate. `.check1_has_headline_scale()` asks the spec first
(the component's `measures`) and the delivery second, and both models stay
`NULL`. This path is reachable in a way Model 2's never was: Model 2 was
always gated behind the academic table first.

## The confidence band

**Greenwood, through the complementary log-log scale.** `sigma^2_t = sum B_j
/ (A_j (A_j - B_j))`; the interval is `S^exp(+/- z sigma / |log S|)`, which is
`survfit`'s default. A symmetric band on the share runs outside [0, 1] near
the ends of the curve, which is exactly where a cohort that has nearly all
exited sits.

**Only the estimate is banded.** Column F counts exits against the cohort as
it started -- a denominator that includes students nobody is following any
more. It is not an estimator, and an interval around it would advertise
precision about the wrong quantity. A structural test asserts no CI column
ever attaches to it.

**Two degeneracies are left visible.** An interval that exhausts its risk set
(A = B) has no defined variance and NA propagates forward from it, because
nothing after it is known either. A stretch with no exits has sigma = 0 and
S = 1, so the band is the point -- widening it would invent uncertainty about
something the data settles.

**What the interval covers**, said in the caption: sampling variability in
this cohort's own exit rates. Not whether this cohort stands for any other,
which is the reading a band invites and the one Check 1's sample statement
already rules out.

## The covariate was wrong, and it broke Model 2's test (2026-09-21)

The first build of the fitted half used **that year's** ELP score. The exit
rule is applied to that year's result, so the model regressed exit on the
decision that produced it. Two consequences, both found by running the
alternatives rather than by reading the code:

1. **Structural separation.** The fit pinned probabilities at 0 and 1 and
   reported an odds ratio in the thousands -- and the value moved by a factor
   of six depending on which rows defined the standardising SD, which is what
   an unidentified coefficient looks like. This was never a simulation
   artifact: exit is determined by that score in any state. (It was only
   *quasi*- rather than complete separation because the rule is on the level
   and the covariate was the scale score, whose level cut varies by grade.)

2. **Model 2's test had no power** -- the serious one. Conditioning on the
   deterministic cause of the outcome leaves nothing for any other covariate
   to explain. Lagged achievement alone carried OR 1.40 [1.29, 1.53]; beside
   the same-year language score it collapsed to 1.048 [0.888, 1.239]. The
   Process Overview reads "ratio near 1" as *exit is driven by ELP as
   intended, so Checks 3-4 can be trusted* -- a reading that was guaranteed by
   the specification rather than found in the data. The skimming test could
   not have detected skimming.

The give-away was in the code: the lagged-ELA comment already said "never the
same year: the question is whether prior achievement predicts this year's
exit", while ELP went in contemporaneously. The asymmetry was the defect.

**Resolved (decided 2026-09-21): two fitted models, neither using a
contemporaneous score.**

- `$model1` -- **unconditional**, `EXITED ~ factor(YEAR_IN_STATUS)`. No
  covariate. Being saturated, its fitted hazards *are* the life table's D
  column; a test asserts the identity, which ties the fitted half to the
  descriptive half rather than leaving them two accounts of the same cohort.
  It needs no score, so it runs on deliveries where nothing else does.
- `$model1_baseline` -- adds the student's **first** annual score, fixed for
  their whole run. Non-circular, and it asks the question the exit curve's
  band split asks, in the other register.
- `$model2` -- baseline ELP + lagged achievement.

On the sim: OR 1.30 [1.23, 1.37] for entry score alone; in Model 2, entry
score 1.03 [0.95, 1.12] and lagged achievement 1.36 [1.25, 1.49]. Nothing
separates. **The 1.36 is still not a finding** -- how closely achievement
tracks language proficiency is a generator parameter (`sim_contract.md`
5.2-5.3) -- but the machinery can now register it either way, which is the
whole point. The document's caution had to be rewritten: it previously said a
ratio "near 1" was the generator reporting its design, which read as
incoherent beside a ratio of 1.36.

Rejected: lagging the ELP score by one year. It removes the circularity too,
and the fit is clean (OR 31.8), but a time-varying language score one year
back is still substantially the rule's input, and the entry score answers a
question the stage already asks elsewhere.

## Cluster-robust standard errors (2026-09-21)

A student contributes about four and a half rows. The person-period
likelihood does factorize exactly into those rows -- that is the result the
design rests on, and the point estimates are the student-level MLEs -- but
the factorization is exact only with no unobserved heterogeneity, which
nobody believes. `.check1_cluster_se()` computes a CR1 sandwich on
`STUDENT_ID` using the model's own bread and within-student score sums:
about twenty lines, and no new dependency, which `sandwich` would have been.
Estimates are untouched; the intervals count students. The rendered note
leads with the student count for the same reason.
