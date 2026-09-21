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
