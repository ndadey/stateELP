# NOTES — Check 3

Build notes for the Check 3 prefix: decisions that go beyond what
`internal_docs/steps/7_check3_academics.md` specifies, or that resolve
something it left open. Everything here is `[PROPOSED — CONFIRM]` unless it
cites the instruction file or CLAUDE.md.

## The panel is derived, never chosen

The instruction file's Done-when asks for "one box-plot panel (one grade ×
subject)" and does not say which. Hard-coding a grade would be right for the
sim and wrong for the next state; picking "the grade with the most data"
would move the figure whenever a delivery changed.

`compute_panel()` derives all three coordinates: the **first subject** the
spec's `academic` block declares, the **first grade** that subject is
declared tested in, and the **most recent year** the delivery carries rows
for that pairing. On the sim that resolves to ELA, grade 3, 2025 — and the
year list correctly skips 2020, which has no academic rows at all.

Grades come from the spec, not from the data (`spec_academic_grades()`),
with the EOC fallback to observed grades when a subject declares
`grades = NULL`. Driving the loop from the data would render empty panels
for K–2 and 12, and would hide a grade the state tests but failed to
deliver. Asking for an undeclared grade is an error, not an empty figure.

## The band axis is not a proficiency gradient

The hazard this stage is most exposed to, found while reviewing Checks 1–2
for circularity and confirmed on the sim: among **current** ELs in bands
4.5–4.9 and 5.0+, **100%** are in their last current year, against 12% in
the lower bands. That is structural, not a sim artifact — the exit rule is
`level >= 4.5`, so being observed at that level while still classified means
you are leaving.

So the top of the x-axis holds students on their way out, not a
cross-section of highly proficient ELs. Under a Type 2–6 rule the same cells
would instead hold students a *second* criterion is blocking — a
systematically different population in the same box. It is visible in the
sim's own numbers: the 5.0+ median sits *below* the 4.5–4.9 median.

This is not circularity in Check 1's sense — the outcome is academic
performance and the axis is language proficiency, genuinely different
measurements — but it is a selection effect on the axis that nothing in the
figure reveals, and it lands exactly where the landing statistic is read.
`.check3_band_axis_note()` states it, the document renders it as a warning
callout, and a test asserts it is present. The later pass that builds
`$landing` has to decide what to do about it; this pass only makes sure
nobody reads the axis without being told.

## Students with no annual ELP score keep their column

148 academic rows in the sim's grade-3 panel belong to current ELs with no
annual headline that year — untested, or on the alternate form, whose levels
are a different scale. Dropping them would quietly shrink the panel.

They get their own column, `"No annual ELP score"`, which is the same
treatment and the same wording Check 1's band-split curve uses, so a reader
meets the group twice under one name. The join therefore preserves every
academic row that has a roster match, and a test asserts the row count.

What is still excluded: an academic row with no roster row for that year.
That is a delivery-integrity problem rather than a group, and the coverage
table is where it belongs.

## Box statistics, not student rows

`compute_box_data()` returns the five numbers a box is drawn from plus the
mean and the counts; `plot_box_panel()` draws them with
`geom_boxplot(stat = "identity")`. Two reasons beyond CLAUDE.md §8's rule
that no student-level data may be in scope where a figure is built: a table
of the same panel cannot disagree with the figure, and suppression becomes
meaningful — a suppressed group keeps its place on the axis with a visible
gap instead of silently closing up.

**Suppression keys on `N_PLOTTED`, not `N`.** A group of forty students with
three scores discloses those three. `N` stays visible either way: it is the
denominator, not a disclosive cell.

**A group nobody is in is not suppressed, it is empty**, and the two read
differently — "too few to show" invites a reader to imagine students behind
the gap. Zero-count groups are un-suppressed after the fact (the rule Check 1
applies to a zero outcome) and dropped from the axis by the plot.

## Beyond the Done-when

`$coverage` is filled rather than stubbed. It is a straight call to
`el_coverage()`, which the foundation session already built, so it adds no
analysis — and it is what the instruction file names as the stage's
data-considerations block. Everything else the instruction file lists under
Later passes is `NULL` with a comment and a `skip()`ped test: the summary
table, the logistic and its landing statistics, both fairness checks, and
the contested-zone models.

## Verification

`document()` → `load_all()` → `test(filter = "check3")` → full
`devtools::test()` → `render_step("check3")` in both formats →
`devtools::check()` on a clean export.

## The later passes (2026-09-21)

All four built in one pass, against the decisions in the instruction file
rather than waiting on the open questions recorded above: the proficiency cut
is taken from the spec as-is and renders with its `[DERIVED - VERIFY]` flag,
the SES comparison is the crude filter the instruction file specifies, and
every declared grade is covered.

**Slots are long and keyed, not one panel.** `$box_data`, `$summary_table`,
`$landing`, `$logistic` and the two fairness slots each carry every tested
grade x subject, keyed by `SUBJECT` and `GRADE`. One panel is a filter of
that, which is why `plot_box_panel()` takes a selector rather than the stage
holding a separate focal slot. `run_check3()` orders the focal panel first, so
a reader of any table meets the figure's grade at the top.

**The logistic is fitted on current ELs only.** They are the only students
with an ELP score to regress on. The outcome is "reached the academic cut";
the reported statistic is `-a/b`, the score where the fitted probability
passes half.

**Two guards on that number, both of which fire on the sim:**

- `CROSSING_IN_RANGE` -- whether the crossing sits inside the ELP scores
  actually observed. On the sim it does not, in **12 of 18 panels**: the
  curve is being read past its data, and the crossing is an extrapolation
  rather than a level anyone was seen at. Reported, never silently dropped --
  a crossing above every observed score is itself the finding that no
  observed level of English suffices.
- A panel where the outcome never varies (everyone, or nobody, clears the
  bar) has no curve. That returns an empty crossing rather than an error,
  because it is a fact about the grade.

**The band-axis hazard is now measured, not just captioned.** `$landing`
carries `EXIT_BAND` -- read from the exit rule's structured terms, never
typed -- and flags a landing at or above it. On the sim, **4 of 18 panels
land at all, and all 4 land at or above the exit band.** The generated note
says so in those words. That is the concern from the shell pass turning out
to be exactly where the statistic falls: the only bands that reach the cut
are the ones the exit rule constitutes.

**`NA` landing is an answer.** 14 of 18 panels have no band where half the
students reach the cut. `TOP_BAND_PCT` carries how far the highest band did
get, so the empty answer is readable rather than looking like a failure.

**The fairness checks.** (a) filters the English-only column to the
economically disadvantaged -- `.check3_is_disadvantaged()` accepts Y/YES/1/
TRUE so a crosswalk state does not silently filter to nobody -- and nothing
else in the panel moves, which a test asserts. (b) re-runs the identical
logistic with the bar swapped to the English-only group's **weighted** mean.
On the sim the English-only average sits below the state's cut in 7 of 18
panels, which is the construct-validity signal the check exists to raise.

**On sim data none of these are findings.** Every EL/EO gap is a generator
parameter (contract 5.2-5.3), and the document says so. What the pass
demonstrates is that the machinery registers the signal when it is there and
flags the conditions under which the number should not be read.

## Still not built

A matched (rather than filtered) English-only group, which needs parental
education this delivery does not carry; the contested-zone models, which need
a Type 2-6 spec; confidence intervals around the crossings; and a year
dimension -- each panel takes the most recent year, and whether the landing
moves year to year is a question this stage could ask and does not.
