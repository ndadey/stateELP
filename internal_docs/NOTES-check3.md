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
