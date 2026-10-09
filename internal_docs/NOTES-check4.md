# NOTES — Check 4

Build notes for the Check 4 prefix: decisions that go beyond what
`internal_docs/steps/8_check4_consistency.md` specifies, or that resolve
something it left open. Everything here is `[PROPOSED — CONFIRM]` unless it
cites the instruction file or CLAUDE.md.

## Status — shell pass (2026-10-09)

Built, to the instruction file's Done-when:

- `R/check4_compute.R` — `compute_consistency_curve()` → `$consistency_curve`
  (the four cells and raw agreement at every candidate cut, one panel);
  `compute_youden_j()` → `$youden_j` (sensitivity, specificity and J added,
  from the cells, never from agreement); `compute_j_optimal()` →
  `$j_optimal` (one row per panel: the J optimum, the agreement peak beside
  it, both statistics at the current rule, and Prep 3's error tolerance with
  the side of the optimum it licenses).
- `R/check4_plot.R` — `plot_consistency_curve()`: the two curves, the J
  optimum and the current rule marked.
- `R/check4_render.R` — `ft_check4_optimum()`, `ft_check4_curve()`,
  `check4_optimum_note()`, `check4_tolerance_note()`.
- `R/check4_run.R` — `run_check4()`.
- `inst/templates/check4_consistency.qmd` — renders docx + html.
- The J-vs-consistency skewed fixture, `make_check4_skewed_bundle()` in
  `tests/testthat/helper-check4.R`, with the worked table beside it.

`$candidate_region` is `NULL` (later pass, its two tests still `skip()`ped);
`$dependence` is `NULL` (no copula support); `$policy_as_whole` is `NULL`
(Type-gated, and a Type 2–6 spec names it pending in `$meta$pending`).

## The axis is the level, not the scale score (decided 2026-10-09)

CLAUDE.md §4/§6 said Checks 3–4 run on the scale score. Check 4 runs on the
**level** instead; the instruction file and CLAUDE.md were updated in their
own commit before any code (`a98ad3d`). The reasons:

- **The exit rule is written on the level.** SIM: `overall` level ≥ 4.5.
  The instruction file asks for the current threshold and the J optimum on
  the same figure, and the spec declares a range only for
  `elp$level_scale`; `elp$cuts` is `NULL`, so there is no scale↔level map
  to place the threshold on a scale-score axis.
- **Scale scores are grade-dependent** (sim: `180 + 42·level + 7.5·grade +
  N(0, 9)`, `sim_contract.md` §4), so a scale-score J optimum would not
  compare across grades — and the later candidate region reads across
  grades.
- **In the sim the two axes do not even rank students the same way.** The
  generator derives the scale score from the level with noise, so in grade 3
  level 4.5 spans a ~57-point scale-score overlap (359–416). In real ACCESS
  the level is a per-grade lookup from the scale score, so within a grade
  the two axes rank students identically and agreement and J match at
  matching cuts: the choice is about units, resolution and comparability
  across grades, not substance.
- **It is the only axis ELPA21 has.** Its reported component is a
  level-only determination; on a scale-score axis Check 4 would be `NULL`
  there.

Consequence: Check 3's band landing is on the same axis as Check 4's cuts;
Check 3's logistic crossing stays in scale-score units. But see
"Convergence with Check 3" below: the band landing is not the *same
criterion* as the J optimum. `sim_contract.md` §1 still says "Checks 3–4 run
on the headline scale score" — flagged below, not edited here.

## What J weights — the decision this stage most needs confirmed

**`[PROPOSED — CONFIRM]`, and the one to raise with the Process Overview's
author.** The Process Overview (Prep 3 Decide; Check 4 Interpreting), the
instruction file and CLAUDE.md §6 all say J "assumes false-exit and
false-hold are equally costly". That is true of the two error **rates** —
J = 1 − (share of proficient students held) − (share of not-proficient
students exited) — and it is what the Process Overview's own Looking-for
sentence says ("weighting both error types equally regardless of how common
each is"). It is **not** true per student. Maximising J is minimising
`FP + R·FN` with `R = (1 − π)/π`: at the sim's grade-3 base rate (11%), J
weighs holding back one proficient student as heavily as exiting 7.7 who are
not. Prep 3 elicits the answer per student ("holding a student … longer than
necessary" vs "exiting a student prematurely"), so a direction read straight
off the paraphrase is wrong in two of three cases:

- **premature exit costlier** → land above the J optimum. Right whenever
  `R ≥ 1` (π ≤ ½).
- **equal per student** → the cost-optimal cut is the *agreement* maximum,
  which is above the J optimum when π < ½ — not "at" it.
- **prolonged retention costlier** → below the J optimum only if a hold is
  judged more than `R` times as costly; a milder asymmetry still points
  above. Without a magnitude the side is undetermined.

The first build followed the paraphrase (`equal` → "at", `prolonged_retention`
→ "below"); two independent reviewers reproduced the cost arithmetic on the
sim and it was corrected. What the build now does: `$j_optimal` carries
`IMPLIED_COST_RATIO`; `LEAN` compares the answer with it as above (and is
`NA` where the side depends on a magnitude Prep 3 does not record); every
reader-facing sentence says "equal as rates" and prints the per-student
ratio. The governing documents' wording is **not** edited — that is the
user's call. If Prep 3 is meant to elicit rate-level preferences instead,
the old mapping was right and `.check4_lean()` reverts in one line. A test
pins the identity (the J optimum minimises `FP + R·FN`; `R = 1` gives the
agreement maximum).

## Convergence with Check 3

**`[PROPOSED — CONFIRM]`.** The Process Overview's convergence rule (Checks
3 and 4 agreeing is the strongest evidence) stands. But Check 3's two
landings — the first band where half reach the cut, and the logistic's 50%
crossing — both mark where proficiency becomes *more likely than not*. That
is raw agreement's criterion, not J's (J targets where the per-level rate
crosses the base rate). With π < ½ they land at or above the J optimum by
construction, or not at all (the sim's grade 3: no band reaches half; the
crossing is an out-of-range scale score, 531). The document now says so, and
names the informative comparisons: Check 3's band landing against Check 4's
agreement curve, and both against J. The first build had called Check 3's
crossing "a level"; it is a scale score.

## How Check 4 relates to what already exists

- **Panel.** `run_check4()` takes Check 3's exported `compute_panel()`,
  called unchanged, the way Check 2 takes Check 1's `compute_cohorts()`.
  The two stages cannot disagree about the grade, subject and year.
- **Rows.** Check 3's row builder (`.check3_panel_rows()`) is internal to its
  prefix, so `.check4_panel_rows()` restates the join: academic table
  leading, roster status and the annual level attached by key, current ELs
  with both scores, alternate academic takers out. A test proves the two
  agree on the shipped sim, band by band against `compute_box_data()`'s
  `N_PLOTTED`, and on `N` / `N_ABOVE` against `compute_logistic()`.
- **Proficiency cut.** `.check4_cut()` restates `.check3_cut()` (minimum
  score of the proficient level), with one difference: it guards an `NA`
  `proficient_level` (the TEMPLATE spec's), where Check 3's would error
  inside `if`. A test asserts the cut equals `attr(compute_box_data(),
  "cut")`.
- **Exit threshold.** `.check4_exit_threshold()` is deliberately *not* a copy
  of `.check3_exit_band()`, which takes the first level term on any
  component. Check 4 takes only a term on the reported component's own level
  (`from` absent or `"elp"`), because a domain floor is not a cut on this
  axis; on ELPA21 that correctly finds none. A test asserts the SIM
  threshold falls in the band Check 3 reports as `EXIT_BAND`.
- **Error tolerance.** Prep 3 is unbuilt, so the answer is read from
  `spec$policy$error_tolerance$costlier_error` directly (the instruction
  file allows it). When Prep 3 lands and echoes it into `$meta`, nothing here
  needs to change: the spec is the source either way.

## `[PROPOSED — CONFIRM]`

1. **Suppression is two-step, at Check 2's cell standard.** (a) A cut with
   any non-zero cell under `min_cell` is blanked — every statistic in a row is
   a function of all four cells with the margins, so a small cell is
   recoverable from any of them. (b) The cells are cumulative, so two
   reported cuts difference to the students between them; walking up from
   the lowest cut, a cut is reported only if that step holds 0 or ≥
   `min_cell` proficient students and likewise not proficient — a thin level
   is merged into the next cut that clears it. Step (b) was added after
   review: four reviewers independently showed that row-only suppression let
   adjacent rows difference to groups as small as 3 (and, on six sim panels,
   to whole per-level groups under 10). Blanked cuts keep their row, cut and
   `N`; zeros are shown. **Cost:** about half the cuts are shown (20–26 of
   44–49 per sim panel), dense near the optimum, coarse at the bottom; and
   the J optimum is the best *reportable* cut, which on the sim moves it by
   up to 0.3 in a few panels (ELA grade 11: 3.3 → 3.6). Reporting the true
   optimum at a blanked cut would publish its sensitivity and specificity,
   from which the blanked cells — and so the thin level — come straight
   back. Group-size-only slices (the package's Check 1/3 convention) would
   keep more cuts; the cell standard was kept to match the row rule.
   (c) **Check 3's band table counts as published.** It gives, for the same
   students, each band's n and share at the cut, so the cumulative counts at
   every band break are known to anyone holding both documents; a second
   review found cuts at 4.9 differencing against the 5.0+ band to whole
   groups of 9–10 in 10 of 18 sim panels. A reported cut is therefore also
   checked against the next band break above it (`.check4_band_anchors()`),
   and slices are measured from the last published point — a reported cut
   *or* a break. This treats breaks as constraints only, never as
   exemptions (Check 3 suppresses some bands' shares, so a break is not
   always fully published). The one exemption is the lowest cut: it counts
   everyone, its cells are the margins, which are public in every row, so
   blanking it protects nothing — *given* (d). With no declared bands, every
   level is its own band and every cut is a break.
   (d) **A band Check 3 cannot show silences Check 4.** Check 3 shows every
   band's n but its share at the cut only where the band holds `min_cell` or
   more; Check 4's margins minus the shown bands' shares give the split of
   the rest. So if the bands Check 3 blanks hold, together, a small group of
   proficient or not-proficient students, every Check 4 row is blanked
   (`.check4_unshared_split()`). Found on the third review: never on the full
   sim, but on 6–16 of 72 thinned panels.
   **Cost, all four steps together:** 17–26 of 44–49 cuts are shown per sim
   panel (grade 3 ELA: 17 of 49, still with its J optimum at 4.2); on the sim
   the reported J optimum differs from the unsuppressed one in 2 of 18 panels
   (ELA 8: 3.5 vs 3.8; ELA 11: 3.6 vs 3.3). **Small panels mostly lose the
   curve**: at a 10% thinning of the sim (about 160 students a panel), 41 of
   54 panels report no optimum. A less conservative option, not taken:
   exempt band-break cuts when Check 3 shows every band's share in full,
   which would restore an optimum in about a quarter of those. Limitation,
   not handled: a level outside the declared scale is dropped by Check 4
   but still banded by Check 3, so a delivery error of that kind could
   difference against the top break.
2. **Ties go to the lowest cut**, within 1e-12, and `N_J_TIED` says how many
   tied. The same rule picks the highest reportable agreement. Only
   reportable cuts compete. A different rule (the midpoint of the tied run;
   report the run) belongs with the candidate region.
3. **Candidate cuts are the observed distinct levels inside the declared
   scale.** The lowest observed level is a cut (everyone ready: J = 0), shown
   for completeness. There is no "nobody ready" cut. A level outside the
   scale is dropped and counted in `attr(, "n_out_of_scale")` / `$meta`.
4. **An off-grid rule is read at the next observed cut at or above it.**
   Because students are ordered by level, `level >= 4.5` classifies exactly
   as `level >= (smallest observed level >= 4.5)`; for `op = ">"` the next
   cut strictly above. A rule above every observed level classifies nobody
   ready: J = 0, agreement = 1 − base rate, read off the margins. Only `>=`
   and `>` are read as a threshold; any other op is noted and not marked.
   **The rule's side of the optimum is judged by that cut, not the typed
   value** (`THRESHOLD_SIDE`): an integer-level delivery puts a 4.5 rule on
   the 5.0 row, and when that row is the J optimum the note says the rule
   "classifies exactly as the J optimum", not that it sits below it.
5. **Several terms on the reported level** (a Type 5 auto pathway beside a
   committee pathway): the first non-discretionary one is marked, and
   `$meta$threshold_note` names the count — by the pathway's name, else its
   rule text, else its position. The pathway's rule travels with its term
   (pathway names are optional).
6. **Which side a stated asymmetry licenses** — see "What J weights" above.
   `LEAN` is derived per student against `IMPLIED_COST_RATIO`, and is
   relative to the **reported** J optimum, the best reportable cut. When
   suppression hides the full-data optimum on the other side of it, the
   direction can differ (a random-fixture search found it in 18 of 400
   sparse panels, never on the sim). Equal per-student costs give `"at"`
   when the J optimum is also agreement's maximum, beats the
   everyone-not-ready floor and no cut is hidden; otherwise `"above"` at
   π < ½. Every "land above/below" sentence is phrased "rather than below /
   above it" — a direction, not a claim that the optimum moves.
6a. **No optimum, with a reason** (`NO_OPTIMUM`): every cut suppressed — or
   every cut that could separate the groups, leaving only the lowest
   (everyone ready, J = 0), which says nothing about the data
   (`"suppressed"`); no student or every student proficient (J undefined —
   the first build mislabelled this "suppressed"); or no cut with J above
   zero *and none hidden* (`"uninformative"`). On a 5% thinning of the sim,
   the first version called 40 of 72 panels "uninformative" when the true
   J reached 0.14–0.41 behind the suppression. With no optimum the
   tolerance note records the answer and places it against nothing.
6b. **J's optimum is the best reportable cut**, and the note, the table
   caption and the figure caption say "among the cuts that can be shown";
   `J_MAX_HIDDEN` flags a suppressed cut inside the merged range around it,
   and the note names that range (to the top or bottom of the scale when no
   cut is shown on one side). `J_MAX_HIDDEN = FALSE` does not promise that no
   suppressed cut farther away holds the full-data maximum. A rule whose cut
   falls inside that range is placed "within" it, with no side stated.
   `AGREE_MAX_HIDDEN` is true whenever any cut is suppressed (an interior
   blanked cut can hold agreement's true maximum too). `OPTIMA_DIVERGE` asks
   whether the J optimum is among agreement's *tied* maxima, not whether the
   lowest tied cut is the same one.
6c. **A categorical rule is restated as `>=` the first category it admits**
   ("> Progressing" is "≥ Proficient"); a rule no category satisfies is not
   marked. The marked term is named three ways: *the current rule* (a single
   criterion); *the rule's term* (`RULE_IS_TERM` — its pathway asks for the
   level and another criterion, so students above it can still be blocked:
   the axis note's blocking sentence); *the rule's route* (`RULE_IS_ROUTE` —
   other pathways, or an "any" pathway, let students below it exit another
   way). The figure legend and caption follow the same naming.
7. **The coarse-levels warning** fires when the scale is numeric, every
   observed level in the panel is whole, and the spec draws on a fraction (a
   band break or the threshold) — the same condition the SGP adapter warns
   on, restated because that helper is internal to `el_data.R`. It is a
   classed warning (`check4_coarse_levels`) at the console and a warning
   callout in the document.
8. **One panel in the shell**, the one `compute_panel()` derives (first
   declared subject, its first tested grade, the latest year). The loop over
   every grade lands with the candidate region; every slot is already keyed
   by subject × grade × year, and `compute_j_optimal()` already keeps panels
   apart when handed several.
9. **The sample is current ELs only.** The Process Overview's Check 4 sample
   is "all English learners … who have both an ELP score and a
   content-assessment score". Former and never-EL students have no ELP level
   to classify, so the reference columns Check 3 draws are not here, and the
   Sample line says so.
10. **The figure is two stacked panels with free vertical scales**, marks in
    a legend. On one shared axis J (≈ 0–0.2 on the sim) is flattened under
    agreement (≈ 0.1–0.9) and its peak is unreadable; text labels on the
    plot collided with the curves wherever the J optimum and the rule sit
    close.
11. **The right-hand end of the axis renders as a warning callout** — the
    hazard Check 3 renders for its top bands: a current EL at or above the
    rule is, in most states, in their final year. In the sim every current
    EL at ≥ 4.5 is in their exit year (`sim_contract.md` §4.5). When the
    marked term's pathway asks for a second criterion (`RULE_IS_TERM`), it
    adds Check 3's caveat — those cuts also hold students the other criterion
    is blocking — and the legend, caption and note call the threshold the
    rule's term (see 6c).
12. **The policy-as-whole note separates three states**, after Check 2's
    blocked-groups note: does not apply (Type 1); applies and is pending
    (Types 2–6, this pass — a `skip()`ped test holds the place); built
    (later).
13. **The Decide box's second item records Prep 3's answer** beside the
    region as an input to the Decision stage; it does not choose a landing
    point (the Process Overview gives that to the Decision stage). The first
    build's wording ("at the J optimum if equally costly") was both a scope
    reach and the error in "What J weights".
14. **An underivable panel degrades.** `run_check4()` catches
    `compute_panel()` failing on its own derivation (an end-of-course subject
    declared first with no rows in the delivery) and leaves the slots `NULL`
    with a note in plain words; the R error is kept in `$meta$panel_error`
    for the analyst, never printed. An explicit `subject`/`grade` override is
    the caller's request and errors exactly as `compute_panel()` does.

## Found in other prefixes, not fixed here (CLAUDE.md §14)

Each belongs to its own prefix's session:

- **`compute_panel()` fails with a bare "subscript out of bounds"** on a spec
  that declares no academic subject — the shipped ELPA21 spec, whose academic
  block is a pointer — and on an end-of-course subject (`grades = NULL`) the
  delivery has no rows for. `run_check4()` guards both; `run_check3()`
  guards only on the academic table and would hit them.
- **`compute_box_data()` puts every current EL in `GROUP = NA` when the spec
  declares no bands** (ELPA21): `.check3_group_levels()` uses
  `spec_bands(spec)$labels`, which is `NULL` there, instead of the
  one-band-per-level default `assign_elp_band()` applies.
- **`.check3_exit_band()` ignores the component**, so on ELPA21 it reads a
  domain floor as the exit threshold (and then maps value 4 to a level that
  does not exist on a three-level scale).
- **`.check3_share_at()` is defined twice** in `R/check3_compute.R`.
  Harmless; the second definition wins.
- **`R/check3_contested.R` uses "headline" in a reader-facing note**, against
  CLAUDE.md §9; the render test does not catch it because it greps only the
  `.qmd`.
- **`sim_contract.md` §1** still says "Checks 3–4 run on the headline scale
  score", which the 2026-10-09 decision changes for Check 4. Prose only, but
  the contract is versioned, so it is left for whoever next bumps it.

## Sim limits

- **The J optimum is the generator's.** Academic scores are
  `750 + 4·(grade − 6) − 34 (current EL) + 9·(level − 3) + … + N(0, 33)`
  (`sim_contract.md` §5.2), so the proficiency rate among current ELs at a
  level is roughly Φ((4g + 9L − 84.5)/33). It never reaches one half inside
  the level scale below grade 8 — so raw agreement climbs to the top of the
  scale in most panels by construction — and the base rate runs 0.11–0.43
  across the 18 panels. The reportable J optima run 3.5–4.2 (J 0.15–0.25).
  The slope (9 points per level against an SD of 33) is shallow, so the J
  curve is flat near its top: in grade 3 ELA, J is within 0.01 of its
  maximum at 3.4, 4.1, 4.2 and 4.3 before suppression. That flatness is the
  candidate region's job, not this pass's.
  The `.qmd` captions all of this as machinery, not a finding.
- **The alternate-academic exclusion is invisible in the sim**: every
  alternate academic row belongs to a student on the alternate ELP form, who
  has no annual level anyway. The exclusion is tested on a hand-built
  fixture.

## Verification (2026-10-09)

- **Three adversarial review rounds**, each finding re-checked by a skeptic
  before it counted. Round 1 (four lenses: statistics, guardrails, edge
  cases, the document as read): 25 of 28 findings confirmed — none a wrong
  number; the substantive ones were what J weights per student, row-only
  suppression undone by differencing, the Check 3 comparison, and
  degenerate panels mislabelled. Round 2 re-ran every one (all resolved or
  partly) and found the band-break disclosure across the two documents and
  "uninformative" claimed behind suppression. Round 3 confirmed those and
  found the Check-3-thin-band route through the margins plus wording edges;
  all fixed above. An independent re-implementation matched every cell on
  four sim panels; the skewed fixture's table was checked by hand.
- `test-check4.R`: 370 expectations, 3 `skip()`ped (the two
  `$candidate_region` tests; the Types 2–6 `$policy_as_whole`
  placeholder). Full suite and `devtools::check()` results are in CLAUDE.md
  §3 and the commit message.
- Renders docx + html against the sim + SIM spec.
