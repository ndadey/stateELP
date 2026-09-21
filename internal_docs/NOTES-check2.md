# NOTES -- Check 2 (on-time vs. prolonged exiters)

Complete: every slot the instruction file names is built. Governing file:
`internal_docs/steps/6_check2_flows.md`.

## Built

- `inst/templates/check2_flows.qmd` -- Sample callout, authored Looking at /
  Looking for / Interpreting text, the crosstab, the academic comparison, the
  trajectories with the slide-81 caution, the slide-67 reminder, the Type-gated
  Additional check, and the Decide box.
- `R/check2_compute.R`
  - `compute_exit_flags()` -> `$flags` (the nested indicators plus the
    three-way `EXIT_GROUP`)
  - `compute_cohort_profile()` -> `$profile` (characteristics at entry)
  - `compute_crosstab()` -> `$crosstab` (Total block + five characteristic
    blocks)
  - `compute_academic_summary()` -> `$academic_summary`
  - `compute_trajectories()` -> `$trajectories` (`$levels`, `$composition`)
  - `compute_blocked_groups()` -> `$blocked_groups` (Types 2-6)
- `R/check2_plot.R` -- `plot_academic_summary()`, `plot_trajectories()`.
- `R/check2_render.R` -- `ft_check2_crosstab()`, `ft_check2_academic()`,
  `ft_check2_composition()`, `ft_check2_blocked_groups()`,
  `check2_academic_note()`, and the display-label resolver.
- `R/check2_run.R` -- `run_check2(data, spec, start_grade, subject)`.

No `skip()`ped tests remain in `test-check2.R`.

## Not built, and named as such in the document

The deck's slide-66 further views, all in CLAUDE.md 13's later-additions list:
later start grades; starting grade as its own crosstab rows; a logistic
regression to control for correlated characteristics (the crosstab can only
show one at a time).

## How Check 2 relates to what already exists

- Cohort: `run_check2()` takes the selected row of Check 1's exported
  `compute_cohorts()`. It does not select again, so the two documents cannot
  disagree about the sample.
- Exit dating, nested indicators, on-time / prolonged split: `R/utils.R`'s
  `build_student_spans()` and `add_exit_flags()`, called unchanged.
- Blocked groups: Prep 2's exported `compute_criteria_crosswalk()`, called
  rather than repeated.
- Initial ELP band: Check 1's `.check1_initial_band()` -- see below.

## `[PROPOSED - CONFIRM]`

1. **A zero count is shown as zero, not suppressed** (`.check2_suppress_zero_ok()`).
   `apply_suppression()` blanks `N < min_cell`, which includes 0. In this stage
   an empty cell is usually a finding: the on-time group has no ELP rows once it
   has exited, and a reader must be able to tell "nobody left to measure" from
   "too few to report". Prep 2 makes the same exception. The shared function is
   not edited.
2. **Characteristics are read at entry** -- the cohort's start year, the one
   year every member is current by construction. The alternative (last observed
   year, or modal across years) would let a block describe a moving target.
3. **`BLOCK_TYPE` and `N_BASE`.** The deck's blank template hides that its
   "Other Demographics" rows are independent indicators while the other blocks
   partition the column. `BLOCK_TYPE` states which, and `N_BASE` carries each
   `PCT`'s denominator so no share has to be taken on trust. Distribution
   blocks keep a `(not reported)` row so the shares still sum to 1; indicator
   rows drop unreported students from the denominator rather than counting them
   as a "no".
4. **District size band** is the entry-year district banded by that district's
   own current-EL count in that year -- not the delivered `DISTRICT_SIZE_BAND`
   (a tertile split of something else; the rule Prep 2 also follows), and not
   the latest year's counts.
5. **Display labels.** `inst/lookups/Display_Labels.R` names `exit_category` as
   this stage's vocabulary, so `EXIT_GROUP` carries the coded values
   `on_time` / `prolonged` / `disappeared` and the words come from that lookup
   at render time. Coded roster values (`spa`, `esl_pullout`) stay coded in the
   slots and are labelled the same way, which is why `$crosstab` carries
   `FIELD`. **This is the first code in the package to read that file**; the
   reader lives in the `check2_` prefix, and a shared home is the natural next
   step if another stage wants it.
6. **No "Prolonged" column in the crosstab.** The six columns are the deck's.
   Prolonged = `Exit_Ever` - `Exit_OnTime` + `No_Exit`, and `$flags$PROLONGED`
   carries it per student. Adding the column is small (`PROLONGED` into
   `.CHECK2_FLAG_COLUMNS` plus a header label).
7. **"Mobility"** is in the Process Overview's Looking for but not in the deck's
   row list, and the roster has no school-mobility field
   (`MIGRANT_STATUS`, `IMMIGRANT_STATUS`, `NEWCOMER_FLAG` are nearby; none is
   the same thing). Not built; the Looking-for text still names it as a thing to
   look at.
8. **Comparable point = same `YEAR_IN_STATUS`** (open decision 12), computed
   as arithmetic on the cohort's start year, which every member shares.
9. **Academic headline is the proficiency rate**, not the median scale score:
   it means the same thing in every grade. The median is reported beside it with
   the year's modal grade, so a reader can see whether the row is a
   within-grade comparison. Percentile is not in the delivery.
10. **Trajectories are `NULL` for a categorical level scale.** The level is
    stored as an ordinal index (contract 4.2) so it would average without
    complaint, but the mean of two category names is not a level. The guard
    reads `spec$elp$level_scale$type`, not the column's storage type.
11. **A student blocked by different criteria in different years** takes the one
    that blocked them most often, ties to the first alphabetically.
12. **The Sample line carries `[DERIVED - VERIFY]`** on the on-time timeline
    whenever `policy$timelines$source` is `"provisional"`, as it is for SIM.
13. **The Additional check separates three states**: does not apply (Type 1),
    applies but this delivery yields no blocked groups, and applies with a
    table. `$meta$blocked_note` carries the first two.

## Two things a later session will otherwise rediscover

1. **`.check1_initial_band()` is a `check1_` internal, called from `check2_`.**
   Check 2 must band a student exactly as Check 1's band-split curve does, or
   the two documents disagree about a student's band; calling the same function
   is what guarantees that, and `test-check2.R` asserts the agreement so drift
   fails loudly. But §14 sanctions calling *exported* functions across
   prefixes, and this one is not exported. Either export it from the `check1_`
   prefix or move it to `R/utils.R`; a `check1_` session should decide which.
2. **`add_exit_flags()` reads a by-initial-band on-time rule from the roster's
   *delivered* `INITIAL_ELP`** (`R/utils.R`, `.on_time_years()`), whereas both
   checks derive the band from the annual score and treat a delivered
   `INITIAL_ELP` as validation input only. A state whose `on_time_rule` is keyed
   by initial band would error in Check 2 without the column, and would silently
   use the delivered band with it -- possibly a different band from the one the
   figures show. SIM's `by_initial_band` is `NULL`, so nothing exercises it.
   It is an `R/utils.R` matter, so this session did not touch it.

## Sim limits

- The K cohort has **no disappeared students**, so that column is zero
  throughout and its block cells are blank. The non-trivial cases (a small
  nonzero column that suppresses; a real Disappeared column) are exercised by
  `make_check2_roster()`, whose exit outcomes are stated rather than simulated.
- The shipped ELPA21 spec asks for level 4 on a three-level scale, so under it
  every student fails every criterion and nobody is blocked by exactly one.
  `make_check2_elpa21_spec()` lowers the terms to the top declared level, which
  is the smallest change that lets the criteria disagree. That is a fixture
  decision, not a claim about the consortium's rule; the shipped spec is also
  tested, as the "applies but finds nothing" case.
- Per CLAUDE.md 4, **no group difference on sim data is a finding**: the
  academic gap between the exit groups and the over-representation of any
  subgroup are parameters of the generator. The document says so in its closing
  note.

## Circularity review, 2026-09-21 (from the Check 1 session --- not a fix)

Check 1's fitted models were found to be regressing exit on the exit rule's
own input (`NOTES-check1.md`). Check 2's flag and crosstab logic was read
against the same question: **is any quantity computed from the thing it is
meant to explain?** Recorded here rather than changed, because this is
another prefix.

**The core structure is clean, and deliberately so.**
`compute_cohort_profile()` takes every disaggregation variable from the
**entry year's** roster row -- home language, sex, FRL, IEP, service model --
and derives the district size band from the entry year's own district counts.
None of them can have been affected by an exit that happens later. That is
the correct design and it is what Check 1 failed to do.

**One live risk: `policy$timelines$on_time_rule$by_initial_band`.**

`add_exit_flags()` sets `ON_TIME_YEARS` per student from that rule, and
`EXIT_ONTIME` compares each student's exit year against their own allowance.
The crosstab then reports an **Initial ELP** row block. For a state that keys
its on-time timeline by initial band, those two meet: a band allowed seven
years is mechanically likelier to be "on time" than one allowed three, so the
on-time rates down that block are partly the state's rule restated, not a
finding about students. A reader comparing bands would read the policy back
to itself.

Verified live rather than assumed: `build_student_spans()` does carry
`INITIAL_ELP`, so setting `by_initial_band` runs without error and produces
`ON_TIME_YEARS` spread over 4-7 on the sim cohort.

**Dormant today.** `00_SIM_EL_Spec.R` has `by_initial_band = NULL`, so every
student gets the same five-year allowance and the block means what it appears
to mean. The distortion appears the first time a real state's rule is keyed
by band.

Severity is lower than Check 1's, which was active in the shipped default and
silently removed Model 2's ability to answer its own question. This one is
off by default and affects the interpretation of one row block. But it is the
same class of defect, it is invisible in the output, and it will not announce
itself.

*Suggested, for the Check 2 session to accept or reject:* when
`by_initial_band` is non-`NULL`, caption the Initial ELP block to say the
on-time column there is partly definitional -- and consider reporting
`EXIT_MAX` (a fixed window, the same for every band) beside it, which stays
comparable across bands whatever the on-time rule does.

`by_start_grade` needs nothing: Check 2 analyzes one entry grade, so the
allowance is constant within the table and cannot distort a comparison.

**For the unbuilt `$academic_summary`:** comparing achievement between
on-time and prolonged groups is not circular under a Type 1 rule, but under
**Type 3** the exit rule itself carries an academic criterion, so the groups
would be defined partly by the score being compared. Check 1 captions exactly
this for `$type3_covariate`; the academic summary will need the same gate.
