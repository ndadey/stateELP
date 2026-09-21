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
