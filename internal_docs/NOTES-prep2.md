# NOTES -- Prep 2, population analysis

Build notes, not governing. The contract is `steps/3_prep2_population.md`;
where this file records a departure, the departure is `[PROPOSED — CONFIRM]`
until the instruction file is refined in its own commit.

## What was built

A port of the standalone draft `stateData/MT/prep2_population_V2.qmd`, whose
~1,100 lines of inline analysis are now split the package way:

| File | Holds |
|---|---|
| `R/prep2_compute.R` | `compute_*()`: counts, EL share, grade, span, district, entry dating, time in status, initial ELP, profile, monitoring, criteria crosswalk. Plain data out. |
| `R/prep2_plot.R` | `plot_*()`: one ggplot per figure, from a slot, no recomputing. |
| `R/prep2_render.R` | `ft_prep2_*()` flextables, formatting, `.prep2_memo()`. |
| `R/prep2_run.R` | `run_prep2(data, spec)`: assembly, `$meta`, the Sample text. |
| `inst/templates/prep2_population.qmd` | Prose, captions, one call per figure/table. |

The `.qmd` is deliberately thin. To change a caption, a paragraph, the order of
sections or which figures appear, edit the `.qmd`; to change a number, edit
`R/`. Iterate with `source("dev/render_step.R"); rs("prep2", formats = "html")`.

## Mapping from the standalone

| Standalone | Package |
|---|---|
| inline `spec <- list(...)` | the active `el_spec` (`read_el_spec()`); nothing state-specific left in the document |
| file / `pkg_path` loading, merged-file split, duplicate checks, grade and column normalization | `as_el_data()` (already validates all of it) |
| `kable` tables | `flextable` (Word is primary) |
| lowercase columns | ALL-CAPS columns everywhere |
| `criteria` in the spec | `policy$exit$pathways[[i]]$terms` |
| `prep2_results.rds` written by the document | `render_step(save_results = TRUE)` |

## Decisions `[PROPOSED — CONFIRM]`

1. **Scope.** Built the whole draft, not only the instruction file's shell
   Done-when (`$counts_by_year` as the one real element).
2. **Slots beyond the instruction file:** `$entry_sources`, `$entry_by_year`,
   `$entry_cohorts`, `$time_in_status`, `$blocked_groups`. `$blocked_groups`
   is student-level because Check 2 needs the membership; the per-student
   entry table is *not* returned.
3. **Delivered entry fields are sources, not just validation.** CLAUDE.md §4
   says derived variables are "recomputed, never trusted from a delivery".
   Prep 2 dates entry from the first available of `LEP_PROGRAM_START_DATE`,
   `COHORT_START_YEAR`, `YEAR_IN_STATUS`, `ENTRY_GRADE` (capped at the first
   observed current year), because the panel cannot see entry before it opens.
   Exit dating and the cohort machinery in `R/utils.R` are untouched. Every
   disagreement with the panel is reported in `$meta$notes$entry`.
4. **Two new optional spec fields under `data`:** `entry_sources`,
   `entry_date_rule` (`"academic_end_year"` default, or `"calendar_year"`),
   plus `years_in_status_first_value`; and `policy$exit$zone_half_width`
   (default 0.5). The validator does not restrict these. **The SIM spec now
   sets `entry_date_rule = "calendar_year"`**: the sim stamps
   `LEP_PROGRAM_START_DATE` as 15 September of the *spring-year* label, so under
   the academic-calendar rule every sim entry would land a year late (the
   validation note reads 0.0%; with the field, 100.0%).
5. **Suppression: a cell is hidden when `0 < n < min_cell`.** Zeros stay zeros
   (a true zero discloses nothing, and "—" for a year with no former ELs reads
   as a suppressed cell). `apply_suppression()` also blanks zeros; it is used
   where every row has n >= 1 (`$entry_cohorts`).
6. **K cohorts come from `build_cohort()`**, so `$entry_cohorts` and Check 1's
   `$cohorts` cannot disagree (tested). The entry-*grade* view differs on one
   point by design: it counts a K student in the panel's first year as a K
   entrant, while `build_cohort()` excludes first-year entrants when the roster
   has no `LEFT_TRUNCATED`. Check 1 may want the same rule.
7. **Initial ELP follows `policy$timelines$initial_elp_source`**
   (`"first_annual"`: first annual headline at or after entry; otherwise the
   entry year). The standalone always used the entry year. The result is
   checked against the delivered `INITIAL_ELP` where present.
8. **Long-term share is a range** (at least / at most) while any entry is
   unknown; no mean or average time in status anywhere (tested by a source
   grep and a column-name sweep).
9. **Grade-span bars are 100% stacked** (deck slide 35, per the instruction
   file); the standalone stacked counts. `plot_by_grade_span(type = "count")`
   restores that.
10. **Section order** follows CLAUDE.md §6 (Additional check before the closing
    Decide box); the standalone put it last.
11. **District small-N line** is the first interior break of
    `policy$district_size_bands` (100 in the deck default), not a literal.
12. **Crosswalk is conjunctive, one pathway.** It uses the first pathway with
    two or more ELP terms and assumes all must pass. `from = "academic"`
    terms (Type 3) are not evaluated here.

## Known issues, not fixed here

- `00_SIM_EL_Spec.R` labels its middle district band `"101-999"` while
  `breaks = c(0, 100, 1000, Inf)` puts a district of exactly 100 in it. The
  deck's own labels leave the same gap. Prep 2 bins left-closed (like the ELP
  bands), so this is a label, not a count, problem. Foundation-owned.
- The Type-gated table chunks emit "FloatRefTarget with no content" warnings
  in the docx render when the crosswalk is `NULL` (the Type 1 sim). Cosmetic;
  Check 1's empty-slot chunks behave the same.
- `NAMESPACE` and `man/` are not regenerated (CLAUDE.md §3: one assembly
  pass runs `devtools::document()`). `load_all()`, `test()` and `render_step()`
  see everything without it.

## Still to do (later passes)

- A real Type 2--6 exemplar to exercise the crosswalk on something other than
  a spec edited in the test (the ELPA21 fixture populates it structurally
  only, and has no headline criterion).
- Real state delivery: the MT standalone ran on Montana files; the package
  ships no MT spec, and ingest of real files is a later addition (§13).
