# stateELP

**Evaluate and set English Learner exit thresholds.**

> *It's 2026. Do you know how your exit criteria are doing?*

`stateELP` is an R package that helps a state education agency check whether
its English Learner (EL) exit criteria are working as intended. It covers how
long students stay in EL status, who exits on time and who gets stuck, where
academic proficiency falls relative to English language proficiency (ELP), and
how consistent the exit decision is with content-area proficiency. It then
supports a decision about whether to keep, adjust or restructure the criteria.

The package does not prescribe a cut score. It describes how the current rule
is functioning and gives a state the evidence it needs to defend a range of
reasonable thresholds.

`stateELP` follows the conventions of
[`ReportR`](https://github.com/ndadey/ReportR), a related package for
summative assessment reporting that is itself at a preliminary stage. The
shared conventions are ALL-CAPS long-format columns, a single spec object that
holds every state-specific value, and results computed in R and rendered
through Quarto. `stateELP` is standalone and does not import `ReportR`.

> [!IMPORTANT]
> **Status: in development (version 0.0.0.9000).** Four of the eight stages
> are built and render end to end. The rest are planned and have placeholder
> tests. Function names and result slots may still change.

---

## How it works

The package follows a nine-step process called *Evaluating and Setting English
Learner Exit Thresholds*:

- three **Prep steps** that establish context,
- four **Data Checks** that analyze the data, and
- one **Decision** stage that brings the evidence together.

**The deliverable is a set of Quarto documents, one per stage.** Every function
in `R/` exists to make one of those documents render well. Each document opens
with its **Sample** statement, because the population being analyzed changes
from stage to stage: the full statewide EL population, a single entry cohort,
or a cross-section of all ELs with comparison groups. Misreading the sample is
the most common way to misread a stage.

| Stage | Question it answers | Sample | Status |
|---|---|---|---|
| **Prep 1**: Policy documentation | What does our exit policy actually say, and which type of exit rule is it? | none (reads the spec) | planned |
| **Prep 2**: Population analysis | Who are our ELs: how many, in which grades and districts, entering when and at what level? | Full statewide EL population, ≥5 years | **built** |
| **Prep 3**: Program implementation | What changes for a student the moment they exit, and which error is costlier, exiting too early or keeping students too long? | none (reads the spec) | planned |
| **Check 1**: Cohort survival | How long do students spend in EL status? | One entry cohort (e.g., kindergarten entrants) followed forward | **built** |
| **Check 2**: On-time vs. prolonged exiters | Who exits on time, and who gets stuck? | The same cohort as Check 1 | **built** |
| **Check 3**: Academic patterns | At what ELP level does academic performance reach the proficiency cut? | Cross-section: all ELs by grade and year, plus former and never-EL comparison groups | **built** |
| **Check 4**: Decision consistency | How well do ELP-based and content-based classifications agree across candidate cuts? | Same as Check 3 | planned |
| **Decision**: Synthesis and next steps | Change anything? If so, what, and how will we monitor it? | none (synthesizes prior stages) | planned |

What the built stages produce:

- **Prep 2**: EL counts and share of enrollment by year, grade span and grade;
  ranked district distributions; district-size bins; entry grade and timing;
  initial ELP distribution; time in status; student profile shares.
- **Check 1**: a discrete-time life table, cumulative exit curves (overall and
  by initial ELP band), a survival estimate with Greenwood intervals, median
  years to exit, the share exited within the state's timeline, and two
  person-period hazard models. Model 1 uses ELP only. Model 2 adds lagged
  academic achievement as a test for selection ("skimming") bias.
- **Check 2**: nested exit indicators (`EXIT_EVER` ⊇ `EXIT_MAX` ⊇
  `EXIT_ONTIME`, plus `NO_EXIT` and `DISAPPEARED`), a crosstab of exit group by
  student characteristics, an academic summary for on-time vs. prolonged
  exiters, and ELP trajectories by exit group.
- **Check 3**: box plots of academic scale score by ELP band (with former and
  never-EL reference columns and the proficiency cut drawn in), the matching
  summary table, a logistic model of P(proficient) on the ELP scale score with
  its 50% crossing point, landing statistics across grades, and two fairness
  checks against English-only peers.

---

## Example reports

These are the stage documents rendered on the package's **simulated data**.
They show what each stage produces, not anything about a real state: every
gap and ratio in them comes from how the data were generated (see
[the warning below](#a-warning-about-the-simulated-data)). Rendered on
2026-10-09 from commit `5f4c86f`.

| Stage | Report | Word |
|---|---|---|
| Prep 1: Policy documentation | *not yet built* | |
| Prep 2: Population analysis | [View](https://ndadey.github.io/stateELP/prep2_population.html) | [.docx](docs/prep2_population.docx) |
| Prep 3: Program implementation | *not yet built* | |
| Check 1: Cohort survival | [View](https://ndadey.github.io/stateELP/check1_survival.html) | [.docx](docs/check1_survival.docx) |
| Check 2: On-time vs. prolonged exiters | [View](https://ndadey.github.io/stateELP/check2_flows.html) | [.docx](docs/check2_flows.docx) |
| Check 3: Academic patterns | [View](https://ndadey.github.io/stateELP/check3_academics.html) | [.docx](docs/check3_academics.docx) |
| Check 4: Decision consistency | *in progress* | |
| Decision: Synthesis | *not yet built* | |

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("ndadey/stateELP")
```

Rendering the stage documents also requires the
[Quarto CLI](https://quarto.org/docs/get-started/) and the `quarto` R package:

```r
install.packages("quarto")
```

The package is developed and tested on R 4.4.2. `DESCRIPTION` requires
R ≥ 4.3.0.

---

## Quick start

The package ships with simulated data and a matching spec, so every built stage
runs out of the box.

```r
library(stateELP)

# 1. The spec: one object that holds every state-specific value
spec <- read_el_spec("00_SIM_EL_Spec")
spec

# 2. The data bundle: validated, with the spec carried along
bundle <- as_el_data(el_long_sim,
                     roster   = el_roster_sim,
                     academic = academic_long_sim,
                     spec     = spec)

# 3. Run a stage. Results are a named list of plain data frames
res <- run_check1(bundle, spec)
res$meta$sample      # read this first: what population is this?
res$life_table
plot_exit_curve(res$exit_curve, spec)
```

To render a stage document (Word and HTML by default, written to `reports/`):

```r
render_step("check1")
render_step("check3", formats = "html")
stage_ids()          # "prep1" "prep2" "prep3" "check1" ... "decision"
```

### A warning about the simulated data

`el_long_sim`, `el_roster_sim` and `academic_long_sim` are a **synthetic
fixture**, built to exercise code and render documents. They are not a sample
of any real state. Every EL/English-only gap, every advantage for exited ELs
and every hazard ratio in the simulated data is a **parameter of the
generator**, so the numbers come out that way by construction. No figure
produced from them should be presented as a finding about any state. The
generator and its full codebook are in [`data-raw/el_long_sim.R`](data-raw/el_long_sim.R)
and [`sim_contract.md`](sim_contract.md).

---

## Data model

ELP instruments do not share a score structure. WIDA ACCESS reports four
domains, three composites and an overall score. ELPA21 reports domain scores
plus a categorical determination. So the thing that varies is a **row key, not
a set of columns**. The package works on three long tables, and **every column
name is ALL CAPS**, following the SGP `LONG_DATA` / ReportR
`student_results_long` conventions.

| Table | Grain | Shipped object | Role |
|---|---|---|---|
| **Base** | student × year × component | `el_long_sim` | Every ELP measurement, and the only source of it (`COMPONENT`, `SCALE_SCORE`, `ACHIEVEMENT_LEVEL`, `ELP_FORM`) |
| **Roster** | student × year | `el_roster_sim` | The enrollment frame: `GRADE`, `EL_STATUS` (`current` / `former` / `never`), demographics, program fields. **The only table students are counted from** |
| **Academic** | student × year × subject | `academic_long_sim` | Content-assessment scores |

The **headline** ELP score (the component a state's exit rule and analyses run
on, e.g. WIDA's Overall Composite) is never stored. Every stage reads it
through one accessor, `elp_headline(bundle)`, which resolves it from the base
table using `spec$elp$headline` and bands it using `spec$elp$bands`.

**Minimum viable delivery:**

- A roster with `STUDENT_ID`, `YEAR`, `GRADE` and `EL_STATUS`, plus base-table
  rows for the headline component, runs Check 1 (Model 1) and Check 2.
- Adding the headline scale score and an academic table (with `GRADE`) unlocks
  Check 1 Model 2, Check 3 and Check 4.
- Demographic and program fields unlock the disaggregated rows of each view.

Anything a stage needs but the data lacks becomes a `NULL` results slot with a
note explaining why. A missing input never causes a crash or an invented
denominator.

### Using your own data

`as_el_data()` is the adapter between a state's delivery and the analysis
model. It accepts:

1. **Separate objects**: base, roster and academic tables, as in the quick
   start.
2. **A single merged long file**, with roster attributes repeated on every
   component row. Pass it as `elp` with `roster = NULL` and it is split on
   intake.
3. **SGP `LONG_DATA`**, through `as_el_data_sgp()`, which maps `CONTENT_AREA`
   values to components through the spec (never by label), recodes `GRADE "0"`
   to `"K"`, and honors `VALID_CASE`.

`as_academic_long()` standardizes a ReportR- or SGP-shaped academic file.
Delivered column names are mapped to the package's names through the spec's
`data$columns` crosswalk.

The stage documents currently render against the shipped simulated data. To
analyze a state's own delivery, build a bundle and call `run_<stage>()`
directly. The built documents also accept a `results_file` parameter (a saved
`run_<stage>()` result), so they can be rendered from results computed
elsewhere. Ingest of raw state files is out of scope for now.

---

## Specs: `el_spec`

Every state-specific value lives in one spec object, and no analysis function
reads a state value from anywhere else. A spec has these blocks:

- `program`: who the spec describes
- `elp`: the instrument, its components, the headline, the level scale and the
  bands (things the **vendor** changes)
- `policy`: the exit rule, timelines, growth targets, the long-term EL
  definition, suppression, the academic benchmark, district-size bands, the
  program inventory and error tolerance (things the **agency or legislature**
  changes)
- `academic`: subjects, tested grades and proficiency cuts
- `data`: the column crosswalk and value maps (things that change with **file
  delivery**)
- `verification`, `source_documents`

**Every policy value carries a `source`.** A value marked
`source = "provisional"` renders as **`[DERIVED — VERIFY]`**, and the validator
rejects a policy value that has no source.

Three specs ship in `inst/specs/`:

| Spec | Instrument | Exit type | Purpose |
|---|---|---|---|
| `00_SIM_EL_Spec` | WIDA-shaped | Type 1 | Matches the simulated data |
| `00_TEMPLATE_EL_Spec` | WIDA ACCESS | Type 1 | **Starting point for a real state spec.** Uses a non-identity column crosswalk and vendor component names. Every policy value is a placeholder and describes no state. No data |
| `00_ELPA21_EL_Spec` | ELPA21 | Type 6 | Guardrail against WIDA assumptions: a categorical headline, a level-only component and a domain-profile exit rule. Illustrative values, no data |

```r
el_spec_files()                         # list shipped specs
spec <- read_el_spec("00_SIM_EL_Spec")  # by id ...
spec <- read_el_spec("path/to/MY_EL_Spec.R")  # ... or by path
validate_el_spec(spec)
```

To set up a new state, copy `inst/specs/00_TEMPLATE_EL_Spec.R`, replace every
placeholder with the state's value and its `source`, keep anything not taken
from an official document marked `provisional`, and set
`verification$status`. **No shipped spec describes a real state's exit
criteria.**

### Exit-criteria typology

`policy$exit$types` declares which kinds of exit rule a state uses. A state can
use more than one. The validator checks the declaration against the structure
of the rule's terms.

| Type | Exit rule |
|---|---|
| 1 | Single overall composite |
| 2 | Composite plus a domain floor |
| 3 | Composite plus another (non-ELP) source |
| 4 | Compensatory combination |
| 5 | Tiered or discretionary |
| 6 | Domain profile |

Several stages carry **Additional checks** that apply only to multi-criterion
rules, such as the criteria crosswalk, blocked-by-one-criterion groups and the
contested ELP zone. Their results slots are `NULL` unless the spec declares a
relevant type. A Type 1 state, like the simulated one, skips them all.

---

## Guardrails

These rules are enforced as tests, not just stated as conventions:

- **No mean time-in-status.** Censoring biases it. The package reports median
  years to exit and the share exited within the timeline instead.
- **A student who disappears from the data is censored, not a non-exiter.**
  Exit is dated to the first year a student appears as `former`.
- **Duration is cohort-based, not cross-sectional.** A kindergarten-entry
  cohort is a best case, and its results don't carry over to later entrants.
- **Grades are never pooled** in the academic analyses. Every grade × subject
  is its own panel.
- **No model covariate is the current year's ELP score.** The exit decision is
  made on that score, so using it would build the answer into the model.
  Covariates are fixed at entry or lagged.
- **Small cells are suppressed to `NA`, never dropped.** The threshold is the
  spec's `policy$suppression$min_cell`.
- **Never-EL rows are weighted** wherever they enter a statewide share.
- **No code names an ELP component.** Components come from the spec, so the
  package works for any instrument.
- **Reader-facing text names the component** (e.g. "Overall Composite"), never
  the package's internal word "headline".

---

## Repository layout

```
stateELP/
├── R/                    # flat; files grouped by stage prefix
│   ├── spec.R            #   el_spec() constructor, validator, accessors
│   ├── el_data.R         #   as_el_data(), adapters, elp_headline(), coverage
│   ├── utils.R           #   shared derivations: exit dating, flags, cohorts,
│   │                     #   suppression, banding, weighting
│   ├── render_step.R     #   render_step(): render one stage document
│   └── <stage>_compute.R / _plot.R / _render.R / _run.R
├── inst/
│   ├── templates/        # the stage .qmd documents (the deliverable)
│   ├── specs/            # shipped el_spec instantiations
│   └── lookups/          # display labels (render-side only)
├── data/                 # the three simulated tables (.rda)
├── data-raw/             # the simulation generator
├── docs/                 # example reports on the simulated data (GitHub Pages)
├── tests/testthat/       # one test file per stage + spec + el_data
├── internal_docs/
│   ├── steps/            # binding per-stage instruction files
│   └── NOTES-<stage>.md  # build notes and decisions awaiting confirmation
├── dev/                  # render loop helper, session prompts
├── sim_contract.md       # codebook and rationale for the simulated data
└── CLAUDE.md             # the full design contract
```

Within each stage, functions follow a strict split:

- `compute_*()` returns plain data and never plots or reads files.
- `plot_*()` takes a `compute_*()` result and returns a ggplot.
- `ft_<stage>_*()` builds a flextable.
- `run_<stage>()` assembles the stage's results list, including `$meta`
  (spec id, sample statement, data version, package version, run date).

---

## Development

```r
renv::restore()
renv::hydrate(c("devtools", "roxygen2"))   # dev toolchain, not in the lockfile

devtools::load_all()
devtools::test()                           # or test(filter = "check1")

source("dev/render_step.R")
rs("check1")                               # load_all() + render, docx + html
rs_all()                                   # every stage that has a document
rs_site()                                  # refresh the example reports in docs/
```

Working conventions:

- **One stage per session, one `compute_*`/`plot_*` pair at a time.** Each
  stage is governed by its instruction file in `internal_docs/steps/`, which
  lists the question, sample, data needs, method, output slots, figures,
  guardrails and definition of done.
- **`NAMESPACE` and `man/` are generated** by `devtools::document()`. Never
  edit them by hand.
- **Unbuilt functions have existing, `skip()`ped tests.** Guardrails are
  failing tests, not comments.
- **Regenerate the data, don't hand-edit it.** Run
  `Rscript --vanilla data-raw/el_long_sim.R` and bump the version in
  `sim_contract.md` whenever a column, grain or distribution changes.
- **Run `devtools::check()` on a clean export.** `R CMD build` copies the whole
  directory before applying `.Rbuildignore`, so a locked file anywhere in the
  tree can abort it. Export the tracked files instead:

  ```sh
  git archive --format=tar HEAD | tar -x -C <staging-dir>
  # remove .Rprofile, renv.lock and renv/ from the copy, then in R:
  # tools::checkDocFiles(dir = "<staging-dir>"); devtools::check("<staging-dir>")
  ```

[`CLAUDE.md`](CLAUDE.md) is the complete design contract: the stage-by-stage
specification, the data rules, open decisions and recorded later additions.
Decisions still awaiting confirmation are marked `[PROPOSED — CONFIRM]` there
and in the `NOTES` files.

### Source documents

The process structure comes from *Process Overview: Evaluating and Setting
English Learner Exit Thresholds*. The figure and table templates come from the
Spring 2026 EL Collaborative "Data Views" presentation (Molly Faulkner-Bond).
Working extractions of both live in `internal_docs/source/` and are **not
published in this repository**. If you need them, ask the maintainer.

---

## License

MIT © 2026 National Center for the Improvement of Educational Assessment.
See [`LICENSE`](LICENSE).

Maintainer: Nathan Dadey ([ndadey@nciea.org](mailto:ndadey@nciea.org))
