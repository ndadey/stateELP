# Stage documents

One `.qmd` per stage; these are the deliverable, and every function in `R/`
exists to make one of them render well.

| Stage | File | Results object |
|---|---|---|
| Prep 1 | `prep1_policies.qmd` | `prep1_results` |
| Prep 2 | `prep2_population.qmd` | `prep2_results` |
| Prep 3 | `prep3_program.qmd` | `prep3_results` |
| Check 1 | `check1_survival.qmd` | `check1_results` |
| Check 2 | `check2_flows.qmd` | `check2_results` |
| Check 3 | `check3_academics.qmd` | `check3_results` |
| Check 4 | `check4_consistency.qmd` | `check4_results` |
| Decision | `decision_synthesis.qmd` | `decision_results` |

File names carry **no run-order numbers** — those live on the prompt and
instruction files only. `render_step()` resolves a stage id by the `<id>_`
prefix, so `render_step("check1")` finds `check1_survival.qmd`.

Every stage document opens with its **Sample** statement on its own line, and
its results object carries the same text in `$meta$sample`. The sample shifts
across stages, and misstating it is the canonical way to misread a stage.

Params are the ReportR set: `pkg_path`, `spec_file`, `results_file`
(optional), `min_n`. Analysis logic lives in `R/`, never in the `.qmd`.
