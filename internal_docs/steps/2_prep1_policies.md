# Stage instruction — Prep 1: Policy documentation

**Question.** Exactly how does a student exit EL status in this state, who
holds decision authority at each point, and which exit-criteria Type(s)
(1–6) describe the policy?

**Sample.** Not applicable — policy documents and program rules, no student
records. `run_prep1(spec)` takes spec only; a data argument is an error
(tested).

**Data needs.** The full spec: `elp` (instrument, components + `from`
weights, level scale, bands), `policy$exit` (pathways, `terms`, `types`,
`set_date`/`set_by`), `policy$timelines` (max timeline, on-time rule, LTEL
definition, `exit_dating`, `initial_elp_source`), `policy$growth` +
`how_set`, `policy$accountability_indicator`, `policy$suppression`,
`policy$academic_benchmark`. Every value with its `source` and
`verification` status.

**Method.** Render the spec as a review-and-confirm inventory — this step is
an inventory, not an analysis. The exit-dating convention and the Type
identification render as explicit confirm questions. Any
discretionary/committee pathway is flagged distinctly (the Indiana review
found selection effects concentrate there; Checks 1–2 track it separately).

**Output slots.** `$policy_table`, `$components_table`, `$exit_table`,
`$types`, `$confirm_questions`, `$meta`.

**Figures/tables.** Flextables only: the policy review table (value | source
| verification status), the component structure with weights, the exit
pathways table, the typology table with the state's declared Types checked.

**Static text.** Process Overview Prep 1 block (Sample / Looking at /
Looking for / Interpreting / Decide); the Additional-check paragraph on Type
identification.

**Guardrails (tests).** Spec-only (data argument errors); every rendered
policy value shows a `source`; `[DERIVED — VERIFY]` displays for
`source = "provisional"`; nothing hardcodes a component name; the three
shipped specs all render through this stage without error.

**Done-when (shell).** `run_prep1(sim_spec)` returns all slots; the `.qmd`
renders docx + html opening with the Sample line and carrying the full
review table including Types; tests above pass.

**Later passes.** Rendering polish across NJ/ELPA21 specs;
audience-differentiated depth (later addition).
