# Stage instruction — Prep 3: Program implementation

**Question.** What actually changes for a student the moment they exit EL
status — and which error do stakeholders see as costlier: exiting a student
prematurely, or holding them longer than necessary?

**Sample.** Not applicable — program and policy structures, no student
records. (The students it matters most for are wherever Prep 2 showed
enrollment concentrates; render that cross-reference.) `run_prep3(spec)`
takes spec only; a data argument is an error (tested).

**Data needs.** `policy$program` (program models, access to grade-level
content and advanced coursework, educator credentialing, anything
conditioned on EL status or ELP level), `policy$error_tolerance`
(`premature_exit` / `prolonged_retention` / `equal` / `unknown`, plus who
was asked and when).

**Method.** Render the program inventory as a review-and-confirm table whose
product is a plain statement of what exit unlocks. Render the
error-tolerance elicitation with the Process Overview's framing verbatim in
substance: this is a values judgment the data cannot answer; Youden's J
(Check 4) weights both errors equally by default; a stated asymmetry is a
documented reason to land the final threshold off the J-optimal point.
`unknown` renders as an open confirm item, and downstream stages caption
accordingly.

**Output slots.** `$program_table`, `$error_tolerance`,
`$confirm_questions`, `$meta`.

**Figures/tables.** Two flextables: the program inventory; the
error-tolerance record (value, elicited from, date, notes).

**Static text.** Process Overview Prep 3 block, including the
forward-pointers into Check 4 and the Decision stage.

**Guardrails (tests).** Spec-only; `error_tolerance` value echoed into
`$meta` (Check 4 and Decision read it from there); the SIM spec's `unknown`
renders as an open item, not an error; every rendered value shows a
`source`.

**Done-when (shell).** Renders both formats from the SIM spec with the
Sample line and both tables (SIM: placeholder program fields,
`error_tolerance = "unknown"`).

**Later passes.** Audience-differentiated depth (later addition); nothing
else — this stage is small by design.
