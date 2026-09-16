# Stage instruction — Decision & next steps

**Question.** Keep the current threshold, adjust it, or restructure the
criteria — and what defensible region, monitoring plan, and explicit
evidence trail support that call?

**Sample.** Not applicable — synthesis across all previous stages; no new
analysis is run here (guardrail).

**Data needs.** `run_decision(results, spec)`: a named list of prior stage
results (the natural RDS-mode consumer — each stage's saved RDS), plus the
spec (`policy$exit` for the current rule, `policy$error_tolerance`).
Missing stages are handled gracefully: the evidence map shows them as
not-run, nothing errors.

**Method.** The **evidence map**: one row per stage — what it found, its
sample, its reliability annotation (Check 1's weights carry their
experimental label wherever they appear), and its direction (supports keep
/ adjust / restructure). The **defensible region**: a range, not a single
"true" number — with which evidence most drove the landing point named
explicitly, including how the error-tolerance answer shaped where in the
range, and how any Check 3 fairness gap factored in. The **monitoring
plan**: committed tracking against the ESSA timeline regardless of whether
anything changes. Beyond-the-data considerations render as authored text:
other states' practice, field feedback (educators, families, ILP
committees), equity around any discretionary pathway given Checks 1–2. The
restructuring trade-off renders as authored text: more criteria always
reduce exit rates, against the risk of unnecessary retention.

**Additional check (Types 2–6, gated).** State the recommendation in terms
of every piece of the rule (composite range, floor, compensatory schedule,
…) and which pieces the evidence supported changing vs leaving alone.

**Output slots.** `$evidence_map`, `$defensible_region`,
`$monitoring_plan`, `$recommendation_scaffold`, `$meta`.

**Static text.** Process Overview Decision block, including the three
Decide items (change at all; specific numbers/bands including any
discretionary pathway's fate; monitoring plan).

**Guardrails (tests).** No new computation on student-level data in this
stage; missing-results handling (partial `results` list renders a partial
map, no error); experimental labels propagate; region-not-point (the
rendered recommendation scaffold carries a range field, not a single
value).

**Done-when (shell).** The evidence-map skeleton renders — one row per
stage with run status — from whatever results exist; `.qmd` renders both
formats with the Sample line; missing-results test passes.

**Later passes.** Full synthesis rendering once Checks 1–4 are built;
recommendation scaffold; Types 2–6 per-piece statement (needs a Type 2–6
spec).
