# Stage instruction — Check 4: Decision consistency + dependence

**Question.** Across candidate ELP cuts, where do ELP-based and
content-proficiency classifications best agree — and, given base rates well
below 50%, what does Youden's J say the raw consistency statistic can't?

**Sample.** Identical to Check 3's paired cross-section (all ELs per grade ×
year with both scores), by grade and year — state it first.

**Data needs.** The headline **level** (`ACHIEVEMENT_LEVEL`) via
`elp_headline()` — the level, not the scale score (decided 2026-10-09: the
exit rule is written on the level and only the level scale is declared in
the spec; see `NOTES-check4.md`); academic proficiency vs
`policy$academic_benchmark` + `academic` cuts; `policy$error_tolerance` via
Prep 3's `$meta` (or spec directly); `policy$exit` (current threshold, for
reference lines); Check 1's reliability annotations where available.

**Method.** Decision consistency (percent agreement) across candidate cuts
on the headline level scale — the levels the delivery actually carries, so
an integer-only delivery yields few cuts and says so; **Youden's J as the primary optimal-cut statistic**
— when the EL academic-proficiency rate sits well below 50% (typical), raw
consistency's peak is pulled toward the majority classification and can be
distorted or fail to appear in a realistic range; J weights both error
types equally regardless of base rates. Render the current exit threshold
and the J optimum on the same figure. **Error tolerance:** render Prep 3's
answer beside the J optimum — J assumes equal costs; a stated asymmetry is
a documented reason to land above/below the J-optimal point (a values
input, not a departure from evidence); `unknown` → caption says the
question is open. Output a **candidate region**, not a single number,
annotated with which grades/years carry the most and least reliable
evidence per Check 1. The **copula / Kendall's-tau dependence analysis**
(tail structure: low-end independence vs high-end coupling) has no package
support yet — the `.qmd` states plainly what is turnkey (consistency, J:
the Cook et al. methods) vs custom (copula); `$dependence` is `NULL`
pending it.

**Additional check (Types 2–6, gated; `NULL` in sim).** The combined
policy's own pass/fail outcome as the classifier input — how the policy as
a whole performs; *which* criterion drives it comes from Prep 2's crosswalk
and Check 3's contested zone, not from here.

**Output slots.** `$consistency_curve`, `$youden_j`, `$j_optimal`,
`$candidate_region`, `$dependence`, `$policy_as_whole`, `$meta`.

**Fixture (must exist from the first pass).** The J-vs-consistency skewed
fixture: a small paired sample with a proficiency base rate well below 50%
where the raw-consistency peak and the J-optimal cut demonstrably diverge;
the code must return the J optimum. This encodes the Process Overview's
checkpoint ("confirm you are actually computing J") as executable proof —
a demonstrated gap, not a theoretical one.

**Static text.** Process Overview Check 4 block including the base-rate
caution and the convergence rule (Checks 3 + 4 agreeing is the strongest
evidence; any single method alone gets more caution).

**Guardrails (tests).** J computed, not consistency relabeled (the
fixture); per-grade keying, never pooled; candidate cuts span the spec's
level scale (`elp$level_scale$range` for a numeric scale, its declared
`levels` for a categorical one); the current exit threshold is read from
the `policy$exit` term on the headline level, never typed; on sim data the J-optimal point is a generator
artifact — caption as machinery demo.

**Done-when (shell).** The consistency and J curves for one grade × subject
render with the J optimum marked as the real element; the skewed fixture
passes; other slots placeholders with `skip()`ped tests; `.qmd` renders
both formats with the Sample line.

**Later passes.** Candidate region across grades with Check 1 annotations →
policy-as-whole (needs a Type 2–6 spec) → `$dependence` (blocked on copula
support).
