# =============================================================================
# 00_IL_EL_Spec.R  --  Illinois, ACCESS for ELLs (WIDA): the 2017-2026 tests.
# -----------------------------------------------------------------------------
# Written in the short form el_spec() takes (CLAUDE.md 5).
# FOUR FIELDS ARE SOURCED -- `exit`, `timelines`, `academic` and `min_cell` --
# from the Illinois State Board of Education's (ISBE) own documents, the
# Illinois Administrative Code and the U.S. Department of Education (USED).
# `sources` names them; the documents are listed at the end. Each was checked
# against its document online on 2026-11-17. `district_sizes` and
# `costlier_error` are the package defaults and read [DERIVED - VERIFY].
# Where this file converts ISBE's wording into the package's counting it
# says so, marked [PROPOSED - CONFIRM]. status = "provisional": nothing has
# been confirmed with ISBE and no delivery has been inspected.
#
# THE EXIT RULE is Type 1, a single overall composite:
#
#     Overall Composite proficiency level >= 4.8
#
# "From the 2017 ACCESS, ELs in Illinois need to receive an overall composite
# score of 4.8 on ACCESS to be English proficient. There are no longer
# requirements for domain specific score in ACCESS 2.0" (ISBE, 2017-2018
# ACCESS for ELLs page). The Illinois Bilingual Advisory Council recommended
# the score in June 2017 (ESSA plan, Amendment 5, p. 22, n. 24), and ISBE
# told families on 23 June 2017 that it is "retroactive to the 2016-17 school
# year". Kindergarten ACCESS is included: ISBE's reclassification chart gives
# 4.8 for "K - 12th grade". The rule stands in ISBE's 2026 documents: the
# August 2026 EL guidance, and the 2026 Report Card business rules (28
# October 2026), which count 2026 exits at 4.8.
#
# EXIT IS AUTOMATIC: no committee, no local criterion, no discretion.
# The State Superintendent posts the "proficient" score by 1 July; a student
# who scores proficient "shall exit the program of bilingual education
# services" and is identified as a former EL (23 Ill. Adm. Code
# 228.25(b)(2)). In SIS a 4.8 sets the EL indicator to EL-No automatically,
# with the end reason "Transitioned", which "Only the ISBE Assessment
# Department can apply" (August 2026 guidance). There is no committee and no
# local criterion. Below 4.8 a student stays EL ("not exited ... until
# attaining English language proficiency", ESSA plan, p. 22), and a district
# that sets EL-No without the score leaves the student "Current EL -
# Transition Incomplete", still an EL to ISBE. So Checks 1-2 can read the
# observed exits as the rule's own.
#
# WHICH TEST YEARS. A spec holds one rule; this is the rule for the
# 2017-2026 ACCESS administrations (January to early March). YEAR is the
# spring year. A student reaching 4.8 on the YEAR t test is first `former` in
# school year t/t+1, package YEAR t + 1 (the first-former-year convention).
#
#   ACCESS YEAR  Rule                                     Source
#   <= 2013      Overall >= 4.8 and Literacy >= 4.2       SY 2014 EL report
#   2014-2016    Overall >= 5.0, Reading >= 4.2 and       SY 2014 EL report;
#                Writing >= 4.2                           2015 ISBE deck
#   2017-2026    Overall >= 4.8   <- THIS SPEC            (above)
#   2027+        Under review (below)                     isbe.net/elexit
#
#   * "It was 4.8 Overall CPL and 4.2 Literacy CPL until 2013", raised
#     "beginning with Jan. 2014" (SY 2014 statistical report). The 2017
#     change followed WIDA's ACCESS 2.0 standard setting, which ISBE says
#     "requires English learners (ELs) to demonstrate higher language
#     abilities than they have in previous years".
#   * Pass ELP and academic rows for YEAR 2017-2026 and roster rows for
#     2017-2027 (2027 holds the first former year of 2026 exits). Exits on
#     the 2016 and earlier tests were made under other rules. Build EL_STATUS
#     and LEFT_TRUNCATED from the full SIS history first, then restrict.
#     [PROPOSED - CONFIRM]
#   * 2026 was the first rescaled WIDA ACCESS. WIDA: its proficiency levels
#     "will reflect the old proficiency level cut scores". ISBE: its scale
#     scores are "on a new scale (still 100-600)" and "CANNOT be used in the
#     ELP calculation"; the ELP indicator uses a WIDA file of "2026
#     performance on the 2025 scale". No document read says which 2026 level
#     the automated reclassification read [VERIFY]. Compare 2026's level
#     distribution with 2025's before pooling them, never pool scale scores
#     across 2025/2026, and run_check3(..., year = 2025) keeps Check 3 on the
#     old scale. [PROPOSED - CONFIRM]
#   * 2027 and later. ISBE is re-examining the ACCESS criteria after WIDA's
#     July 2026 standard setting, which "established a new scale and new
#     performance levels": working group 5-6 November 2026, public comment to
#     15 January 2027, criteria-setting meeting 25-26 February 2027, Board
#     action 14 April 2027, submission to USED 16 April 2027. The page names
#     no first test year. Do not apply this spec to 2027 or later results.
#
# ALTERNATE ACCESS HAS NO EXIT RULE: "Illinois does not currently have
# established reclassification criteria for students taking Alt-ACCESS"
# (isbe.net/elexit). USED's approval letter of 18 April 2025 required ISBE to
# propose them in a fall 2025 amendment; Amendment 5 (March 2026) still says
# ISBE "will set new reclassification criteria" once WIDA's research is in,
# and the 2026-27 review is to set them. An Alternate ACCESS taker cannot
# exit on that test, and its scores are on another scale (the ELP indicator
# leaves them out). Mark those rows with mark_alternate_elp() so they stay
# out of the headline and are reported as a group.
#
# TIMELINES, converted to the package's count [PROPOSED - CONFIRM].
#   Illinois sets "a targeted maximum timeline of five years for English
#   Learners to achieve ELP on the annual ELP assessment, commencing in first
#   grade" (ESSA plan, p. 22). The target is the ACCESS in grade "Baseline
#   Grade + 5 + Partial Years", the baseline being the grade of
#   identification, or grade 1 for a student identified in pre-K or K
#   (p. 23). The package dates exit to the first former year and counts it
#   as a year in status, year 1 being the first current year (R/utils.R):
#     identified in K     target test in grade 6 = year 7; first former
#                         year 8                       -> on time if <= 8
#     identified in 1-12  target test in year 6; first former year 7 -> <= 7
#   Checks 1 and 2 build kindergarten cohorts by default, so on_time = 8.
#   For run_check2(start_grade = "1") or later Illinois's number is 7, and
#   the short form holds one number (it dropped by_start_grade). "Partial
#   years" (134 or fewer days enrolled) extend a student's target; not
#   modelled.
#   max = 8 too: Illinois sets no maximum beyond this target. The three years
#   in 105 ILCS 5/14C-3 are a period of program participation, not of EL
#   status: a district may continue services past three years, or end
#   TBE/TPI with a 228.27 plan for continued services, and either way the
#   student stays EL and keeps taking ACCESS (23 Ill. Adm. Code
#   228.25(b)(3)). The cost: build_cohort() needs nine years of panel,
#   so a roster running to 2027 supports K cohorts entering 2017-2019 (2017
#   only if LEFT_TRUNCATED comes from pre-2017 history). Read literally,
#   "five years" would be max = on_time = 5,
#   two to three years stricter than Illinois's own target.
#   lt_el = 8: the Report Card's long-term EL is a current EL, not proficient
#   that year, "who [has] been in an EL program for more than 7 years",
#   excluding kindergarten, computed as a first EL record "before the school
#   year - 6" (2026 Report Card business rules, pp. 147-148): year 8 or later
#   here, where add_lt_el() flags current rows at or beyond lt_el. The 2025
#   Report Card glossary agrees ("within seven years of their initial EL
#   identification"). ISBE's EL statistical reports (SY 2021-22, SY 2024-25)
#   say "within six years of programming" instead, which would be 7. Two gaps
#   remain: ISBE counts from the first EL record statewide, the package from
#   the first current year in the panel; and ISBE drops a student who
#   reaches 4.8 that year, whose exit-year row is still current here.
#
# ACADEMIC TESTS. Grades 3-8: PARCC 2015-2018, the Illinois Assessment of
# Readiness (IAR) from 2019 (none in 2020), on one 650-850 scale. Grade 11:
# the SAT 2017-2024, the ACT with Writing from 2025. From the 2025 results
# ISBE reports four unified levels -- Below Proficient, Approaching
# Proficient, Proficient, Above Proficient -- and "Levels 3 and 4 are
# proficient" (Board vote 13 August 2025; 2026 Report Card business rules,
# pp. 18-19). Before 2025 the IAR had five levels, proficient from level 4
# (750 in every grade), and the SAT four, Meets Standards being level 3.
#   * proficient_cut is ISBE's 2025 Proficient cut by grade (tables of 13
#     August 2025). The scale did not move ("this change does not affect
#     students' raw scale scores", ISBE FAQ, 19 August 2025), so Checks 3 and
#     4 classify every IAR year against the 2025 standard. PARCC 2017-2018
#     shares the scale [PROPOSED - CONFIRM].
#   * Grade 11 has no cut, because the SAT (200-800) and the ACT (1-36)
#     cannot share one: Checks 3 and 4 fall back to the delivered level >= 3.
#     For a run on 2025-2026 alone, the ACT Proficient cuts are ELA 18 and
#     math 19.
#   * Check 2's academic summary always reads the delivered level against
#     proficient_level = 3. Before 2025 the IAR's level 3 is "Approached
#     Expectations", so in prep re-derive ACHIEVEMENT_LEVEL for IAR/PARCC
#     2017-2024 from the scale score with the 2025 cuts below
#     [PROPOSED - CONFIRM]. SAT levels 1-4 pass through as delivered (level
#     3 = Meets Standards, 540 in both subjects). Deliver numeric levels:
#     `level_labels` parses only the 2025 names.
#
#     2025 IAR cuts: Approaching / Proficient / Above Proficient
#       grade   ELA              Math
#         3     685 / 735 / 780  705 / 732 / 781
#         4     695 / 737 / 780  708 / 740 / 784
#         5     700 / 739 / 780  709 / 740 / 782
#         6     705 / 741 / 780  705 / 742 / 773
#         7     710 / 743 / 785  712 / 745 / 781
#         8     710 / 745 / 795  705 / 745 / 791
#
#   * ISBE says families and educators "should not compare 2025 proficiency
#     rates to prior years": its published rates moved with the standard.
#     Re-derived levels put every year on one standard; published rates do
#     not.
#
# SUPPRESSION: the Report Card's rule, "for any metric where the student
# count is less than 10, then no data will be displayed" (2026 Report Card
# business rules, p. 13). Accountability uses 20 for "different purposes"
# (2026 summative designation business rules, p. 7); not applied here.
#
# BANDS, a known tension left for the user [PROPOSED - CONFIRM]: 4.8 sits
# INSIDE the deck's "4.5-4.9", so Check 3's exit band and Check 2's crosstab
# rows are coarser than the rule. bands = c(1, 2, 3, 4, 4.5, 4.8, 5) would
# split it ("4.5-4.7", "4.8-4.9") but changes every Check 2 and Check 3
# figure, so it is not taken (the MA spec makes the same call).
#
# NO DELIVERY HAS BEEN INSPECTED, so there is no `delivery` block. What the
# August 2026 guidance says about building EL_STATUS from ISBE's Student
# Information System (SIS):
#   * ISBE calculates each student's EL status from the SIS EL indicator,
#     screener and ACCESS data: Current EL; Current EL - Transition
#     Incomplete (EL-No set without the protocol; the Report Card counts it
#     as EL, reporting status 4); Former EL (met 4.8 on ACCESS; kept "through
#     high school graduation"); Never EL; Former EL - Pre-K Only (EL in
#     pre-K, proficient on the kindergarten screener). Map the first two to
#     current and Former EL to former. Map Never EL and Former EL - Pre-K
#     Only to never: the latter was never a K-12 EL and never took ACCESS.
#     [PROPOSED - CONFIRM]
#   * The districts' Calculated EL Status report is new as of SY 2025; the
#     Report Card reads an "ISBE EL Status Table", and ISBE "began tracking
#     Former English learners in 2017" (isbe.net/elexit). Ask for that table
#     as far back as it goes. Without it, a district's own record is an EL
#     indicator and an EL record (a current flag only): derive former from a "Transitioned" end reason or an ACCESS of
#     4.8 or more, and build `never` from the longest history ISBE can supply.
#   * Timing. Preliminary ACCESS scores post in SIS in May (11 May 2026;
#     final scores 12 August 2026), and a 4.8 triggers the automated EL-No
#     update, so an end-of-year record can show a spring exiter as former in
#     the year of the score. Take status from the fall (Fall Housing) record,
#     or treat a student-year with ACCESS scores as current; the first former
#     year is the next one. [PROPOSED - CONFIRM]
#   * Not exits. "Withdrawn by Parents" (the first year of a refusal; then
#     the placement "Parents Refused"): the student stays EL and takes
#     ACCESS, so current with SERVICE_MODEL "refused_services". The
#     "228.27-Plan" placement (TBE/TPI services ended after three years):
#     current. "Erroneous": an identification reversal, not a former EL.
#   * Monitoring is two years and is not recorded in SIS.
# =============================================================================

# ISBE's unified performance levels, lowest first (from the 2025 results).
il_levels <- c("Below Proficient", "Approaching Proficient", "Proficient",
               "Above Proficient")

IL_EL_Spec <- stateELP::el_spec(
  id     = "IL",
  name   = "Illinois",
  status = "provisional",

  elp = list(
    test   = "ACCESS for ELLs (WIDA)",
    score  = "overall",
    label  = "Overall Composite",
    levels = c(1, 6),
    bands  = c(1, 2, 3, 4, 4.5, 5)        # 4.8 sits inside 4.5-4.9 (header)
  ),

  exit = c(overall = 4.8),                # K-12, automatic in SIS, no floor

  # Years in EL status, counted the package's way (header). [PROPOSED -
  # CONFIRM] 8 is Illinois's target for a K entrant; 7 from grade 1 on.
  timelines = list(
    max     = 8,
    on_time = 8,
    lt_el   = 8                           # Report Card: more than 7 years
  ),

  # IAR grades 3-8: the 2025 Proficient cut by grade. Grade 11 (SAT to 2024,
  # ACT from 2025) has no cut and reads the delivered level instead.
  academic = list(
    ELA = list(
      grades           = c(3:8, 11),
      proficient_cut   = c("3" = 735, "4" = 737, "5" = 739, "6" = 741,
                           "7" = 743, "8" = 745),
      proficient_level = 3,
      level_labels     = il_levels
    ),
    Math = list(
      grades           = c(3:8, 11),
      proficient_cut   = c("3" = 732, "4" = 740, "5" = 740, "6" = 742,
                           "7" = 745, "8" = 745),
      proficient_level = 3,
      level_labels     = il_levels
    )
  ),

  min_cell = 10,

  sources = c(
    exit = paste(
      "ISBE, 2017-2018 ACCESS for ELLs page: from the 2017 ACCESS, an",
      "overall composite of 4.8, no domain requirements; ISBE letter to",
      "families, 23 June 2017; 23 Ill. Adm. Code 228.25(b)(2); Illinois ESSA",
      "plan, Amendment 5 (3 March 2026), p. 22 and n. 24; ISBE Guidance for",
      "English Learner Students (August 2026): Transitioned, automated EL-No",
      "at 4.8."),
    timelines = paste(
      "Illinois ESSA plan, Amendment 5 (3 March 2026), pp. 22-23: targeted",
      "maximum of five years commencing in first grade, target grade =",
      "baseline grade + 5, baseline grade 1 for pre-K/K identification; ISBE",
      "2026 Report Card business rules (v16.0, 28 October 2026), pp. 147-148:",
      "long-term EL after more than 7 years. Converted to package years",
      "[PROPOSED - CONFIRM]."),
    academic = paste(
      "ISBE, ELA and Math Cut Scores and Score Ranges (13 August 2025); ISBE,",
      "Unified Performance Levels FAQ (19 August 2025); ISBE 2026 Report Card",
      "business rules, pp. 18-19: levels 3 and 4 are proficient."),
    min_cell = paste(
      "ISBE 2026 Report Card business rules (v16.0), p. 13: no data shown",
      "where the student count is less than 10; ISBE, Student Data",
      "Safeguarding: Report Card groups of fewer than 10 are replaced by",
      "asterisks.")
  )
)

# SOURCES (read online 2026-11-17)
#   ISBE, 2017-2018 ACCESS for ELLs (archived page)
#     https://www.isbe.net/Pages/EL2017-2018AccessforELL.aspx
#   ISBE, letter to families on ACCESS 2.0 (23 June 2017)
#     https://www.isbe.net/Documents/3_ACCESS_Letter_Parents_English.pdf
#   ISBE, Assessment listserv, New English Proficiency Level Criteria
#     (7 September 2017)
#     https://www.isbe.net/Lists/Assessment%20Listserv/AssessmentDisplay.aspx?ID=636
#   ISBE, screener and ACCESS 2.0 reclassification score chart (undated)
#     https://www.isbe.net/Documents/ScreenerScoresGuidanceChart.pdf
#   23 Ill. Adm. Code 228.25 (amended effective 13 July 2022)
#     https://ilga.gov/ftp/JCAR/AdminCode/023/023002280000250R.html
#   105 ILCS 5/14C-3
#     https://ilga.gov/legislation/ilcs/documents/010500050K14C-3.htm
#   Illinois ESSA consolidated State plan, Amendment 5 final draft
#     (3 March 2026)
#     https://www.isbe.net/Documents/ESSA-Amendment-5-Final-Draft.pdf
#   USED, approval of Illinois's amended State plan (18 April 2025)
#     https://www.isbe.net/Documents/IL-Amendment-App-Letter.pdf
#   ISBE, WIDA ACCESS and WIDA Alternate ACCESS Exit Criteria
#     https://www.isbe.net/elexit
#   ISBE, Guidance for English Learner Students: District Identification,
#     Screening, and Data Submission (cover dated August 2026 when read)
#     https://www.isbe.net/Documents/ISBE-Guidance-for-English-Learner-Students.pdf
#   ISBE, End of SY 2025-26 and Planning for SY 2026-27, SIS (29 April 2026)
#     https://www.isbe.net/Documents/End-of-SY26-Planning-SY27.pdf
#   ISBE, AFS Measure by Measure: English Learner Progress (undated)
#     https://www.isbe.net/Documents/AFS-Measure-By-Measure-ELP.pdf
#   ISBE, Public Business Rules: 2026 Report Card Metrics (v16.0,
#     28 October 2026)
#     https://www.isbe.net/Documents/Public-Business-Rules-2026-RC-Metrics.pdf
#   ISBE, 2026 Summative Designation Business Rules (v1.0, 1 October 2026)
#     https://www.isbe.net/Documents/2026-Official-Summative-Designation-Business-Rules.pdf
#   ISBE, 2025 Report Card Glossary of Terms
#     https://www.isbe.net/Documents/2025-Report-Card-Glossary-of-Terms.pdf
#   ISBE, Student Data Safeguarding
#     https://www.isbe.net/Pages/Student-Data-Safeguarding.aspx
#   ISBE, English Learner Statistical Reports, SY 2021-22 (July 2023) and
#     SY 2024-25 (transmitted 16 June 2026)
#     https://www.isbe.net/Documents/2022-Eng-Learner-Stat-Report.pdf
#     https://www.isbe.net/Documents/English-Learner-Statistical-Report-2024-25.pdf
#   ISBE, SY 2014 Bilingual Education Programs and English Learners
#     statistical report (August 2016)
#     https://www.isbe.net/Documents/el-program-stat-rpt14.pdf
#   ISBE, standards and assessments presentation (2015): Overall 5.0,
#     Reading 4.2 and Writing 4.2
#     https://www.isbe.net/Documents/3-stds-asmts-2015.pdf
#   ISBE, Alternate ACCESS Participation Guidance 2012-2013
#     https://www.isbe.net/Documents/access-part-guid-2013.pdf
#   ISBE, ELA and Math Cut Scores and Score Ranges (13 August 2025)
#     https://www.isbe.net/Documents/ELA-cut-scores-and-score-ranges.pdf
#     https://www.isbe.net/Documents/Math-cut-scores-and-score-ranges.pdf
#   ISBE, Unified Performance Levels FAQ (19 August 2025)
#     https://www.isbe.net/Documents/Unified-Perf-Levels-FAQ.pdf
#   ISBE, IAR Reporting Resources (five levels, 650-850 scale)
#     https://www.isbe.net/Documents_TAC/IAR-Reporting-Resources.pdf
#   ISBE, Statewide SAT Performance Levels FAQ for administrators
#     https://www.isbe.net/Documents/Statewide-SAT-Performance-Levels-Admin-FAQ.pdf
#   WIDA, WIDA ACCESS in 2025-26
#     https://wida.wisc.edu/revisingaccess
