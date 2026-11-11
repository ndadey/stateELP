# English Learner Exit Criteria: Information Form

This form collects the information needed to evaluate how a state's English
learner (EL) exit criteria are working. The analysis uses the `stateELP` R
package. **You don't need to know R to fill it in.** Your answers will be typed
into a setup tool, `stateELP::build_el_spec()`, that builds the package's
settings file (the spec).

**How to fill it in**

- Each question has an ID (like **C2**) that matches the setup tool's prompts.
- Answer in your own words. Examples show the kind of answer expected.
- If you don't know an answer, write **"Not sure"** and who might know. Blank
  is fine for questions marked *optional*.
- For any rule, say **where it's written down**: a regulation, guidance
  document, or memo, with a link or title if you have one. A rule with no
  written source is marked "to verify" in every report.

**Who usually answers each part**

| Part | Topic | Usually answered by |
|---|---|---|
| A | About the program | Anyone |
| B | The English language proficiency (ELP) test | Assessment office |
| C | The exit rule | EL program office |
| D | Timelines | EL program office |
| E | Growth targets | EL program office or accountability |
| F | Reporting rules | Data or accountability office |
| G | State academic tests (e.g., reading, math) | Assessment office |
| H | Programs and priorities | EL program office |
| I | Data files | Data team |
| J | Sign-off | Whoever reviews the finished form |

---

## Part A. About the program

**A1. Which state is this for?** (two-letter postal code)
*Example:* XX

> Answer:

**A2. What is the name of the state education agency?**
*Example:* State Department of Education

> Answer:

**A3. What short label should reports use for this setup?**
A few letters, used as a tag on every result. Don't use "SIM", which the
package reserves for its simulated data.
*Example:* XX_ACCESS

> Answer:

**A4. Which English language proficiency (ELP) test does your state use?**

- [ ] WIDA ACCESS for ELLs
- [ ] ELPA21
- [ ] Another test (name it):

If you pick WIDA ACCESS or ELPA21, skip every question marked
**Skip for WIDA ACCESS and ELPA21**. Those answers are the same in every state
that uses the test, and are filled in for you.

> Answer:

**A5. Who publishes or scores the test?**
**Skip for WIDA ACCESS and ELPA21** (filled in: WIDA / DRC; ELPA21).
*Example:* the test vendor's name

> Answer:

**A6. What is the most recent year the test was given?** Use the spring year
(2024-25 school year = 2025).
*Example:* 2025

> Answer:

**A7. When during the year is the test given?**
*Example:* January to March

> Answer:

---

## Part B. The ELP test

Answer in the column for the test you picked in A4.

**B1. Which ELP scores are in your student data file?** Tick each one. (The
file may hold fewer scores than the test reports; some hold only the
Overall Composite.)

| WIDA ACCESS | ELPA21 |
|---|---|
| [ ] Listening | [ ] Listening |
| [ ] Speaking | [ ] Speaking |
| [ ] Reading | [ ] Reading |
| [ ] Writing | [ ] Writing |
| [ ] Oral (Listening + Speaking) | [ ] Overall scale score |
| [ ] Literacy (Reading + Writing) | [ ] Overall Performance Determination |
| [ ] Comprehension (Listening + Reading) | |
| [ ] Overall Composite | |

*Another test:* list each score in the file and what your state calls it.

> Answer:

**B2. Which score is the main one in your exit rule?** Pick one. Every score
a state's exit rule can be built on is listed. If the rule has more than one
requirement (for example, an Overall Composite minimum plus a Literacy
minimum), pick the main one here and list the others in C2.

| WIDA ACCESS | ELPA21 |
|---|---|
| [ ] Overall Composite *(most WIDA states)* | [ ] Overall Performance Determination *(most ELPA21 states)* |
| [ ] Literacy (Reading + Writing) | [ ] Overall scale score |
| [ ] Oral (Listening + Speaking) | [ ] Listening |
| [ ] Comprehension (Listening + Reading) | [ ] Speaking |
| [ ] Listening | [ ] Reading |
| [ ] Speaking | [ ] Writing |
| [ ] Reading | |
| [ ] Writing | |

*Another test:* name the score.

> Answer:

**B3. How are proficiency levels reported?**
**Skip for WIDA ACCESS and ELPA21.** Filled in:

- *WIDA ACCESS:* a number from 1.0 to 6.0, to one decimal. Whole levels:
  1 Entering, 2 Emerging, 3 Developing, 4 Expanding, 5 Bridging, 6 Reaching.
- *ELPA21:* the Overall Performance Determination is one of Emerging,
  Progressing or Proficient; each domain is a level from 1 to 5.

*Another test:* say whether levels are a number on a scale (give the lowest
and highest values and the name of each whole level) or named categories
(list them from lowest to highest).

> Answer:

**B4. Does your state also give an alternate ELP test** (for students with the
most significant cognitive disabilities)?
If yes:

- **B4a.** Its name. **Skip for WIDA ACCESS** (filled in: WIDA Alternate
  ACCESS). *ELPA21 or another test:* name it (for example, Alt ELPA).
- **B4b.** Who takes it. *Example:* Students with an IEP who meet the
  alternate assessment participation criteria
- **B4c.** How students taking it exit EL status. *Example:* Not yet set
  by the state

> Answer:

---

## Part C. The exit rule

**C1. In one sentence, what does a student need to exit EL status?**
Use the policy's wording if you can.
*Example (WIDA ACCESS):* An Overall Composite proficiency level of 4.5 or
higher.
*Example (ELPA21):* A determination of Proficient: level 4 or 5 in each of
listening, speaking, reading and writing.

> Answer:

**C2. List each separate requirement in the rule.**
For each, give: which score, whether it's a **proficiency level** or a
**scale score**, and the minimum.
*Example (one requirement):* Overall Composite, proficiency level, at least 4.5
*Example (two requirements):* (1) Overall Composite, proficiency level, at
least 4.2; (2) Literacy, proficiency level, at least 3.9
*Example (ELPA21, four requirements):* listening, speaking, reading and
writing, each proficiency level at least 4

If a requirement is on a single domain, also say: **if that domain wasn't
tested** (for example, waived because of a disability), is the student judged
on the other requirements alone?
*Example:* Yes, a waived domain is skipped

> Answer:

**C3. If there's more than one requirement, must a student meet all of them,
or any one?**
*Example:* All of them

> Answer:

**C4. Which of these best describes the rule?** (more than one may apply)

1. A single overall score
2. An overall score plus a minimum on another score
3. An overall score plus something outside the ELP test (such as a state academic
   test, a grade, or a teacher rating)
4. Scores that can make up for each other (a high score in one area offsets
   a lower one)
5. Different routes or tiers, or a decision that involves judgment
6. A minimum in every domain (listening, speaking, reading, writing)

> Answer:

**C5. Who makes the exit decision?**
*Example:* It's automatic once scores are reported; no committee is involved.

> Answer:

**C6. Can a committee or educator override the scores, either way?**
*Example:* No

> Answer:

**C7. Does the rule differ by grade?**
*Example:* No

> Answer:

**C8. Where is the exit rule written down, and when was it adopted?**
*Example:* State administrative regulation [number], adopted [year]; or a
department guidance document with its title and date

> Answer:

**C9. Score bands for charts.** **Skip for ELPA21, and for WIDA ACCESS when
the exit level is 2.0, 3.0, 4.0, 4.5 or 5.0** (those are already band edges).
Reports group students into bands of
proficiency level. The standard ACCESS bands are 1.0-1.9, 2.0-2.9, 3.0-3.9,
4.0-4.4, 4.5-4.9 and 5.0+. If your exit score falls inside a band (for example
4.2), the setup tool can split that band at your cut. Keep the standard bands,
or split at the cut?
*Example:* Keep the standard bands

> Answer:

---

## Part D. Timelines

**D1. At most, how many years should it take a student to exit EL status?**
*Example:* 6

> Answer:

**D2. How many years does your ESSA plan set as the timeline to reach English
proficiency?** *(optional)*
*Example:* 5

> Answer:

**D3. A student who exits within how many years counts as exiting "on time"?**
*(optional; usually the same as D2)*
*Example:* 5

> Answer:

**D4. Does your state define "long-term English learner"?** *(optional)*
If yes, after how many years in EL status does a student count as long-term,
and what is the full definition?
*Example:* Year 6 or later. "A student who has been identified as an English
learner for six or more years without exiting."

> Answer:

**D5. Where are these timelines written down?**
*Example:* The state's ESSA Consolidated State Plan, with the section number

> Answer:

---

## Part E. Growth targets *(optional)*

**E1. How much growth is a student expected to make each year, in proficiency
levels?**
*Example:* 0.4 levels per year

> Answer:

**E2. How are growth targets set?**
*Example:* Annual targets based on the student's starting level and grade, set
in the ESSA plan

> Answer:

**E3. Where are the growth targets written down?**

> Answer:

---

## Part F. Reporting rules

**F1. What is the smallest group size you are allowed to report?**
Smaller groups are hidden in every table and chart.
*Example:* 10

> Answer:

**F2. Where is that rule written down?**
*Example:* The department's data suppression policy

> Answer:

**F3. Districts are grouped by EL enrollment for some charts: under 100,
101-999, and 1,000 or more. Keep those groups, or use your own?**
*Example:* Keep them

> Answer:

---

## Part G. State academic tests

These are the state's general academic assessments, the reading and math
tests all students take (for example, the state summative assessment).
They are **not** the English language proficiency test. The analysis compares
students' ELP levels with how they do on these tests.

**G1. Which academic test subjects should be included?** Usually reading (or
English language arts) and math. List only subjects in the data file. For each
subject, give:

- **a.** The test's name. *Example:* State Summative Assessment,
  Reading
- **b.** The grades tested. *Example:* 3-8
- **c.** The performance levels from lowest to highest, **exactly as they
  appear in the data file**. *Example:* BB, B, P, A (Below Basic, Basic,
  Proficient, Advanced)
- **d.** The cut scores: the minimum scale score for each level above the
  lowest. Say whether they are **the same in every grade** or **differ by
  grade** (most state tests). If they differ, attach the table, one row per
  grade. If they are not available yet, say so: the analysis then reads
  proficiency from the performance level in the data file instead.
  *Example:* differ by grade; grade 3: 400, 450, 500; grade 4: 405, 455, 505;
  ...

> Answer:

**G2. Which performance level counts as proficient?**
*Example:* Proficient (the 3rd level)

> Answer:

**G3. Where are the academic test's performance levels and cut scores
published?**
*Example:* The state test's technical report, 2024-25

> Answer:

---

## Part H. Programs and priorities

These questions describe what EL status means for students day to day.

**H1. What kinds of EL services does your state offer?**
*Example:* Push-in support, pull-out ESL, sheltered content classes

> Answer:

**H2. Does EL status affect which courses a student can take?**
*Example:* Newcomer students take a sheltered English course in place of
English I

> Answer:

**H3. Are there credentials tied to language, like a Seal of Biliteracy?**
*Example:* Seal of Biliteracy on the diploma

> Answer:

**H4. What changes for a student the moment they exit?** For example,
services that stop, testing accommodations that end, or monitoring that begins.
*Example:* ESL services and accommodations end; the student is monitored for
four years

> Answer:

**H5. Which mistake is worse?**

- [ ] Exiting a student before they're ready
- [ ] Keeping a student in EL status too long
- [ ] Both are equally bad
- [ ] Not sure / not discussed

> Answer:

**H6. Who answered H5, when, and why?**
*Example:* EL program director, October 2026. Exiting too early removes
supports a student still needs.

> Answer:

**H7. Where are these program details written down?** *(optional)*

> Answer:

---

## Part I. Data files

Give the **column name** used in your data files for each item. Leave blank if
the file doesn't have it. The values each column must hold (for example,
EL status as current / former / never, and the ELP test form as annual /
alternate) are listed in the package's data format guide,
`stateELP::el_data_format()`; `stateELP::check_el_delivery()` checks the files
against it.

**I1. Student and year file** (one row per student per year)

| Item | Column name | Example |
|---|---|---|
| I1a. Student ID | | ssid |
| I1b. School year | | year |
| I1c. Grade | | grade |
| I1d. EL status (current / former / never) | | EL_STATUS |
| I1e. ELP test form (annual / alternate) | | ELP_FORM |
| I1f. District | | district_number |
| I1g. School | | school_code |
| I1h. Special education / IEP (Y / N) | | iep |
| I1i. Gender | | SEX |
| I1j. Race / ethnicity | | Ethnic |

**I2. How is kindergarten coded in the grade column?**
*Example:* 0

> Answer:

**I3. ELP score file** (one row per student, year and score)

| Item | Column name | Example |
|---|---|---|
| I3a. Student ID | | ssid |
| I3b. School year | | year |
| I3c. Scale score | | scale_score |
| I3d. Proficiency level | | proficiency_level |

**I4. Academic test file** (the state academic test from Part G; one row per
student, year and subject)

| Item | Column name | Example |
|---|---|---|
| I4a. Student ID | | ID |
| I4b. School year | | YEAR |
| I4c. Grade | | GRADE |
| I4d. Subject | | CONTENT_AREA |
| I4e. Scale score | | SCALE_SCORE |
| I4f. Performance level | | ACHIEVEMENT_LEVEL |

**I5. How is each subject coded in the subject column?**
*Example:* READING = reading test, MATHEMATICS = math test

> Answer:

---

## Part J. Sign-off

**J1. How complete is this information?**

- [ ] Draft: some answers still need checking
- [ ] Complete but not yet reviewed
- [ ] Reviewed and confirmed

**J2. Who reviewed it?**

> Answer:

**J3. Date reviewed:**

> Answer:
