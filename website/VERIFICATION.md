# Verification table

Every important numerical claim on the website, with its displayed value and source.
Values marked **reproduced** are re-derived from the raw repository data by
`scripts/verify_from_source.py` (which passes: 44/44 checks). Values marked
**transcribed** are copied from a published table in the paper/repo. "Author review"
flags items in [`AUTHOR_REVIEW_REQUIRED.md`](AUTHOR_REVIEW_REQUIRED.md).

Sources: **Paper** = latest public PDF (`https://www.emmawiles.com/storage/reskill.pdf`).
Repo files are relative to the repository root.

| # | Website claim | Displayed | Paper section | Table/Figure | Repo file | Status | Author review |
|---|---------------|-----------|---------------|--------------|-----------|--------|:---:|
| 1 | Consultants randomized in the RCT | 986 | Intro; Methods | Table 1 (row 1) | `writeup/stable/balance_table_small.tex` | transcribed | yes¹ |
| 2 | Began the survey (abstract N) | 573 | Abstract; Methods | Table 1 | `writeup/stable/balance_table_small.tex` | transcribed | — |
| 3 | Analysis sample (submitted both tasks) | 487 | Results; Methods | Table 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 4 | Per-task samples | coding 300 / stats 330 / prediction 298 | Results | Table 1; Fig 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 5 | Data-scientist benchmark | 44 | Results; Methods | — | `data/complete_data_DS.dta` | transcribed | — |
| 6 | Coding treatment effect | +49 pp, 95% CI [0.42, 0.56] | §2.1 | Fig 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 7 | Statistics treatment effect | +20 pp, 95% CI [0.15, 0.25] | §2.1 | Fig 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 8 | Prediction treatment effect | +17 pp, 95% CI [0.09, 0.25] | §2.1 | Fig 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 9 | Coding group means (control / treatment) | −0.63 / −0.14 | §2.1 | Fig 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 10 | Statistics group means | −0.32 / −0.12 | §2.1 | Fig 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 11 | Prediction group means | −0.43 / −0.26 | §2.1 | Fig 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 12 | Treatment groups remain below benchmark (all tasks) | reject equality | §2.1–2.2 | Fig 1 | `computed_objects/experimental_data.csv` | reproduced | — |
| 13 | Competent coders — coding | TE +0.51 [0.38,0.64]; level −0.05; BF 0.25 | §2.2 | Fig 2 | `computed_objects/experimental_data.csv` | reproduced (BF transcribed) | — |
| 14 | Competent coders — statistics | TE +0.16 [0.08,0.23]; level −0.05; BF 0.37 | §2.2 | Fig 2 | `computed_objects/experimental_data.csv` | reproduced (BF transcribed) | — |
| 15 | Competent coders — prediction | TE +0.27 [0.13,0.40]; still below benchmark | §2.2 | Fig 2 | `computed_objects/experimental_data.csv` | reproduced | — |
| 16 | Figure 2 effects (never/basic coded, all tasks) | see chart & note | §2.2 | Fig 2 | `computed_objects/experimental_data.csv` | reproduced | — |
| 17 | Coding-experience category labels | Never coded / Basic coding / Competent coder | §2.2 | Fig 2 | `computed_objects/experimental_data.csv` (`know_code`) | reproduced | — |
| 18 | No improvement in unaided knowledge (5 Qs) | control/treatment shares per Q | §2.3 | Fig 3; Table B14 | `data/complete_data_all.dta`; `writeup/tables/learning.tex` | reproduced | yes² |
| 19 | Learning treatment effects | −7.7 to +4.8 pp; none significant | §2.3 | Fig 3; Table B14 | `data/complete_data_all.dta` | reproduced | — |
| 20 | Learning joint test | F(5,198)=0.83, p=0.531 | §2.3 | — | `analysis/table_learning.R` | transcribed | — |
| 21 | Learning Bayes factors | 0.10, 0.18, 0.50, 0.13, 0.62 | §2.3 | — | Paper §2.3 | transcribed | — |
| 22 | Calibration / overconfidence effects (7 Qs) | TEs +0.3 to +9.6 pp on confidence | §2.4 | Fig 4; Table B8 | `writeup/callibration.tex` | transcribed (CI checked) | — |
| 23 | All four "GPT-4 cannot" questions significant | p = 0.012 / 0.024 / <0.001 / 0.039 | §2.4 | Fig 4; Table B8 | `writeup/callibration.tex` | transcribed | — |
| 24 | Calibration control base rates | 65–78% | §2.4 | Table B8 | `writeup/callibration.tex` | transcribed | — |
| 25 | Calibration analysis is exploratory | not pre-registered | §2.4; Methods | — | Paper | transcribed | — |
| 26 | Paper title | *Generative AI and the Temporary Upskilling of Knowledge Workers* | Title page | — | latest public PDF | transcribed | yes³ |
| 27 | Author list & affiliations | 8 authors; BU / BCG / OpenAI | Title page; author statement | — | latest public PDF | transcribed | yes⁴ |
| 28 | Publication status | Forthcoming, *Nature Human Behaviour* | — | — | `README.md` (repo); latest PDF | transcribed | yes⁵ |
| 29 | Two 20-minute trainings; ChatGPT (GPT-4) vs. Google/Stack Overflow/Khan Academy | as stated | Intro; Methods | Fig A1 | latest public PDF | transcribed | — |
| 30 | Tasks designed so GPT-4 cannot solve them alone | as stated | Intro; §4.2 | — | latest public PDF | transcribed | — |

**Author-review footnotes**

1. Table 1's header prints **983** total while the two arms (493 + 493) sum to **986**;
   the body text uses 986. The site uses 986 (the headline figure). See review item A.
2. The latest PDF's **Figure 3 note lists per-question Ns (323/270/270/151/270)** from a
   task-started subsample, but the plotted bars and the text statistics correspond to the
   **full sample (573/399/418/253/408)**. The site uses the full-sample values (which match
   the plotted figure and the paper text). See review item B.
3. The repository's `writeup/reskill (9).pdf` is an **older draft** titled
   *"Generative AI as a Temporary Exoskeleton for Upskilling Knowledge Workers."* The site
   uses the **latest public PDF's** title. See review item C.
4. Author affiliation superscripts in the older draft were ambiguous (line numbers merged
   with superscripts); the site uses the **latest PDF + competing-interests statement**.
   See review item D.
5. Publication label ("Forthcoming in *Nature Human Behaviour*") is taken from the repo
   README and the paper; final citation year/volume/DOI are pending. See review item E.
