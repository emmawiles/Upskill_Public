# Author review required

Open items that need an author's sign-off before the site is considered final. None
of these blocks the site from functioning; each is a place where the source materials
were ambiguous or internally inconsistent, and the site made a **documented, defensible
choice** rather than guessing silently. Please confirm or correct each.

The website never displays an unresolved number as if it were verified. Where a choice
was made, it is the one that matches the **latest public PDF** and the **plotted
figures**.

---

## A. Sample-size label in Table 1 (983 vs 986)

- **Issue:** Table 1 ("Building the experimental sample") prints **983** in its total row,
  but the treatment and control columns are **493 + 493 = 986**, and the paper's body text
  says the RCT involved **986** consultants.
- **Site's choice:** Uses **986** as the headline experiment size (matching the body text
  and the brief), **573** as "began the survey" (the abstract's N), and **487** as the
  analysis sample.
- **Action:** Confirm 986 is the correct total, and that the 983 in Table 1 is a typo.
Confirmed!

## B. Figure 3 sample sizes vs. plotted values (internal inconsistency in the latest PDF)

- **Issue:** In the latest public PDF, the **Figure 3 note** reports per-question sample
  sizes of **323 / 270 / 270 / 151 / 270** (a subsample restricted to workers who started
  the related task). But the **plotted bars** and the paper's **text statistics** (TEs of
  −7.7 to +4.8 pp, joint F(5,198)=0.83, Bayes factors) correspond to the **full sample**
  (**573 / 399 / 418 / 253 / 408**, matching Appendix Table B14). If one instead uses the
  task-started subsample, two questions become individually significant (one positive, one
  negative), which the text does not report.
- **Site's choice:** Uses the **full-sample** values, because they match the plotted figure,
  the reported joint test, the Bayes factors, and the paper's stated conclusion of *no
  evidence of improved unaided knowledge*. The site notes "sample size varies by question
  (253–573)."
- **Action:** Confirm which sample underlies Figure 3, and correct the figure note or the
  plotted values so they agree. Confirm the website should keep the full-sample version.
  Do exactly what we do in the paper.

## C. Paper title (repo draft vs. latest PDF)

- **Issue:** The repository contains `writeup/reskill (9).pdf`, titled **"Generative AI as a
  Temporary Exoskeleton for Upskilling Knowledge Workers."** The latest public PDF
  (`emmawiles.com/storage/reskill.pdf`) is titled **"Generative AI and the Temporary
  Upskilling of Knowledge Workers."**
- **Site's choice:** Uses the **latest public PDF title** throughout.
- **Action:** Confirm the latest title is authoritative (and, if convenient, replace the
  stale draft PDF in the repo to avoid future confusion).
  Use "Generative AI as a Temporary Exoskeleton for Upskilling Knowledge Workers." as the title of the website, but use the correct paper citation with "Generative AI and the Temporary
  Upskilling of Knowledge Workers." title
  
## D. Author affiliations

- **Site's rendering:**
  - Emma Wiles — Boston University (joint first & corresponding author)
  - Lisa Krayer — BCG Henderson Institute (joint first & corresponding author)
  - Mohamed Abbadi — BCG Henderson Institute
  - Urvi Awasthi — BCG Henderson Institute
  - Ryan Kennedy — BCG Henderson Institute
  - Pamela Mishkin — OpenAI (Economic Impacts Research)
  - Daniel Sack — BCG Henderson Institute (joint supervising author)
  - Francois Candelon — BCG Henderson Institute (joint supervising author)
- **Note:** The older draft's superscripts were ambiguous (line numbers were interleaved with
  affiliation markers during extraction); one draft appeared to give Urvi Awasthi a second
  (OpenAI) affiliation. The latest PDF and the competing-interests statement support the
  single-affiliation rendering above.
- **Action:** Confirm each author's affiliation and the joint-authorship roles, and whether
  any author holds a second affiliation.
  Every author from BCG Henderson should have their affiliation replaced with just "BCG"

## E. Publication status and citation

- **Site's choice:** Labels the paper **"Forthcoming in *Nature Human Behaviour*"** (from the
  repo README's "Reproducibility Guide (Nature Human Behaviour)" and the paper). The citation
  block shows year as **(forthcoming)** and states that final **year, volume, and DOI are
  pending**. No DOI, volume, or BibTeX entry was invented.
- **Action:** Confirm the journal and status, and provide the final citation details when
  available so the "forthcoming" placeholders can be replaced.

## F. Preregistration and supplementary links

- **Issue:** The paper references a preregistration and an online appendix/supplement via
  "here" links that are not resolvable from the repository alone.
- **Site's choice:** Does **not** link to a preregistration or supplement URL (to avoid a
  fabricated or dead link). The site links only to the two confirmed URLs: the paper PDF and
  the GitHub repository.
- **Action:** If you want the preregistration (e.g., AsPredicted/OSF) and/or supplementary
  information linked, provide the canonical URLs and they can be added.

---

### Wording confirmed as within-scope (no change expected, listed for transparency)

The site deliberately avoids over-claiming. It states there is **"no evidence of improved
unaided technical knowledge immediately after the experiment"** (not "participants learned
nothing"), presents Bayes factors as **evidence favoring similarity** only where the paper
does, describes the calibration result as **forecasting accuracy** (not self-reported
confidence), and does **not** claim the study shows whether AI can or cannot serve as a
teacher. Please confirm this framing is acceptable.
