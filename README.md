Reproducibility Guide (Nature Human Behaviour)

This README maps the paper's tables and figures to the scripts, data inputs, and output files that reproduce them.

Working directory

Run the analysis scripts from `analysis/`, not from the repository root. The scripts use relative paths such as `../data/...` and `../computed_objects/...`.

Recommended invocation pattern:

```bash
cd analysis
Rscript co_experiment_data.R
```

Canonical data sources

The analysis uses two canonical raw datasets:

- `data/complete_data_DS.dta`: task-level dataset including treatment, control, and data scientist (DS) arms.
- `data/complete_data_all.dta`: full cohort dataset used for sample-flow, completion, timing, learning, and attrition analyses.

Derived datasets used across analyses

The repository builds two core derived datasets:

- `computed_objects/experimental_data.csv`: built from `data/complete_data_DS.dta` by `analysis/co_experiment_data.R`. This is the main analysis dataset used by most treatment-effect tables and figures.
- `computed_objects/experimental_data_all.csv`: built from `data/complete_data_all.dta` by `analysis/co_experiment_data_all.R`. This is used for completion, timing, and related analyses.

Dependencies

- R packages used by the analysis scripts must be installed in the local R environment.
- Stata is only required for the `.do` files used for Lee bounds and attrition analyses.

Required R packages

The analysis scripts import the following R packages:

- `dplyr`
- `tidyr`
- `ggplot2`
- `stargazer`
- `haven`
- `lfe`
- `lmtest`
- `sandwich`
- `forcats`
- `magrittr`
- `msm`
- `car`
- `broom`
- `readr`
- `latex2exp`
- `quantreg`
- `JJHmisc`

For CRAN packages, you can install them with:

```r
install.packages(c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "stargazer",
  "haven",
  "lfe",
  "lmtest",
  "sandwich",
  "forcats",
  "magrittr",
  "msm",
  "car",
  "broom",
  "readr",
  "latex2exp",
  "quantreg",
  "devtools"
))
```

Install `JJHmisc` from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install JJHmisc from GitHub
devtools::install_github("johnjosephhorton/JJHmisc")
```

Output directories

Generated outputs are written to multiple locations in `writeup/`, primarily:

- `writeup/plots/`
- `writeup/tables/`
- `writeup/stable/`
- selected files directly in `writeup/`

Main text tables and figures

Table 1. Building the experimental sample
- Output: `writeup/stable/balance_table_small.tex`
- Script: `analysis/table_sample.R`
- Data: `data/complete_data_all.dta`, `computed_objects/experimental_data.csv`, `computed_objects/experimental_data_all.csv`

Figure 1. Effect of AI treatment on workers' ability to solve data science problems
- Output: `writeup/plots/grades.pdf`
- Script: `analysis/plot_task_grades.R`
- Data: `computed_objects/experimental_data.csv`

Figure 2. Effect of AI treatment by prior coding experience
- Output: `writeup/plots/grades_by_code.pdf`
- Script: `analysis/plot_task_grades_het.R`
- Data: `computed_objects/experimental_data.csv`

Figure 3. Post-experiment data science knowledge without GenAI
- Output: `writeup/plots/learning.pdf`
- Script: `analysis/table_learning.R`
- Data: `data/complete_data_all.dta`

Figure 4. Predictions about ChatGPT capabilities / calibration treatment effects
- Output: `writeup/plots/callibration_te.pdf`
- Script: `analysis/table_callibration.R`
- Data: `computed_objects/experimental_data.csv`

Appendix tables and figures

Table B9
- Output: `writeup/performanceDSBaseline.tex`
- Script: `analysis/table_benchmark.R`
- Data: `data/complete_data_DS.dta`

Tables B10-B13
- Outputs:
  - `writeup/task_correct_het1.tex`
  - `writeup/task_correct_het2.tex`
  - `writeup/task_correct_het3.tex`
  - `writeup/task_correct_het4.tex`
- Script: `analysis/table_task_grades.R`
- Data: `computed_objects/experimental_data.csv`

Table B14
- Output: `writeup/tables/learning.tex`
- Script: `analysis/table_learning.R`
- Data: `data/complete_data_all.dta`

Learning robustness table
- Output: `writeup/learning_robustness.tex`
- Script: `analysis/table_learning.R`
- Data: `computed_objects/experimental_data.csv`

Table B15 (Lee bounds)
- Script: `analysis/leebounds.do`
- Data: `data/complete_data_DS.dta`, restricted to `treatment_arm` in `{0,1}`
- Note: requires Stata.

Tables B16-B18
- Outputs:
  - `writeup/tables/robust_coding.tex`
  - `writeup/tables/robust_stats.tex`
  - `writeup/tables/robust_ps.tex`
- Script: `analysis/table_robust.R`
- Data: `computed_objects/experimental_data.csv`

Tables B19-B21
- Outputs:
  - `writeup/tables/grades_detail_coding.tex`
  - `writeup/tables/grades_detail_stats.tex`
  - `writeup/tables/grades_detail_ps.tex`
- Script: `analysis/tab_task_detailed.R`
- Data: `computed_objects/experimental_data.csv`

Professional identity / belief appendix tables
- Outputs:
  - `writeup/tables/professional_identity.tex`
  - `writeup/tables/belief_chatgpt.tex`
- Scripts:
  - `analysis/table_confidencechange.R`
  - `analysis/table_careerbeliefs.R`
- Data: `data/complete_data_DS.dta`

Completion and timing appendix tables
- Outputs:
  - `writeup/tables/task_finish.tex`
  - `writeup/tables/task_finish2.tex`
  - `writeup/time.tex`
- Scripts:
  - `analysis/table_finish.R`
  - `analysis/table_time.R`
- Data: `computed_objects/experimental_data_all.csv`

Calibration regression table
- Output: `writeup/callibration.tex`
- Script: `analysis/table_callibration.R`
- Data: `computed_objects/experimental_data.csv`

Figure B3
- Output: `writeup/plots/grades_dist.pdf`
- Script: `analysis/plot_task_grades.R`
- Data: `computed_objects/experimental_data.csv`

Figure B4
- Output: `writeup/plots/grades_by_code_dist.pdf`
- Script: `analysis/plot_task_grades_het.R`
- Data: `computed_objects/experimental_data.csv`

Figure B5
- Output: `writeup/plots/learning_dist.pdf`
- Script: `analysis/table_learning.R`
- Data: `data/complete_data_all.dta`

Recommended execution order

The sequence below is the recommended order for reproducing the paper-facing outputs.

Step 1: build the main analysis dataset

```bash
cd analysis
Rscript co_experiment_data.R
```

Produces `computed_objects/experimental_data.csv`.

Step 2: build the full-sample derived dataset

```bash
Rscript co_experiment_data_all.R
```

Produces `computed_objects/experimental_data_all.csv`.

Step 3: run the paper-facing table and figure scripts

```bash
Rscript table_sample.R
Rscript plot_task_grades.R
Rscript plot_task_grades_het.R
Rscript table_learning.R
Rscript table_callibration.R
Rscript table_benchmark.R
Rscript table_task_grades.R
Rscript table_robust.R
Rscript tab_task_detailed.R
Rscript table_finish.R
Rscript table_time.R
Rscript table_confidencechange.R
Rscript table_careerbeliefs.R
```

Step 4: run the Stata robustness / attrition files if needed

```bash
stata -b do leebounds.do
stata -b do attrition.do
```

These scripts require Stata and are needed for the Lee-bounds / attrition appendix outputs.

# Upskill_Public
