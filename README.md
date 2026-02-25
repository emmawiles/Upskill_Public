Reproducibility Guide (Nature Human Behaviour)

Canonical Data Sources
The analysis uses two canonical raw datasets:

data/complete_data_DS.dta: Task-level dataset that includes treatment, control, and data scientist (DS) arms.

data/complete_data_all.dta: Full cohort dataset used for sample-flow, completion, and attrition analyses.

Derived Datasets Used Across Analyses
The repository builds two core derived datasets:

computed_objects/experimental_data.csv: Built from complete_data_DS.dta by analysis/co_experiment_data.R. This is the main analysis dataset used by most treatment-effect tables and figures.

computed_objects/experimental_data_all.csv. Built from complete_data_all.dta by analysis/co_experiment_data_all.R. This is used for completion/time/attrition-related analyses.

Main Text Tables and Figures: Script and Data Mapping
Table 1 (Building the experimental sample)
Output: balance_table_small.tex
Script: table_sample.R
Data: uses complete_data_all.dta for Panel A (allocation/sample flow counts) and combines experimental_data.csv (Panel B completed-task counts) with experimental_data_all.csv (Panel B started-task denominators for p-values).

Figure 1 (Effect of AI treatment on workers’ ability to solve data science problems)
Output: plots/grades.pdf
Script: analysis/plot_task_grades.R
Data: computed_objects/experimental_data.csv

Figure 2 (Effect of AI treatment by prior coding experience)
Output: plots/grades_by_code.pdf
Script: analysis/plot_task_grades_het.R
Data: computed_objects/experimental_data.csv

Figure 3 (Post-experiment data science knowledge without GenAI)
Output: plots/learning.pdf
Script: analysis/table_learning.R
Data: data/complete_data_all.dta (plus derived terms computed in script)

Figure 4 (Predictions about ChatGPT capabilities / calibration treatment effects)
Output: plots/callibration_te.pdf
Script: analysis/table_callibration.R
Data: main analysis data (intended: computed_objects/experimental_data.csv)

Appendix Tables and Figures: Script and Data Mapping

Table B9
Script: analysis/table_benchmark.R
Data: data/complete_data_DS.dta

Tables B10–B13
Script: analysis/table_task_grades.R
Data: computed_objects/experimental_data.csv

Table B14
Script: analysis/table_learning_new.R
Data: data/complete_data_all.dta

Table B15 (Lee bounds)
Script: analysis/leebounds.do
Data: data/complete_data_DS.dta, restricted to treatment_arm in {0,1}

Tables B16–B18
Script: analysis/table_robust.R
Data: computed_objects/experimental_data.csv

Tables B19–B21
Script: analysis/tab_task_detailed.R
Data: computed_objects/experimental_data.csv

Figure B3
Script: analysis/plot_task_grades.R
Data: computed_objects/experimental_data.csv

Figure B4
Script: analysis/plot_task_grades_het.R
Data: computed_objects/experimental_data.csv

Figure B5

Script: analysis/table_learning.R
Data: data/complete_data_all.dta

Recommended Execution Order

Step 1: Build main analysis dataset
Run analysis/co_experiment_data.R
Produces computed_objects/experimental_data.csv

Step 2: Build full-sample derived dataset
Run analysis/co_experiment_data_all.R
Produces computed_objects/experimental_data_all.csv

Step 3: Run table and figure scripts
Includes task-performance, heterogeneity, robustness, learning, calibration, benchmark, detailed components, completion/time scripts

Step 4: Run robustness/attrition do-files
Run analysis/leebounds.do
Run analysis/attrition.do

# Upskill_Public
