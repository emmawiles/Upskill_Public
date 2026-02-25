* Build the attrition comparison table only.
clear all
set more off

* Resolve project root whether run from repo root or /analysis.
local ROOT "`c(pwd)'"
capture confirm file "`ROOT'/data/complete_data_all.dta"
if _rc {
    local ROOT = subinstr("`ROOT'", "/analysis", "", .)
}
capture confirm file "`ROOT'/data/complete_data_all.dta"
if _rc {
    di as error "Could not find data/complete_data_all.dta. Run this from the project root or /analysis."
    exit 601
}

use "`ROOT'/data/complete_data_all.dta", clear

* Harmonized indicators used in attrition comparisons.
gen treatment = .
replace treatment = 1 if Group == "Test"
replace treatment = 0 if Group == "Control"

gen female            = (Gender == 1) if !missing(Gender)
gen europe            = (OfficeLocation == 4) if !missing(OfficeLocation)
gen native_english    = (EnglishProficiency == 4) if !missing(EnglishProficiency)
gen low_tenure        = (TenureYears == 1) if !missing(TenureYears)
gen prof_coder        = (CodingPre == 2) if !missing(CodingPre)
gen never_coded       = (CodingPre == -1) if !missing(CodingPre)
gen at_most_1_lang    = (ProgrammingLanguagesNo <= 1) if !missing(ProgrammingLanguagesNo)
gen chatgptdaily      = (ChatGPT4Work == 5) if !missing(ChatGPT4Work)
gen prompt_familiar   = (PromptEngFamiliarity > 4) if !missing(PromptEngFamiliarity)
gen never_code_work   = (FrequencyCodeWork == 1) if !missing(FrequencyCodeWork)

* Match attrition definition used in the project.
gen ps_score = PSMAE_score_adjusted * -1
gen attrit = ///
    (missing(CodingProcessGradePercent) & missing(StatsMCCorrectnessPercent)) | ///
    (missing(ps_score) & missing(CodingProcessGradePercent)) | ///
    (missing(ps_score) & missing(StatsMCCorrectnessPercent))

* Keep only control/treatment arms.
keep if inlist(treatment, 0, 1)

tempfile attrition_results
tempname memhold
postfile `memhold' int ord str10 treatment_group str45 covariate ///
    double sample_mean attritor_mean p_value using `attrition_results', replace

capture confirm variable PhD
if _rc gen PhD = .

local vars female europe native_english low_tenure prof_coder never_coded at_most_1_lang PhD chatgptdaily prompt_familiar never_code_work
local label_1  "Female"
local label_2  "Office in Europe or Middle East"
local label_3  "Native English speaker"
local label_4  "New hire (<1 year)"
local label_5  "Proficient coder or better"
local label_6  "Never coded"
local label_7  "At most 1 coding language"
local label_8  "PhD"
local label_9  "Uses ChatGPT daily for work"
local label_10 "Familiar with prompt engineering"
local label_11 "Never code for work"

local i = 1
foreach v of local vars {
    local label "`label_`i''"

    foreach tr in 0 1 {
        quietly summarize `v' if treatment == `tr' & attrit == 0
        local m_sample = r(mean)

        quietly summarize `v' if treatment == `tr' & attrit == 1
        local m_attrit = r(mean)

        quietly capture ttest `v' if treatment == `tr', by(attrit)
        local p = .
        if !_rc local p = r(p)

        local grp = cond(`tr' == 0, "Control", "Treatment")
        post `memhold' (`i') ("`grp'") ("`label'") (`m_sample') (`m_attrit') (`p')
    }
    local ++i
}
postclose `memhold'

use `attrition_results', clear
sort ord treatment_group
drop ord

export delimited using "`ROOT'/computed_objects/attrition.csv", replace

* Write identical LaTeX table to both stable/ and tables/.
foreach outtex in "`ROOT'/writeup/stable/attrition.tex" "`ROOT'/writeup/tables/attrition.tex" {
    file open fh using "`outtex'", write replace text
    file write fh "\begin{table}[]" _n
    file write fh "\caption{Comparing those who submit something for both tasks (primary analysis sample) to attritors}" _n
    file write fh "  \label{tab:attrit}" _n
    file write fh "\begin{tabular}{lllll}" _n
    file write fh "Treatment Group & Covariate & Sample Mean & Attritor Mean & P-value \\\\ \hline" _n

    forvalues r = 1/`=_N' {
        local g = "`=treatment_group[`r']'"
        local c = "`=covariate[`r']'"
        local ms = sample_mean[`r']
        local ma = attritor_mean[`r']
        local pv = p_value[`r']

        local msf : display %4.2f `ms'
        local maf : display %4.2f `ma'
        local pvf : display %5.3f `pv'

        file write fh "`g' & `c' & `msf' & `maf' & `pvf' \\\\" _n
    }

    file write fh "\end{tabular}" _n
    file write fh "\end{table}" _n
    file close fh
}

di as text "Wrote:"
di as text " - `ROOT'/writeup/tables/attrition.tex"
