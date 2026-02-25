clear all
set more off

* Run from project root or /analysis.
local ROOT "`c(pwd)'"
capture confirm file "`ROOT'/data/complete_data_DS.dta"
if _rc local ROOT = subinstr("`ROOT'", "/analysis", "", .)

use "`ROOT'/data/complete_data_DS.dta", clear
keep if inlist(treatment_arm, 0, 1)

* Lee bounds: treatment_arm==1 (treated) vs treatment_arm==0 (control).
leebounds treatment_arm PSMAEGradeAdjusted
leebounds treatment_arm StatsOverallRelativeNorm
leebounds treatment_arm CodingProcessGradeRelativeNorm
