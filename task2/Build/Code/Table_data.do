clear

use "$rootdir/Build/Input/AK91.dta"
drop v8
gen COHORT=20.29
replace COHORT=30.39 if YOB<=39 & YOB >=30
replace COHORT=40.49 if YOB<=49 & YOB >=40
replace AGEQ=AGEQ-1900 if CENSUS==80
gen AGEQSQ= AGEQ*AGEQ

// Generate YOB dummies using a loop and macro
local yob_base = 1920
forvalues i = 20/29 {
    gen YR`i' = 0
    replace YR`i' = 1 if inlist(YOB, `yob_base' + (`i' - 20), `yob_base' + (`i' - 20) + 10, `yob_base' + (`i' - 20) + 20)
}

// Generate QOB dummies using a loop
forvalues q = 1/4 {
    gen QTR`q' = 0
    replace QTR`q' = 1 if QOB == `q'
}

// Generate YOB*QOB interaction dummies using nested loops
forvalues q = 1/4 {
    forvalues i = 20/29 {
        gen QTR`q'`i' = QTR`q' * YR`i'
    }
}

save "Analysis/Input/Table_data.dta", replace