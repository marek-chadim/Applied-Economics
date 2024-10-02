clear

use "$rootdir/Build/Input/AK91.dta"
drop v8
gen COHORT=20.29
replace COHORT=30.39 if YOB<=39 & YOB >=30
replace COHORT=40.49 if YOB<=49 & YOB >=40
replace AGEQ=AGEQ-1900 if CENSUS==80
gen AGEQSQ= AGEQ*AGEQ

// Generate YOB dummies using a loop
forvalues i = 20/29 {
    gen YR`i' = (YOB == 19`i' | YOB == `i' + 10 | YOB == `i' + 20)
}

// Generate QOB dummies
forvalues i = 1/4 {
    gen QTR`i' = (QOB == `i')
}

*xi i.QOB

// Generate YOB*QOB interaction dummies
forvalues q = 1/4 {
    forvalues y = 20/29 {
        gen QTR`q'`y' = QTR`q' * YR`y'
    }
}

save "Analysis/Input/Table_data.dta", replace