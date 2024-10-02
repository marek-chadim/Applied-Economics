clear

local infile "$rootdir/Analysis/Input/Table_data.dta"
local outfile "$rootdir/Analysis/Output/TableV"

use "`infile'", clear
log using "`outfile'", text replace

preserve
keep if COHORT>30.00 & COHORT <30.40


local controls "RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT"
local qtr "QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28"

** Col 1 3 5 7 ***
reg  LWKLYWGE EDUC  YR20-YR28 
reg  LWKLYWGE EDUC  YR20-YR28 AGEQ AGEQSQ 
reg  LWKLYWGE EDUC  `controls' YR20-YR28
reg  LWKLYWGE EDUC  `controls' YR20-YR28 AGEQ AGEQSQ 


** Col 2 4 6 8 ***
ivregress 2sls LWKLYWGE YR20-YR28 (EDUC =  `qtr')
ivregress 2sls LWKLYWGE YR20-YR28 AGEQ AGEQSQ (EDUC =  `qtr')
ivregress 2sls LWKLYWGE YR20-YR28 `controls' (EDUC =  `qtr')
ivregress 2sls LWKLYWGE YR20-YR28 `controls' AGEQ AGEQSQ (EDUC =  `qtr')

restore
log close