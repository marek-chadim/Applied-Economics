
clear

local infile "$rootdir/Analysis/Input/Table_data.dta"
local outfile "$rootdir/Analysis/Output/Columns1357"

use "`infile'", clear
log using "`outfile'", text replace

local controls "RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT"
local qtr "QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28"

program define columns1357 

*Table IV
eststo clear
preserve
keep if COHORT<20.30
reg  LWKLYWGE EDUC  YR20-YR28
estimates store c1
reg  LWKLYWGE EDUC  YR20-YR28 AGEQ AGEQSQ
estimates store c3
reg  LWKLYWGE EDUC  `controls' YR20-YR28
estimates store c5
reg  LWKLYWGE EDUC  `controls' YR20-YR28 AGEQ AGEQSQ 
estimates store c7
esttab c1 c3 c5 c7
esttab c1 c3 c5 c7 using "$rootdir/Analysis/Output/TableIVcol1357.tex"
restore

*Table V
eststo clear
preserve
keep if COHORT>30.00 & COHORT <30.40
reg  LWKLYWGE EDUC  YR20-YR28 
estimates store c1
reg  LWKLYWGE EDUC  YR20-YR28 AGEQ AGEQSQ
estimates store c3
reg  LWKLYWGE EDUC  `controls' YR20-YR28
estimates store c5
reg  LWKLYWGE EDUC  `controls' YR20-YR28 AGEQ AGEQSQ 
estimates store c7
esttab c1 c3 c5 c7
esttab c1 c3 c5 c7 using "$rootdir/Analysis/Output/TableVcol1357.tex"
restore

*Table VI
eststo clear
preserve
keep if COHORT>40.00
reg  LWKLYWGE EDUC  YR20-YR28 
estimates store c1
reg  LWKLYWGE EDUC  YR20-YR28 AGEQ AGEQSQ
estimates store c3
reg  LWKLYWGE EDUC  `controls' YR20-YR28
estimates store c5
reg  LWKLYWGE EDUC  `controls' YR20-YR28 AGEQ AGEQSQ 
estimates store c7
esttab c1 c3 c5 c7
esttab c1 c3 c5 c7 using "$rootdir/Analysis/Output/TableVIcol1357.tex"
restore
end

log close