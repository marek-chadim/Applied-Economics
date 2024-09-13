clear

*Normalize the data set AL99
use "$rootdir/Build/Temp/final5.dta", clear
isid  schlcode classid grade
duplicates report schlcode classid grade
use "$rootdir/Build/Temp/final4.dta", clear
isid  schlcode classid grade
duplicates report schlcode classid grade

merge 1:1 schlcode classid grade using "$rootdir/Build/Temp/final5.dta", nogen
gen flgrm= cond(grade == 4, flgrm4, flgrm5)
gen mrkgrm= cond(grade == 4, mrkgrm4, mrkgrm5)
gen ngrm= cond(grade == 4, ngrm4, ngrm5)
gen flmth= cond(grade == 4, flmth4, flmth5)
gen mrkmth= cond(grade == 4, mrkmth4, mrkmth5)
gen nmth= cond(grade == 4, nmth4, nmth5)
drop *4 *5 

*Rename variables and save in Build/Input folder.
destring c_tip, replace force
rename c_* cohort_*

save "$rootdir/Build/Input/AL99.dta", replace