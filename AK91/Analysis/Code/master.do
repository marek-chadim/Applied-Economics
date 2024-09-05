/***************************************
* Program to produce Tables IV-VI in Angrist and Kreuger (1991)
****************************************/

global rootdir "C:/Users/chadi/Dropbox/Applied-Economics/AK91"
cd $rootdir

*Load raw data to Build/Input folder. First remove and create Build/Input directory, then load data.
!rmdir "Build/Input" /s /q
mkdir "Build/Input"
copy Raw/NEW7080.dta Build/Input/

*Build basic data sets in Analysis/Input
!rmdir "Analysis/Input" /s /q
mkdir "Analysis/Input"
do Build/Code/TableIV_data.do
do Build/Code/TableV_data.do
do Build/Code/TableVI_data.do

* Analysis to replicate tables
!rmdir "Analysis/Output" /s /q
mkdir "Analysis/Output"
do Analysis/Code/TableIV.do
do Analysis/Code/TableV.do
do Analysis/Code/TableVI.do

!rmdir "Build/Input" /s /q
!rmdir "Analysis/Input" /s /q