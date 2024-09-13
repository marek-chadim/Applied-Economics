/***************************************
* Program to execute Task 2
****************************************/

global rootdir "C:/Users/chadi/Dropbox/Applied-Economics/task2"
cd $rootdir

*Task 2a: Clean Data Procedure
*Angrist and Krueger (1991) and Angrist and Lavy (1999)

*1.Import the raw data into a raw data folder.
*Deny writing to this folder.

*2. Normalize the data set AL99
*rename variables and save in Build/Input folder
!rmdir "Build/Temp" /s /q
mkdir "Build/Temp"
copy Raw/final4.dta Build/Temp/
copy Raw/final5.dta Build/Temp/
!rmdir "Build/Input" /s /q
mkdir "Build/Input"
do Build/Code/AL99.do

copy Raw/NEW7080.dta Build/Temp/
do Build/Code/AK91.do
!rmdir "Build/Temp" /s /q

*3. Write a program that does a values review of the AK91 and AL99 data.
* Loop over datasets
* Loop over variable types (string, integer, float)
* Do a values review for each type and print to an output file
cd $rootdir
!rmdir "Build/Output" /s /q
mkdir "Build/Output"
adopath ++ "C:/Users/chadi/Dropbox/Applied-Economics/task2/Build/Code"
cd $rootdir/Build/Input
cap program drop value_review
value_review "AK91 AL99"

* Task 2b: Clean AK91 code.
cd $rootdir
!rmdir "Analysis/Input" /s /q
mkdir "Analysis/Input"
do Build/Code/Table_data.do

* Analysis to replicate tables
cd $rootdir
!rmdir "Analysis/Output" /s /q
mkdir "Analysis/Output" 
do Analysis/Code/TableIV.do
do Analysis/Code/TableV.do
do Analysis/Code/TableVI.do

!rmdir "Build/Input" /s /q
!rmdir "Analysis/Input" /s /q