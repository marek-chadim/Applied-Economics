/***************************************
* Program to execute Task 2
****************************************/

global rootdir "C:/Users/chadi/Dropbox/Applied-Economics/AK91/AK91"
cd $rootdir

*Task 2a: Clean Data Procedure
*Angrist and Krueger (1991) and Angrist and Lavy (1999)

*1.Import the raw data into a raw data folder.
*Deny writing to this folder.

*2. Normalize the data set AL99
*rename variables and save in Build/Input folder
!rmdir "Build/Temp" /s /q
mkdir "Build/Temp"
copy Raw/NEW7080.dta Build/Temp/

!rmdir "Build/Input" /s /q
mkdir "Build/Input"
do Build/Code/AK91.do
!rmdir "Build/Temp" /s /q

* Task 2b: Clean AK91 code.
* Review names.
* Remove duplication of code and data.
cd $rootdir
!rmdir "Analysis/Input" /s /q
mkdir "Analysis/Input"
do Build/Code/Table_data.do

* Print output regression tables.
cd $rootdir
!rmdir "Analysis/Output" /s /q
mkdir "Analysis/Output" 
do Analysis/Code/TableIV.do
do Analysis/Code/TableV.do
do Analysis/Code/TableVI.do

* Write a program that runs regressions and prints output tables
* Call this program to create columns 1 3 5 7 in the tables
adopath ++ "$rootdir/Analysis/Code"
cap program drop columns1357
columns1357