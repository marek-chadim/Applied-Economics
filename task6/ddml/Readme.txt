Replication files for:

Chernozhukov Victor, Chetverikov Denis, Demirer Mert, Duflo Esther, Hansen Christian, Newey Whitney, James Robins "Double/Debiased Machine Learning for Treatment and Structural Parameters" The Econometrics Journal.

Please contact mdemirer@mit.edu with any questions/comments. This folder contains four subfolders:

401K  : This folder contains the code to replicate Table 2 and Table 3.
AJR   : This folder contains the code to replicate Table 4.
Bonus : This folder contains the code to replicate Table 1.
Sim   : This folder contains the code to replicate Figures 1 and 2.

Each of the three first three folders contains the following files:

� Data Files : penn jae.dat in Bonus, sipp1991.dta in 401K. You can find the data sources and definitions of the variables in Master.R
� Master.R   : This program generates the estimates reported in Tables 1-4 and calls the two programs below.
� Moment_Functions.R : This program contains the functions for estimating moments.
� ML_Functions.R : This program contains the functions for implementing machine learning methods.

Replication Instructions:
In each folder Master.R replicates the empirical results. Note that this program replicates only the first rows (partially linear model in Bonus and 401K and IV model in AJR, 2 fold cross-fitting) in the corresponding tables.
To replicate other results set change the "est" and "nfolds" arguments in Master.R accordingly. See Master.R for more information.
Before running the code set the working directory to the folder where Master.R is located. 
In addition, running the code requires users to download R packages listed at the beginning of the program.
The code is run using paralel computing over 12 cluster nodes taking 12-48 hours depending on the estimation method and number of cross-fitting folds. The execution time can be reduced by setting "split" to a smaller value.


The Sim folder contains two Matlab .m files:

Figure1.m:	Replicates Figure 1
Figure2.m:	Replicates Figure 2

Replication Instructions:

Open the file corresponding to the desired figure and run it in Matlab.