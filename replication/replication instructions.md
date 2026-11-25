# Replication Instructions
To replicate this paper run the script "master file.R" to execute all code files in the correct order. This will acquire, clean and merge all the data from scratch; generate summary statistics; run the empirical analysis and save the main and appendix plots; run the model simulations and save the plots; and generate the plot of FERS annuity vs term. From start to finish the replication process takes roughly an hour, though total run time depends on internet speed when accessing RVoteview and the Congress API.

NOTE: built on Python 3.14.0 and R version 4.6.0

NOTE: creating the data from scratch requires acquiring a Congress.gov API key and setting it in .Renviron. It also requires installing Rvoteview from Github, which is done automatically by script 01 and only needs to be done once.