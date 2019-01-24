#readme for Averill et al. 2019 ISMEJ.

This project contains Code to replicate analyses and figures in Averill et al. 2019 ISMEJ.

paths.r: this file contains the directory structure and filepaths necessary for all the scripts to talk to eachother. There is a main data directory, which contains raw data files, data files worked up for analysis, and output of analyses used for visualization. So long as you have all files within the raw directory you *should* be able to replicate all data carpentry, analysis and visualization. The scripts in this project will generate several intermediate data and analysis products, which will be stored within this main data directory, and their respective paths are defined in paths.r

data consturction: This project analyzes 6 independent data sets. Averill_ISMEJ_2018/Scripts/1._data_construction/ contains all code necessary to work raw data products into analysis read data products.

data analysis: all analysis code is broken out by data set. 

Methods Validation data sets: Each methods validation data set (BCI, NEON_HF, Kivlin_bacteria, Kivlin_fungi), has its own directory within Averill_ISMEJ_2018/Scripts/2._analyses/ which contains code to 

1. test for space-time signatures in community similarity. (<dataset>_analysis.r)
2. estimate space and time parameter confidence intervals with orthogonal space-time observations. (<dataset>_bootstrap.r)
3. estimate space and time parameter confidence intervals without orthogonal space-time observations. (<dataset>_bootstrap_sub100.r)

Analysis data sets: Each analysis data set (Talbot_2014, Tedersoo_2014), has its own directory within Averill_ISMEJ_2018/Scripts/2._analyses/ which contains code to:

1. Fit MRM models to test for space-time signatures in community similarity, in the presence or absence of envrionemntal covariates (<dataset>_analysis.r)
2. Estimate space and time parameter confidence intervals in the absence of environmental covariates (<dataset>_bootstrap_st.r)
3. Estimate space and time parameter confidence intervals in the presence of environmental covariates (<dataset>_bootstrap_st.env.r)
4. Tedersoo_2014 also contains Tedersoo_2014_monte_carlo.r for simulating the null expectation of temporal parameter effect sizes.

Some of these directories also contain "_batch.txt" files, which are used to submit jobs to our comptuing cluster. This will need to be modified to work with your particular computing resources. Some of these scripts will complete in a reasonable amount of time during an interactive R session, while others take hours-days running in parallel on a computing cluster.

Figure Scipts:
The directory Averill_ISMEJ_2018/Scripts/3._figure_scripts/ contains code to replicate all figures and tables presented in the manuscript. Code is organized within subdirectories corresponding to main text figures, main text tables, and supplementary figures. Script names correspond with figure and table numbers in the published manuscript.

Transfer Scripts:
Averill_ISMEJ_2018/transfer_scripts contains scripts used to sync data across multiple computers during this project. There are certainly more efficient ways to do this, but this way is straightforward. Transfers are triggered by executing scripts from the bash shell or similar. If you are syncing files across multiple computers you will definitely need to reconfigure these.