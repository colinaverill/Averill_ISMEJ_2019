## Code Description for Averill et al. 2019 ISME Journal.

This project contains Code to replicate analyses and figures in Averill et al. 2019 ISMEJ. At the end of this document is a tutorial for how to use the function `Scripts/functions/space_time_power_analysis.r`, whcih can be used to assess statistical power to detect temporal effect sizes given space-time confounding in a given sampling design.

# paths.r

The file `paths.r` contains the directory structure and filepaths necessary for all the scripts to talk to eachother. There is a main data directory, which contains raw data files, data files worked up for analysis, and output of analyses used for visualization. So long as you have all files within the raw sub-directory you *should* be able to replicate all data carpentry, analysis and visualization. The scripts in this project will generate several intermediate data and analysis products, which will be stored within this main data directory. All paths are defined in `paths.r`.

# data construction: 
This project analyzes 6 independent data sets. `Averill_ISMEJ_2018/Scripts/1._data_construction/` contains all code necessary to work raw data products into analysis ready data products. Each of the 6 scripts is named to correspond with its respective data set.

# data analysis: 
All analysis code is broken out by data set, and stored within a folder named for the respective data set.

**Methods Validation Data Sets:** Each methods validation data set (BCI, NEON_HF, Kivlin_bacteria, Kivlin_fungi), has its own directory within `Averill_ISMEJ_2018/Scripts/2._analyses/` which contains code to 

1. test for space-time signatures in community similarity. (`<dataset>_analysis.r`)
2. estimate space and time parameter confidence intervals with orthogonal space-time observations. (`<dataset>_bootstrap.r`)
3. estimate space and time parameter confidence intervals without orthogonal space-time observations. (`<dataset>_bootstrap_sub100.r`)

**Analysis data sets:** Each analysis data set (Talbot_2014, Tedersoo_2014), has its own directory within `Averill_ISMEJ_2018/Scripts/2._analyses/` which contains code to:

1. Fit MRM models to test for space-time signatures in community similarity, in the presence or absence of envrionemntal covariates (`<dataset>_analysis.r`)
2. Estimate space and time parameter confidence intervals in the absence of environmental covariates (`<dataset>_bootstrap_st.r`)
3. Estimate space and time parameter confidence intervals in the presence of environmental covariates (`<dataset>_bootstrap_st.env.r`)
4. Tedersoo_2014 also contains `Tedersoo_2014_monte_carlo.r` for simulating the null expectation of temporal parameter effect sizes.

Some of these directories also contain `_batch.txt` files, which are used to submit parallel processing jobs to our computing cluster. These will need to be modified to work with your particular computing resources. Some of these scripts will complete in a reasonable amount of time during an interactive R session, while others take hours-days running in parallel on a computing cluster. Even if these scripts will work with your particular computing environment, paths are "hard-coded" within them and will need to be changed.

# figure scripts:
The directory `Averill_ISMEJ_2018/Scripts/3._figure_scripts/` contains code to replicate all figures and tables presented in the manuscript. Code is organized within subdirectories corresponding to main text figures, main text tables, and supplementary figures. Script names correspond with figure and table numbers in the published manuscript.

# transfer scripts:
`Averill_ISMEJ_2018/transfer_scripts` contains scripts used to sync data across multiple computers during this project. There are certainly more efficient ways to do this (and more complicated), but this way is straight forward. Transfers are "manually" triggered by executing scripts from the bash shell or similar. If you are syncing files across multiple computers you will definitely need to reconfigure these. If you want to auto-sync files across computers only when they change, I recommend looking into something like Rsync.

# functions:
This is a directory of custom functions used in this project for various analyses, and are called from other scripts.

# space_time_power_analysis.r tutorial:
In an effort to help others understand if they can separate spatial-temporal effects within their own dataset, we created the function `space_time_power_analysis()`. So long at spatial-temporal observations are not completely confounded, it is possible to do this, assuming the analysis has sufficient statistical power. Statistical power will depend on many things. These include how correlated temporal effects are with other predictors (multicollinearity), observation and process error, how much time is being analyzed, the temporal effect size, and sample size. The relative importance of all these factors will depend on the particular data set being analyzed.

The function `space_time_power_analysis.r` Takes a species matrix (i.e. OTU table), a mapping file of covariates, and a formula relating community similarity (specified as `y`) to environmental covariates. Syntax is the same as the `lm()` function in R. The function uses a bootstrap resampling technique (as described in the manuscript) to estimates parameter values and 95% parameter confidence intervals. In addition, the script performs a second bootstrap analysis where the community similarity matrix is randomized, and models are refit. This generates the "null" estimate of parameter uncertainty in the case of no effect. The script returns parameter estimates and 95% confidence intervals, null parameter estimate 95% confidence intervals, as well as the fraction of simulations a parameter was observed to be "significant" at p < 0.05 (or some other p threshold set by user) within all bootstrap simulations. It is important to realize that a parameter estimate can fall within the 95% confidence interval of the "null" parameter estimate, and still be found significant within the analysis. This is always true and should be expected 5% of the time even if the true parameter value is zero, if p<0.05 is the threshold of significance. However, if the parameter estimate is different than zero, then the fraction of simulations that detect a significant effect will increase as the actual parameter value moves towards the tails of the null parameter distribution. Once the parameter estimate is outside the 95% confidence interal of the null parameter distribution, it will be detected as significant (p<0.05) in >95% of simulations. This property is why these results can be useful in the context of power analysis.

As a researcher, you should consider a biologically meaningful temporal effect size before analyzing your data. If that effect size falls within the 95% confidence interval of the null parameter estimate, and your analysis find no significant temporal effect, you are in a tough spot. It may be that there is no temporal effect. However, the analysis also suggests that even if the biologically meaningful effect was there, you should not expect to detect it most of the time. If you did the same exact sampling/experiment many many times, you wouldn't expect to see the effect 95% of the time. In this case, the absence of evidence (parameter estimate p<0.05) is not evidence of absence (parameter estimate not different from 0).

**Function Documentation:**
```
#' space_time_power_analysis.r
#' This function performs a power analysis to determine the user's ability to detect spatial-temporal effects.
#' This function takes a mapping file of covariates, a normalized species matrix (otu table) and a formula as input.
#' This function assumes your species matrix is normalized, such that the columns sum to 1, though it might work without this, but has not been tested.
#' number of rows in mapping file should match number of columns in otu file. These should be ordered such that samples can be matched.
#' "space" is a special covariate name. If space is used in the supplied formula, x-y coordinates should be present in the mapping file, named 'x' and 'y'. 
#' User can choose to natural-log transform community similarity values, if desired.
#' If this is done, code automatically checks if there are zero values in similarity matrix. If there are, it adds the smallest, non-zero similarity value to all observations before log transforming.
#'
#' @param formula   Formula of model to be vetted for statistical power.
#' @param map       Mapping file of covariates.
#' @param otu       Normalized OTU table / species matrix such that columns sum to 1.
#' @param n.straps  Number of bootstrap simulations. Default 2000.
#' @param p.check   Level of statistical significance to consider. Default p=0.05.
#' @param warn      Set warn to FALSE if you want to suppress warnings. Default TRUE.
#' @param log       Natural-log transform similarity values? Default FALSE.
```

**Function Example:** Here is an example using the Kivlin et al. 2016 bacterial data post data construction, assuming the present R project is your working directory.
```{r}
rm(list=ls()) #clear environment.
source('paths.r')
source('Scripts/functions/space_time_power_analysis.r')
map <- readRDS(kiv_clean_fun_map.path)
otu <- readRDS(kiv_clean_fun_otu.path)
space_time_power_analysis(y ~ space + time.num, map, otu, n.straps = 1000, warn=F)

```


