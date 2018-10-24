#calculating spatial-temporal equivalency.
#can only get how many kilometers is an intra-annual year for JT. inter-annual N.S.
#only space significant LT.
rm(list=ls())
source('paths.r')

#load lt and jt models.
lt <- readRDS(ted_model_output.path)
jt <- readRDS(tal_model_output.path)

#get space-time only models.
jt.st <- jt$st.mrm
lt.st <- lt$st.mrm

#get space-time + environment models.
jt.st.env <- jt$st.env.mrm
lt.st.env <- lt$st.env.mrm

#get bootstrap
jt.st.boot <- readRDS(tal_bootstrap_st.only_output.path)
lt.st.boot <- readRDS(ted_bootstrap_st.only_output.path)

#if we used the bootstrap output we can put uncertainty on this.
#how much space is an intra-annual year (JT data set)
jt.st$coef[4,1] / jt.st$coef[2,1] #3,800km.
space_intra.jt_mu <- mean(jt.st.boot$seas_pos / jt.st.boot$space)
space_intra.jt_ci <-   sd(jt.st.boot$seas_pos / jt.st.boot$space) * 1.96

#how much time is 100km?
(jt.st$coef[2,1]*100 / jt.st$coef[4,1] ) * 265
intra_space.jt_mu <- mean((jt.st.boot$space *100 )/ jt.st.boot$seas_pos) * 265
intra_space.jt_ci <-   sd((jt.st.boot$space *100 )/ jt.st.boot$seas_pos) * 265 * 1.96

#with env covariates
jt.st.env.boot <- readRDS(tal_bootstrap_st.env_output.path)
mean(jt.st.env.boot$seas_pos / jt.st.env.boot$space)
  sd(jt.st.env.boot$seas_pos / jt.st.env.boot$space) * 1.96
  
  
#monte carlo
mc <-readRDS(ted_monte_carlo_results.path)
nrow(mc[mc$inter_pval < 0.05,])
nrow(mc[mc$intra_pval < 0.05,])  

#intra-annual turnover with and withut env. covariates Talbot 2014.
jt$st.mrm
exp(min(jt$st.dat$intra.norm))
exp(max(jt$st.dat$intra.norm))


k <- jt$st.dat

