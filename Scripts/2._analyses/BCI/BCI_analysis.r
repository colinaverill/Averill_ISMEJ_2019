#analyzing BCI data in space and time.
#load data, clear R environment
rm(list=ls())
library(ecodist)

#load data, set output paths.----
#load data.
data <- readRDS(bci_all_matrix_list.path)
lower  <- lapply(data, lower)
d      <- as.data.frame(lapply(lower, c))
sub.data <- readRDS(bci_sub_matrix_list.path)
sub.lower  <- lapply(sub.data, lower)
sub.d      <- as.data.frame(lapply(sub.lower, c))

#setup output paths.
fitted_output.path <- bci_fitted.path
   MRM_output.path <- bci_MRM_output.path
   MRM_params.path <- bci_MRM_params.path

#free up some RAM.----
rm(list = c('data', 'lower'))
gc()

#Effects of both space and time. ----
m <- MRM((bray.sim) ~ (space) + (time), data = d); m
parms <- as.data.frame(m$coef); parms
#use lm to get standard errors of parameter estimates. 
lm.m <- lm((bray.sim) ~ (space) + (time), data = d)
#grab standard error, multiply by sqrt N
parms$sd <- summary(lm.m)$coefficients[,2] * sqrt(nrow(d))
#grab total rsq and pvalues
summary <- list()
summary[[1]] <- m$r.squared[1]
summary[[2]] <- m$r.squared[2]
names(summary) <- c('rsq','pval')

###Repeat analysis of data subset. ----
sub.m <- MRM((bray.sim) ~ (space) + (time), data = sub.d);sub.m
sub.parms <- as.data.frame(sub.m$coef); sub.parms
#use lm to get standard errors of parameter estimates. 
lm.sub.m <- lm((bray.sim) ~ (space) + (time), data = sub.d)
#grab standard error, multiply by sqrt N
sub.parms$sd <- summary(lm.m)$coefficients[,2] * sqrt(nrow(sub.d))



#calculate fitted values----
d$fitted = (     parms[ 1,1]
                 + parms[ 2,1] * (d$space)
                 + parms[ 3,1] * (d$time )
)
sub.d$fitted = (     sub.parms[ 1,1]
                 + sub.parms[ 2,1] * (sub.d$space)
                 + sub.parms[ 3,1] * (sub.d$time )
)


#calculate space normalized values
d$space.norm = (d$bray.sim) - (parms[1,1] + parms[2,1] * (d$space) + parms[3,1] *     (d$time)) +
                             (parms[1,1] + parms[2,1] * (d$space) + parms[3,1] * mean(d$time))

#sub.d$space.norm = (sub.d$bray.sim) - (sub.parms[1,1]
#                                  #+ sub.parms[ 2,1] * (sub.d$space)
#                                  + sub.parms[ 3,1] * (sub.d$time )
#)


#calculate time normalized values
d$time.norm = (d$bray.sim) - (parms[1,1] + parms[2,1] *     (d$space) + parms[3,1] * (d$time)) +
                             (parms[1,1] + parms[2,1] * mean(d$space) + parms[3,1] * (d$time))
#sub.d$time.norm = (sub.d$bray.sim) - (sub.parms[1,1]
#                                          + sub.parms[ 2,1] * (sub.d$space)
                                          #+ sub.parms[ 3,1] * (sub.d$time )
#)

#add to fitted values total model R2 and p-value.
d <- list(d,summary)
names(d) <- c('fitted','summary')

#save model output and data for plotting.----
saveRDS(d        , fitted_output.path)
saveRDS(m        , MRM_output.path)
saveRDS(parms    , MRM_params.path)
#saveRDS(sub.d    , '/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/figure_data/sub_BCI_data_fitted.rds')
#saveRDS(sub.m    , '/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/figure_data/sub_BCI_MRM_output.rds')
#saveRDS(sub.parms, '/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/figure_data/sub_BCI_MRM_parms.rds')

#How much space is how much time in full data set?----
m.space <- MRM(d$space.norm~d$space); s.pars <- m.space$coef
m.time  <- MRM( d$time.norm~d$time ); t.pars <- m.time $coef

#how many minutes is 1km?
#(((s.pars[2] + s.pars[1] - t.pars[1])/t.pars[2])*24*60) / (60*24)
(((s.pars[2])/t.pars[2])*24*60) / (60*24)
#1km is 109 days.

#how many km is 1 year?
(t.pars[2]*365)/s.pars[2]
#one year is 3.4km kilometers.