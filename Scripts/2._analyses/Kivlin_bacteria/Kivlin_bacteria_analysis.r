#analyzing Kivlin tropics data in space and time.
#load data, clear R environment
rm(list=ls())
source('paths.r')

#load data, set output paths.----
#output path
figure_dat_output.path <- kiv_bac_figure_data.path
 parameter_output.path <- kiv_bac_param.path

#data
map <- readRDS(kiv_clean_bac_map.path)
otu <- readRDS(kiv_clean_bac_otu.path)

#generate analysis matrices.----
#bray matrix.
bray.dis <- vegan::vegdist(t(otu))
bray.sim <- 1 - bray.dis

#space matrix.
space.m <- as.matrix(spatstat::pairdist(map$x, map$y))

#time matrix.
time.m  <- as.matrix(dist(map$time.num, method="euclidean", diag = FALSE, upper = FALSE))

#make a list, grab lower matrices, transform to dataframe.
data <- list(bray.sim,space.m,time.m)
lower  <- lapply(data, ecodist::lower)
dat    <- as.data.frame(lapply(lower, c))
colnames(dat) <- c('bray.sim','space','time')

#Analyze.----
m <- MRM(bray.sim ~ space + time, data = dat);m

#grab total rsq and pvalues
summary <- list()
summary[[1]] <- m$r.squared[1]
summary[[2]] <- m$r.squared[2]
names(summary) <- c('rsq','pval')

#grab model parameter values.
parms <- as.data.frame(m$coef)

#Get fitted and normalized values.----
#calculate fitted values
dat$fitted = (     parms[ 1,1]
                 + parms[ 2,1] * dat$space
                 + parms[ 3,1] * dat$time
)

#calculate space normalized values
dat$space.norm =    (dat$bray.sim) - (parms[1,1] + parms[2,1] * dat$space + parms[3,1]     * dat$time) + 
  (parms[1,1] + parms[2,1] * dat$space + parms[3,1] * mean(dat$time))


#calculate time normalized values
dat$time.norm = (dat$bray.sim) - (parms[1,1] + parms[2,1] *      dat$space + parms[3,1] * dat$time) +
  (parms[1,1] + parms[2,1] * mean(dat$space)+ parms[3,1] * dat$time)


#add to fitted values total model R2 and p-value.
d <- list(dat,summary)
names(d) <- c('fitted','summary')


#How much space is how much time in full data set?
m.space <- MRM(dat$space.norm~dat$space); s.pars <- m.space$coef
m.time  <- MRM( dat$time.norm~dat$time ); t.pars <- m.time $coef

#how many minutes is 1km? - have to use intercept here, as its significant
(((s.pars[2] + s.pars[1] - t.pars[1])/t.pars[2])*24*60) / (60*24)
#(((s.pars[2])/t.pars[2])*24*60) / (60*24)
#1km is 171 days.

#how many km is 1 year? - have to use intercept here, as its significant
(t.pars[2]*365 + t.pars[1] - s.pars[1])/s.pars[2]
#(t.pars[2]*365)/s.pars[2]
#one year is 1,461 kilometers.


#save model output and data for plotting.----
saveRDS(d,figure_dat_output.path)
#saveRDS(sub.d,'/fs/data3/caverill/Microbial_Space_Time_data/kivlin_tropics.data/figure_data/sub_data_figure.rds')

#save parameters
saveRDS(parms    , parameter_output.path)
#saveRDS(parms.sub, '/fs/data3/caverill/Microbial_Space_Time_data/kivlin_tropics.data/params_sub.rds')
