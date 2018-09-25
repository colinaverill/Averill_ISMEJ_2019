#analyzing Kivlin tropics data in space and time.
#load data, clear R environment
rm(list=ls())
source('paths.r')
#library(ecodist)
#library(vegan)

#load data, specify output paths.----
#load data.
map <- readRDS(harv_neon_clean_map.path)
otu <- readRDS(harv_neon_clean_otu.path)
#output paths.
fig_data_output.path <- harv_neon_fig_data.path
         params.path <- harv_neon_params.path

#Generate analysis matrices.----
time <- map$doy
X <- map$decimalLongitude
Y <- map$decimalLatitude
bray.dis <- vegan::vegdist(t(otu), method='bray')
bray.sim <- 1 - as.matrix(bray.dis)
points <- as.matrix(data.frame(X,Y))
space.m <- geosphere::distm(points)
time.m  <- as.matrix(dist(time, method="euclidean", diag = FALSE, upper = FALSE))
d <- list(bray.sim,space.m,time.m)
names(d) <- c('bray.sim','space','time')
lower  <- lapply(d, ecodist::lower)
d      <- as.data.frame(lapply(lower, c))

#run analysis.----
m <- ecodist::MRM(bray.sim ~ space + time, data = d);m

#grab total rsq and pvalues.----
summary <- list()
summary[[1]] <- m$r.squared[1]
summary[[2]] <- m$r.squared[2]
names(summary) <- c('rsq','pval')


#grab parameter values.
parms <- as.data.frame(m$coef)

#calculate fitted values.----
d$fitted = (     parms[ 1,1]
                 + parms[ 2,1] * d$space
                 + parms[ 3,1] * d$time
)

#calculate space normalized values
d$space.norm =    (d$bray.sim) - (parms[1,1] + parms[2,1] * d$space + parms[3,1]     * d$time) + 
  (parms[1,1] + parms[2,1] * d$space + parms[3,1] * mean(d$time))


#calculate time normalized values
d$time.norm = (d$bray.sim) - (parms[1,1] + parms[2,1] *      d$space + parms[3,1] * d$time) +
  (parms[1,1] + parms[2,1] * mean(d$space)+ parms[3,1] * d$time)


#add to fitted values total model R2 and p-value.
d <- list(d,summary)
names(d) <- c('fitted','summary')


#How much space is how much time in full data set?
#m.space <- ecodist::MRM(d$space.norm~d$space); s.pars <- m.space$coef
#m.time  <- ecodist::MRM( d$time.norm~d$time ); t.pars <- m.time $coef


#save model output and data for plotting.----
saveRDS(d, fig_data_output.path)

#save parameters
saveRDS(parms, params.path)
