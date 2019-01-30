#generate simulation figure
rm(list=ls())
source('paths.r')
library(data.table)

#load data, set output path, do some manipulation.----
output_path <- fig_S10.path
#data
d <- readRDS(ted_monte_carlo_results.path)
d <- data.table(d)

sig.time <- d[inter_pval < 0.05,]
pos.time <- sig.time[inter_effect > 0,]
neg.time <- sig.time[inter_effect < 0,]
sig.doy  <- d[intra_pval < 0.05,]
pos.doy  <- sig.doy[intra_effect > 0,]
neg.doy  <- sig.doy[intra_effect < 0,]

#setup save path.----
png(filename=output_path,width=8,height=3.5,units='in',res=300)

#begin plot.----
par(mar=c(4,4,2,1),
    mfrow = c(1,2))
limy <- c(0,250)
limx_inter <- c(min(d$inter_effect), max(d$inter_effect))
limx_intra <- c(min(d$intra_effect), max(d$intra_effect))

#time effects
hist(d$inter_effect, ylim=limy, xlim=limx_inter, main = NA, ylab=NA, xlab = NA)
par(new=T)
hist(neg.time$inter_effect, ylim=limy, xlim=limx_inter, col='purple', main=NA,ylab=NA, xlab=NA, axes=F)
par(new=T)
hist(pos.time$inter_effect, ylim=limy, xlim=limx_inter, col='green' , main=NA,ylab=NA, xlab=NA, axes=F)
mtext('inter-annual time effect', side = 3, line = 0)
mtext('a.', side = 3, line = -2, adj = 0.05)

#legend
legend(2e-05, 200, 
       c('+ effect','-  effect'),
       fill=c('green','purple'),
       bty='n',
       x.intersp=.5)

#doy effects
hist(d$intra_effect, ylim=limy,  xlim=limx_intra,main = NA, ylab=NA, xlab = NA)
par(new=T)
hist(neg.doy$intra_effect, ylim=limy,  xlim=limx_intra, col='purple', main=NA,ylab=NA, xlab=NA, axes=F)
par(new=T)
hist(pos.doy$intra_effect, ylim=limy,  xlim=limx_intra, col='green', main=NA,ylab=NA, xlab=NA, axes=F)
mtext('intra-annual time effect', side = 3, line = 0)
mtext('b.', side = 3, line = -2, adj = 0.05)

#out labels
mtext('effect size', side = 1, cex = 1, line = -1.2, outer = T)
mtext('frequency'  , side = 2, cex = 1, line = -1.2, outer = T)

#end plot.----
dev.off()
