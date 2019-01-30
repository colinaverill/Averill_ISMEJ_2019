#Figure S1. Distrubtion of spatial and temporal distances across data sets.
#clear R environment.
rm(list=ls())
source('paths.r')

#load data set output path.----
#set figure save destination.
figure_file.path <- 'test.png'
figure_file.path <- fig_S01.path

#grab fitted values that have space-time distances with them.
kiv <- readRDS(kiv_bac_figure_data.path)
bci <- readRDS(bci_fitted.path)
neo <- readRDS(harv_neon_fig_data.path)
kiv <- kiv$fitted
bci <- bci$fitted
neo <- neo$fitted


#save dimensions, destination.----
png(filename=figure_file.path,width=5.5,height=8,units='in',res=300)

#Figure code.----
#setup panels
par(mfrow = c(3,2),
    oma = c(1,1,1,0))
o.cex <- .9 #outer axis label size
m.cex <- 1.2 #header label size

#BCI space
hist(bci$space, main = NA, xlab = NA, ylab=NA)
mtext('Frequency', side = 2, line = 3, cex = o.cex)
mtext('space', side = 1, line = 3, cex = o.cex)
mtext('(kilometers)', side=1, line=4.5, cex=o.cex)

#BCI time
hist(bci$time, main = NA, xlab = NA, ylab = NA)
mtext('Frequency', side = 2, line = 3, cex = o.cex)
mtext('inter-annual time', side = 1, line = 3, cex = o.cex)
mtext('(days)', side =1, line=4.5, cex=o.cex)

#Kivlin space
hist(kiv$space, main = NA, xlab = NA, ylab=NA)
mtext('Frequency', side = 2, line = 3, cex = o.cex)
mtext('space', side = 1, line = 3, cex = o.cex)
mtext('(kilometers)', side=1, line=4.5, cex=o.cex)

#Kivlin time
hist(kiv$time, main = NA, xlab = NA, ylab = NA)
mtext('Frequency', side = 2, line = 3, cex = o.cex)
mtext('inter-annual time', side = 1, line = 3, cex = o.cex)
mtext('(days)', side =1, line=4.5, cex=o.cex)

#NEON-HF space
hist(neo$space, main = NA, xlab = NA, ylab=NA)
mtext('Frequency', side = 2, line = 3, cex = o.cex)
mtext('space', side = 1, line = 3, cex = o.cex)
mtext('(kilometers)', side=1, line=4.5, cex=o.cex)

#NEON-HF time
hist(neo$time, main = NA, xlab = NA, ylab = NA)
mtext('Frequency', side = 2, line = 3, cex = o.cex)
mtext('inter-annual time', side = 1, line = 3, cex = o.cex)
mtext('(days)', side =1, line=4.5, cex=o.cex)

#labels.
mtext('Barro-Colorado Island, Panama', side =3, cex = m.cex, outer = T, line = -2)
mtext('La Selva, Costa Rica', side =3, cex = m.cex, outer = T, line = -22)
mtext('Harvard Forest, U.S.A.',side =3, cex = m.cex, outer = T, line = -42)

#end plot.----
dev.off()