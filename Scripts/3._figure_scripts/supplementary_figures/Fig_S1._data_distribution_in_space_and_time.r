#Figure S1. Distrubtion of spatial and temporal distances across data sets.
#clear R environment.
rm(list=ls())
source('paths.r')

#load data set output path.----
#set figure save destination.
figure_file.path <- fig_S1.path

#grab Leho Tedersoo and Jenny Talbot fitted vs. observed values based on MRM model.
d.lt <- readRDS(ted_model_output.path)
d.jt <- readRDS(tal_model_output.path)
d.jt <- d.jt$st.dat
d.lt <- d.lt$st.dat


#save dimensions, destination.----
png(filename=figure_file.path,width=8,height=5.5,units='in',res=300)

#Figure code.----
#setup panels
par(mfrow = c(2,3),
    oma = c(1,1,1,0))

#JT intra
hist(d.jt$seas_pos, xlim = c(-0.1,1.1), ylim = c(0,30000), main = NA, xlab = NA, ylab=NA)
mtext('Frequency', side = 2, line = 3, cex = 1.2)
mtext('intra-annual time (seasonal distance)', side = 1, line = 3, cex = 1.2)

#JT inter
hist(d.jt$epoch.date, main = NA, xlab = NA, ylab = NA, ylim = c(0,60000), xlim = c(0,800))
mtext('Frequency', side = 2, line = 3, cex = 1.2)
mtext('North American Fungi', side =3, line = 1.5, cex = 1.5)
mtext('inter-annual time (days)', side = 1, line = 3, cex = 1.2)

#JT space
hist(d.jt$space, main = NA, xlab = NA, ylab = NA, xlim = c(0,6500), ylim = c(0,40000))
mtext('space (kilometers)', side = 1, line = 3, cex = 1.2)
mtext('Frequency', side = 2, line = 3, cex = 1.2)

#LT intra
hist(d.lt$seas_pos, xlim = c(-0.1,1.1), ylim = c(0,900), main = NA, xlab = NA, ylab = NA)
mtext('Frequency', side = 2, line = 3, cex = 1.2)
mtext('intra-annual time (seasonal distance)', side = 1, line = 3, cex = 1.2)

#LT inter
hist(d.lt$epoch.date, ylim = c(0,2000), xlim = c(0,1300), main = NA, xlab = NA, ylab = NA)
mtext('Frequency', side = 2, line = 3, cex = 1.2)
mtext('Global Northern Temperate Fungi', side =3, line = 1.5, cex = 1.5)
mtext('inter-annual time (days)', side = 1, line = 3, cex = 1.2)

#LT space
hist(d.lt$space, xlim = c(0,14000), ylim = c(0,1300), main = NA, xlab = NA, ylab = NA)
mtext('Frequency', side = 2, line = 3, cex = 1.2)
mtext('space (kilometers)', side = 1, line = 3, cex = 1.2)

#end plot.----
dev.off()