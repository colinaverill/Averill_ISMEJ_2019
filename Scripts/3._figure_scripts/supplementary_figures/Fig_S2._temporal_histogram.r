#Histograms of where observatinos are distributed in time.
#clear environment, load packages
rm(list = ls())
source('paths.r')
#library(data.table)

#set paths, load and format data.----
#output path
out.path <- fig_S2.path

#Load data.
lt <- readRDS(ted_clean_map.path)
jt <- readRDS(tal_clean_map.path)

#get as POSIX dates.
jt.date <- as.POSIXct.Date(as.Date(jt$Date.Sampled,format='%m/%d/%Y'))
jt.date <- as.POSIXlt(as.Date(jt.date, "%y-%m-%d")) 
jt.date$year <- jt.date$year + 2000
lt.date <- as.POSIXlt(lt$human.date, format = "%m/%d/%y")

#set x limits
limx <- as.Date(c("2010-04-20","2013-9-30"))
cex.lab <- 1.3

#setup png output settings.----
png(filename=out.path,width=7,height=5.5,units='in',res=300)

#begin plot----
par(mfrow = c(2,1), mai = c(.3,1,.5,1), oma = c(3,1,.1,.1))
hist(as.Date(lt.date), 'months', freq = T, xlim = limx, main = NA, xlab = NA, ylab = NA)
mtext('Global Temperate Soil Fungi', side = 3, cex = cex.lab)
hist(as.Date(jt.date), 'months', freq = T, xlim = limx, main = NA, xlab = NA, ylab = NA)
mtext('North American Soil Fungi', side = 3, cex = cex.lab)
#outer labels
mtext('date sampled', side = 1, cex = cex.lab, line = 3)
mtext('number of observations', side = 2, cex = cex.lab, outer = T, line = -2)

#end plot----
dev.off()