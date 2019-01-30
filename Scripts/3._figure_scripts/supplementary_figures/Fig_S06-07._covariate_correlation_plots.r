rm(list=ls())
library(PerformanceAnalytics)
source('Scripts/functions/chart.Correlation.r')
source('paths.r')


#load data----
ted <- readRDS(ted_clean_map.path)
ted <- ted[,c('MAT','MAP','MAT_CV','MAP_CV','NPP','C','C_N','pH','Moisture')]
colnames(ted) <- c('MAT','MAP','MAT_CV','MAP_CV','NPP','%C','C:N','pH','Soil Moisture')
tal <- readRDS(tal_clean_map.path)
tal <- tal[,c('MAT','MAP','MAT_CV','MAP_CV','NPP','Perc.C','CNRatio','pH','Perc.Soil.Moisture')]
colnames(tal) <- c('MAT','MAP','MAT_CV','MAP_CV','NPP','%C','C:N','pH','Soil Moisture')
tal <- tal[complete.cases(tal),]


#Tedersoo plot.----
png(filename=fig_S06.path,width=8,height=8,units='in',res=300)
chart.Correlation(ted, histogram = F, pch = 19, cex = 0.5, rsq = T, cex.cor = 0.8, raw.p_val = T)
dev.off()

#Talbot plot.----
png(filename=fig_S07.path,width=8,height=8,units='in',res=300)
chart.Correlation(tal, histogram = F, pch = 19, cex = 0.5, rsq = T, cex.cor = 0.8, raw.p_val = T)
dev.off()
