#check pairwise correlations.
rm(list=ls())
source('paths.r')
#function to drop NAs into r2 matrix when pval > 0.05
na.r2 <- function(r2,pval){
  for(i in 1:ncol(r2)){
    for(j in 1:nrow(r2)){
      if(pval[j,i] > 0.05){
        r2[j,i] <- NA
      }
    }
  }
  return(r2)
}

#load data----
ted <- readRDS(ted_clean_map.path)
ted <- ted[,c('MAT','MAP','MAT_CV','MAP_CV','NPP','C','C_N','pH','Moisture')]
tal <- readRDS(tal_clean_map.path)
tal <- tal[,c('MAT','MAP','MAT_CV','MAP_CV','NPP','Perc.C','CNRatio','pH','Perc.Soil.Moisture')]
tal <- tal[complete.cases(tal),]

pairs(ted)
ddat.cor <- picante::cor.table(ted)
ted.r2 <- ddat.cor$r
ted.pval <- ddat.cor$P
ted.r2 <- na.r2(ted.r2,ted.pval)
ted.r2^2

pairs(tal)
ddat.cor <- picante::cor.table(tal)
tal.r2 <- ddat.cor$r
tal.pval <- ddat.cor$P
tal.r2 <- na.r2(tal.r2, tal.pval)
tal.r2^2
