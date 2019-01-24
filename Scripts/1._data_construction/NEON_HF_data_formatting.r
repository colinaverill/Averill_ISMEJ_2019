#Taking sequences from NEON Harvard Forest 2014, may june july august.
rm(list=ls())
source('paths.r')
library(vegan)
library(geosphere)
library(ecodist)
library(spatstat)
library(doParallel)

#load data, specify output paths.----
#load mapping file.
map <- read.csv(harv_neon_raw_map.path)
map <- map[map$siteID == 'HARV',]
map <- map[grep('ITS',map$rawDataFileName),] #only ITS sequences.
loc <- readRDS(harv_neon_raw_loc.path) #phys properties with horizons, coordinates

#load SV.table.
sv.table <- readRDS(harv_neon_raw_otu.path)

#specift output paths.
map_path <- harv_neon_clean_map.path
otu_path <- harv_neon_clean_otu.path #calling otu for consistency, despite SV.

#subset SV table to only include harvard samples and vice versa.----
rownames(sv.table) <- substring(rownames(sv.table),4,nchar(rownames(sv.table)))
map <- map[map$deprecatedVialID %in% rownames(sv.table),]
sv.table <- sv.table[rownames(sv.table) %in% map$deprecatedVialID,]

#kill singletons and SV's w/ zero observation from harvard forest.
sv.table <- sv.table[,colSums(sv.table) > 1]

#merge map and loction data.----
#Soil horizons., sampling locations, sampling time-points.
loc <- loc[loc$geneticSampleID %in% map$geneticSampleID,]
loc <- loc[,c('geneticSampleID',"coreCoordinateX","coreCoordinateY",'decimalLatitude','decimalLongitude','horizon')]
map <- merge(map,loc, by = 'geneticSampleID')
map$doy <- strftime(map$collectDate, format = "%j")

#Dial in columns of interest.----
to_analyze <- map[,c('deprecatedVialID','plotID','collectDate','doy','decimalLatitude','decimalLongitude','horizon')]
sv.table <- sv.table[,colSums(sv.table) > 0]
sv.table <- sv.table[rowSums(sv.table) > 1000,]
#subset the mapping files to reflect SV filtering.
to_analyze <- to_analyze[to_analyze$deprecatedVialID %in% rownames(  sv.table),]

#downstream functions want SV tables with colnames as sample names.
sv.table <- t(sv.table)

#normalize otu table. ----
pro.function <- function(otu){
  for(i in 1:ncol(otu)){
    otu[,i] <- otu[,i] / sum(otu[,i])
  }
  return(otu)
}
sv.table <- pro.function(  sv.table)

#put map in the same order.
to_analyze <- to_analyze[order(to_analyze$deprecatedVialID),]

#make sure rownames of map and column names of otu are the sample IDs.
rownames(to_analyze) <- to_analyze$deprecatedVialID
check <- sum(colnames(sv.table) %in% rownames(to_analyze)) == length(colnames(sv.table))
if(check == F){
  stop('Stop. Rownames of mapping file do not match column names of OTU table.')
}

#make sure plotID in mapping file is a chracter, not factor vector.
to_analyze$plotID <- as.character(to_analyze$plotID)

#save----
saveRDS(to_analyze,map_path)
saveRDS(  sv.table,otu_path)
