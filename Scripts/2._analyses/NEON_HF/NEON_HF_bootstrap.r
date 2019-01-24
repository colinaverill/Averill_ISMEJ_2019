#Analyzing 2014 NEON Harvard Forest Fungi.
#1. Get full analysis with bootstrap confidence.
#2. in a second script, do the subsetted permutation analysis.
rm(list=ls())
source('paths.r')
source('Scripts/functions/tic_toc.r')
source('Scripts/functions/subsample_map.r')
source('Scripts/functions/bootstrap.r')
library(doParallel)

#register parallel environment.----
#Tell R we are working with 2 cores on pecan2 or 28 on scc1
registerDoParallel(cores=2)
#registerDoParallel(cores=28)

#load data, set output paths.----
#specify output path
output.path <- harv_neon_all_boostrap.path

#load cleaned up map and otu files
otu <- readRDS(harv_neon_clean_otu.path)
map <- readRDS(harv_neon_clean_map.path)
rownames(map) <- map$deprecatedVialID

#TESTING: subset otu and map file to 10 samples.
#otu <- otu[,1:10]
#map <- map[rownames(map) %in% colnames(otu),]

#put things in the same order.
otu <- otu[,order(colnames(otu))]
map <- map[order(rownames(map)),]

#run bootstrap analysis.----
tic()
output <- bootstrap(map,
                    otu,
                    X = map$decimalLongitude,
                    Y = map$decimalLatitude,
                    time = map$doy,
                    sample_ID = map$deprecatedVialID,
                    lat_lon = T,
                    min_bump = F,
                    n.straps = 1000)
toc()

#save output.----
saveRDS(output,output.path)
