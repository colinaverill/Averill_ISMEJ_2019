#Jacknife analysis of Kivlin 2016 bacteria data. Colin Averill, September 6, 2017.
#clear R environment, source packages and custom functions.
rm(list=ls())
library(doParallel)
source('paths.r')
source('Scripts/functions/bootstrap.r')
source('Scripts/functions/tic_toc.r')

#Tell R we are working with 2 cores on pecan2 or 28 on scc1
registerDoParallel(cores=2)
#registerDoParallel(cores=28)

#specify output path
output.path <- kiv_bac_bootstrap.path

#load cleaned up map and otu files
otu <- readRDS(kiv_clean_bac_otu.path)
map <- readRDS(kiv_clean_bac_map.path)
rownames(map) <- map$X


#first, subset otu and map file to 10 samples for testing.
#otu <- otu[,1:10]
#map <- map[X %in% colnames(otu),]

tic()
output <- bootstrap(map,
                   otu,
                   X = map$x,
                   Y = map$y,
                   time = map$time.num,
                   sample_ID = map$X,
                   lat_lon = F,
                   min_bump = F,
                   n.straps = 1000)
toc()

#save output.
saveRDS(output, output.path)