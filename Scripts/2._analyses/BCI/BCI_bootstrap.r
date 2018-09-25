#Bootstrap sampling BCI data
#Colin Averill, May 24, 2017
#clear R environment, load packages.
rm(list=ls())
source('paths.r')
source('Scripts/functions/bootstrap.r')
source('Scripts/functions/tic_toc.r')
source('Scripts/functions/subsample_map.r')

#Load data, specify output path.
output.path <- bci_all_boostrap.path

#setup multicore environment
registerDoParallel(cores=28)

#load cleaned up map and otu files
otu <- readRDS(bci_otu_clean.path)
map <- readRDS(bci_map_clean.path)

###TESTING ON PECAN2###
#registerDoParallel(cores=2)
#source('Scripts/space_time_functions.r')
#output.path <- '/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/out_bci_bootstrap.rds'
#otu <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/BCI_otu.rds')
#map <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/BCI_map.rds')
#otu <- otu[,1:10]
#map <- map[rownames(map) %in% colnames(otu),]
#map <- data.table(map)

#run jacknife script.
tic()
output <- bootstrap(map,otu,
                   X = map$PX.mean,
                   Y = map$PY.mean,
                   time = map$epoch_date.mean,
                   sample_ID = map$plot.time,
                   lat_lon = F, 
                   min_bump = T)
toc()

#save output.
saveRDS(output, output.path)