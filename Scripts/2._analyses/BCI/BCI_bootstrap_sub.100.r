#BCI sub-boostrap script. subsampling 1000 times, such that no location has been sampled multiple times, then implementing bootstrap function.
#clear environment, load packages.
rm(list=ls())
source('paths.r')
source('Scripts/functions/bootstrap.r')
source('Scripts/functions/tic_toc.r')
source('Scripts/functions/subsample_map.r')
library(doParallel)

#register parallel environment.----
#Tell R we are working with 28 cores on scc1
registerDoParallel(cores=28)

#Load data, choose number of iterations, specify output.----
#Subsample our mapping file n.samples times, then perform 100 bootstraps per subset.
n.samples <- 1000

output.path <- bci_sub_bootstrap.path

#load local map and otu files
otu <- readRDS(bci_otu_clean.path)
map <- readRDS(bci_map_clean.path)

###FOR TESTING ON PECAN2###
#source('Scripts/space_time_functions.r')
#registerDoParallel(cores=2)
#output.path <- '/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/out_bci_bootstrap_sub.100.rds'
#otu <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/BCI_otu.rds')
#map <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/BCI_map.rds')
#first, subset otu and map file to 10 samples for testing.
#otu <- otu[,1:1000]
#map <- map[rownames(map) %in% colnames(otu),]

#run the loop.----
#empty datarframe for saving bootstrap output
boot.list <- list()
tic()
#foreach(i = 1:n.samples) %dopar% {
for(i in 1:n.samples){
  #subset map and otu files
  sub.map  <- subsample.map(map, map$plot.code)
  sub.otu  <- otu[,colnames(otu) %in% sub.map$plot.time]
  
  #sort map and otu so that the orders match.
  sub.map <- sub.map[order(sub.map$plot.time),]
  sub.otu <- sub.otu[, order(colnames(sub.otu))]
  
  #perform bootstrap.
  boot.out <- bootstrap(map = sub.map, 
                        otu = sub.otu,
                        X = sub.map$PX.mean, 
                        Y = sub.map$PY.mean, 
                        time = sub.map$epoch_date.mean, 
                        ID = sub.map$plot.time, 
                        lat_lon = F,
                        min_bump = T,
                        n.straps = 1)
  
  #add output to list
  boot.list[[i]] <- boot.out
  
  #print status update
  cat(paste0(i,' of ',n.samples,' subsample iterations completed...\n'))
}
toc()

#convert output to data frame. 300 rows per iteration * 100 iterations = 30,000 rows.
boot.frame <- do.call('rbind',boot.list)

#save output.----
saveRDS(boot.frame, output.path)