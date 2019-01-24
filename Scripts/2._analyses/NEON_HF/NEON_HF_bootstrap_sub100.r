#Subsetted bootstrap analysis of 2014 NEON Harvard Forest Fungi.
rm(list=ls())
source('paths.r')
source('Scripts/functions/tic_toc.r')
source('Scripts/functions/subsample_map.r')
source('Scripts/functions/bootstrap.r')
library(doParallel)
#Tell R we are working with 2 cores on pecan2 or 28 on scc1
registerDoParallel(cores=2)

#Load data, specify number of bootstrap samples, specify output path.----
n.samples <- 1000

#specify output path
output.path <- harv_neon_sub_boostrap.path

#load cleaned up map and otu files
otu <- readRDS(harv_neon_clean_otu.path)
map <- readRDS(harv_neon_clean_map.path)
rownames(map) <- map$deprecatedVialID

#Create a column such that timepoints are unique.
map$plot.time <- paste(map$plotID,map$doy,sep='_')


#Begin bootstrap looping.----
#we now loop this in parallel. Set number of times to randomly subsample data. 
boot.list <- list()

#run the loop. 
tic()
#for(i in 1:n.samples){
boot.list <-
  foreach(i = 1:n.samples) %dopar% {
  #subset map and otu files
  #subsample to get plot-time points you wanna keep. 
  to_keep <- subsample_map((map), map$plotID)$plot.time
  sub.map  <- map[map$plot.time %in% to_keep,]
  sub.otu  <- otu[,colnames(otu) %in% sub.map$deprecatedVialID]
  
  #sort map and otu so that the orders match.
  sub.map <- sub.map[order(sub.map$deprecatedVialID),]
  sub.otu <- sub.otu[, order(colnames(sub.otu))]
  
  #perform the regression analysis with bootstrap function, but only 1 iteration.
  #This is because the real iterations are driven by this outer loop.
  boot.out <- bootstrap(sub.map, 
                        sub.otu,
                        X = sub.map$decimalLongitude, 
                        Y = sub.map$decimalLatitude, 
                        time = sub.map$doy, 
                        sample_ID = sub.map$deprecatedVialID,
                        lat_lon = T,
                        min_bump = F,
                        n.straps = 1)
  
  #add output to list
  #boot.list[[i]] <- boot.out
  return(boot.out)
  
  #print status update.
    cat(paste0(i,' of ',n.samples,' subsample iterations completed...\n'))

} #end loop.
toc()

#convert output to data frame. 300 rows per iteration * 100 iterations = 30,000 rows.
boot.frame <- do.call('rbind',boot.list)

#save output.----
saveRDS(boot.frame, output.path)
