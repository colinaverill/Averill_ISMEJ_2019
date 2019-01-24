#SUBSETTED BOOTSTRAP analysis of Kivlin 2016 fungi data Sep 24, 2018.
#Colin Averill
#clear R environment, load packages
rm(list=ls())
source('paths.r')
source('Scripts/functions/bootstrap.r')
source('Scripts/functions/subsample_map.r')
source('Scripts/functions/tic_toc.r')
library(doParallel)

#Tell R we are working with 2 cores on pecan2 or 28 on scc1
registerDoParallel(cores=2)

#specify output path
output.path <- kiv_fun_sub100_bootstrap.path

#load cleaned up map and otu files
otu <- readRDS(kiv_clean_fun_otu.path)
map <- readRDS(kiv_clean_fun_map.path)
rownames(map) <- map$X


#we now loop this in parallel. Set number of times to randomly subsample data. 
n.samples <- 1000

#empty datarframe for saving bootstrap output
boot.list <- list()

#run the loop
tic()
for(i in 1:n.samples){
  #subset map and otu files
  sub.map  <- subsample_map(map, map$block.plot)
  sub.otu  <- otu[,colnames(otu) %in% sub.map$X]
  
  #sort map and otu so that the orders match.
  sub.map <- sub.map[order(sub.map$X),]
  sub.otu <- sub.otu[, order(colnames(sub.otu))]
  rownames(sub.map) <- sub.map$X
  
  #perform bootstrap
  boot.out <- bootstrap(sub.map, 
                        sub.otu,
                        X = sub.map$x, 
                        Y = sub.map$y, 
                        time = sub.map$time.num, 
                        sub.map$X,
                        lat_lon = F,
                        min_bump = F,
                        n.straps = 1)
  
  #add output to list
  boot.list[[i]] <- boot.out
  
  #print status update
  cat(paste0(i,' of ',n.samples,' subsample iterations completed...\n'))
}
toc()

#convert output to data frame.
boot.frame <- do.call('rbind',boot.list)

#save output
saveRDS(boot.frame, output.path)