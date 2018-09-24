#BOOTSTRAP analysis of Tedersoo 2014. Colin Averill Sep 24, 2018.
#clear R environment, load packages
rm(list=ls())
source('paths.r')
source('Scripts/functions/tic_toc.r')
library(doParallel)

#number of bootstrap simulations.
n.straps <- 1000

#register parallel environment.
registerDoParallel(cores=28)

#set output path
output.path <- ted_bootstrap_output.path

#load cleaned up map and otu files.----
otu <- readRDS(ted_clean_otu.path)
map <- readRDS(ted_clean_map.path)
#grab model data for relevant covariates.
mod <- readRDS(ted_model_output.path)
mod <- mod$st.env.mrm
covs <- rownames(mod$coef)
covs <- covs[!(covs %in% c('Int','space'))]#drop intercept and space. 


#run bootstrap loop.----
tic()
out.boot <- 
  foreach(i = 1:n.straps) %dopar% {
    #sample columns of the OTU table with replacement
    myOrder <- sample(ncol(otu), replace = T)
    otu.j <- otu[,myOrder]
    map.j <- map[myOrder,]
    
    #calculate bray-curtis similarity. transpose of otu table necessary to get distances.
    bray.dis <- vegan::vegdist(t(otu.j), method='bray')
    bray.sim <- 1 - as.matrix(bray.dis)
    #account for zeros in bray.sim by adding the smallest similarity value in the dataset to every observation
    bray.sim <- bray.sim + min(bray.sim[bray.sim > 0])
    
    #calulate spatial distance, accounting for curvature of Earth.
    points <- map.j[,c('longitude','latitude')]
    space.m <- geosphere::distm(points)
    
    #remaining covariates.
    cov <- map.j[,colnames(map.j) %in% covs]
    cov.matrices <- list()
    for(i in 1:ncol(cov)){
      cov.matrices[[i]] <- as.matrix(dist(cov[,i], method="euclidean", diag=F, upper=F))
    }
    names(cov.matrices) <- colnames(cov)
    cov.matrices$space <- space.m
    cov.matrices$bray.sim <- bray.sim
    
    #analyze data subset i. 
    lower  <- lapply(cov.matrices, ecodist::lower)
    dat      <- as.data.frame(lapply(lower, c))
    
    #convert distance in m to distance in km so regression doesn't choke.
    dat$space <- dat$space / 1000
    
    #run model - lm is fine. We are testing significance with distribution of effect sizes via boostrap, not permutation via MRM.
    preds <- paste(c('space',covs), collapse = '+')
    mod.formula <- paste0('log(bray.sim) ~ ',preds)
    m <- ecodist::MRM(mod.formula, data=dat)
    
    #return output 
    return(c(m$coef[,1]))
    
    #print status update
    cat(paste0(i,' of ',n.straps,' iterations completed...\n'))
  } #end loop. 
toc()

#collapse output list to data.frame
output <- data.frame(do.call('rbind',out.boot))

#save output.
saveRDS(output, output.path)

#end script.