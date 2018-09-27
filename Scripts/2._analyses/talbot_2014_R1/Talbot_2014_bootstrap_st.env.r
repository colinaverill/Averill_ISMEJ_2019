#BOOTSTRAP analysis of Tedersoo 2014. Colin Averill Sep 21, 2017.
#clear R environment, load packages
rm(list=ls())
source('paths.r')
source('Scripts/functions/tic_toc.r')
library(doParallel)

#number of bootstrap simulations.
n.straps <- 10000

#register parallel environment.
registerDoParallel(cores=28)

#set output path
output.path <- tal_bootstrap_st.env_output.path

#load cleaned up map and otu files.----
otu <- readRDS(tal_clean_otu.path)
map <- readRDS(tal_clean_map.path)
#grab model data for relevant covariates.
mod <- readRDS(tal_model_output.path)
mod <- mod[[8]]
covs <- rownames(mod$coef)
covs <- covs[!(covs %in% c('Int','space'))]#drop intercept and space. 
  
#quick complete cases.----
old.cov.names <- c('epoch.date','doy','seas_pos','Perc.Soil.Moisture','CNRatio','pH','Perc.C','NPP','MAP','MAT','MAT_CV','MAP_CV')
new.cov.names <- c('epoch.date','doy','seas_pos','Moisture','C_N','pH','C','NPP','MAP','MAT','MAT_CV','MAP_CV')
for(i in 1:length(old.cov.names)){colnames(map)[which(names(map) == old.cov.names[i])] <- new.cov.names[i]}

#get complete cases.
to_keep <- c(new.cov.names,'latitude.dd','longitude.dd','Mapping.ID','Sample.ID')
map <- map[,colnames(map) %in% to_keep]
map <- map[complete.cases(map),]
otu <- otu[,colnames(otu) %in% map$Mapping.ID]
map <- map[order(map$Mapping.ID),]
otu <- otu[,order(colnames(otu))]

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
    points <- map.j[,c('longitude.dd','latitude.dd')]
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
    m <- lm(mod.formula, data=dat)
    
    #return output 
    to_return <- coef(m)
    return(to_return)
    
    #print status update
    cat(paste0(i,' of ',n.straps,' iterations completed...\n'))
  } #end loop. 
toc()

#collapse output list to data.frame
output <- data.frame(do.call('rbind',out.boot))
colnames(output) <- c('intercept','space','epoch.date','seas_pos')

#save output.
saveRDS(output, output.path)

#end script.