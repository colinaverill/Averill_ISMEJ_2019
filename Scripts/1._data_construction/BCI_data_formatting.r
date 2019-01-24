#building BCI 'OTU' table and mapping files. 
#clear R environment, load packages.
rm(list=ls())
source('paths.r')
require(data.table)

#load data, set output paths.----
bci_map_output.path <- bci_map_clean.path
bci_otu_output.path <- bci_otu_clean.path
bci_sub_map_output.path <- bci_sub_map.path
bci_sub_otu_output.path <- bci_sub_otu.path
bci_all_matrix_output.path <- bci_all_matrix_list.path
bci_sub_matrix_output.path <- bci_sub_matrix_list.path

#load all data.
bci.1982 <- read.table(bci_1982.path, header = T, na.strings=c("", "NA"), check.names = FALSE, sep = "\t")
bci.1985 <- read.table(bci_1985.path, header = T, na.strings=c("", "NA"), check.names = FALSE, sep = "\t")
bci.1990 <- read.table(bci_1990.path, header = T, na.strings=c("", "NA"), check.names = FALSE, sep = "\t")
bci.1995 <- read.table(bci_1995.path, header = T, na.strings=c("", "NA"), check.names = FALSE, sep = "\t")
bci.2000 <- read.table(bci_2000.path, header = T, na.strings=c("", "NA"), check.names = FALSE, sep = "\t")
bci.2005 <- read.table(bci_2005.path, header = T, na.strings=c("", "NA"), check.names = FALSE, sep = "\t")
bci.2010 <- read.table(bci_2010.path, header = T, na.strings=c("", "NA"), check.names = FALSE, sep = "\t")
bci.2015 <- read.table(bci_2015.path, header = T, na.strings=c("", "NA"), check.names = FALSE, sep = "\t")
all <- rbind(bci.1982, bci.1985, bci.1990, bci.1995, bci.2000, bci.2005, bci.2010, bci.2015)

d.list <- list(bci.1982, bci.1985, bci.1990, bci.1995, bci.2000, bci.2005, bci.2010, bci.2015)
names(d.list) <- c('bci.1982','bci.1985','bci.1990','bci.1995','bci.2000','bci.2005','bci.2010','bci.2015')

#1. - break each year of data into 300 sub plots- 2400 time*space observations.----
#Just add a vector to each data set that is a plot number. 
min(bci.2015$PX);max(bci.2015$PX) #1000m in x direction
min(bci.2015$PY);max(bci.2015$PY) # 500m in y direction

#first remove all NAs in X and y coords
na.function <- function(data){
  completeVec <- complete.cases(data[,c('PX','PY')])
  return(data[completeVec, ])
}

d.list <- lapply(d.list, na.function)

#write a function to get the plot code as 300 grids. 
grid.function <- function(data){
  x.cuts <- 25
  y.cuts <- 12
  data$plot.xy <- interaction(cut(data$PX, x.cuts), cut(data$PY, y.cuts), sep="_")
  plot.key <- data.frame(unique(data$plot.xy), c(1:300))
  colnames(plot.key) <- c('plot.xy','plot.code')
  out <- merge(data,plot.key, by='plot.xy', all.x=T)
  return(out)
}

#lapply function to list of data frames
d.list <- lapply(d.list, grid.function)


#Then get a mean time point for each year, add that in. Sample name is plot(1-50).time.----
time.function <- function(data){
  date.lookup <- format(seq(as.Date("1980-01-01"), as.Date("2016-12-31"), by = "1 day"))
  data$epoch_date <- match(data$Date, date.lookup)
  #get mean sample time per grid cell
  time_key <- aggregate(epoch_date ~ plot.code, data = data, FUN=mean)
  #get a merge time by plot code. 
  time_key$plot.time <- paste(time_key$plot.code,time_key$epoch_date,sep='.')
  colnames(time_key) <- c('plot.code','epoch_date.mean','plot.time')
  out <- merge(data,time_key, by='plot.code',all.x=T)
  return(out)  
}

#lapply function to list of data frames.
d.list <- lapply(d.list,time.function)

#get a mean x and y value for each observation for pairwise distance calculation.----
space.function <- function(data){
  space_key    <- aggregate(PX ~ plot.code, data = data, FUN = mean)
  colnames(space_key) <- c('plot.code','PX.mean')
  space_key$PY.mean <- aggregate(PY ~ plot.code, data = data, FUN = mean)[,2]
  out <- merge(data,space_key, by='plot.code',all.x=T)
  return(out)
}
d.list <- lapply(d.list, space.function)

#Then merge all of these data frames together, check the number of unique sample IDs
all <- do.call(rbind, d.list)
length(unique(all$plot.time)) #for 50 grids over 8 years you should have 2400 unique plot.times. 


#2. - generate an OTU table - it will have 2400 samples (300 locations x 8 time points)----
sub <- data.frame()
sub <- all[,c('Latin','plot.time')]
otu <- table(sub$Latin, sub$plot.time)

#3. - generate a mapping file that has plot centroid coordinate and mean sample date.----
all <- data.table(all)
setkey(all,'plot.time')
map <- all[!duplicated(plot.time),]

#grab relevant map columns
map <- map[,.(plot.time,plot.code,PX.mean,PY.mean,epoch_date.mean)]
map <- as.data.frame(map)

#make sure map and otu table are in the same damn order
head(colnames(otu))
head(map$plot.time)
#order OTU table and the map so that they match. 
map <- map[order(map$plot.time),]
otu <- otu[, order(colnames(otu))]

#make the labels the map rownames
rownames(map) <- map$plot.time

#4. - generate a subsetted map and OTU table confounded in the same way as your data- it will have 300 samples.----
#It will be each of the 2500 sub plots, sample at some random point in time.
#its randomly sampling 1 time point from each of the 300 plots.
map <- as.data.frame(map)
sub.map <- lapply(split(map, map$plot.code),
                  function(subdf) subdf[sample(1:nrow(subdf), 1),]
)
sub.map <- data.frame(do.call('rbind', sub.map))

#get a subsetted otu table
sub.otu <- otu[,colnames(otu) %in% sub.map$plot.time]

#order sub.otu table and the sub.map so that they match. 
sub.map <- sub.map[order(sub.map$plot.time),]
sub.otu <- sub.otu[, order(colnames(sub.otu))]
#double check!
head(colnames(sub.otu))
head(sub.map$plot.time)

#make plot.time codes the sub.map rownames
rownames(sub.map) <- sub.map$plot.time

#save map and otu tables for bootstrap experiment.----
saveRDS(otu, bci_map_output.path)
saveRDS(map, bci_otu_output.path)
saveRDS(sub.otu, bci_sub_map.path)
saveRDS(sub.map, bci_sub_otu.path)


#5. - generate pairwise distance matrices, save as list of matrices.----
#normalize otu columns
#This function converts each OTU seq abundance to a fractional percentage of the total number of reads within a sample. 
pro.function <- function(otu){
  for(i in 1:ncol(otu)){
    otu[,i] <- otu[,i] / sum(otu[,i])
  }
  return(otu)
}

#normalize and double check all column sums are 1
otu_pcorrected <- pro.function(otu)
colSums(otu_pcorrected)
sub.otu_pcorrected <- pro.function(sub.otu)
colSums(sub.otu_pcorrected)


#get similarity matrix----
require(vegan)
bray.dis <- vegdist(t(otu_pcorrected), method='bray')
bray.dis <- as.matrix(bray.dis)
#convert to similarity matrix
bray.sim <- 1 - bray.dis
#repeat for subset
sub.bray.dis <- vegdist(t(sub.otu), method='bray')
sub.bray.dis <- as.matrix(sub.bray.dis)
#convert to similarity matrix
sub.bray.sim <- 1 - sub.bray.dis

#get space and time distance matrices----
require(spatstat)
time.m <- as.matrix(dist(map$epoch_date.mean, method="euclidean", diag = FALSE, upper = FALSE))
space.m <- pairdist(map$PX.mean,map$PY.mean)
space.m <- as.matrix(space.m)
#repeat for subset
sub.time.m  <- as.matrix(dist(sub.map$epoch_date.mean, method="euclidean", diag = FALSE, upper = FALSE))
sub.space.m <- pairdist(sub.map$PX.mean,sub.map$PY.mean)
sub.space.m <- as.matrix(sub.space.m)

#save both as a list of matrices----
output <- list(bray.sim,time.m,space.m)
names(output) <- c('bray.sim','time','space')
sub.output <- list (sub.bray.sim,sub.time.m,sub.space.m)
names(sub.output) <- c('bray.sim','time','space')
saveRDS(    output, bci_all_matrix_output.path)
saveRDS(sub.output, bci_sub_matrix_output.path)
