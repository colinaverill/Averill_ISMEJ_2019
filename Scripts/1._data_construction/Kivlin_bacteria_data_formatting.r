#checking space for time signals in Kivlin tropics data
#clear R environment
rm(list=ls())
source('paths.r')

#load data, set output paths.----
#load OTU table
otu <- read.csv(kiv_raw_bac_otu.path, header = TRUE)
#otu <- data.frame(otu)
#load mapping file.
map <- read.csv(kiv_raw_bac_map.path, header = TRUE, na.strings=c("", "NA"))
map <- data.frame(map)

#set output paths.
map_output.path <- kiv_clean_bac_map.path
otu_output.path <- kiv_clean_bac_otu.path

#Format time data.----
#generate time data. 4 time points. 
map$time.num <- (map$Year - 2012)*365
map$time.num <- ifelse(map$Date == 'March',map$time.num + 74, map$time.num + 258)

#get unique plot locations by merging block and plot
map$block.plot <- do.call(sprintf, c(map[,c('Block','Plot')], '%s.%s'))

#make the first column of otu table the rownames.
rownames(otu) <- otu[,1]
otu[,1] <- NULL
#otu <- as.matrix(otu)

#remove observations in otu file not present in mapping file. 
otu <- otu[,colnames(otu) %in% map$X]

#order OTU table and the map so that they match. 
map <- map[order(map$X),]
otu <- otu[, order(colnames(otu), map$X)]


#normalize otu table.----
pro.function <- function(otu){
  for(i in 1:ncol(otu)){
    otu[,i] <- otu[,i] / sum(otu[,i])
  }
  return(otu)
}

#normalize and double check all column sums are 1
otu <- pro.function(otu)
colSums(otu)

#save formatted OTU table and map.----
saveRDS(otu,otu_output.path)
saveRDS(map,map_output.path)
