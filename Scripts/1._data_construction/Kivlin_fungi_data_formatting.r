#formatting Kivlin tropics fungal data
#clear R environment
rm(list=ls())
source('paths.r')

#load data, set output paths.----
#load OTU table
otu <- read.csv(kiv_raw_fun_otu.path, header = TRUE)
#otu <- data.frame(otu)
#load mapping file.
map <- read.csv(kiv_raw_fun_map.path, header = TRUE, na.strings=c("", "NA"))
map <- data.frame(map)

#set output paths.
otu_output.path <- kiv_clean_fun_otu.path
map_output.path <- kiv_clean_fun_map.path

#format time data.----
#generate time data. 4 time points. 
#Only March and September samplings, across 2 years.
map$time.num <- (map$Year - 2012)*365
map$time.num <- ifelse(map$Date == 'March',map$time.num + 74, map$time.num + 258)

#get block.plot
map$block.plot <- paste(map$Block,map$Plot, sep = '.')

#one site does not have an X or Y coordinate. Remove this observation from map file.
map <- map[!is.na(map$x),]

#make the first column of otu table the rownames.
rownames(otu) <- otu[,1]
otu[,1] <- NULL
#otu <- as.matrix(otu)

#remove observations in otu file not present in mapping file. 
otu <- otu[,colnames(otu) %in% map$X]

#order OTU table and the map so that they match. 
map <- map[order(map$X),]
otu <- otu[, order(colnames(otu), map$X)]

#Normalize otu table.----
#function to normalize otu table. 
pro.function <- function(otu){
  for(i in 1:ncol(otu)){
    otu[,i] <- otu[,i] / sum(otu[,i])
  }
  return(otu)
}

#normalize and double check all column sums are 1
otu <- pro.function(otu)
colSums(otu)

#save OTU table and map for bootstrap script.----
saveRDS(otu,otu_output.path)
saveRDS(map,map_output.path)
