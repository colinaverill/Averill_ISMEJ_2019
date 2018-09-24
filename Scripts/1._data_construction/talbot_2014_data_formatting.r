#Building bray curtis similarity and predictor distance matrices for MRM analysis of Talbot et al. 2014
#first clear R environment.
rm(list=ls())
source('paths.r')
source('Scripts/functions/grab_season.r')
#library(data.table)
#library(vegan)
#source('Scripts/space_time_functions.r')
#source('/home/caverill/NEFI_microbe/NEFI_functions/fg_assign.r')
#source('/home/caverill/NEFI_microbe/NEFI_functions/lineaus.r')

#Load data, set output paths.----
#set output paths.
otu_output.path <- tal_clean_otu.path
map_output.path <- tal_clean_map.path

#load mapping file.
map <- readRDS(tal_raw_map.path)
#map <- data.table(map)
#load otu file
#otu <- read.table('/fs/data3/caverill/Microbial_Space_Time_data/talbot_2014.data/DOB_Soil_OTU_Table_11Feb13_UNITETAX.txt',header = T)
otu <- read.csv(tal_raw_otu.path,header = T)
#remove otu ID column, peel off taxonomy.
otu <- otu[,-1]
tax <- otu[,600]

#This is the bray dissimilarity matrix used for publication.
#subset mapping and OTU to only include samples present in this matrix.
bray.dis <- read.csv(tal_ref_sim.path, header = TRUE, row.names = 1, check.names = FALSE)

#Subset to samples used in Talbot 2014 publication.----
#Only use the 516 in the OTU file that are also in bray.dis.
otu <- otu[,colnames(otu) %in% colnames(bray.dis)] 
map <- map[map$Mapping.ID %in% colnames(otu),]

#sort so they are in the same order
otu <- otu[,sort(colnames(otu))]
sort.key <- colnames(otu)
map <- map[match(sort.key,map$Mapping.ID),]

#Get day of year.----
#test <- strftime(map$Date.Sampled2, format = "%j")
map$doy <- as.Date(map$Date.Sampled,format='%m/%d/%Y')
map$year <- lubridate::year(map$doy)
map$doy  <- lubridate::yday(map$doy)
map$time <- ifelse(map$year == 11, map$doy,
                   ifelse(map$year == 12, map$doy + 365,
                          ifelse(map$year == 13, map$doy + 730, NA)))
map$year <- paste0('20',map$year)
#Extract seasonality data.----
seas <- grab_season(latitude = map$latitude.dd, longitude = map$longitude.dd, year = map$year, ID = map$Mapping.ID)
seas <- seas[,c('ID','season_start_doy','season_end_doy','season_total')]
map <- merge(map,seas, by.x='Mapping.ID',by.y='ID')
map$seas_pos <- NA
for(i in 1:length(map$seas_pos)){
  #normal case: season starts after Jan 1, ends before Dec 1.
  if(map$doy[i] > map$season_start_doy[i] & map$doy[i] < map$season_end_doy[i]){
    map$seas_pos[i] <- (map$doy[i] - map$season_start_doy[i])/ map$season_total[i]
  }
  #Season starts before Dec 31.
  if(map$doy[i] < map$season_start_doy[i] & map$doy[i] < map$season_end_doy[i]){
    #calulate days to end of season, subtract from total duration, then divide.
    map$seas_pos[i] <- (map$season_total[i] - (map$season_end_doy[i] - map$doy[i]))/map$season_total[i]
  }
  #Season ends after Dec 31.
  if(map$doy[i] > map$season_start_doy[i] & (map$season_start_doy[i] + map$season_total[i]) > map$doy[i]){
    map$seas_pos[i] <- (map$doy[i] - map$season_start_doy[i])/ map$season_total[i]
  }
}

#normalize the otu table.----
pro.function <- function(otu){
  for(i in 1:ncol(otu)){
    otu[,i] <- otu[,i] / sum(otu[,i])
  }
  return(otu)
}
otu <- pro.function(otu)
#make sure column sums are 1.
colSums(otu)

#Subset based on latitude and season.----
map <- map[!is.na(map$seas_pos),]
#map <- map[map$n_seasons < 2,]
map <- map[map$seas_pos < 1.1,]
map <- map[map$seas_pos > -0.1,]

#dial in map and otu table to be same order.
otu <- otu[,colnames(otu) %in% map$Mapping.ID]
otu <- otu[,order(colnames(otu))]
map <- map[order(map$Mapping.ID),]


#save filtered mapping and otu files.----
saveRDS(otu,otu_output.path)
saveRDS(map,map_output.path)
