#Formatting data from Tedersoo et al. 2014 for analysis.
#clear R environment
rm(list=ls())
source('paths.r')
library(rvest)
library(jsonlite)
library(stringi)
source('/home/caverill/NEFI_microbe/NEFI_functions/fg_assign.r')
source('/home/caverill/NEFI_microbe/NEFI_functions/lineaus.r')
source('Scripts/functions/grab_season.r')

#Load data, setup output paths.----
#load OTU table
otu <- read.table(ted_raw_otu.path, header = TRUE, row.names = 1, check.names = FALSE, sep = "\t")
#load mapping file.
map <- read.csv(ted_raw_map.path, header = TRUE, na.strings=c("", "NA"))
#load separate time data sent by Leho Tedersoo
time <- read.csv(ted_raw_time.path, header = TRUE, row.names=1, check.names = FALSE)

#output.paths
map.out.path <- ted_clean_map.path
otu.out.path <- ted_clean_otu.path

#Begin Filtering.----
#remove samples w/ incomplete data in map file.
map <- na.omit(map)

#Remove samples in OTU table that are not in the mapping file.
otu <- otu[,colnames(otu) %in% map$tedersoo.code]

#Deal with Time data formatting.####
#format the time data frame (get rid of an empty column, etc.)
colnames(time)[1] <- 'human.date'
time[2] <- NULL
#convert human readable date to days since epoch
time$epoch.date <- round(
  as.numeric(
    as.POSIXlt(time$human.date, 
               format = "%m/%d/%y", origin = "01/01/1970"))/86400)
#get day of year (doy) as well.
time$doy  <- lubridate::yday(as.Date(time$human.date,format='%m/%d/%Y'))
time$year <- lubridate::year(as.Date(time$human.date,format='%m/%d/%Y'))
time$epoch.date <- (time$year - 9)*365 + time$doy

#drop samples in time table not present in mapping file.
time <- time[row.names(time) %in% map$tedersoo.code,]
#push times into the mapping file
time$tedersoo.code <- rownames(time)
map <- merge(map,time, by = 'tedersoo.code', all.x=T)
map$year <- paste0('20',map$year)

#Extract Season data.----
seas <- grab_season(latitude = map$latitude, longitude = map$longitude, year = map$year, ID = map$tedersoo.code)
seas <- seas[,c('ID','season_start_doy','season_end_doy','season_total','n_seasons')]
map <- merge(map,seas, by.x = 'tedersoo.code',by.y='ID')
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
  #Sampled pre-season, normal season.
  if(map$season_start_doy[i] < map$season_end_doy[i] & map$doy[i] < map$season_start_doy[i]){
    map$seas_pos[i] <- (map$doy[i] - map$season_start_doy[i])/ map$season_total[i]
  }
  #Sample post-season, normal season.
  if(map$season_start_doy[i] < map$season_end_doy[i] & map$doy[i] > map$season_start_doy[i]){
    map$seas_pos[i] <- (map$doy[i] - map$season_start_doy[i])/ map$season_total[i]
  }
  #No seasonal observation.
  if(map$season_total[i] == -1){
    map$seas_pos[i] <- NA
  }
}

#get EVI position.
seas <- grab_season(latitude = map$latitude, longitude = map$longitude, year = map$year, ID = map$tedersoo.code, EVI2=T)
seas <- seas[,c('ID','season_start_doy','season_end_doy','season_total','n_seasons')]
seas<- merge(seas, map[,c('tedersoo.code','doy')], by.x = 'ID', by.y='tedersoo.code')
seas$seas_pos <- NA
for(i in 1:length(seas$seas_pos)){
  #normal case: season starts after Jan 1, ends before Dec 1.
  if(seas$doy[i] > seas$season_start_doy[i] & seas$doy[i] < seas$season_end_doy[i]){
    seas$seas_pos[i] <- (seas$doy[i] - seas$season_start_doy[i])/ seas$season_total[i]
  }
  #Season starts before Dec 31.
  if(seas$doy[i] < seas$season_start_doy[i] & seas$doy[i] < seas$season_end_doy[i]){
    #calulate days to end of season, subtract from total duration, then divide.
    seas$seas_pos[i] <- (seas$season_total[i] - (seas$season_end_doy[i] - seas$doy[i]))/seas$season_total[i]
  }
  #Season ends after Dec 31.
  if(seas$doy[i] > seas$season_start_doy[i] & (seas$season_start_doy[i] + seas$season_total[i]) > seas$doy[i]){
    seas$seas_pos[i] <- (seas$doy[i] - seas$season_start_doy[i])/ seas$season_total[i]
  }
  #Sampled pre-season, normal season.
  if(seas$season_start_doy[i] < seas$season_end_doy[i] & seas$doy[i] < seas$season_start_doy[i]){
    seas$seas_pos[i] <- (seas$doy[i] - seas$season_start_doy[i])/ seas$season_total[i]
  }
  #Sample post-season, normal season.
  if(seas$season_start_doy[i] < seas$season_end_doy[i] & seas$doy[i] > seas$season_start_doy[i]){
    seas$seas_pos[i] <- (seas$doy[i] - seas$season_start_doy[i])/ seas$season_total[i]
  }
  #No seasonal observation.
  if(seas$season_total[i] == -1){
    seas$seas_pos[i] <- NA
  }
}
#change names, merge evi2 position into mapping file.
colnames(seas)[ncol(seas)] <- 'seas_pos.evi2'
map <- merge(map,seas[,c('ID','seas_pos.evi2')], by.x = 'tedersoo.code',by.y='ID')

#order OTU table to match the mapping file
otu <- otu[, order(colnames(otu), map$tedersoo.code)]


#Normalize otu table.----
#This function converts each OTU seq abundance to a fractional percentage of the total number of reads within a sample. 
pro.function <- function(otu){
  for(i in 1:ncol(otu)){
    otu[,i] <- otu[,i] / sum(otu[,i])
  }
  return(otu)
}

#normalize and double check all column sums are 1
otu <- pro.function(otu)
colSums(otu)

#Subset map and otu N temperate and appropriate seasons.----
#subset your mapping file to region of interest.
map <- map[map$latitude > 23.5 & map$latitude < 65,]
#do the seas_pos filtering.
map <- map[!is.na(map$seas_pos),]
map <- map[map$n_seasons < 2,]
map <- map[map$seas_pos <  1.1,]
map <- map[map$seas_pos > -0.1,]
map <- map[map$seas_pos.evi2 <  1.1,]
map <- map[map$seas_pos.evi2 > -0.1,]

#dial in map and otu table to be same order.
otu <- otu[,colnames(otu) %in% map$tedersoo.code]
otu <- otu[,order(colnames(otu))]
map <- map[order(map$tedersoo.code),]

#weird NAs popping up. kill.
to_kill <- rownames(map[(grep('NA',rownames(map))),])
map <- map[!(rownames(map) %in% to_kill),]


#Save output.----
saveRDS(otu, otu.out.path)
saveRDS(map, map.out.path)
