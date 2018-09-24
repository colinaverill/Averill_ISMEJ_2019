#' grab_season.r
#' This function uses data from https://lpdaac.usgs.gov/dataset_discovery/measures/measures_products_table/vipphen_ndvi_v004
#' reference: Didan, K., Barreto, A. (2016). NASA MEaSUREs Vegetation Index and Phenology (VIP) Phenology EVI2 Yearly Global 0.05Deg CMG [Data set]. NASA EOSDIS Land Processes DAAC. doi: 10.5067/MEaSUREs/VIP/VIPPHEN_EVI2.004
#' Will return values of -1 if your coordinate is fake.
#'
#' @param latitude     #a numeric vector of latitude 
#' @param longitude    #a numeric vector of longitude.
#' @param year         #a numeric vector of years paired to the lat/lon observations.
#' @param raster.dir   #path to directory where phenology .hdf files are stored.
#' @param dl_raw       #do you want to download raw phenology files for your dates? They are big, 1.2G each. Deafult = FALSE.
#' @param EVI2         #Use EVI2 product for calculation? defaults is F, and therefore uses NDVI product.
#'
#' @return             #returns data frame with start and end of season in doy, as well as growing season length appended to lat/lon/year.
#' @export
#'
#' @examples
grab_season <- function(latitude,longitude,year,ID,raster.dir='/fs/data3/caverill/test_VIPPHEN/',dl_raw=F,EVI2=F){
  #organize your data to extract.----
  points <- cbind(longitude, latitude)
  dat <- data.frame(ID,year,points)
  colnames(dat) <- c('ID','year','lon','lat')
  
  #download raw files?----
  #this is broken. Numeric extension is not the same for each. Need to scrap the list of files from the URL, then grep out year and NDVI or EVI.
  #if(dl_raw == T){
  #  system(paste0('mkdir -p ',raster.dir)) #make raster directory if it doesn't already exist.
  #  files <- list.files(raster.dir)
  #  base.path <- 'https://vip.arizona.edu/vipdata/V4/DATAPOOL/PHENOLOGY/VIPPHEN_NDVI.A@YEAR@.004.2018171122736.hdf'
  #  to_get <- unique(year)
  #  for(i in 1:length(to_get)){
  #    path <- gsub('@YEAR@',to_get[i], base.path)
  #    check <- basename(path)
  #    if(check %in% files){
  #      next
  #    }
  #    cmd <- paste0('wget ',path,' -P ',raster.dir)
  #    system(cmd)
  #  }
  #}
  
  #Grab data one year at a time.----
  to_get <- unique(year)
  files <- list.files(raster.dir)
  output <- list()
  
  #loop over unique years.
  for(i in 1:length(to_get)){
    #get one year's data together.
    ddat <- dat[dat$year == to_get[i],]
    points <- cbind(ddat$lon,ddat$lat)
    
    #grab appropriate file path.
    path <- files[grep(paste0('NDVI.A',to_get[i]),files)]
    if(EVI2==T){
      path <- files[grep(paste0('EVI2.A',to_get[i]), files)]
    }
    
    #check if that file exists. If it don't you can't extract it.
    #This may cause an rbind problem downstream with the list...
    if(identical(path, character(0)) == T){
      cat(paste0('No data for year ',to_get[i],'. Moving along. Try downloading!\n'))
      next
    }
    path <- paste0(raster.dir,path)
    
    #get layers within the hdf file, covnert to rasters.
    layer.names <- gdalUtils::get_subdatasets(path)
    start <- rgdal::readGDAL(layer.names[ 1])
      end <- rgdal::readGDAL(layer.names[ 2])
    total <- rgdal::readGDAL(layer.names[ 3])
    nseas <- rgdal::readGDAL(layer.names[25])
    start <- raster::raster(start)  
      end <- raster::raster(  end)
    total <- raster::raster(total)
    nseas <- raster::raster(nseas)
      
    #extract points.
    start.data <- raster::extract(start,points)
      end.data <- raster::extract(  end,points)
    total.data <- raster::extract(total,points)
    nseas.data <- raster::extract(nseas,points)
     to_return <- cbind(ddat,start.data,end.data,total.data,nseas.data)
       to_name <- c('season_start_doy','season_end_doy','season_total','n_seasons')
     colnames(to_return)[(ncol(to_return) - (length(to_name)-1)):ncol(to_return)] <- to_name
      
    #drop into output.
    output[[i]] <- to_return
    cat(i,'of',length(to_get),'years queried.\n')
  }
  #return output.
  output <- do.call(rbind,output)
  return(output)
} #end function.----
