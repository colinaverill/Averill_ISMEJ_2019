#bootstrap function----
#' boostrap.r Performs bootstrap resampling analysis to determine confidence intervals around community similarity paraemter estiamtes from MRM.
#'
#' @param map       #environmental mapping file. rownames must be sample IDs.
#' @param otu       #otu or SV table. column names must be sample IDs.
#' @param X         #x coordinates of sampling locations.
#' @param Y         #y coordinates of sampling locations.
#' @param time      #time whens amples were taken. Must be an integer value.
#' @param sample_ID #vector of sample IDs.
#' @param lat_lon   #Are X Y values longitude/latitude? default F.
#' @param min_bump  #Add lowest value of community similarity to the community similarity matrix (avoid zeros)? Default F.
#' @param n.straps  #Number of bootstrap simulations to perform. Default 1000.
#'
#' @return          #Returns a dataframe with itnercept, space and time parameters from each bootstraph simulation.
#' @export
#'
#' @examples
bootstrap <- function(map,otu,X,Y,time,sample_ID,lat_lon, min_bump,n.straps = 1000){
  #TESTS.----
  #make sure all observations present in both OTU table and mapping file.
  check1 <- sum(colnames(otu) %in% rownames(map)) == length(colnames(otu))
  if(check1 == F){
    stop('Column names of otu table are not all present in sample_ID vector.')
  }
  #put mapping file and otu table in the same order.
  otu <- otu[,order(colnames(otu))]
  map <- map[order(rownames(map)),]
  #double check that worked.
  check2 <- sum(colnames(otu) == rownames(map)) == length(colnames(otu))
  if(check2 == F){
    stop('otu and mapping file orders are not matching. Check otu column names and mapping file rownames.')
  }
  
  #run bootstrap loop.----
  output<-
    foreach(i = 1:n.straps) %dopar% {
      #randomly sample with replacement the columns of the otu table.
      myOrder <- sample(ncol(otu), replace = T)
      otu.j <- otu[,myOrder]
      map.j <- map[myOrder,]
      #snip out observation form X, Y and time vectors as well. 
      X.j    <-    X[myOrder]
      Y.j    <-    Y[myOrder]
      time.j <- time[myOrder]
      
      #calculate bray-curtis similarity. transpose of otu table necessary to get distances.
      bray.dis <- vegan::vegdist(t(otu.j), method='bray')
      bray.sim <- 1 - as.matrix(bray.dis)
      #account for zeros in bray.sim by adding the smallest similarity value in the dataset to every observation
      if(min_bump == T){
        bray.sim <- bray.sim + min(bray.sim[bray.sim > 0])
      }
      
      #generate space matrix.
      if(lat_lon == F){
        space.m <- as.matrix(spatstat::pairdist(X.j, Y.j))
      }
      
      if(lat_lon == T){
        points <- data.frame(Y.j, X.j)
        points <- as.matrix(points)
        space.m <- geosphere::distm(points)
      }
      
      #generate time matrix
      time.m  <- as.matrix(dist(time.j, method="euclidean", diag = FALSE, upper = FALSE))
      
      #stack them together as list
      d <- list(bray.sim,space.m,time.m)
      names(d) <- c('bray.sim','space','time')
      
      #analyze data subset i. 
      lower  <- lapply(d, ecodist::lower)
      d      <- as.data.frame(lapply(lower, c))
      
      #run model.
      m <- lm(bray.sim ~ space + time, data=d)
      
      #grab output summary.
      out.boot <- m$coef
      
      #save output 
      return(out.boot)
      
      #print status update
      cat(paste0(i,' of 1000 iterations completed...\n'))
    } #end loop. 
  
  #convert result output to data frame
  output <- data.frame(matrix(unlist(output), nrow=length(output), byrow=T))
  
  #add row and column names. 
  colnames(output) <- c('Int','space','time')
  
  #bootstrap returns this output
  return(output)
  
  #close bootstrap function loop.
}
