#' space_time_analysis.r
#' This function analyzes the effects of space, time, and other covariates on Bray-Curtis community similarity.
#' This function takes a mapping file of covariates, a normalized species matrix (otu table) and a formula as input.
#' This function returns parameter estimates, and their 95% confidence intervals, calculated via bootstrap simulation. Also returns raw bootstrap output.
#' This function assumes your species matrix is normalized, such that the columns sum to 1, though it might work without this, but has not been tested.
#' number of rows in mapping file should match number of columns in otu file. These should be ordered such that samples can be matched.
#' "space" is a special covariate name. If space is used in the supplied formula, x-y coordinates should be present in the mapping file, named 'x' and 'y'. 
#' User can choose to natural-log transform community similarity values, if desired.
#' If this is done, code automatically checks if there are zero values in similarity matrix. If there are, it adds the smallest, non-zero similarity value to all observations before log transforming.
#'
#' Function returns a list of output.
#' output$parameters is a table of parameter estimates and their associated confidence intervals.
#' output$MC_output is the parameter estimates generate from each Monte Carlo bootstrap simulation.
#' output$conf_interval is the conf_interval that was supplied.
#'
#' @param formula       Model formula, syntax same as lm. Has not been tested with interactions.
#' @param map           Mapping file of covariates.
#' @param otu           Normalized OTU table / species matrix such that columns sum to 1.
#' @param n.straps      Number of bootstrap simulations. Default 2000.
#' @param conf_interval What confidence interval would you like reported? Must be between 0-1. Default is 0.95 (report 95% confidence intervals).
#' @param warn          Set warn to FALSE if you want to suppress warnings. Default TRUE.
#' @param log           Natural-log transform similarity values? Default FALSE.
#'
#' @return
#' @export
#'
#' @examples
#' #load data
#' source('paths.r')
#' source('Scripts/functions/space_time_analysis.r')
#' map <- readRDS(kiv_clean_fun_map.path)
#' otu <- readRDS(kiv_clean_fun_otu.path)
#' space_time_power_analysis(y ~ space + time.num, map, otu)
space_time_power_analysis <- function(formula, map, otu, log = F,
                                      n.straps = 2000, conf_interval = 0.95, 
                                      warn = T){
  #Checks to make sure data seems correctly formatted, necessary packages present.----
  pack_req <- c('vegan','spatstat','ecodist')
  check <- pack_req %in% rownames(installed.packages())
  if(sum(check) != length(pack_req)){
    pack_need <- pack_req[!check]
    pack_need <- paste(pack_need, collapse = ', ')
    stop(paste('You must install the following package(s):',pack_need))
  }
  #check map and otu files set up correctly.
  if(nrow(map) != ncol(otu)){
    stop('The number of rows in the supplied mapping file does not match the number of columns in the otu table.')
  }
  check <- sum(rownames(map) == colnames(otu)) / ncol(otu)
  if(check != 1){
    if(warn == T){
      warning('rownames of mapping file do not match column names of otu file. Be certain everything is ordered correctly.\n')
    }
  }
  #Make sure confidence interval makes sense.
  check <- ifelse(conf_interval < 0 | conf_interval > 1, 1, 0)
  if(check == 1){
    stop('Supplied confidence interval is either less than 0 or greater than 1. This value needs be on the interval (0,1).\n')
  }

  #Format distance matrices.----
  grab <- all.vars(formula)
  d.sub <- map[,colnames(map) %in% grab]
  d.sub$x <- NULL
  d.sub$y <- NULL
  
  #Make community similarity matrix.
  cat('Generating distance matrices...\n')
  y <- 1 - vegan::vegdist(t(otu))
  
  #Get list of predictor matrices.
  matrix.list <- list()
  matrix.list[[1]] <- y
  for(i in 1:ncol(d.sub)){
    matrix.list[[i+1]] <- dist(d.sub[,i])
  }
  names(matrix.list) <- c('y',colnames(d.sub))
  
  #Need to do something special for space.
  if('space' %in% grab){
    space <- as.matrix(spatstat::pairdist(map$x, map$y))
    matrix.list[[length(matrix.list) + 1]] <- space
    names(matrix.list)[length(matrix.list)] <- 'space'
  }
  
  #Generate the data frame.
  lower  <- lapply(matrix.list, ecodist::lower)
  dat    <- as.data.frame(lapply(lower, c))
  colnames(dat) <- names(matrix.list)
  
  #Log transform?
  if(log == T){
    #log transform cannot handle zero values.
    if(min(dat$y) == 0){
      bump <- min(dat$y[dat$y > 0])
      dat$y <- dat$y + bump
    }
    dat$y <- log(dat$y)
  }
  
  #bootstrap parameter uncertainty.----
  cat('Bootstrapping parameter uncertainty...\n')
  obs.par <- list()
  for(i in 1:n.straps){
    test.dat <- dat[sample(nrow(dat), nrow(dat), replace = T),]
    mod <- ecodist::MRM(formula, data = test.dat)
    obs.par[[i]] <- mod$coef[,1]
  }
  obs.par <- do.call(rbind, obs.par)

  #get parameter quantiles.----
  obs.quant <- list()
  lo_95 <- (1-conf_interval) / 2
  hi_95 <- 1 - lo_95
  for(i in 1:ncol(obs.par)){
    obs.quant[[i]] <- quantile( obs.par[,i], probs = c(lo_95, hi_95))
  }
  names(obs.quant) <- colnames(obs.par)
  observed.parameter.distribution <- do.call(rbind, obs.quant)
  par.mean <- colMeans(obs.par)
  par.table <- cbind(par.mean,observed.parameter.distribution)
  colnames(par.table)[1] <- 'parameter_estimate'
  rownames(par.table)[1] <- 'intercept'

  #wrap output to return.----
  output <- list(par.table, obs.par,conf_interval)
  names(output) <- c('parameters','MC_output','paramter_confidence_interval_value')
  return(output)
  cat('Analysis complete.\n')
  
} #end function.
