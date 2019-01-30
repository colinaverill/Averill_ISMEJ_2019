#' space_time_power_analysis.r
#' This function performs a power analysis to determine the user's ability to detect spatial-temporal effects.
#' This function takes a mapping file of covariates, a normalized species matrix (otu table) and a formula as input.
#' This function assumes your species matrix is normalized, such that the columns sum to 1, though it might work without this, but has not been tested.
#' number of rows in mapping file should match number of columns in otu file. These should be ordered such that samples can be matched.
#' "space" is a special covariate name. If space is used in the supplied formula, x-y coordinates should be present in the mapping file, named 'x' and 'y'. 
#' User can choose to natural-log transform community similarity values, if desired.
#' If this is done, code automatically checks if there are zero values in similarity matrix. If there are, it adds the smallest, non-zero similarity value to all observations before log transforming.
#'
#' @param formula   Formula of model to be vetted for statistical power.
#' @param map       Mapping file of covariates.
#' @param otu       Normalized OTU table / species matrix such that columns sum to 1.
#' @param n.straps  Number of bootstrap simulations. Default 2000.
#' @param p.check   Level of statistical significance to consider. Default p=0.05.
#' @param warn      Set warn to FALSE if you want to suppress warnings. Default TRUE.
#' @param log       Natural-log transform similarity values? Default FALSE.
#'
#' @return
#' @export
#'
#' @examples
#' #load data
#' source('/home/caverill/Averill_ISMEJ_2018/paths.r')
#' map <- readRDS(kiv_clean_fun_map.path)
#' otu <- readRDS(kiv_clean_fun_otu.path)
#' space_time_power_analysis(y ~ space + time.num, map, otu)
space_time_power_analysis <- function(formula, map, otu, log = F,
                                      n.straps = 2000, p.check = 0.05, 
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
  
  #bootstrap parameter uncertainty of observed effects.----
  cat('Bootstrapping observed parameter uncertainty...\n')
  mod <- ecodist::MRM(formula, data = dat)
  observed.parameters <- mod$coef[,1]
  obs.par <- list()
  obs.sig <- list()
  for(i in 1:n.straps){
    test.dat <- dat[sample(nrow(dat), nrow(dat), replace = T),]
    mod <- ecodist::MRM(formula, data = test.dat)
    obs.par[[i]] <- mod$coef[,1]
    obs.sig[[i]] <- mod$coef[,2]
  }
  obs.par <- do.call(rbind, obs.par)
  obs.sig <- do.call(rbind, obs.sig)
  
  #frequency p value less than 0.05.----
  sig.check <- list()
  for(i in 1:ncol(obs.sig)){
    sig.check[[i]] <- sum(obs.sig[,i] < p.check) / nrow(obs.sig)
  }
  sig.check <- unlist(sig.check)
  names(sig.check) <- colnames(obs.par)
  
  #generate null distributions of covariates.----
  cat('Generating null distributions of parameter estimates...\n')
  null.par <- list()
  for(i in 1:n.straps){
    #shuffle similarity values.
    test.dat <- dat
    test.dat$y <- sample(test.dat$y)
    #Fit model.
    mod <- ecodist::MRM(formula, data = test.dat)
    null.par[[i]] <- mod$coef[,1]
  }
  null.par <- do.call(rbind, null.par)

  #get null and observed parameter quantiles.----
  par.quant <- list()
  obs.quant <- list()
  for(i in 1:ncol(null.par)){
    par.quant[[i]] <- quantile(null.par[,i], probs = c(0.025,0.975))
    obs.quant[[i]] <- quantile(obs.par[,i], probs = c(0.025, 0.975))
  }
  names(par.quant) <- colnames(null.par)
  names(obs.quant) <- colnames(obs.par)
      null.parameter.distribution <- do.call(rbind, par.quant)
  observed.parameter.distribution <- do.call(rbind, obs.quant)
  
  #wrap output to return.----
  output <- list(observed.parameters,observed.parameter.distribution,null.parameter.distribution,sig.check)
  names(output) <- c('observed.parameters','observed_parameter_distribution','null_parameter_distribution','parameter_significance_frequency')
  return(output)
  cat('Power analysis complete.\n')
  
} #end function.
