#Monte Carlo analysis- will we spurriously detect time effects given the structure of our data?
#Approach 1: repeat Tedersoo 2014 analysis, but randomize time vector. See if data structure generates spurrious time effects.
#Approach 2: Simulate community similarity values in data sets that are not orthogonal, with and w/o space and time signals, test for false positives.
#Clear R environment, load paths and packages.
rm(list=ls())
source('Scripts/functions/tic_toc.r')
source('paths.r')

#set output path, number of simulations, load and format data.----
output.path <- ted_monte_carlo_results.path

#run 1000 simulations
N <- 1000

#load tedersoo final MRM model fit and data set.
dat <- readRDS(ted_model_output.path)
mod <- dat$st.env.mrm
dat <- dat$st.env.dat

#grab predictors of interest.
preds <- c(rownames(mod$coef)[-1])
to_keep <- c('bray.sim',preds)
dat <- dat[,colnames(dat) %in% to_keep]

#get model formula
my.formula <- paste(preds, collapse = '+')
my.formula <- paste0('log(bray.sim) ~ ',my.formula)

#setup output matrix.
out<-matrix(rep(0,N*4),nrow=N,dimnames=list(NULL,c('inter_effect','inter_pval','intra_effect','intra_pval')))

#begin simulation loop.----
for(i in 1:N){
  #randomize the time data
  d <- dat
  d$epoch.date <- sample(d$epoch.date)
  d$seas_pos  <- sample(d$seas_pos)

  #run MRM model
  m <- ecodist::MRM(my.formula,
           data = d)
  
  #extract the effect size of the time predictor, as well as its p value.
  inter_effect <- m$coef[3,1]
  inter_pval   <- m$coef[3,2]
  intra_effect <- m$coef[4,1]
  intra_pval   <- m$coef[4,2]
  #bin up output.
  output <- c(inter_effect, inter_pval, intra_effect, intra_pval)
  output <- unlist(output)
  
  #update output
  out[i,] <- c(output)
  
  #print status update
  cat(paste0(i,' of 1000 iterations completed...\n'))
}

#save output of simulation (it takes like an hour to do this on my macbook pro)
out <- data.frame(out)
saveRDS(out, output.path)

#calculate number of false positives. 
fp_inter <- (nrow(out[out$inter_pval < 0.05,]) / N)*100
fp_intra <- (nrow(out[out$intra_pval < 0.05,]) / N)*100
cat(paste0(fp_inter),'% and ',fp_intra,'% of inter- and intra-annual temporal predictors generated false positives, respectively. Null expectation is 5%.')
