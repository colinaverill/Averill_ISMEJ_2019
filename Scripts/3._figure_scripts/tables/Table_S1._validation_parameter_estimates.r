#get a table of full and subsetted parameter estimates with uncertainties from bootstrap procedure.
#clear environment, load results.
rm(list=ls())
source('paths.r')

#load bootstrap results.
bci     <-             readRDS(bci_all_boostrap.path)
kiv.bac <-  data.frame(readRDS(kiv_bac_bootstrap.path))
kiv.fun <-  data.frame(readRDS(kiv_fun_bootstrap.path))
neo.har <-  data.frame(readRDS(harv_neon_all_boostrap.path))

    sub.bci <- readRDS(bci_sub_bootstrap.path)
sub.kiv.bac <- readRDS(kiv_bac_sub100_bootstrap.path)
sub.kiv.fun <- readRDS(kiv_fun_sub100_bootstrap.path)
sub.neo.har <- readRDS(harv_neon_sub_boostrap.path)

#get means and 95% confidence intervals.
mean_interval <- function(d){
  space_mu <- mean(d$space)
  space_95 <-   sd(d$space) * 1.96
   time_mu <- mean(d$time )
   time_95 <-   sd(d$time ) * 1.96
  out <- list(space_mu,space_95,time_mu,time_95)
  for(i in 1:length(out)){
    out[[i]] <- format(signif(out[[i]],3), scientific = T)
    out[[i]] <- as.numeric(out[[i]])
    out[[i]] <- sprintf("%.2e",out[[i]])
  }
  #round to 2 decimals in scientific notation.
  names(out) <- c('space','space_95','time','time_95')
  return(out)
}
interval <- function(x, mu, CI){
  arrows(x, 
         (mu - CI), 
         x, 
         (mu + CI), 
         length=0.05, angle=90, code=3)
  
}

        bci.out <- mean_interval(bci)
    sub.bci.out <- mean_interval(sub.bci)
    kiv.bac.out <- mean_interval(kiv.bac)
sub.kiv.bac.out <- mean_interval(sub.kiv.bac)
    kiv.fun.out <- mean_interval(kiv.fun)
sub.kiv.fun.out <- mean_interval(sub.kiv.fun)
    neo.har.out <- mean_interval(neo.har)
sub.neo.har.out <- mean_interval(sub.neo.har)

#Get meged data
output.list <-list()
to_ag <- list(bci.out, sub.bci.out, kiv.bac.out, sub.kiv.bac.out, kiv.fun.out, sub.kiv.fun.out, neo.har.out, sub.neo.har.out)
for(i in 1:length(to_ag)){
  output.list[[i]] <- c(paste0(to_ag[[i]][[1]],' +/- ',to_ag[[i]][[2]]), paste0(to_ag[[i]][[3]],' +/- ',to_ag[[i]][[4]]))
}
to_table <- data.frame(do.call(rbind, output.list))
colnames(to_table) <- c('space parameter', 'time parameter')
to_table$study <- c('BCI Trees','BCI Trees subset','La Selva Bacteria','La Selva Bacteria subset','La Selva Fungi','La Selva Fungi subset','Harvard Forest Fungi','Harvard Forest Fungi subset')
to_table <- to_table[,c('study','space parameter','time parameter')]
to_table
