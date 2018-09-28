#Getting how much space is how much time table together, +/- 1 SD.
#clear environment, load packages
rm(list=ls())

#load all jacknife results from full data sets.
#No North American Soil Fungi data, as there is no temporal effect.
bci <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/BCI50ha_data/out_bci_bootstrap.rds')
kiv <- data.frame(readRDS('/fs/data3/caverill/Microbial_Space_Time_data/kivlin_tropics.data/out_kiv_bootstrap.rds'))
ted <- data.frame(readRDS('/fs/data3/caverill/Microbial_Space_Time_data/tedersoo_2014.data/tedersoo2014_bootstrap_data.rds'))
ted <- data.frame(readRDS('/fs/data3/caverill/Microbial_Space_Time_data/tedersoo_2014.data/tedersoo2014_nt.int_bootstrap_data.rds'))
ted.ecm <- data.frame(readRDS('/fs/data3/caverill/Microbial_Space_Time_data/tedersoo_2014.data/tedersoo2014_nt.int_ECM_bootstrap_data.rds'))
ted.sap <- data.frame(readRDS('/fs/data3/caverill/Microbial_Space_Time_data/tedersoo_2014.data/tedersoo2014_nt.int_SAP_bootstrap_data.rds'))
tal <- data.frame(readRDS('/fs/data3/caverill/Microbial_Space_Time_data/talbot_2014.data/talbot2014_int_bootstrap_data.rds'))
tal.ecm <- data.frame(readRDS('/fs/data3/caverill/Microbial_Space_Time_data/talbot_2014.data/talbot2014_int_ECM_bootstrap_data.rds'))
tal.sap <- data.frame(readRDS('/fs/data3/caverill/Microbial_Space_Time_data/talbot_2014.data/talbot2014_int_SAP_bootstrap_data.rds'))

#d.lt <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/tedersoo_2014.data/figure_data/tedersoo_data_fitted_cov_nt_int.rds')
#d.jt <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/talbot_2014.data/figure_data/talbot_data_fitted_cov.int.rds')
d.lt <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/tedersoo_2014.data/figure_data/tedersoo_data_fitted_st.env.rds')
d.jt <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/talbot_2014.data/figure_data/talbot_data_fitted_st.env.rds')
d.jt <- d.jt[[1]]
d.lt <- d.lt[[1]]

#new space time only fits
#you might want the bootstrap data here.
ted.st.env <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/tedersoo_2014.data/figure_data/tedersoo_data_fitted_st.env.rds')
tal.st.env <- readRDS('/fs/data3/caverill/Microbial_Space_Time_data/talbot_2014.data/figure_data/talbot_data_fitted_st.env.rds')
tal.st <- summary(tal.st.env[[1]])
ted.st <- summary(ted.st.env[[1]])

#grab parameter estiamates
  tal.doy <- tal.st$coefficients[4,1]
 tal.time <- tal.st$coefficients[3,1]
tal.space <- tal.st$coefficients[2,1]
  ted.doy <- ted.st$coefficients[4,1]
 ted.time <- ted.st$coefficients[3,1]
ted.space <- ted.st$coefficients[2,1]


#kilometers per day is time divided by space.
#days per kilometer is space divided by time.

#how much space is 100 intra-anual days?
(tal.doy*100)/tal.space #1114km to match a 100d of temporal seasonal distance.
(ted.doy*100)/ted.space
#how much time is 100km of distance?
(tal.space*100)/tal.doy
(ted.space*100)/ted.doy

tal.space/tal.doy
tal.doy/tal.space #11km of space to match 1 day

tal.space/tal.time
tal.doy/tal.time
tal.time/tal.doy


#THIS IS WARPING YO BRAIN.
#intra and interannual time parameters need to context of the interaction.
#add in the mean of the other (intra/inter) time the interaction parameter to get the normalized intra/inter parameter.
lt.time_interaction <- mean(ted$time_int)
lt.inter.mu <- mean(d.lt$time)
lt.intra.mu <- mean(d.lt$doy)
ted.inter.time.p <- mean(ted[,3]) + lt.intra.mu*lt.time_interaction
ted.intra.time.p <- mean(ted[,4]) + lt.inter.mu*lt.time_interaction
ted.ecm.inter.time.p <- mean(ted.ecm[,3]) + lt.intra.mu * mean(ted.ecm$time_int)
ted.ecm.intra.time.p <- mean(ted.ecm[,4]) + lt.inter.mu * mean(ted.ecm$time_int)
ted.sap.inter.time.p <- mean(ted.sap[,3]) + lt.intra.mu * mean(ted.sap$time_int)
ted.sap.intra.time.p <- mean(ted.sap[,4]) + lt.inter.mu * mean(ted.sap$time_int)

jt.time_interaction <- mean(tal$time_int)
jt.inter.mu <- mean(d.jt$time)
jt.intra.mu <- mean(d.jt$doy)
tal.inter.time.p <- mean(tal[,3]) + jt.intra.mu*jt.time_interaction
tal.intra.time.p <- mean(tal[,4]) + jt.inter.mu*jt.time_interaction
tal.ecm.inter.time.p <- mean(tal.ecm[,3]) + jt.intra.mu * mean(tal.ecm$time_int)
tal.ecm.intra.time.p <- mean(tal.ecm[,4]) + jt.inter.mu * mean(tal.ecm$time_int)
tal.sap.inter.time.p <- mean(tal.sap[,3]) + jt.intra.mu * mean(tal.sap$time_int)
tal.sap.intra.time.p <- mean(tal.sap[,4]) + jt.inter.mu * mean(tal.sap$time_int)

#How many kilometers is an inter-annual day
kiv.km.interday <- mean(kiv$time) / (mean(kiv$space)/1000)
bci.km.interday <- mean(bci$time) / (mean(bci$space)/1000)
ted.km.interday <- ted.inter.time.p / mean(ted$space)
tal.km.interday <- tal.inter.time.p / mean(tal$space)
ted.ecm.km.interday <- ted.ecm.inter.time.p / mean(ted.ecm$space)
tal.ecm.km.interday <- tal.ecm.inter.time.p / mean(tal.ecm$space)
ted.sap.km.interday <- ted.sap.inter.time.p / mean(ted.sap$space)
tal.sap.km.interday <- tal.sap.inter.time.p / mean(tal.sap$space)
#kiv.km.interday.sd <- sd(kiv$time / (kiv$space/1000))
#bci.km.interday.sd <- sd(bci$time / (bci$space/1000))
#ted.km.interday.sd <- sd(ted$time / ted$space)
#tal.km.interday.sd <- sd(tal$time / tal$space)

#How many kilometers is an intra-annual day
ted.km.intraday <- ted.intra.time.p / mean(ted$space) #this is like dividing by zero as space is not significant in ted.
tal.km.intraday <- tal.intra.time.p / mean(tal$space)
ted.ecm.km.intraday <- ted.ecm.intra.time.p / mean(ted.ecm$space)
tal.ecm.km.intraday <- tal.ecm.intra.time.p / mean(tal.ecm$space)
ted.sap.km.intraday <- ted.sap.intra.time.p / mean(ted.sap$space)
tal.sap.km.intraday <- tal.sap.intra.time.p / mean(tal.sap$space)
#ted.km.intraday.sd <- sd(ted$doy / ted$space)
#tal.km.intraday.sd <- sd(tal$doy / tal$space)

#How many inter-annual days is an intra-annual day?
ted.days.doy    <- ted.intra.time.p / ted.inter.time.p
tal.days.doy    <- tal.intra.time.p / tal.inter.time.p #also like dividing by zero.
ted.ecm.days.doy    <- ted.ecm.intra.time.p / ted.ecm.inter.time.p
tal.ecm.days.doy    <- tal.ecm.intra.time.p / tal.ecm.inter.time.p
ted.sap.days.doy    <- ted.sap.intra.time.p / ted.sap.inter.time.p
tal.sap.days.doy    <- tal.sap.intra.time.p / tal.sap.inter.time.p
ted.days.doy.sd <-   sd(ted$doy / ted$time)
tal.days.doy.sd <-   sd(tal$doy / tal$time)


#table 
site <- c('BCI Trees','La Selva Bacteria',
          'Nort American Soil Fungi','Global Northern Temperate Soil Fungi',
          'Nort American Ectomycorrhizal Fungi','Global Northern Temperate Ectomycorrhizal Fungi',
          'Nort American Saprotrophic Fungi','Global Northern Temperate Saprotrophic Fungi')
#km.interday <- c(paste0(round(bci.km.interday,1),' +/- ',round(bci.km.interday.sd,1),' kilometers'),
#                 paste0(round(kiv.km.interday,1),' +/- ',round(kiv.km.interday.sd,1),' kilometers'),
#                 paste0(round(tal.km.interday,1),' +/- ',round(tal.km.interday.sd,1),' kilometers'),
#                 paste0(round(ted.km.interday,1),' +/- ',round(ted.km.interday.sd,1),' kilometers'))

#intra.days.to.km <- c('NA',
#                      'NA',
#                      paste0(round(tal.km.intraday,1),' +/- ',round(tal.km.intraday.sd,1),' kilometers'),
#                      paste0(round(ted.km.intraday,1),' +/- ',round(ted.km.intraday.sd,1),' kilometers'))

#inter.to.intra.days <- c('NA',
#                         'NA',
#                         paste0(round(tal.days.doy,1),' +/- ',round(tal.days.doy.sd,1),' days'),
#                         paste0(round(ted.days.doy,1),' +/- ',round(ted.days.doy.sd,1),' days'))
 
#data.frame(site,km.interday, intra.days.to.km,inter.to.intra.days)

#without uncertainty estimates
km.interday <- c(paste0(round(bci.km.interday,1),' kilometers'),
                 paste0(round(kiv.km.interday,1),' kilometers'),
                 #paste0(round(tal.km.interday,1),' kilometers'),
                 'NA', #no tal all interannual effect
                 paste0(round(ted.km.interday,1),' kilometers'),
                 paste0(round(tal.ecm.km.interday,1),' kilometers'),
                 #paste0(round(ted.ecm.km.interday,1),'kilometers'),
                 'NA', #no ted ecm interannual effect.
                 #paste0(round(tal.sap.km.interday,1),' kilometers'),
                 'NA', #no tal ecm interannual effect.
                 #paste0(round(ted.sap.km.interday,1),' kilometers')
                 'NA' #no ted sap interannual effect.
                 )

intra.days.to.km <- c('NA',
                      'NA',
                      paste0(round(tal.km.intraday,1),' kilometers'),
                      paste0(round(ted.km.intraday,1),' kilometers'),
                      paste0(round(tal.ecm.km.intraday,1),' kilometers'),
                      #paste0(round(ted.ecm.km.intraday,1),' kilometers'),
                      'NA', #no ted ecm intraannual effect.
                      paste0(round(tal.sap.km.intraday,1),' kilometers'),
                      #paste0(round(ted.sap.km.intraday,1),' kilometers')
                      'NA'  #no ted sap intraannual effect.
                      )

inter.to.intra.days <- c('NA',
                         'NA',
                         #paste0(round(tal.days.doy,1),' days'),
                         'NA', #no tal all intra effect.
                         paste0(round(ted.days.doy,1),' days'),
                         paste0(round(tal.ecm.days.doy,1),' days'),
                         #paste0(round(ted.ecm.days.doy,1),' days'),
                         'NA', #no ted ecm intra or inter effect.
                         #paste0(round(tal.sap.days.doy,1),' days'),
                         'NA', #no tal sap inter effect.
                         #paste0(round(ted.sap.days.doy,1),' days')
                         'NA'  #no ted sap inter or intra effect.
                         )

data.frame(site, intra.days.to.km, km.interday, inter.to.intra.days)