#analyzing tedersoo 2014 without ridiculous shit. temperate analysis only for final publication.
rm(list=ls())
source('paths.r')
source('Scripts/functions/variable_pred_data.r')

#Load data, set output path.----
map <- readRDS(ted_clean_map.path)
otu <- readRDS(ted_clean_otu.path)
output.path <- ted_model_output.path

#Generate analysis matrices.----
#bray dependent variable.
t.bray.dis <- vegan::vegdist(t(otu), method = 'bray')
t.bray.sim <- 1 - as.matrix(t.bray.dis)
t.bray.sim <- t.bray.sim + min(t.bray.sim[t.bray.sim > 0])

#covariates.
#space is special.
points <- cbind(map$longitude,map$latitude)
points <- as.matrix(points)
space.m <- geosphere::distm(points)
#convert to km- necessary for MRM to run.
space.m <- space.m/1000

#remaining covariates.
cov <- map[,c('epoch.date','doy','seas_pos','seas_pos.evi2','Moisture','C_N','pH','C','LogP','NPP','MAP','MAT','MAT_CV','MAP_CV')]
cov.matrices <- list()
for(i in 1:ncol(cov)){
  cov.matrices[[i]] <- as.matrix(dist(cov[,i], method="euclidean", diag=F, upper=F))
}
names(cov.matrices) <- colnames(cov)
cov.matrices$space <- space.m
cov.matrices$bray.sim <- t.bray.sim
dat <- lapply(cov.matrices, ecodist::lower)
dat <- as.data.frame(lapply(dat, c))

#RUN MODELS.----
#raw space-time (st) models
st.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + seas_pos , data = dat);st.mrm
st     <-           lm(log(bray.sim) ~ space + epoch.date + seas_pos , data = dat)
#ENV models.
env.mrm <- ecodist::MRM(log(bray.sim) ~  C_N + C + pH + Moisture + NPP + MAP + MAT + MAP_CV + MAT_CV, data = dat);env.mrm
env.mrm <- ecodist::MRM(log(bray.sim) ~  C_N     + pH + Moisture             + MAT + MAP_CV         , data = dat);env.mrm
env     <-           lm(log(bray.sim) ~  C_N     + pH + Moisture             + MAT + MAP_CV         , data = dat)
#st.env
#doy
#st.env.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + doy           + C_N + pH + Moisture + NPP + MAT + MAP + MAP_CV + MAT_CV, data = dat);st.env.mrm
#st.env.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + doy           + C_N + pH                  + MAT       + MAP_CV         , data = dat);st.env.mrm
#st.env     <-           lm(log(bray.sim) ~ space + epoch.date + doy           + C_N + pH                  + MAT       + MAP_CV         , data = dat)
#evi2 
#st.env.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + seas_pos.evi2 + C_N + pH + Moisture + NPP + MAT + MAP + MAP_CV + MAT_CV, data = dat);st.env.mrm
#st.env.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + seas_pos.evi2 + C_N + pH +                  MAT       + MAP_CV         , data = dat);st.env.mrm
#st.env     <-           lm(log(bray.sim) ~ space + epoch.date + seas_pos.evi2 + C_N + pH +                  MAT       + MAP_CV         , data = dat)
#ndvi
st.env.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + seas_pos      + C_N + pH + Moisture + NPP + MAT + MAP + MAP_CV + MAT_CV, data = dat);st.env.mrm
st.env.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + seas_pos      + C_N + pH                  + MAT       + MAP_CV         , data = dat);st.env.mrm
st.env     <-           lm(log(bray.sim) ~ space + epoch.date + seas_pos      + C_N + pH                  + MAT       + MAP_CV         , data = dat)

#Generate predicted vs. observed output.----
#get "mean-ed" data sets for prediction.
    st_pred_space <- variable_pred_data(dat, st    , 'space'     )
    st_pred_inter <- variable_pred_data(dat, st    , 'epoch.date')
    st_pred_intra <- variable_pred_data(dat, st    , 'seas_pos'  )
st.env_pred_space <- variable_pred_data(dat, st.env, 'space'     )
st.env_pred_inter <- variable_pred_data(dat, st.env, 'epoch.date')
st.env_pred_intra <- variable_pred_data(dat, st.env, 'seas_pos')

#get fitted and model normalized values.
    st.dat <- dat
st.env.dat <- dat

#fitted
    st.dat$fitted <- fitted(st)
st.env.dat$fitted <- fitted(st.env)

#st normalized
st.dat$space.norm <- log(st.dat$bray.sim) - fitted(st) + predict(st, st_pred_space)
st.dat$intra.norm <- log(st.dat$bray.sim) - fitted(st) + predict(st, st_pred_intra)
st.dat$inter.norm <- log(st.dat$bray.sim) - fitted(st) + predict(st, st_pred_inter)
#st.env normalized
st.env.dat$space.norm <- log(st.env.dat$bray.sim) - fitted(st.env) + predict(st.env, st.env_pred_space)
st.env.dat$intra.norm <- log(st.env.dat$bray.sim) - fitted(st.env) + predict(st.env, st.env_pred_intra)
st.env.dat$inter.norm <- log(st.env.dat$bray.sim) - fitted(st.env) + predict(st.env, st.env_pred_inter)

#bind up data and rsq values for downstream plotting.----
st.out <- list(st,env,st.env,st.dat,st.env.dat,st.mrm,env.mrm,st.env.mrm)
names(st.out) <- c('st.lm','env.lm','st.env.lm','st.dat','st.env.dat','st.mrm','env.mrm','st.env.mrm')

#Save output.----
saveRDS(st.out,output.path)

