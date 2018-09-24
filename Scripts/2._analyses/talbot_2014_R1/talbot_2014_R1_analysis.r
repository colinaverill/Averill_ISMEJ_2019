#analyzing tedersoo 2014.
rm(list=ls())
source('paths.r')
source('Scripts/functions/variable_pred_data.r')

#LOAD DATA, set output path.----
map <- readRDS(tal_clean_map.path)
otu <- readRDS(tal_clean_otu.path)
output.path <- tal_model_output.path

#drop any old data.table formatting.
map <- as.data.frame(map)
otu <- as.data.frame(otu)

#Generate analysis matrices.----
#Change coviaraiate names to match tedersoo and below code.
old.cov.names <- c('epoch.date','doy','seas_pos','Perc.Soil.Moisture','CNRatio','pH','Perc.C','NPP','MAP','MAT','MAT_CV','MAP_CV')
new.cov.names <- c('epoch.date','doy','seas_pos','Moisture','C_N','pH','C','NPP','MAP','MAT','MAT_CV','MAP_CV')
for(i in 1:length(old.cov.names)){colnames(map)[which(names(map) == old.cov.names[i])] <- new.cov.names[i]}

#get complete cases.
to_keep <- c(new.cov.names,'latitude.dd','longitude.dd','Mapping.ID','Sample.ID')
map <- map[,colnames(map) %in% to_keep]
map <- map[complete.cases(map),]
otu <- otu[,colnames(otu) %in% map$Mapping.ID]
map <- map[order(map$Mapping.ID),]
otu <- otu[,order(colnames(otu))]

#generate covariates distance matrices.
cov <- map[,colnames(map) %in% new.cov.names]
cov.matrices <- list()
for(i in 1:ncol(cov)){
  cov.matrices[[i]] <- as.matrix(dist(cov[,i], method="euclidean", diag=F, upper=F))
}
names(cov.matrices) <- colnames(cov)

#space is special.
points <- cbind(map$longitude.dd,map$latitude.dd)
points <- as.matrix(points)
space.m <- geosphere::distm(points)
#convert to km- necessary for MRM to run.
space.m <- space.m/1000

#bray dependent variable.
t.bray.dis <- vegan::vegdist(t(otu), method = 'bray')
t.bray.sim <- 1 - as.matrix(t.bray.dis)
t.bray.sim <- t.bray.sim + min(t.bray.sim[t.bray.sim > 0])

#final analysis frame.
cov.matrices$space <- space.m
cov.matrices$bray.sim <- t.bray.sim
dat <- lapply(cov.matrices, ecodist::lower)
dat <- as.data.frame(lapply(dat, c))


#RUN MODELS.----
#raw space-time (st) models
st.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + doy      , data = dat);st.mrm
st.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + seas_pos , data = dat);st.mrm
st     <-           lm(log(bray.sim) ~ space + epoch.date + seas_pos , data = dat)
#ENV models.
env.mrm <- ecodist::MRM(log(bray.sim) ~  C_N + C + pH + Moisture + NPP + MAP + MAT + MAP_CV + MAT_CV, data = dat);env.mrm
env.mrm <- ecodist::MRM(log(bray.sim) ~      + C + pH +            NPP + MAP + MAT + MAP_CV + MAT_CV, data = dat);env.mrm
env     <-           lm(log(bray.sim) ~      + C + pH +            NPP + MAP + MAT + MAP_CV + MAT_CV, data = dat)
#st.env
st.env.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + seas_pos + C_N + C + pH + Moisture + NPP + MAP + MAT + MAP_CV + MAT_CV, data = dat);st.env.mrm
st.env.mrm <- ecodist::MRM(log(bray.sim) ~ space + epoch.date + seas_pos       + C + pH + Moisture       + MAP + MAT + MAP_CV + MAT_CV, data = dat);st.env.mrm
st.env     <-           lm(log(bray.sim) ~ space + epoch.date + seas_pos       + C + pH + Moisture       + MAP + MAT + MAP_CV + MAT_CV, data = dat)

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
st.out <- list(st,env,st.env,st.dat,st.env.dat,st.mrm,st.mrm,env.mrm,st.env.mrm)
names(st.out) <- c('st.lm','env.lm','st.env.lm',
                   'st.dat','env.dat','st.env.dat',
                   'st.mrm','env.mrm','st.env.mrm')

#Save output.----
saveRDS(st.out,output.path)

