#Figure 2 Soil Fungi fits to space, intra- and inter-annual time, no environmental covariates.
#WITHOUT environmental covariates.
#clear R environment.
rm(list=ls())
source('paths.r')
library(wesanderson)
cols <- wes_palette("Moonrise3", 5)

#set output path, load data anda bootstrap results.----
#set figure save destination.
figure_file.path <- fig_3.path

#grab Leho Tedersoo and Jenny Talbot fitted vs. observed values based on MRM model.
d.lt <- readRDS(ted_model_output.path)
d.jt <- readRDS(tal_model_output.path)

#break out data and r.sq
mod.lt <- d.lt$st.mrm
mod.jt <- d.jt$st.mrm
d.jt.rsq <- round(summary(d.jt$st.lm)$r.squared,2)
d.lt.rsq <- round(summary(d.lt$st.lm)$r.squared,2)
d.jt <- d.jt$st.dat
d.lt <- d.lt$st.dat

#grab bootstrap parameter values for reporting
#TED results
lt.p <- readRDS(ted_bootstrap_st.only_output.path)
lt.space.mean      <- formatC(mean(lt.p$space)        , format = "e", digits = 2)
lt.space.ci        <- formatC(1.96*sd(lt.p$space)     , format = "e", digits = 2)
lt.time.intra.mean <- formatC(mean(lt.p$seas_pos)     , format = "e", digits = 2)
lt.time.intra.ci   <- formatC(1.96*sd(lt.p$seas_pos)  , format = "e", digits = 2)
lt.time.inter.mean <- formatC(mean(lt.p$epoch.date)   , format = "e", digits = 2)
lt.time.inter.ci   <- formatC(1.96*sd(lt.p$epoch.date), format = "e", digits = 2)

#TAL results
#get time interaction
jt.p <- readRDS(tal_bootstrap_st.only_output.path)
jt.space.mean      <- formatC(mean(jt.p$space)        , format = "e", digits = 2)
jt.space.ci        <- formatC(1.96*sd(jt.p$space)     , format = "e", digits = 2)
jt.time.intra.mean <- formatC(mean(jt.p$seas_pos)     , format = "e", digits = 2)
jt.time.intra.ci   <- formatC(1.96*sd(jt.p$seas_pos)  , format = "e", digits = 2)
jt.time.inter.mean <- formatC(mean(jt.p$epoch.date)   , format = "e", digits = 2)
jt.time.inter.ci   <- formatC(1.96*sd(jt.p$epoch.date), format = "e", digits = 2)



###########THIS IS WHERE YOU STOPPED COLIN.#######
#1. Make sure bootstrap and true parmaeter estimates agree.
#2. do not report N.S. parameter estimates.
#3. Make sure to update data paths to get st only bootstrap results.


#subset for playing with plotting
d.lt <- d.lt[1:1000,]
d.jt <- d.jt[1:1000,]


#save dimensions, destination.----
png(filename=figure_file.path,width=8,height=5.5,units='in',res=300)

#begin plot.----
par(mfrow=c(2,4),
    oma = c(4,4, 0, 0),
    mar = c(2,2,3,2))

####Plot of LT fit####
#set y and x predictors for LT
y <- log(d.lt$bray.sim)
y.space <- d.lt$st.space.norm
y.time  <- d.lt$st.inter.norm
y.time.intra <- d.lt$st.intra.norm
y.time.inter <- d.lt$st.inter.norm
x1 <- d.lt$st.fitted
x2 <- d.lt$space
x3 <- d.lt$doy
x4 <- d.lt$time
r.sq <- d.lt.rsq

# Plot Fitted vs. Observed
plot(y ~ x1, cex = 0.05, 
     xlab = "Fitted", ylab = "Observed", col=cols[1])
abline(0,1, lty = 2, lwd = 3)
lines(lowess(y ~ x1), lty=2, col='green', lwd = 2)
mtext('fitted', side = 1, cex = 0.7, line = 2)
mtext('total model fit', side =3 , cex = 0.7, line = -1.2, adj = 0.05)
mtext('a.', side = 1, cex = 0.7, adj = .025, line = -1.1)

# Plot R2, title, and predictors
legend("bottomright", bty = "n", 
       c(as.expression(bquote("Adjusted" ~ R^2 ~ "=" ~.(r.sq))), 
         as.expression(bquote("p-value < 0.001"))), 
       cex = 0.8)

#LT community similarity ~ space
plot(y.space ~ x2, cex = 0.05, 
     xlab = "Space (m)", ylab = "Bray-Curtis Similarity", col = cols[2])
abline(lm(y.space ~ x2), lty = 2, lwd = 3)
lines(lowess(y.space ~ x2), lty=2, col='green', lwd = 2)
#mtext("Global Northern Temperate Soil Fungi", font = 2, cex = 1, line = 0.5)
mtext('Space (km)', side = 1, cex = 0.7, line = 2)
mtext('space', side =3 , cex = 0.7, line = -1.2, adj = 0.5)
mtext('b.', side = 1, cex = 0.7, adj = .025, line = -1.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -2.4, cex = 0.7)
mtext(as.expression(paste(lt.space.mean,'+/-',lt.space.ci)), side = 3, line = -3.2, cex = 0.7)


#LT community similarity ~ time - intraannual
plot(y.time.intra ~ x3, cex = 0.05, col = cols[3])
abline(lm(y.time.intra ~ x3), lty = 2, lwd = 3)
lines(lowess(y.time.intra ~ x3), lty=2, col='green', lwd = 2)
mtext('Time (days)', side = 1, cex = 0.7, line = 2)
mtext('time (intra-annual)', side =3 , cex = 0.7, line = -1.2, adj = 0.5)
mtext('c.', side = 1, cex = 0.7, adj = .025, line = -1.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -2.4, cex = 0.7)
mtext(as.expression(paste(lt.time.intra.mean,'+/-',lt.time.intra.ci)), side = 3, line = -3.2, cex = 0.7)

#LT community similarity ~ time - inter annual
plot(y.time.inter ~ x4, cex = 0.05, col = cols[5])
abline(lm(y.time.inter ~ x4), lty = 2, lwd = 3)
lines(lowess(y.time.inter ~ x4), lty=2, col='green', lwd = 2)
mtext('Time (days)', side = 1, cex = 0.7, line = 2)
mtext('time (inter-annual)', side =3 , cex = 0.7, line = -1.2, adj = 0.5)
mtext('d.', side = 1, cex = 0.7, adj = .025, line = -1.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -2.4, cex = 0.7)
mtext(as.expression(paste(lt.time.inter.mean,'+/-',lt.time.inter.ci)), side = 3, line = -3.2, cex = 0.7)


#set y and x predictors for JT
y <- log(d.jt$bray.sim)
y.space <- d.jt$st.space.norm
y.time  <- d.jt$st.inter.norm
y.time.intra <- d.jt$st.intra.norm
y.time.inter <- d.jt$st.inter.norm
x1 <- d.jt$st.fitted
x2 <- d.jt$space
x3 <- d.jt$doy
x4 <- d.jt$time
r.sq <- d.jt.rsq

# Plot Fitted vs. Observed
plot(y ~ x1, cex = 0.05, 
     xlab = "Fitted", ylab = "Observed", col=cols[1])
abline(0,1, lty = 2, lwd = 3)
lines(lowess(y ~ x1), lty=2, col='green', lwd = 2)
#mtext("Community Similarity: North American Soil Fungi", font = 2, cex = 0.7, line = 0.1)
mtext('fitted', side = 1, cex = 0.7, line = 2)
mtext('total model fit', side =3 , cex = 0.7, line = -1.2, adj = 0.05)
mtext('e.', side = 1, cex = 0.7, adj = .025, line = -1.1)

# Plot R2, title, and predictors
legend("bottomright", bty = "n", 
       c(as.expression(bquote("Adjusted" ~ R^2 ~ "=" ~.(r.sq))), 
         as.expression(bquote("p-value < 0.001"))), 
       cex = 0.8)

#JT community similarity ~ space
plot(y.space ~ x2, cex = 0.05, 
     xlab = "Space (m)", ylab = "Bray-Curtis Similarity", col = cols[2])
abline(lm(y.space ~ x2), lty = 2, lwd = 3)
lines(lowess(y.space ~ x2), lty=2, col='green', lwd = 2)
#mtext("North American Soil Fungi", font = 2, cex = 1, line = 0.5)
mtext('Space (km)', side = 1, cex = 0.7, line = 2)
mtext('space', side =3 , cex = 0.7, line = -1.2, adj = 0.5)
mtext('f.', side = 1, cex = 0.7, adj = .025, line = -1.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -2.4, cex = 0.7)
mtext(as.expression(paste(jt.space.mean,'+/-',jt.space.ci)), side = 3, line = -3.2, cex = 0.7)

#JT community similarity ~ time - intra-annual
plot(y.time.intra ~ x3, cex = 0.05, col = cols[3])
abline(lm(y.time.intra ~ x3), lty = 2, lwd = 3)
lines(lowess(y.time.intra ~ x3), lty=2, col='green', lwd = 2)
mtext('Time (days)', side = 1, cex = 0.7, line = 2)
mtext('time (intra-annual)', side =3 , cex = 0.7, line = -1.2, adj = 0.5)
mtext('g.', side = 1, cex = 0.7, adj = .025, line = -1.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -2.4, cex = 0.7)
mtext(as.expression(paste(jt.time.intra.mean,'+/-',jt.time.intra.ci)), side = 3, line = -3.2, cex = 0.7)

#JT community similarity ~ time - inter-annual
plot(y.time.inter ~ x4, cex = 0.05, col = cols[5])
abline(lm(y.time.inter ~ x4), lty = 2, lwd = 3)
lines(lowess(y.time.inter ~ x4), lty=2, col='green', lwd = 2)
mtext('Time (days)', side = 1, cex = 0.7, line = 2)
mtext('time (inter-annual)', side =3 , cex = 0.7, line = -1.2, adj = 0.5)
mtext('h.', side = 1, cex = 0.7, adj = .025, line = -1.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -2.4, cex = 0.7)
mtext(as.expression(paste(jt.time.inter.mean,'+/-',jt.time.inter.ci)), side = 3, line = -3.2, cex = 0.7)


#set outer y-axis label
mtext('log transformed Bray-Curtis Similarity', side=2, out=T, cex=1, line = 1)
mtext('Global Northern Temperate Soil Fungi', side = 3, out = T, cex = 1, font = 2, line = -2.5)
mtext('North American Soil Fungi', side = 3, out = T, cex = 1, font = 2, line = -21.5)

#end plot----
dev.off()
