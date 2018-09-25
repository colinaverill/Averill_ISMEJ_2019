#plotting CS fitted vs. observed, space and time for all validation data sets.
#clear envrionment, load packages.
rm(list=ls())
source('paths.r')
library(wesanderson)

#load data, setup output path.----
output.path <- fig_S4.path

#load and organize data.
    bci <- readRDS(bci_fitted.path)
kiv.bac <- readRDS(kiv_bac_figure_data.path)
kiv.fun <- readRDS(kiv_fun_figure_data.path)
neo.har <- readRDS(harv_neon_fig_data.path)

#subset bci data - too much to plot.
bci[[1]] <- bci[[1]][sample(nrow(bci[[1]]),5000),]

#load bootstrap results.
bci.boot     <- readRDS(bci_all_boostrap.path)
kiv.bac.boot <-  data.frame(readRDS(kiv_bac_bootstrap.path))
kiv.fun.boot <-  data.frame(readRDS(kiv_fun_bootstrap.path))
neo.har.boot <-  data.frame(readRDS(harv_neon_all_boostrap.path))


#Begin Plot: png call for saving.----
png(filename=output.path,width=8,height=10,units='in',res=300)

#plot formatting.----
cols <- wes_palette("Moonrise3", 5) #colors
par(mfrow = c(4,3))                 #4x3 plot
par(oma = c(1,3,1,1))               #outer margins
par(mai = c(rep(0.4,4)))            #inner margins
p.cex <- 0.15                       #point size.

#BCI plots.----
name <- 'BCI Trees'
dat <- bci
dat.boot <- bci.boot
d <- dat[[1]]
rsq <- round(dat$summary$rsq,2)
pval <- round(dat$summary$pval,3)
m.space <- mean(dat.boot$space)
m.time <- mean(dat.boot$time)
space.95 <- 1.96*(sd(dat.boot$space))
time.95 <- 1.96*(sd(dat.boot$time))
panel.lab <- c('a.','b.','c.')

#format parameters to correct scientific notation.
m.space <- format(signif(m.space ,3), scientific = T)
m.time <- format(signif(m.time  ,3), scientific = T)
space.95 <- format(signif(space.95,3), scientific = T)
time.95 <- format(signif(time.95 ,3), scientific = T)

# Plot Fitted vs. Observed
plot((d$bray.sim) ~ (d$fitted), cex = p.cex, xlab = NA, ylab = NA, col = cols[1])
abline(0,1, lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$fitted), lty=2, col='green', lwd = 2)
mtext(paste0("Community Similarity: ",name), font = 2, cex = 0.7, line = 0.1)
lab<- bquote('Adjusted R'^{2} ~ "=" ~ .(rsq))
mtext(lab, side = 3, line = -1.4, adj=0.1, cex = 0.7)
mtext(paste0('p-value < ',pval), side = 3, line = -2.2, adj=0.1, cex = 0.7)
mtext('observed', side=2, cex = 0.7, line = 2)
mtext('fitted'  , side = 1, cex = 0.7, line = 2)
mtext(panel.lab[1], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Plot Similarity vs. Space
plot((d$bray.sim) ~ (d$space), cex = p.cex, xlab = NA, ylab = NA, col = cols[2])
abline(lm(d$bray.sim ~ d$space), lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$space), lty=2, col='green', lwd = 2)
mtext(paste0("Space: ",name), font = 2, cex = 0.7, line = 0.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -1.4, cex = 0.7)
mtext(as.expression(paste(m.space,'+/-',space.95)), side = 3, line = -2.2, cex = 0.7)
mtext('Space (m)', side = 1, cex = 0.7, line = 2)
mtext(panel.lab[2], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Plot Similarity vs. Time
plot((d$bray.sim) ~ (d$time), cex = p.cex, xlab = NA, ylab = NA, col = cols[3])
abline(lm(d$bray.sim ~ d$time), lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$time), lty=2, col='green', lwd = 2)
mtext(paste0("Time: ",name), font = 2, cex = 0.7, line = 0.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -1.4, cex = 0.7)
mtext(as.expression(paste(m.time,'+/-',time.95)), side = 3, line = -2.2, cex = 0.7)
mtext('Time (days)', side = 1, cex = 0.7, line = 2)
mtext(panel.lab[3], side = 1, cex = 0.7, adj = .975, line = -1.1)

#La Selva Bacteria plots.----
name <- 'La Selva Bacteria'
dat <- kiv.bac
dat.boot <- kiv.bac.boot
d <- dat[[1]]
rsq <- round(dat$summary$rsq,2)
pval <- round(dat$summary$pval,3)
m.space <- mean(dat.boot$space)
m.time <- mean(dat.boot$time)
space.95 <- 1.96*(sd(dat.boot$space))
time.95 <- 1.96*(sd(dat.boot$time))
panel.lab <- c('d.','e.','f.')

#format parameters to correct scientific notation.
m.space <- format(signif(m.space ,3), scientific = T)
m.time <- format(signif(m.time  ,3), scientific = T)
space.95 <- format(signif(space.95,3), scientific = T)
time.95 <- format(signif(time.95 ,3), scientific = T)

# Plot Fitted vs. Observed
plot((d$bray.sim) ~ (d$fitted), cex = p.cex, xlab = NA, ylab = NA, col = cols[1])
abline(0,1, lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$fitted), lty=2, col='green', lwd = 2)
mtext(paste0("Community Similarity: ",name), font = 2, cex = 0.7, line = 0.1)
lab<- bquote('Adjusted R'^{2} ~ "=" ~ .(rsq))
mtext(lab, side = 3, line = -1.4, adj=0.1, cex = 0.7)
mtext(paste0('p-value < ',pval), side = 3, line = -2.2, adj=0.1, cex = 0.7)
mtext('observed', side=2, cex = 0.7, line = 2)
mtext('fitted'  , side = 1, cex = 0.7, line = 2)
mtext(panel.lab[1], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Plot Similarity vs. Space
plot((d$bray.sim) ~ (d$space), cex = p.cex, xlab = NA, ylab = NA, col = cols[2])
abline(lm(d$bray.sim ~ d$space), lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$space), lty=2, col='green', lwd = 2)
mtext(paste0("Space: ",name), font = 2, cex = 0.7, line = 0.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -1.4, cex = 0.7)
mtext(as.expression(paste(m.space,'+/-',space.95)), side = 3, line = -2.2, cex = 0.7)
mtext('Space (m)', side = 1, cex = 0.7, line = 2)
mtext(panel.lab[2], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Plot Similarity vs. Time
plot((d$bray.sim) ~ (d$time), cex = p.cex, xlab = NA, ylab = NA, col = cols[3])
abline(lm(d$bray.sim ~ d$time), lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$time), lty=2, col='green', lwd = 2)
mtext(paste0("Time: ",name), font = 2, cex = 0.7, line = 0.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -1.4, cex = 0.7)
mtext(as.expression(paste(m.time,'+/-',time.95)), side = 3, line = -2.2, cex = 0.7)
mtext('Time (days)', side = 1, cex = 0.7, line = 2)
mtext(panel.lab[3], side = 1, cex = 0.7, adj = .975, line = -1.1)

#La Selva Fungi plots.----
name <- 'La Selva Fungi'
dat <- kiv.fun
dat.boot <- kiv.fun.boot
d <- dat[[1]]
rsq <- round(dat$summary$rsq,2)
pval <- round(dat$summary$pval,3)
m.space <- mean(dat.boot$space)
m.time <- mean(dat.boot$time)
space.95 <- 1.96*(sd(dat.boot$space))
time.95 <- 1.96*(sd(dat.boot$time))
panel.lab <- c('d.','e.','f.')

#format parameters to correct scientific notation.
m.space <- format(signif(m.space ,3), scientific = T)
m.time <- format(signif(m.time  ,3), scientific = T)
space.95 <- format(signif(space.95,3), scientific = T)
time.95 <- format(signif(time.95 ,3), scientific = T)


# Plot Fitted vs. Observed
plot((d$bray.sim) ~ (d$fitted), cex = p.cex, xlab = NA, ylab = NA, col = cols[1])
abline(0,1, lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$fitted), lty=2, col='green', lwd = 2)
mtext(paste0("Community Similarity: ",name), font = 2, cex = 0.7, line = 0.1)
lab<- bquote('Adjusted R'^{2} ~ "=" ~ .(rsq))
mtext(lab, side = 3, line = -1.4, adj=0.1, cex = 0.7)
mtext(paste0('p-value < ',pval), side = 3, line = -2.2, adj=0.1, cex = 0.7)
mtext('observed', side=2, cex = 0.7, line = 2)
mtext('fitted'  , side = 1, cex = 0.7, line = 2)
mtext(panel.lab[1], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Plot Similarity vs. Space
plot((d$bray.sim) ~ (d$space), cex = p.cex, xlab = NA, ylab = NA, col = cols[2])
abline(lm(d$bray.sim ~ d$space), lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$space), lty=2, col='green', lwd = 2)
mtext(paste0("Space: ",name), font = 2, cex = 0.7, line = 0.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -1.4, cex = 0.7)
mtext(as.expression(paste(m.space,'+/-',space.95)), side = 3, line = -2.2, cex = 0.7)
mtext('Space (m)', side = 1, cex = 0.7, line = 2)
mtext(panel.lab[2], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Plot Similarity vs. Time
plot((d$bray.sim) ~ (d$time), cex = p.cex, xlab = NA, ylab = NA, col = cols[3])
abline(lm(d$bray.sim ~ d$time), lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$time), lty=2, col='green', lwd = 2)
mtext(paste0("Time: ",name), font = 2, cex = 0.7, line = 0.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -1.4, cex = 0.7)
mtext(as.expression(paste(m.time,'+/-',time.95)), side = 3, line = -2.2, cex = 0.7)
mtext('Time (days)', side = 1, cex = 0.7, line = 2)
mtext(panel.lab[3], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Harvard Forest Fungi Plots.----
name <- 'Harvard Forest Fungi'
dat <- neo.har
dat.boot <- neo.har.boot
d <- dat[[1]]
rsq <- round(dat$summary$rsq,2)
pval <- round(dat$summary$pval,3)
m.space <- mean(dat.boot$space)
m.time <- mean(dat.boot$time)
space.95 <- 1.96*(sd(dat.boot$space))
time.95 <- 1.96*(sd(dat.boot$time))
panel.lab <- c('d.','e.','f.')

#format parameters to correct scientific notation.
m.space <- format(signif(m.space ,3), scientific = T)
m.time <- format(signif(m.time  ,3), scientific = T)
space.95 <- format(signif(space.95,3), scientific = T)
time.95 <- format(signif(time.95 ,3), scientific = T)


# Plot Fitted vs. Observed
plot((d$bray.sim) ~ (d$fitted), cex = p.cex, xlab = NA, ylab = NA, col = cols[1])
abline(0,1, lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$fitted), lty=2, col='green', lwd = 2)
mtext(paste0("Community Similarity: ",name), font = 2, cex = 0.7, line = 0.1)
lab<- bquote('Adjusted R'^{2} ~ "=" ~ .(rsq))
mtext(lab, side = 3, line = -1.4, adj=0.1, cex = 0.7)
mtext(paste0('p-value < ',pval), side = 3, line = -2.2, adj=0.1, cex = 0.7)
mtext('observed', side=2, cex = 0.7, line = 2)
mtext('fitted'  , side = 1, cex = 0.7, line = 2)
mtext(panel.lab[1], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Plot Similarity vs. Space
plot((d$bray.sim) ~ (d$space), cex = p.cex, xlab = NA, ylab = NA, col = cols[2])
abline(lm(d$bray.sim ~ d$space), lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$space), lty=2, col='green', lwd = 2)
mtext(paste0("Space: ",name), font = 2, cex = 0.7, line = 0.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -1.4, cex = 0.7)
mtext(as.expression(paste(m.space,'+/-',space.95)), side = 3, line = -2.2, cex = 0.7)
mtext('Space (m)', side = 1, cex = 0.7, line = 2)
mtext(panel.lab[2], side = 1, cex = 0.7, adj = .975, line = -1.1)

#Plot Similarity vs. Time
plot((d$bray.sim) ~ (d$time), cex = p.cex, xlab = NA, ylab = NA, col = cols[3])
abline(lm(d$bray.sim ~ d$time), lty = 2, lwd = 3)
lines(lowess((d$bray.sim) ~ d$time), lty=2, col='green', lwd = 2)
mtext(paste0("Time: ",name), font = 2, cex = 0.7, line = 0.1)
mtext(as.expression(paste('parameter estimate')), side = 3, line = -1.4, cex = 0.7)
mtext(as.expression(paste(m.time,'+/-',time.95)), side = 3, line = -2.2, cex = 0.7)
mtext('Time (days)', side = 1, cex = 0.7, line = 2)
mtext(panel.lab[3], side = 1, cex = 0.7, adj = .975, line = -1.1)



#outer labels.----
#set outer y-axis label
mtext('Bray-Curtis Similarity', side=2, out=T, cex=1.5, line = 0)

#end plot.----
dev.off()