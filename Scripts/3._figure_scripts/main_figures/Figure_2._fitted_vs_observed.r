#predicted vs. observed plots for space time analysis, September 2018.
rm(list=ls())
source('paths.r')
library(wesanderson)
cols <- wes_palette("Moonrise3", 5)

#Load data, set output path.----
output.path <- fig_2.path #output path.

#load data.
d.lt <- readRDS(ted_model_output.path)
d.jt <- readRDS(tal_model_output.path)

#get actual observations.
d.lt.st     <- d.lt$st.dat
d.lt.st.env <- d.lt$st.env.dat
d.jt.st     <- d.jt$st.dat
d.jt.st.env <- d.jt$st.env.dat

#subset for playing with plotting
#d.lt.st     <- d.lt.st[1:1000,]
#d.lt.st.env <- d.lt.st.env[1:1000,]
#d.jt.st     <- d.jt.st[1:1000,]
#d.jt.st.env <- d.jt.st.env[1:1000,]

#get r2 values.
r2.lt.st     <- round(d.lt$st.mrm    $r.squared[1], 2)
r2.lt.st.env <- round(d.lt$st.env.mrm$r.squared[1], 2)
r2.jt.st     <- round(d.jt$st.mrm    $r.squared[1], 2)
r2.jt.st.env <- round(d.jt$st.env.mrm$r.squared[1], 2)


#setup plot export to .png.----
png(filename=output.path,width=7,height=7,units='in',res=300)

#Begin plots.----
#general plotting control.
par(mfrow=c(2,2),
    mar = c(rep(2.5,4)),
    oma = c(3,3,1.3,0.2))
p.cex <- 0.05    #point size
i.cex <- 1.00    #within panel text size.
o.cex <- 1.30    #outer panel text size.
p.col <- cols[1] #point color
#p.col <- 'gray' - if we need gray-scale figure.

####Panel 1- Tedersoo space-time only.
d <- d.lt.st
r.sq <- r2.lt.st
panel.letter <- 'a.'

plot(log(d$bray.sim) ~ d$fitted, cex = p.cex,  xlab = NA, ylab = NA, col=p.col)
abline(0,1, lty = 2, lwd = 3)
lines(lowess(log(d$bray.sim) ~ d$fitted), lty=2, col='green', lwd = 2)
#labels
mtext('space-time only', side=3, cex=i.cex, line=-1.2, adj=0.05)
mtext(panel.letter, side = 1, cex = i.cex, adj = .025, line = -1.1)
# Plot R2, title, and predictors
legend("bottomright", bty = "n", 
       c(as.expression(bquote("Adjusted" ~ R^2 ~ "=" ~.(r.sq))), 
         as.expression(bquote("p-value < 0.001"))), 
       cex = i.cex)

####Panel 2- Tedersoo space-time + environment.
d <- d.lt.st.env
r.sq <- r2.lt.st.env
panel.letter <- 'b.'

plot(log(d$bray.sim) ~ d$fitted, cex = p.cex,  xlab = NA, ylab = NA, col=p.col)
abline(0,1, lty = 2, lwd = 3)
lines(lowess(log(d$bray.sim) ~ d$fitted), lty=2, col='green', lwd = 2)
#labels
mtext('space-time + environment', side=3, cex=i.cex, line=-1.2, adj=0.05)
mtext(panel.letter, side = 1, cex = i.cex, adj = .025, line = -1.1)
# Plot R2, title, and predictors
legend("bottomright", bty = "n", 
       c(as.expression(bquote("Adjusted" ~ R^2 ~ "=" ~.(r.sq))), 
         as.expression(bquote("p-value < 0.001"))), 
       cex = i.cex)

####Panel 3- Talbot space-time only.
d <- d.jt.st
r.sq <- r2.jt.st
panel.letter <- 'c.'

plot(log(d$bray.sim) ~ d$fitted, cex = p.cex,  xlab = NA, ylab = NA, col=p.col)
abline(0,1, lty = 2, lwd = 3)
lines(lowess(log(d$bray.sim) ~ d$fitted), lty=2, col='green', lwd = 2)
#labels
mtext('space-time only', side=3, cex=i.cex, line=-1.2, adj=0.05)
mtext(panel.letter, side = 1, cex = i.cex, adj = .025, line = -1.1)
# Plot R2, title, and predictors
legend("bottomright", bty = "n", 
       c(as.expression(bquote("Adjusted" ~ R^2 ~ "=" ~.(r.sq))), 
         as.expression(bquote("p-value < 0.001"))), 
       cex = i.cex)

####Panel 4- Talbot space-time + environemnt.
d <- d.jt.st.env
r.sq <- r2.jt.st.env
panel.letter <- 'd.'

plot(log(d$bray.sim) ~ d$fitted, cex = p.cex,  xlab = NA, ylab = NA, col=p.col)
abline(0,1, lty = 2, lwd = 3)
lines(lowess(log(d$bray.sim) ~ d$fitted), lty=2, col='green', lwd = 2)
#labels
mtext('space-time + environment', side=3, cex=i.cex, line=-1.2, adj=0.05)
mtext(panel.letter, side = 1, cex = i.cex, adj = .025, line = -1.1)
# Plot R2, title, and predictors
legend("bottomright", bty = "n", 
       c(as.expression(bquote("Adjusted" ~ R^2 ~ "=" ~.(r.sq))), 
         as.expression(bquote("p-value < 0.001"))), 
       cex = i.cex)

####Outer labels.
mtext('Observed log-transformed Bray-Curtis Similarity', side=2, cex=o.cex, line= 0.5 , outer=T)
mtext('Fitted log-transformed Bray-Curtis Similarity'  , side=1, cex=o.cex, line= 1 , outer=T)
mtext('Global Northern Temperate Soil Fungi', side=3, cex=o.cex, line=-1.2 , outer=T)
mtext('North American Soil Fungi',            side=3, cex=o.cex, line=-20.5, outer=T)

#end plot.----
dev.off()
