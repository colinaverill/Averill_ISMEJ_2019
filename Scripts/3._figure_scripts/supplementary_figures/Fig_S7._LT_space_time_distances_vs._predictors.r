#Pairwise regression plots of space, time and predictors.
rm(list=ls())
source('paths.r')
library(data.table)

#set save path, load data, do some data manipulation.----
#set figure save destination.
figure_file.path <- fig_S7.path

#grab Leho Tedersoo and Jenny Talbot fitted vs. observed values based on MRM model.
d.lt <- readRDS(ted_model_output.path)
d.lt <- data.table(d.lt$st.env.dat)

#grab variables of interest.
d <- d.lt
vars  <- list(d$MAT, d$MAP, d$MAT_CV, d$MAP_CV, d$NPP, d$C, d$C_N, d$pH, d$Moisture)
names <- c('MAT','MAP','MAT_CV','MAP_CV','NPP','% C','C:N','pH','soil moisture')
fin <- length(vars)

#subset for testing.
#d.lt <- d.lt[sample(.N,1000)]
#d.jt <- d.jt[sample(.N,1000)]


#save dimensions, destination.----
png(filename=figure_file.path,width=5.5,height=8,units='in',res=300)

#Begin plot.----
name <- 'Global Northern Temperate Fungi'
sz <- 0.2 #cex size for points
y.ln <- 4 #distance of y axis label from axis
r.col <- 'purple' #r2 label color.
x.pos <- 0.9 #x position r2 label

par(mfrow = c(9,3),
    oma = c(4,6,4,1),
    mar = c(2,2,3,2),
    mai = c(0,0,0,0))

#upper rows of panels
for(i in 1:(length(vars) -1)){
  space.line <- lm(vars[[i]] ~      space, data = d)
   time.line <- lm(vars[[i]] ~ epoch.date, data = d)
    doy.line <- lm(vars[[i]] ~   seas_pos, data = d)
  plot(vars[[i]] ~     space, data = d, pch = 16, cex = sz, xaxt = 'n', las = 1   ); axis(side=1, labels=F); mtext(names[i],side = 2, line = y.ln, cex = 0.8)
  abline(space.line, lwd = 2, col = 'gray')
  mtext(bquote(R^2 == .(round(summary(space.line)$r.squared, digits = 2))),side = 3, line=-2, cex = 0.7, adj = x.pos, col = r.col)
  plot(vars[[i]] ~   seas_pos, data = d, pch = 16, cex = sz, xaxt = 'n', yaxt = 'n'); axis(side=1, labels=F);axis(side=2, labels=F)
  abline(  doy.line, lwd = 2, col = 'gray')
  mtext(bquote(R^2 == .(round(summary(  doy.line)$r.squared, digits = 2))),side = 3, line=-2, cex = 0.7, adj = x.pos, col = r.col)
  plot(vars[[i]] ~ epoch.date, data = d, pch = 16, cex = sz, xaxt = 'n', yaxt = 'n'); axis(side=1, labels=F);axis(side=2, labels=F)
  abline( time.line, lwd = 2, col = 'gray')
  mtext(bquote(R^2 == .(round(summary( time.line)$r.squared, digits = 2))),side = 3, line=-2, cex = 0.7, adj = x.pos, col = r.col)
}

#Bottom row of panels
space.line <- lm(vars[[fin]] ~      space, data = d)
 time.line <- lm(vars[[fin]] ~ epoch.date, data = d)
  doy.line <- lm(vars[[fin]] ~   seas_pos, data = d)
plot(vars[[fin]] ~ space, data = d, pch = 16, cex = sz, xaxt = 'n', las = 1   ); axis(side=1, labels=T); mtext(names[[fin]],side = 2, line = y.ln, cex = 0.8);mtext('spatial distance',side=1, line=2, cex=0.8)
abline(space.line, lwd = 2, col = 'gray')
mtext(bquote(R^2 == .(round(summary(space.line)$r.squared, digits = 2))),side = 3, line=-2, cex = 0.7, adj = x.pos, col = r.col)
plot(vars[[fin]] ~ seas_pos, data = d, pch = 16, cex = sz, xaxt = 'n', yaxt = 'n'); axis(side=1, labels=T);axis(side=2, labels=F);mtext('intra-annual distance',side=1, line=2, cex=0.8)
abline(  doy.line, lwd = 2, col = 'gray')
mtext(bquote(R^2 == .(round(summary(  doy.line)$r.squared, digits = 2))),side = 3, line=-2, cex = 0.7, adj = x.pos, col = r.col)
plot(vars[[fin]] ~ epoch.date, data = d, pch = 16, cex = sz, xaxt = 'n', yaxt = 'n'); axis(side=1, labels=T);axis(side=2, labels=F);mtext('inter-annual distance',side=1, line=2, cex=0.8)
abline( time.line, lwd = 2, col = 'gray')
mtext(bquote(R^2 == .(round(summary( time.line)$r.squared, digits = 2))),side = 3, line=-2, cex = 0.7, adj = x.pos, col = r.col)

#outer labels
mtext(name, side = 3, cex = 1.3, outer = T, line = 1)

#end plot.----
dev.off()