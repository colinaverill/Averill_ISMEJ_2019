#Maps of how data are distributed in space.
#clear environment, load packages
rm(list=ls())
source('paths.r')
library(ggplot2)
library(ggalt)

#laod data, set output path.----
#output path
out.path <- fig_S3.path

#Load data.
lt <- readRDS(ted_clean_map.path)
jt <- readRDS(tal_clean_map.path)

#setup save routine.----
png(filename=out.path,width=7,height=5,units='in',res=300)

#build map.----
#Slightly cutting off right edge of x-axis.
world <- map_data('world')
map <- ggplot() + geom_cartogram(data = world, map = world, aes(x=long, y = lat, group = group, map_id=region))
map <- map + coord_proj("+proj=wintri", ylim = c(23.5, 66.5))
map <- map + geom_point(data = lt, aes(x = longitude,    y = latitude   ), color = "yellow"    , size = 2)
map <- map + geom_point(data = jt, aes(x = longitude.dd, y = latitude.dd), color = "light blue", size = 2)
map <- map + labs(x='longitude', y='latitude')
map
#add legend. Couldn't get this to work.
#map <- map + theme(legend.position = 'bottom')
map

#end plot.----
dev.off()
