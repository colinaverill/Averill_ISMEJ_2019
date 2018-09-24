#subsample a mapping file such that there is only one temporal observation per site.-----
subsample_map <- function(map, plot_ID){
  sub.map <- lapply(split(map, plot_ID),
                    function(subdf) subdf[sample(1:nrow(subdf), 1),])
  sub.map <- data.frame(do.call('rbind', sub.map))
  return(sub.map)
}
