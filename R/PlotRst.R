# plot maps from raster 

PlotRst <- function(rst, date, variable, layer, limits, legend="",
                               lowcol="#f7fbff", highcol="#08306b")
{
  #map <- raster(map)
  
  #convert the raster to points for plotting
  map.p <- rasterToPoints(rst)
  #Make the points a dataframe for ggplot
  df <- data.frame(map.p)
  #Make appropriate column headings
  colnames(df) <- c("Longitude", "Latitude", "MAP")
#   
  if (is.null(layer)) {
    var_layer_date <- paste(variable, " | ", date, sep=" ")
  } else {
    var_layer_date <- paste(variable, " | layer ", layer, " | ", date, sep=" ")
  }
  
  ggp <- ggplot(data=df, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=MAP)) +
    # geom_point(data=sites, aes(x=x, y=y), color="white", size=3, shape=4) +
    theme_bw() +
    coord_equal() +
    scale_fill_gradient(legend, limits=limits, low = lowcol, high = highcol, 
                        space = "Lab", na.value = "grey50", 
                        guide = "colourbar") +
    theme(axis.title.x = element_text(size=13),
          axis.title.y = element_text(size=13),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12, angle=75),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank(),
          plot.title =element_text(size=14) ) +
    ggtitle(var_layer_date)

  return(ggp)
}