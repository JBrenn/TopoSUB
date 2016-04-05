# function to retrieve representative cluster centroid for each obs station
TopoSUB_getCluster4verification <- function(wpath)
{
  # reading obs station meta data
  meta_data <- read.csv(file.path(wpath,"obs/meta.csv"))
  
  # reading listpoint data
  listpoint_data <- read.csv(file.path(wpath,"listpoints.txt"))
  
  # reading landform raster data 
  files_wpath <- dir(wpath)
  
  landform_asc <- files_wpath[grepl("landform", files_wpath)]
  landform_rst <- raster(file.path(wpath,landform_asc))
  
  # get cluster centroid representing obs data
  #? how to get representative cluster centroid
  meta_data$cluster_centroids <- raster::extract(landform_rst, meta_data[,c("x","y")])
  
  # join listpoint data
  meta_data <- merge.data.frame(meta_data, listpoint_data, by.x = "cluster_centroids", by.y = "id")
  
  return(meta_data)
}