<<<<<<< HEAD
TopoSUB_POSTcritSWC_fc_wp <- function(data, dry_thres = 30, variable)
=======
TopoSUB_POSTcritSWC_fc_wp <- function(data, dry_thres = 30, variable = variable)
>>>>>>> 12a3830ba7e593b6195e09c3a8c3a6595f8fc717
{
  # get topo attributes from listpoints
  listpt <- data.table::fread(file.path(wpath,"listpoints.txt"))
  
  # soil characteristics
  nr_soiltypes <- get.geotop.inpts.keyword.value(keyword="SoilLayerTypes", wpath=wpath, numeric=TRUE)
  
  soil_input <- get.geotop.inpts.keyword.value(keyword="SoilParFile", wpath=wpath, data.frame=TRUE, 
                                               level = 1:nr_soiltypes)

  soil_thickness_header <- get.geotop.inpts.keyword.value(keyword="HeaderSoilDz", wpath=wpath)
  soil_thickness <- soil_input[[1]][,soil_thickness_header]
  
  mean_soil_depth <- cumsum(c(0,soil_thickness)) + diff(c(0,soil_thickness,0))/2
  
 # calculate SWC threshold for each soil type
 # dependent on soil type 
 # after Jasper(2006), Allen(2010)
 # severe:   theta_30* = theta_wp + 0.3*(theta_fc - theta_wp)
 # moderate: theta_50* = theta_wp + 0.5*(theta_fc - theta_wp)  

 soil_input_theta <-   
   lapply(X = soil_input, FUN = function(x){
     x$theta <- x$vwc_w + dry_thres/100 * (x$vwc_fc - x$vwc_w)
     return(x)
   })  
  
 # calculate (consecutive) days of "drougth stress" in vegetation period
  
  # get depth of SMC variable in mm
 soil_depth <- as.integer(strsplit(variable,"_")[[1]][3])
  
  # row for retrieving theta thresholds 
 row <- which.min(abs(mean_soil_depth - soil_depth)) 
 
 theta <- 
   sapply(listpt$soil, function(x){
     soil_input_theta[[x]][row,c("theta")]
   })
   
 data_theta <- 
   sapply(X = 1:length(theta), FUN = function(x){
     y <- data[,x] <= theta[x]
     as.integer(y)
   })
   
 data_zoo <- zoo(data_theta, time(data))
 
}