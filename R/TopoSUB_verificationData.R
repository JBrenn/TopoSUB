# function to get obs and sim data for input to Rmd file

TopoSUB_verificationData <- function(wpath, station, variable, meta_data)
{
  
  if (grepl("SWC",variable)) {
    split <- strsplit(variable, "_") 
    depth_ <- which(grepl("z",split[[1]]))
    depth  <- as.integer(substr(split[[1]][depth_],2,nchar(split[[1]][depth_])))*10
    variable_sim <- paste(split[[1]][1], "total", depth, sep="_")
  } else {
    variable_sim <- variable
  }
  
  # select cluster centroid
  cluster_cent <- meta_data$cluster_centroids[meta_data$name==station]
  
  # read in verification data from sim folder "obs"
  # structure of obs data:
  # data frames (csv) for every station 
  # containing
  # data: year, month, day, variables (GEOtop variable names) - files named after station ID
  # meta data: station OID (OID), station name (name), coordinates (x,y), elevation (h)
  
  st_data <- read.csv(file.path(wpath,paste("obs/",station,".csv", sep="")), header=T)
  
  date <- as.Date(st_data$Index)
  obs_zoo <- zoo(st_data[,variable], date)
  
  start_num <- as.numeric(date[1])
  end_num   <- as.numeric(tail(date,1))
  
  # reading simulation data from raw data or RSQLite database
  # station wise / according to representaticve cluster centroid
  # for time frame given by longest record in obs data
  
  db <- dbConnect(drv = SQLite(), file.path(wpath,"sim_out.sqlite"))
  query <- paste("select Date, ", variable_sim, " from raw where Date>", start_num, " and Date<", end_num, 
                 " and IDpoint=", cluster_cent, sep="")
  out <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  sim_zoo <- zoo(out[,variable_sim], as.Date(out$Date, origin = as.Date("1970-01-01")))
  
  data <- merge(sim_zoo, obs_zoo)
  names(data) <- c("Simulation","Observation")
  
  return(data)
  
}