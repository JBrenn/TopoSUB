# restructure TopoSUB output data

# wpath <- "Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/toposub/sim/1d/1d_001/000002/"
# wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/toposub/sim/1d/1d_001/000002/"

# keys <- c("PointOutputFileWriteEnd", "SoilAveragedTempProfileFileWriteEnd",
#           "SoilLiqContentProfileFileWriteEnd", "SoilIceContentProfileFileWriteEnd")

TopoSUB_restrOutput <- function(wpath, keys, setup_filename="setup.txt", location_filename="location.txt")
{
  
  # get setup info
  setup <- read.csv(file.path(wpath,setup_filename), header = F)
  apply(X = setup[,c(2,3)], MARGIN = 1, 
        FUN = function(x) assign(x = x[1], value = as.numeric(x[2]), envir = .GlobalEnv) )
  
  # extract start and end of simulation
  start <- get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="UTC")
  end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="UTC")
  
  start_day <- as.Date(start)
  end_day   <- as.Date(end)
  dates <- seq(from = start_day+1, to = end_day, by = 1)
  
  restrDataID <- list()
  restrDataDate <- list()
  
  for (i in keys)
  {
    data_name <- get.geotop.inpts.keyword.value(wpath = wpath, keyword = i)
    data <- read.csv(file.path(wpath,paste(data_name,".txt",sep="")), header=TRUE)
    
    data_dates <- as.Date(data$Date12.DDMMYYYYhhmm., format = "%d/%m/%Y %H:%M")
    
    restrDataID[[i]] <- lapply(1:Nclust, function(x){
      filter <- which(data$IDpoint==x)
      y <- data[filter,]
    })
    
    restrDataDate[[i]] <- lapply(dates, function(x){
      filter <- which(data_dates==x)
      y <- data[filter,]
    })
  }
  
  save(list = "restrDataID", file = file.path(wpath, "restrDataID.RData"))
  save(list = "restrDataDate", file = file.path(wpath, "restrDataDate.RData"))
  save(list = "data", file = file.path(wpath, "rawdata.RData"))
  
  return(NULL)
}