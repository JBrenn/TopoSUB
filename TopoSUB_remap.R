# data      data from function TopoSUBread
# variable  variable of interest
# wpath     working path of topoSUB simulation
#           e.g. 
# periods   processing of climate periods - default: list(baseline=c(1980,2010), per1=c(2020,2050), per2=c(2045,2075), per3=c(2070,2100))
# sequence  processing of monthly/yearly data, default: list(c(1980,2010), aggregation="year")
# aggr_fun  function to aggregate variable

TopoSUB_remap <-  function(data, variable, wpath, location.file, setup.file,
                           periods=list(baseline=c(1980,2010), per1=c(2020,2050), per2=c(2045,2075), per3=c(2070,2100)),
                           sequence = list(period=c(1980,2010), aggregation=c("year","season","month")),
                           postprocess = NULL)
{
  
  # read location file
  locations <- read.csv(file.path(wpath,location.file), header = F, colClasses="character")
  apply(X = locations[,c(2,3)], MARGIN = 1, 
        FUN = function(x) assign(x = x[1], value = x[2], envir = .GlobalEnv) )
  
  # read setup file
  setup <- read.csv(file.path(wpath,setup.file), header = F)
  apply(X = setup[,c(2,3)], MARGIN = 1, 
        FUN = function(x) assign(x = x[1], value = as.numeric(x[2]), envir = .GlobalEnv) )
  
  # read landform raster map for remaping
  landform <- raster(x = file.path(wpath, paste("landform_",Nclust,".asc",sep="")))
  landformValues <- getValues(landform)
  landform[] <- ifelse(landformValues==landformValues[1], NA, landformValues)
  
  # spread data by IDpoints
  data_spread_zoo <- TopoSUB_spreadVAR(data=data, var=variable, do.zoo = TRUE)
  
  if (!is.null(periods))
  {
   
    data4periods <- lapply(X = periods, FUN = function(x) {
      
      # get start and end day of period in Date format
      start_day <- as.Date(paste(x[1]  ,"-01-01", sep=""))
      end_day   <- as.Date(paste(x[2]-1,"-12-31", sep=""))
      
      # window zoo object
      data_per <- window(x = data_spread_zoo, start = start_day, end = end_day)
      
#       if(!is.null(postprocess))
#       {
#         # doing postprocessing on variable
#       }
      
    })
     
  }
  
  if (!is.null(sequence))
  {
    
    # get start and end day of period in Date format
    start_day <- as.Date(paste(sequence$period[1]  ,"-01-01", sep=""))
    end_day   <- as.Date(paste(sequence$period[2]-1,"-12-31", sep=""))
    
    # window zoo object
    data4sequence <- window(data_spread_zoo, start = start_day, end = end_day)
    
    data_aggr <- list()
    if (any(sequence$aggregation=="year"))
    {
      
    }
    if (any(sequence$aggregation=="month"))
    {
      
    }
    if (any(sequence$aggregation=="season"))
    {
      
    }
    
  }
  
}