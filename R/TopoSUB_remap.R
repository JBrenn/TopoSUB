# data      data from function TopoSUBread
# variable  variable of interest
# wpath     working path of topoSUB simulation
#           e.g. 
# periods   processing of climate periods - default: list(baseline=c(1980,2010), per1=c(2020,2050), per2=c(2045,2075), per3=c(2070,2100))
# sequence  processing of monthly/yearly data, default: list(c(1980,2010), aggregation="year")
# aggr_fun  function to aggregate variable

  TopoSUB_remap <-  function(data, variable, wpath, location.file = "locations.txt", setup.file = "setup.txt",
                           periods=list(baseline=c(1980,2010), per1=c(2020,2050), per2=c(2045,2075), per3=c(2070,2100)),
                           periods_aggr = list(aggr=c("season", "veg_period"), fun="mean", diff = c("absolute","percentage")),
                           sequence = list(period=c(1980,2010), aggr=c("year","season","month")),
                           postprocess = NULL,
                           coords = "+proj=utm +zone=32 ellps=WGS84")
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
  
  # yearly mean/sums
  data_spread_zoo_y <- aggregate(data_spread_zoo, years(time(data_spread_zoo)), periods_aggr$fun)
  
  # monthly mean/sums
  data_spread_zoo_m <- aggregate(data_spread_zoo, as.yearmon(time(data_spread_zoo)), periods_aggr$fun)
  
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
      
      # yearly mean/sums
      data_per_y <- aggregate(data_per, years(time(data_per)), periods_aggr$fun)
      
      # monthly mean/sums
      data_per_m <- aggregate(data_per, as.yearmon(time(data_per)), periods_aggr$fun)
      

      
      # mean/sd value period 
      period.fun <- apply(X = data_per_y, MARGIN = 2, FUN = mean)
      out <- as.data.frame(period.fun)
      
      out$period.sd <- apply(X = data_per_y, MARGIN = 2, FUN = sd)
     
      if (!is.null(periods_aggr$aggr)) 
      {
        # mean/sd value period per season
        month_int <- as.integer(format(time(data_per_m),"%m"))
        seasons <- list( MAM = c(month_int==3 | month_int==4 | month_int==5), 
                         JJA = c(month_int==6 | month_int==7 | month_int==8),
                         SON = c(month_int==9 | month_int==10 | month_int==11),
                         DJF = c(month_int==12 | month_int==1 | month_int==2),
                         VEG = c(month_int==4 | month_int==5 | month_int==6 | month_int==7 | month_int==8 | month_int==9 | month_int==10))
      }

      if(any(grepl(pattern = "season", x = periods_aggr$aggr))) 
      {

        # MAM
        out$MAM.fun <- apply(X = data_per_m[seasons$MAM,], MARGIN = 2, FUN = mean)
        out$MAM.sd  <- apply(X = data_per_m[seasons$MAM,], MARGIN = 2, FUN = sd)
        
        # JJA
        out$JJA.fun <- apply(X = data_per_m[seasons$JJA,], MARGIN = 2, FUN = mean)
        out$JJA.sd  <- apply(X = data_per_m[seasons$JJA,], MARGIN = 2, FUN = sd)
        
        # SON
        out$SON.fun <- apply(X = data_per_m[seasons$SON,], MARGIN = 2, FUN = mean)
        out$SON.sd  <- apply(X = data_per_m[seasons$SON,], MARGIN = 2, FUN = sd)
        
        # DJF
        out$DJF.fun <- apply(X = data_per_m[seasons$DJF,], MARGIN = 2, FUN = mean)
        out$DJF.sd  <- apply(X = data_per_m[seasons$DJF,], MARGIN = 2, FUN = sd)
      }

      # mean/sd value vegetation period
      if(any(grepl(pattern = "veg", x = periods_aggr$aggr)))
      {
        # VEG
        out$VEG.fun <- apply(X = data_per_m[seasons$VEG,], MARGIN = 2, FUN = mean)
        out$VEG.sd  <- apply(X = data_per_m[seasons$VEG,], MARGIN = 2, FUN = sd)
      }
      
      return(out)
             
    })
    
    # substitute landform and data for data4periods
      maps_sub_all <- list()
      
      for (i in names(data4periods))
      {
        df <- data4periods[[i]]
        df_names <- names(df)
        df$IDpoint <- as.integer(rownames(df))
        
        map_rst <- sapply(X = df_names, FUN = function(x, landf=landform, y=df, by="IDpoint") {
          df <- df[,c("IDpoint", x)]
          rst <- subs(x=landf, y=df, by="IDpoint", which=x)
        })
        
        names(map_rst) <- df_names
        #map_brick <- brick(map_rst)
        maps_sub_all[[i]] <- brick(map_rst)
      }
  
    # substitute landform and data for differences (absolute and percent)
      for (i in names(data4periods)[!grepl("base",names(periods))])
      {
        if (any(grepl("abs",periods_aggr$diff))) {
          df <- data4periods[[i]] - data4periods[["baseline"]]
          df <- df[,grepl(".fun", names(df))]
          df_names <- names(df)
          df$IDpoint <- as.integer(rownames(df))
          
          map_rst <- sapply(X = df_names, FUN = function(x, landf=landform, y=df, by="IDpoint") {
            df <- df[,c("IDpoint", x)]
            rst <- subs(x=landf, y=df, by="IDpoint", which=x)
          })
          
          names(map_rst) <- df_names
          #map_brick <- brick(map_rst)
          maps_sub_all[[paste("diff","abs",i,sep="_")]] <- brick(map_rst)
        }
        
        if (any(grepl("perc",periods_aggr$diff))) {
          df <- (data4periods[[i]] - data4periods[["baseline"]]) /  data4periods[["baseline"]] *100
          df <- df[,grepl(".fun", names(df))]
          df_names <- names(df)
          df$IDpoint <- as.integer(rownames(df))
          
          map_rst <- sapply(X = df_names, FUN = function(x, landf=landform, y=df, by="IDpoint") {
            df <- df[,c("IDpoint", x)]
            rst <- subs(x=landf, y=df, by="IDpoint", which=x)
          })
          
          names(map_rst) <- df_names
          #map_brick <- brick(map_rst)
          maps_sub_all[[paste("diff","perc",i,sep="_")]] <- brick(map_rst)
        }
    }
    
    # add projection
      for (i in names(maps_sub_all))
        crs(maps_sub_all[[i]]) <- coords
      
    dir.create(file.path(wpath, "OUTperiods"), recursive = T)    
  
  # write raster .tif  
    lapply(names(maps_sub_all), function(x) {
#       writeRaster(x=maps_sub_all[[x]], filename = file.path(wpath, "periods/raster", paste(x,".tif")),  
#                   options="INTERLEAVE=BAND", overwrite=TRUE)
      dir.create(file.path(wpath, "OUTperiods", variable, x), recursive = T)
      writeRaster(maps_sub_all[[x]], filename=file.path(wpath, "OUTperiods", variable, x, names(maps_sub_all[[x]])), 
                                                        bylayer=TRUE, format="GTiff", overwrite=T)
    })
    
  # write monthly / yearly means
    dir.create(file.path(wpath, "OUTcsv", variable))
    write.zoo(x = data_spread_zoo_y, file = file.path(wpath, "OUTcsv", variable, "average_year.csv"), 
              row.names = F, sep=",", quote=F)
    write.zoo(x = data_spread_zoo_m, file = file.path(wpath, "OUTcsv", variable, "average_month.csv"), 
              row.names = F, sep=",", quote=F)
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