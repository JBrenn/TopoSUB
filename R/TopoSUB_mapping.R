# postprocess

# function TopoSUB_visMAP

# library(geotopbricks)
# library(ggplot2)
# 
# wpath <- wpath <- "Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/toposub/sim/1d/1d_001/000002/"
# key <- c("PointOutputFileWriteEnd","SoilLiqContentProfileFileWriteEnd")
# vars <- list(c("ETA","Ptot","Tair.C.","snow_melted.mm.","SWin.W.m2.","Relative_Humidity..."),
#             c("X20.000000","X50.000000","X200.000000","X500.000000","X1000.000000","X2000.000000"))
# date <- seq(from = as.Date("01/05/2009", format="%d/%m/%Y"), to = as.Date("01/08/2009", format="%d/%m/%Y"), by = 1)
# date <- NULL #all dates
# fun=mean
# Nclust <-150
# devision=NULL #evtl. number of years of climatic mean of yearly sums
# 
# #one day
# date <- as.Date("01/08/2009", format="%d/%m/%Y")
# 
# #test mutiple maps
# wpath <- wpath <- "Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/toposub/sim/1d/1d_001/000002/"
# key <- c("PointOutputFileWriteEnd")
# vars <- list(c("ETA","Ptot","Tair.C.","snow_melted.mm.","SWin.W.m2.","Relative_Humidity..."))
# date <- NULL
# fun=NULL
# Nclust <-150
# devision=NULL #evtl. number of years of climatic mean of yearly sums

TopoSUB_mapping <- function(wpath, key, vars, date, fun, Nclust, devision)
{
  if (!exists(x = "restrDataDate")) load(file.path(wpath,"restrDataDate.RData"))
  
  dir.create(file.path(wpath,"ascii"))
  dir.create(file.path(wpath,"pdf"))
  
  # load landscape info 
  landform <- raster(x = file.path(wpath, paste("landform_",Nclust,".asc",sep="")))
  landformValues <- getValues(landform)
  landform[] <- ifelse(landformValues==landformValues[1], NA, landformValues)
  
  for (k in key)
  {
    # definition variable name subsurfac
    if (k=="SoilAveragedTempProfileFileWriteEnd") { 
      variable <- "Tsoil"; var <- vars[[which(key=="SoilAveragedTempProfileFileWriteEnd")]] }
    if (k=="SoilLiqContentProfileFileWriteEnd") { 
      variable <- "SMCliq"; var <- vars[[which(key=="SoilLiqContentProfileFileWriteEnd")]] }
    if (k=="SoilIceContentProfileFileWriteEnd") { 
      variable <- "SMCice"; var <- vars[[which(key=="SoilIceContentProfileFileWriteEnd")]] }
    if (k=="PointOutputFileWriteEnd") var <- vars[[which(key=="PointOutputFileWriteEnd")]]
    
    data <- restrDataDate[[k]]
  
    start <- as.Date(data[[1]]$Date12.DDMMYYYYhhmm.[1], format="%d/%m/%Y %H:%M")
    end   <- as.Date(tail(data,1)[[1]]$Date12.DDMMYYYYhhmm.[1], format="%d/%m/%Y %H:%M")
    
    dates_data <- seq(from = start,to = end,by = 1)
    
    if (is.null(date)) dates <- seq(from = as.Date(start),to = as.Date(end), by = 1)[-1]
    
    filter <- dates_data%in%date
    filter_nr <- which(filter)
    
    if (sum(filter)==1) {
      # create map for 1 day
      data_day <- data[filter_nr][[1]]
      # add precipitation or evapotranspiration data 
      if ("ETA" %in% var) data_day$ETA <- data_day$Evap_surface.mm. + data_day$Trasp_canopy.mm.
      if ("Ptot" %in% var) data_day$Ptot <- data_day$Prain_over_canopy.mm. + data_day$Psnow_over_canopy.mm.
      
      data_day <- data_day[,c(var,"IDpoint")]
      
      for (i in var)
      {  
        # layer definition depending on key / var
        if (k=="PointOutputFileWriteEnd") {
          layer = NULL
        } else {
          layer = paste(as.integer(substr(i,2,nchar(i))),"mm",sep="")
        }
        
        if (k=="PointOutputFileWriteEnd") variable <- i
        
        # substitute landform and data
        df <- data_day[,c("IDpoint", i)]
        map_rst <- subs(x=landform, y=df, by="IDpoint", which=i)
        # z limits of daily map
        lim <- range(getValues(map_rst),na.rm=T)
        if (k=="PointOutputFileWriteEnd") {
          writeRaster(x = map_rst, filename = file.path(wpath, paste("ascii/",variable,"_",date,".asc",sep="")),
                      overwrite=TRUE)
        } else {
          writeRaster(x = map_rst, filename = file.path(wpath, paste("ascii/",variable,"_L",layer,"_",date,".asc",sep="")),
                      overwrite=TRUE)
        }
     
        # visualize map
        ggp <- PlotRst(rst = map_rst,date = date,variable = variable,layer = layer,limits = lim,legend = "")
        
        if (k=="PointOutputFileWriteEnd") {
          ggsave(filename = file.path(wpath, paste("pdf/",variable,"_",date,".pdf",sep="")))
        } else {
          ggsave(filename = file.path(wpath, paste("pdf/",variable,"_L",layer,"_",date,".pdf",sep="")))
        }
      }
        
    } else {
      # aggregate data according to fun
      data_dates <- data[filter_nr]
      
      # add precipitation or/and evapotranspiration data 
      if ("ETA" %in% var) {
        data_dates <- lapply(X = data_dates, FUN =  function(x) {
          x$ETA <- x$Evap_surface.mm. + x$Trasp_canopy.mm.
          return(x)
        } )
      } 
      
      if ("Ptot" %in% var) {
        data_dates <- lapply(X = data_dates, FUN =  function(x) {
          x$Ptot <- x$Prain_over_canopy.mm. + x$Psnow_over_canopy.mm.
          return(x)
        } )
      } 
      
      data_dates <- lapply(X = data_dates, FUN =  function(x) {
        x <- x[,c("IDpoint", var)]
        return(x)
      } )
      
      min <- sapply(data_dates,function(x) apply(x,2,min))
      max <- sapply(data_dates,function(x) apply(x,2,max))
      
      min <- apply(min,1,min)
      max <- apply(max,1,max)
      
      if (is.null(fun)){
        # plot map for each time step (no .ascii files)
        for (fd in filter_nr)
        {
          # create map for 1 day
          data_day <- data[fd][[1]]
          # add precipitation or evapotranspiration data 
          if ("ETA" %in% var) data_day$ETA <- data_day$Evap_surface.mm. + data_day$Trasp_canopy.mm.
          if ("Ptot" %in% var) data_day$Ptot <- data_day$Prain_over_canopy.mm. + data_day$Psnow_over_canopy.mm.
          
          data_day <- data_day[,c(var,"IDpoint")]
          
          for (i in var)
          {  
            # z limits of all daily map
            lim <- c(min[i], max[i])
            
            dir.create(file.path(wpath,paste("pdf_",i,sep="")))
            dir.create(file.path(wpath,paste(paste("ascii_",i,sep=""))))
            # layer definition depending on key / var
            if (k=="PointOutputFileWriteEnd") {
              layer = NULL
            } else {
              layer = paste(as.integer(substr(i,2,nchar(i))),"mm",sep="")
            }
            
            if (k=="PointOutputFileWriteEnd") variable <- i
            
            # substitute landform and data
            df <- data_day[,c("IDpoint", i)]
            map_rst <- subs(x=landform, y=df, by="IDpoint", which=i)
            
            if (k=="PointOutputFileWriteEnd") {
            writeRaster(x = map_rst, filename = file.path(wpath, paste(paste("ascii_",i,"/",sep=""),variable,"_",dates_data[fd],".asc",sep="")),
                        overwrite=TRUE)
            } else {
            writeRaster(x = map_rst, filename = file.path(wpath, paste(paste("ascii_",i,"/",sep=""),variable,"_L",layer,"_",dates_data[fd],".asc",sep="")),
                        overwrite=TRUE)
            }
            # visualize map
            ggp <- PlotRst(rst = map_rst,date = dates_data[fd],variable = variable,layer = layer,limits = lim,legend = "")
            
            if (k=="PointOutputFileWriteEnd") {
              ggsave(filename = file.path(wpath, paste(paste("pdf_",i,"/",sep=""),variable,"_",dates_data[fd],".pdf",sep="")))
            } else {
              ggsave(filename = file.path(wpath, paste(paste("pdf_",i,"/",sep=""),variable,"_L",layer,"_",dates_data[fd],".pdf",sep="")))
            }
            
          }
        }
        
      } else {
        # function provided to aggregate data
        
        # unlist data, create array, 3rd dimansion: dates
        data_array <- array(unlist(data_dates), 
                            dim = c(nrow(data_dates[[1]]), ncol(data_dates[[1]]), length(data_dates)))
        # apply function to 3rd dimension, date
        data_fun <- apply(X = data_array, MARGIN = c(1,2), FUN = fun)
        colnames(data_fun) <- names(data_dates[[1]])
        data_fun <- as.data.frame(data_fun)
        
        if (!is.null(devision)) data_fun <- data_fun / devision
        
        # reset IDpoint
        data_fun$IDpoint <- data[[1]]$IDpoint
        
        for (i in var)
        {  
          if (k=="PointOutputFileWriteEnd") variable <- i
          
          # layer definition depending on key / var
          if (k=="PointOutputFileWriteEnd") {
            layer = NULL
          } else {
            layer = paste(as.integer(substr(i,2,nchar(i))),"mm",sep="")
          }
          
          df <- data_fun[,c("IDpoint", i)]
          map_rst <- subs(x=landform, y=df, by="IDpoint", which=i)
          # z limits of daily map
          lim <- range(getValues(map_rst),na.rm=T)
          
          writeRaster(x = map_rst, filename = file.path(wpath, paste("ascii/",fun,"_",variable,"_",date[1],"_",tail(date,1),".asc",sep="")),
                      overwrite=TRUE)
          # visualize map
          ggp <- PlotRst(rst = map_rst,date = paste(fun," ",date[1]," / ",tail(date,1), sep=""),
                         variable = variable,layer = layer,limits = lim,legend = "")
          
          if (k=="PointOutputFileWriteEnd") {
            ggsave(filename = file.path(wpath, paste("pdf/",variable,"_",date[1],"_",tail(date,1),".pdf",sep="")))
          } else {
            ggsave(filename = file.path(wpath, paste("pdf/",variable,"_L",layer,"_",date[1],"_",tail(date,1),".pdf",sep="")))
          }
          
        }
      }
  
    }
  }
  
}