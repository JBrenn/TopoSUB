# read TopoSUB output 
# with data.table::fread
# join with dplyr (could be done with data.table too)

# wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/toposub/sim/1d/1d_002/000002/"
 
# keys <- c("PointOutputFileWriteEnd","SoilLiqContentProfileFileWriteEnd")
# 
# select <- list(PointOutputFileWriteEnd=c("Date12[DDMMYYYYhhmm]","IDpoint","Tair[C]","Prain_over_canopy[mm]","Psnow_over_canopy[mm]","snow_water_equivalent[mm]","Evap_surface[mm]","Trasp_canopy[mm]","Hv[W/m2]","LEv[W/m2]","Hg_unveg[W/m2]","LEg_unveg[W/m2]","Hg_veg[W/m2]","LEg_veg[W/m2]","Canopy_fraction[-]"), 
# SoilLiqContentProfileFileWriteEnd=c("Date12[DDMMYYYYhhmm]","IDpoint","20.000000","50.000000","200.000000","500.000000"))
# 

# library(geotopbricks) ::get.geotop.inpts.keyword.value
# library(dplyr)
# library(data.table) ::fread ; ::setnames
# library(stringr)

TopoSUB_read <- function(wpath, keys = c("PointOutputFileWriteEnd","SoilLiqContentProfileFileWriteEnd", "SoilIceContentProfileFileWriteEnd"), 
                         select = list(PointOutputFileWriteEnd=c("Date12[DDMMYYYYhhmm]","IDpoint","Tair[C]","Prain_over_canopy[mm]","Psnow_over_canopy[mm]","snow_water_equivalent[mm]","Evap_surface[mm]","Trasp_canopy[mm]","Hv[W/m2]","LEv[W/m2]","Hg_unveg[W/m2]","LEg_unveg[W/m2]","Hg_veg[W/m2]","LEg_veg[W/m2]","Canopy_fraction[-]"), 
                                       SoilLiqContentProfileFileWriteEnd=c("Date12[DDMMYYYYhhmm]","IDpoint","20.000000","50.000000","200.000000","500.000000"),
                                       SoilIceContentProfileFileWriteEnd=c("Date12[DDMMYYYYhhmm]","IDpoint","20.000000","50.000000","200.000000","500.000000")),
                         doLEHcalc = TRUE, SnowCoverThres = 5, setup.file = "setup.txt", add_listpoint = FALSE,
                         write_sqlite = FALSE)
{
  
  # read setup file
  setup <- read.csv(file.path(wpath, setup.file), header = F)
  apply(X = setup[,c(2,3)], MARGIN = 1, 
        FUN = function(x) assign(x = x[1], value = as.numeric(x[2]), envir = .GlobalEnv) )
  
  # set run_parallel to logical
  run_parallel <- as.logical(run_parallel)
  
  if (run_parallel) {
    par_files <- dir(file.path(wpath, "parallel"), recursive = T)
    par_files <- par_files[grepl(pattern = "out", par_files)]
  } 
  
  data <- list()
  
  for (i in keys)
  {
    print(paste("reading data from key:" , i))
    
    # output path for key
    data_name <- geotopbricks::get.geotop.inpts.keyword.value(wpath = wpath, keyword = i)

    
    if (run_parallel) {
    
    # get parallel folders & file names to read    
      parallel_folders <- str_split(pattern = "/",string = par_files, n = 2) 
      parallel_folders <- unique(matrix(unlist(parallel_folders), ncol = 2, byrow = T)[,1])
      
      file_names <- file.path(wpath,"parallel",parallel_folders,paste(data_name,".txt",sep = ""))
    
    # reading data     
      data[[i]] <- data.table::rbindlist(lapply(file_names,
                                                data.table::fread,header=TRUE,na.strings=c("-9999"),
                                                select = select[[i]]))
      
    } else {
       
      # reading data
      data[[i]] <- data.table::fread(input = file.path(wpath,paste(data_name,".txt",sep="")), header=TRUE, 
                                     na.strings=c("-9999"), showProgress = TRUE, select = select[[i]])

    }
    
    print(paste("postprocess of read data from key: ", i, sep=""))
    
    # remove [] / - from data names
    data.table::setnames(x = data[[i]],old = names(data[[i]]),
             new = stringr::str_replace_all(names(data[[i]]), "[/\\]\\[-]", "_") )
    
    if (i=="PointOutputFileWriteEnd")
    {
      # sum liquid and solid precipitation
      data[[i]][,Ptotal_mm_:=Prain_over_canopy_mm_+Psnow_over_canopy_mm_]
      
      # sum transpiration and evaporation
      data[[i]][,Evaptranspiration_mm_:=Evap_surface_mm_+Trasp_canopy_mm_] 
      
      # calculate sensible and latent heat over canopy
      #with library(AnalyseGeotop)
      if (doLEHcalc)
      {
        LE <- data[[i]][,c("LEg_veg_W_m2_", "LEg_unveg_W_m2_", "LEv_W_m2_", "Canopy_fraction___"), with=FALSE]
        data.table::setnames(LE,old = names(LE),new = c("g_veg","g_unveg","veg","cf"))
        H <- data[[i]][,c("Hg_veg_W_m2_", "Hg_unveg_W_m2_", "Hv_W_m2_", "Canopy_fraction___"), with=FALSE]
        data.table::setnames(H,old = names(H),new = c("g_veg","g_unveg","veg","cf"))
        heat_data <- list(LE=LE, H=H)
        
        #over_canopy <- AnalyseGeotop::GEOtop_EfluxOcanopy(heat_data)
        
        over_canopy <- list()
        
        for (i in names(data)) 
          over_canopy[[i]] <- data[[i]]$cf * (data[[i]]$g_veg + data[[i]]$veg) + (1-data[[i]]$cf) * data[[i]]$g_unveg
        
        data[[i]][,LE_over_canopy_W_m2_:=over_canopy$LE]
        data[[i]][,H_over_canopy_W_m2_:=over_canopy$H]
      }
      
      # calculate snow cover days
      data[[i]][,SnowCover_days_:= as.numeric(snow_water_equivalent_mm_ >= SnowCoverThres)] 
    
    }

    if (i=="SoilAveragedTempProfileFileWriteEnd")
    {
      # change col names
      data.table::setnames(x = data[[i]], old = names(data[[i]]),
                           new = c(names(data[[i]])[1:2], paste("SoilT", as.integer(select[[i]][-c(1:2)]), sep="_")) )
      
      # convert last col to numeric
      set( x = data[[i]], j = length(data[[i]]), value = as.numeric(data[[i]][[length(data[[i]])]]) )
      
     }
    
    if (i=="SoilLiqContentProfileFileWriteEnd")
    {
      
      data.table::setnames(x = data[[i]], old = names(data[[i]]),
                           new = c(names(data[[i]])[1:2], paste("SWC_liq", as.integer(select[[i]][-c(1:2)]), sep="_")) )
      
      data.table::set( x = data[[i]], j = length(data[[i]]), value = as.numeric(data[[i]][[length(data[[i]])]]) )
      
      # calculate drought days / drought statistics (see Shefield 2008)
      # monthly edcf of smc (for control/current state 1970 - 2000)
      # 10% quantile as threshold q0(smc)
      # description of drought (duration, intensity, sevirity)
    }
    
    if (i=="SoilIceContentProfileFileWriteEnd")
    {
      data.table::setnames(x = data[[i]], old = names(data[[i]]),
                           new = c(names(data[[i]])[1:2], paste("SWC_ice", as.integer(select[[i]][-c(1:2)]), sep="_")) )
      
      data.table::set( x = data[[i]], j = length(data[[i]]), value = as.numeric(data[[i]][[length(data[[i]])]]) )
    }

  }
  
  # calculate total soil moisture from LIQ and ICE
  if (any(keys=="SoilLiqContentProfileFileWriteEnd") & any(keys=="SoilIceContentProfileFileWriteEnd"))
  {
    # liq + ice 
    data[["SoilWaterContentProfileFileWriteEnd"]] <- 
      data[["SoilLiqContentProfileFileWriteEnd"]][,3:length(data[["SoilLiqContentProfileFileWriteEnd"]]),with=FALSE] + 
      data[["SoilIceContentProfileFileWriteEnd"]][,3:length(data[["SoilIceContentProfileFileWriteEnd"]]),with=FALSE]
    data[["SoilWaterContentProfileFileWriteEnd"]][,c("Date12_DDMMYYYYhhmm_","IDpoint") := list(data[["SoilLiqContentProfileFileWriteEnd"]]$Date12_DDMMYYYYhhmm_, data[["SoilLiqContentProfileFileWriteEnd"]]$IDpoint)]
    
    setcolorder(data[["SoilWaterContentProfileFileWriteEnd"]], names(data[["SoilLiqContentProfileFileWriteEnd"]]))
    
    # rename
    data.table::setnames(x = data[["SoilWaterContentProfileFileWriteEnd"]], old = names(data[["SoilWaterContentProfileFileWriteEnd"]]),
                         new = c(names(data[["SoilWaterContentProfileFileWriteEnd"]])[1:2], 
                                 paste("SWC_total", as.integer(select[["SoilLiqContentProfileFileWriteEnd"]][-c(1:2)]), sep="_")) )
    keys <- c(keys, "SoilWaterContentProfileFileWriteEnd")
  }
      
  if (length(keys)==1) {
    data <- data[[1]]
  } else {
    data_join <- data[[1]]
    for (i in 2:length(keys)) {
      data_join <- dplyr::left_join(x = data_join, y = data[[i]],
                                    by = c("Date12_DDMMYYYYhhmm_", "IDpoint"))
      #data_join <- data_join[data[[i]]]
    }
    data <- data_join
  }
    # get POSIX datetime
    data.table::setnames(x = data, old = names(data), new = c("Date",names(data)[-1]))
    
    data.table::setkey(x = data, Date)
    
    # str split
    data[, c("Date","time") := data.table::tstrsplit(Date, " ", fixed=TRUE)]
    
    # str paste
    #data[, "Date" := paste(year,"-",month,"-",day, sep="")]
    
    # create POSIX object / date
    #data[, "datetime" := fasttime::fastPOSIXct(x=datetime, required.components=3)]
    data[, "Date" := as.Date(x=Date, format="%d/%m/%Y")]
    
    # order data
    data[order(Date),]
    
    if (add_listpoint) {
      # add topo attributes from listpoints to data
      listpt <- data.table::fread(file.path(wpath,"listpoints.txt"))
      new_names <- names(listpt)
      new_names[2]  <- "IDpoint"
      data.table::setnames(x = listpt, old = names(listpt), new = new_names)
      dplyr::left_join(data, listpt, by= "IDpoint", copy=TRUE)
    }
    
    if (write_sqlite)
    {
      db <- RSQLite::dbConnect(drv = "RSQLite", dbname = "sim_out")
        RSQLite::dbWriteTable(conn = db, name = "raw", value = data)
      RSQLite::dbDisconnect(db)
    }
    
    return(data)
}