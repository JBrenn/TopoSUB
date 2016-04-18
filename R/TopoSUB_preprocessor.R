# Version: 4.1
# improve code structure
# function 1: TopoSUB_preprocessor()
# needs: _master folder with
#   predictor maps1 (dem, --> slope, aspect, sky view factor)
#   predictor maps2 (land use/cover, soil)
#   predictor maps3 (climate variables: precipitation, air temperature, radiation, ...)
#     --> list(topo = c(), clas = c(), clim = c())
#   geotop.intps (soil, land use parameterisation)
#   meteo input in GEOtop format
#   parameter file 
#     --> decide which format to use for easy handling
#     --> 1: locations (character), 2: setup & switches parameter (numeric / boolean)
#   
#==============================================================================
# TopoSUB preprocessor
#==============================================================================
TopoSUB_preprocessor <- function(location.file="locations.txt", setup.file="setup.txt", 
                                  PredNamesList=list(topo=c("dem", "slp", "svf"),
                                                      clas=c("landcover", "soil")),
                                  mode_ls="ols", uniform_class = c(landcover=NULL, soil=NULL))
{
  #load libs raster and geotopbricks (shouldn't be loaded in function but installed/sourced before!)
  # no more necessary when package toposub is used
  #require(raster) 
  # to get number of meteo files (not used so far) & number of soil / landuse types from geotop.inpts
  #require(geotopbricks)
  
  #require(parallel)
  
  # method to concatenate
  "&" <- function(...) UseMethod("&") 
  "&.default" <- .Primitive("&") 
  "&.character" <- function(...) paste(...,sep="") 
   
  # read location file
  locations <- read.csv(location.file, header = F, colClasses="character")
  apply(X = locations[,c(2,3)], MARGIN = 1, 
        FUN = function(x) assign(x = x[1], value = x[2], envir = .GlobalEnv) )
  
  # read setup file
  setup <- read.csv(setup.file, header = F)
  apply(X = setup[,c(2,3)], MARGIN = 1, 
        FUN = function(x) assign(x = x[1], value = as.numeric(x[2]), envir = .GlobalEnv) )
  
  # set switches to logical
  copy_master  <- as.logical(copy_master)
  
  run_lsm  <- as.logical(run_lsm)
  run_hidden  <- as.logical(run_hidden)
  run_parallel <- as.logical(run_parallel)
  
  VSCjobs <- as.logical(VSCjobs)
  
  # set working dir 1 | root dir
  setwd(root)
  
  # master folder name 
  mst_folder <- strsplit(inpts.file, "/")[[1]][1]
  
  # get number of meteo files/stations (NrMeteoFiles)
  NrMeteoFiles <- as.integer(geotopbricks::get.geotop.inpts.keyword.value(keyword = "NumberOfMeteoStations", wpath = mst_folder))
   
  # get number of soil types (Nsoil)
  Nsoil <- as.integer(geotopbricks::get.geotop.inpts.keyword.value(keyword = "SoilLayerTypes", wpath = mst_folder))
  
  # get number of soil types (Nlandcover)
  Nlandcover <- as.integer(geotopbricks::get.geotop.inpts.keyword.value(keyword = "NumLandCoverTypes", wpath = mst_folder))
  
  # include source code(s)
  # sapply(X = dir(path = src, full.names = T), FUN = source)
  
  eroot_loc1 <- file.path( root, sim, paste("1d/1d_", exper1, sep="")) # 1d path
  dir <- formatC( as.integer(es), width=6, flag="0" ) # format experiment dir name
  esPath <- file.path(eroot_loc1, dir) # experiment path
  dir.create( esPath, recursive=T ) 
  
  #input locations
  demLoc <- file.path(eroot_loc1, dem.file)
  predRoot <- file.path(eroot_loc1, pred.folder)
  
#   #write logs of script and src files
#   file.copy(paste(root,'src/toposub_script.r', sep=''), paste(esPath,'/toposub_script_copy.r',sep=''),overwrite=T)
#   file.copy(paste(root,'/src/',src, sep=''),  paste(esPath,'/',src,'_copy.r',sep=''),overwrite=T)
#   file.copy(paste(root,'/src/',src1, sep=''),  paste(esPath,'/',src1,'_copy.r',sep=''),overwrite=T)
  
  #set tmp directory for raster package
  dir.create( file.path(esPath, "tmp") )
  options(tmpdir=file.path(esPath, "tmp") )
  
  # set work dir 2 | simulation dir
  setwd(eroot_loc1)
  
  # set up folders/ copy files etc
  dir.create(paste(esPath, '/fuzRst', sep=''))
  dir.create(paste(esPath, '/rec', sep=''))

  # copy location and setup file
  file.copy(file.path(root,location.file), esPath , overwrite=T)
  file.copy(file.path(root,setup.file), esPath , overwrite=T)

  if (copy_master)
    file.copy(from = file.path(root, mst_folder), to = eroot_loc1, recursive = T)

  # copy master folder (nescessary for first run of new simulation 1d_00x)

    if (file.exists(file.path(root,inpts.file))) {
      print("copy GEOtop inpts file to sim folder")
      file.copy(from = file.path(eroot_loc1, inpts.file), to = esPath , overwrite=TRUE)
    } else {
      print("GEOtop inpts file does not exist")
    }
	
  if (exists("obs.folder")){
    print("copy observation files to sim folder")
    file.copy(file.path(eroot_loc1, obs.folder), esPath , recursive=TRUE, overwrite=TRUE)
  }
  
	if (exists("pred.folder")){
		print("copy predictor maps to sim folder")
		file.copy(file.path(eroot_loc1, pred.folder), esPath , recursive=TRUE, overwrite=TRUE)
	}
	
	if (exists("meteo.folder")){
		print("copy meteo input data to sim folder")
		file.copy(file.path(eroot_loc1, meteo.folder), esPath , recursive=TRUE, overwrite=TRUE)
	}
	
	if (exists("soil.folder")){
		print("copy soil input files to sim folder")
		file.copy(file.path(eroot_loc1, soil.folder), esPath , recursive=TRUE, overwrite=TRUE)
	}
	
	if (exists("horizon.folder")){
		print("copy horizon files data to sim folder")
		file.copy(file.path(eroot_loc1, horizon.folder), esPath , recursive=TRUE, overwrite=TRUE)
	}
  
  
  #create output directory
  dir.create(paste(esPath, "/out", sep=""))
  
  #make stack of input predictor ==> read in random sample to improve scalability
  
  # get name of dem map
  demLoc_split <- strsplit(x = demLoc, split = "/")
  demName <- demLoc_split[[1]][length(demLoc_split[[1]])]
  demName <- substr(demName,1,nchar(demName)-4)
  # function stack_raster depends on package 'rgdal'
  predNames <- unlist(PredNamesList)
  
  print("Read in predictor maps")
  gridmaps=stack_raster(demLoc=demLoc, predNames=predNames, predRoot=predRoot, predFormat=predFormat, 
                        demName=demName)
  
  #check inputs slp/asp in degrees and correct if necessary (careful if applied in flat place!)
  if(max(gridmaps$asp,na.rm=T)<7){
    gridmaps$asp = radtodeg(gridmaps$asp)
  }
  if(max(gridmaps$slp,na.rm=T)<2){
    gridmaps$slp = radtodeg(gridmaps$slp)
  }
  
  #na to numeric (-1) (does this work)
  gridmaps@data = natonum(x=gridmaps@data, predNames=predNames, n=-1)
  
  #decompose aspect
  res <- aspect_decomp(gridmaps$asp)
  res$aspC -> gridmaps$aspC
  res$aspS -> gridmaps$aspS
  
  #make 'north' predictor -se molotch
  
  #define new predNames (aspC, aspS)
  allNames<-names(gridmaps@data)
  
  if(nrth==1){
    predNames <- allNames[which(allNames!='asp'&allNames!='aspC')]
  }else{
    predNames <- allNames[which(allNames!='asp')]
  }
  
  if (run_lsm) {
    #cd path for geotop executable
    print("create batch file with path to GEOtop executable")
    batchfile<-esPath&"/batch.txt"
    out <- file(batchfile, "w")
    cat("cd",gtexPath,"\n",file=out,sep=" ")
    close(out)
  }

  #sample inputs
  #need to generalise to accept 'data' argument
  # nRand < Nr. of Grids (1000x1000 Vinschgau: 63*48=3024)
  samp_dat <- FUNsampleRandomGrid(gridmaps=gridmaps, nRand=nRand, predNames=predNames, demName=demName)

  #make scaled (see r function 'scale()')data frame of inputs)
  scaleDat_samp= simpleScale(data=samp_dat, pnames=predNames)
  
  if (findn==1) {
    print("Find appropriate number of clusters")
    Nclust <- findN(scaleDat=scaleDat_samp, nMax=nMax.findn, thresh=thresh.findn, eroot_loc1=eroot_loc1)
  } 
  
  
#=================random order of kmeans init conditions (variable order) experiment
  if(randomKmeansInit==1){
    cbind(scaleDat_samp[5],scaleDat_samp[4], scaleDat_samp[3], scaleDat_samp[2], scaleDat_samp[1])->scaleDat_samp
  }
  
  #================pca experiment stuff - sample
  if (pca==1){
    pc.dem <- prcomp(x=scaleDat_samp, scale=F)
    demdata <- as.data.frame(pc.dem$x)
    demdata[1:npca]->scaleDat_samp #select n pc's
  }
  #=============================================
  
  #kmeans on sample
  clust1 <- Kmeans(scaleDat=scaleDat_samp,iter.max=iter.max,centers=Nclust, nstart=nstart1)
  
  #scale whole dataset
  scaleDat_all <- simpleScale(data=gridmaps@data, pnames=predNames)
  
  #====================pca experiment stuff - all
  if (pca==1){
    pc.dem <- prcomp(x=gridmaps@data, scale=F)
    demdata <- as.data.frame(pc.dem$x)
    demdata[1:npca]->scaleDat_all #select pc's
  }
  #==============================================
  
  #kmeans whole dataset
  clust2 <- Kmeans(scaleDat=scaleDat_all,iter.max=iter.max,centers=clust1$centers, nstart=nstart2)
  
  #remove small samples, redist to nearestneighbour attribute space
  if(samp_reduce==1){
    clust3=sample_redist(pix= length(clust2$cluster),samples=Nclust,thresh_per=thresh_per, clust_obj=clust2)# be careful, samlple size not updated only clust2$cluster changed
  }else{clust2->clust3}
  
  #make map of clusters
  gridmaps$clust <- clust3$cluster
  gridmaps$landform <- as.factor(clust3$cluster)
  write.asciigrid(gridmaps["clust"], paste(esPath,"/landform_",Nclust,".asc",sep=''),na.value=-9999)
  
    asp <- meanAspect(dat=gridmaps@data, agg=gridmaps$landform)
    
    fun <- c("mean", "sd", "sum", "median", "min", "max")
    for (FUN in fun){
      # excluded gridmaps$landform from calculations?
      samp=FUN_sampleCentroids(dat=gridmaps@data,predNames=predNames, agg=gridmaps$landform, FUN=FUN)
      assign(paste('samp_', FUN, sep=''), samp)
    }
    
    #write to disk for cmeans(replaced by kmeans 2)
    write.table(samp_mean,paste(esPath, '/samp_mean.txt' ,sep=''), sep=',', row.names=FALSE)
    write.table(samp_sd,paste(esPath, '/samp_sd.txt' ,sep=''), sep=',', row.names=FALSE)
    write.table(samp_sum,paste(esPath, '/samp_sum.txt' ,sep=''), sep=',', row.names=FALSE)
    write.table(samp_median,paste(esPath, '/samp_median.txt' ,sep=''), sep=',', row.names=FALSE)
    write.table(samp_min,paste(esPath, '/samp_min.txt' ,sep=''), sep=',', row.names=FALSE)
    write.table(samp_max,paste(esPath, '/samp_max.txt' ,sep=''), sep=',', row.names=FALSE)
    
    #make driving topo data file  
    lsp <- listpointsMake(samp_mean=samp_mean, samp_sum=samp_sum, asp=asp, predNames = predNames)
    
    #lsp <- lsp[lsp$ele != -1,]
  
  if ("landcover" %in% names(lsp)) 
  {
    lsp$landcover <- round(lsp$landcover)
	lsp$landcover[which(lsp$landcover == 0)] <- 1
	lsp$landcover[which(lsp$landcover == (Nlandcover+1))] <- Nlandcover
    lsp$landcover[which(lsp$landcover == -1)] <- 1
  }
  if ("soil" %in% names(lsp)) 
  {
    lsp$soil <- round(lsp$soil)
	lsp$soil[which(lsp$soil == 0)] <- 1
	lsp$soil[which(lsp$soil == (Nsoil+1))] <- Nsoil
    lsp$soil[which(lsp$soil == -1)] <- 1
  }
  
  if ("dem" %in% names(lsp)) 
    lsp$dem[which(lsp$dem == -1)] <- 1
  if ("slp" %in% names(lsp)) 
    lsp$slp[which(lsp$slp == -1)] <- 1
  if ("svf" %in% names(lsp))  
    lsp$svf[which(lsp$svf == -1)] <- 1
  
  names(lsp) <- c("members", "id", names(samp_mean)[-1], "asp")
  
  if (!is.null(uniform_class["landcover"]))
    lsp$landcover <- uniform_class["landcover"]
  
  if (!is.null(uniform_class["soil"]))
    lsp$soil <- uniform_class["soil"]
    
  write.table(lsp,paste(esPath, '/listpoints.txt' ,sep=''), sep=',', row.names=FALSE, quote=FALSE)
    #make horizon files
  hor(listPath=esPath)
    
if (run_lsm)
{
    #=================================================================================================================================
    #beg inform scale section
    if(doInformScale==1){  	
      
      #training sim of LSM
      setwd(gtexPath)
      system(paste(gtex,esPath, sep=' '))
      
      #rm old gt report files and rec dir
      setwd(esPath)
      #system('rm _FAILED_*')
      system('rm _SUCCESSFUL_*')
      system('rm -r rec')
      dir.create('rec')
      setwd(eroot_loc1)
      
      #read in output
      file1<-'/soil_tem.txt'
      file2<-'/point.txt'
      param1<-read.table(paste(esPath,file1,sep=''), sep=',', header=T)
      param2<-read.table(paste(esPath,file2,sep=''), sep=',', header=T)
      
      for (col in cols){
        
        if(col=="X100.000000"){file<-'/soil_tem.txt'}else{file<-'/point.txt'}
        if(col=="X100.000000"){sim_dat<-param1}else{sim_dat<-param2}
        
        sim_dat_cut=timeSeriesCut(esPath=esPath,col=col, sim_dat=sim_dat, beg=beg, end=end)
        
        timeSeries(esPath=esPath,col=col, sim_dat_cut=sim_dat_cut,FUN=mean)
      }
      
      landform<-raster(paste(esPath,"/landform_",Nclust,".asc",sep=''))
      
      #initialise file to write to
      pvec<-rbind(predNames)
      x<-cbind("tv",pvec,'r2')
      write.table(x, paste(esPath,"/coeffs.txt",sep=""), sep=",",col.names=F, row.names=F)
      write.table(x, paste(esPath,"/decompR.txt",sep=""), sep=",",col.names=F, row.names=F)
      
      
      file1<-'/soil_tem.txt'
      file2<-'/point.txt'
      param1<-read.table(paste(esPath,file1, sep=''), sep=',', header=T)
      param2<-read.table(paste(esPath,file2, sep=''), sep=',', header=T)
      
      
      for(col in cols){
        if(col=="X100.000000"){param<-param1}else{param<-param2}
        param_cut=timeSeriesCut(esPath=esPath,col=col, sim_dat=param, beg=beg, end=end) # cut timeseries
        coeffs=linMod(param=param_cut,col=col, predNames=predNames, mod=mod, scaleIn=scaleIn) #linear model
        write(coeffs, paste(esPath,"/coeffs.txt",sep=""),ncolumns=7, append=TRUE, sep=",")
      }
      
      #mean coeffs table
      weights<-read.table(paste(esPath,"/coeffs.txt",sep=""), sep=",",header=T)
      coeffs_vec=meanCoeffs(weights=weights, nrth=nrth)
      y<-rbind(predNames)
      y <- cbind(y,'r2')
      write.table(y, paste(esPath,"/coeffs_Mean.txt",sep=""), sep=",",col.names=F, row.names=F)
      #write(coeffs_vec[1:(length(predNames)+1)], paste(esPath,"/coeffs_Mean.txt",sep=""),ncolumns=(length(predNames)+1), append=TRUE, sep=",")
      write(coeffs_vec, paste(esPath,"/coeffs_Mean.txt",sep=""),ncolumns=(length(predNames)+1), append=TRUE, sep=",")
      weights_mean<-read.table(paste(esPath,"/coeffs_Mean.txt",sep=""), sep=",",header=T)	
      
      #use original samp_dat
      informScaleDat1=informScale(data=samp_dat, pnames=predNames,weights=weights_mean)
      #kmeans on sample
      clust1=Kmeans(scaleDat=informScaleDat1,iter.max=iter.max,centers=Nclust, nstart=nstart1)
      #scale whole dataset
      informScaleDat2=informScale(data=gridmaps@data, pnames=predNames,weights=weights_mean)
      #kmeans whole dataset
      clust2=Kmeans(scaleDat=informScaleDat2,iter.max=iter.max,centers=clust1$centers, nstart=nstart2)
      
      #remove small samples, redist to nearestneighbour attribute space
      if(samp_reduce==TRUE){
        clust3=sample_redist(pix= length(clust2$cluster),samples=Nclust,thresh_per=thresh_per, clust_obj=clust2)# be careful, samlple size not updated only clust2$cluster changed
      }else{clust2->clust3}
      
      #make map of clusters
      gridmaps$clust <- clust3$cluster
      gridmaps$landform <- as.factor(clust3$cluster)
      write.asciigrid(gridmaps["clust"], paste(esPath,"/landform_",Nclust,".asc",sep=''),na.value=-9999)
      
      #mean aspect
      asp=meanAspect(dat=gridmaps@data, agg=gridmaps$landform)
      
      #get full list predNames again (separate to predictor names)
      allNames<-names(gridmaps@data)
      predNames <- allNames[which(allNames!='clust'&allNames!='landform'&allNames!='asp')]
      
      fun=c('mean', 'sd', 'sum')
      for (FUN in fun){
        samp=sampleCentroids(dat=gridmaps@data,predNames=predNames, agg=gridmaps$landform, FUN=FUN)
        assign(paste('samp_', FUN, sep=''), samp)
      }
      
      #write to disk for cmeans(replaced by kmeans 2)
      write.table(samp_mean,paste(esPath, '/samp_mean.txt' ,sep=''), sep=',', row.names=FALSE)
      write.table(samp_sd,paste(esPath, '/samp_sd.txt' ,sep=''), sep=',', row.names=FALSE)
      
      #make driving topo data file	
      lsp <- listpointsMake(samp_mean=samp_mean, samp_sum=samp_sum, asp=asp)
      write.table(lsp,paste(esPath, '/listpoints.txt' ,sep=''), sep=',', row.names=FALSE)
      
      
      #make horizon files
      hor(listPath=esPath)
      
    }#end inform scale section
    #====================================================================================================================================
    
    
    #calculate fuzzy membership
    if(fuzzy==1){
      #cmeans(x=scaleDat_samp, centers=64, iter.max = 10, verbose = TRUE,dist = "euclidean", method = "cmeans", m = 1.4,    rate.par = NULL, weights = 1, control = list())
      res=extentandcells(rstPath=paste(eroot_loc1,'/_master/ele.asc', sep=''))
      cells <- res$cells
      ext <- res$x
      
      #read in sample centroid data
      samp_mean <- read.table(paste(esPath, '/samp_mean.txt' ,sep=''), sep=',',header=T)
      samp_sd <- read.table(paste(esPath, '/samp_sd.txt' ,sep=''), sep=',', header=T)
      #for (col in cols){
      
      
      #needs to return smal matrix
      fuzzyMember(esPath=esPath,ext=ext,cells=cells,data=gridmaps@data, samp_mean=samp_mean,
                  samp_sd=samp_sd, Nclust=Nclust)
      
    }
    
    #==================================preprocessor output=========================
    #vector sample weights 'lsp$members'
    #crisp membership map  'landform.asc'
    #fuzzyMember maps 'raster_temp' folder
    
    
    #==============================================================================
    # LSM
    #==============================================================================
    
	
      setwd(esPath)
	  
    if (run_parallel)
    {
      # split listpoint file for parallel processing
      lsp_split <- split(lsp, 1:nr_sim)
      
      # create folder for parallel processing
      parallelPath <- paste(esPath, "parallel", sep="/")
      dir.create(parallelPath)
      
      # create parallel computing directories

  fromr2system <-             
      mclapply(names(lsp_split), function(x){
        # create directory 
        es_folder <- formatC( as.integer(x), width=3, flag="0" )
        sim_path  <- file.path(parallelPath,es_folder)
        dir.create(path = sim_path)
        
        #copy simulation data
        # geotop inpts
        if (file.exists(file.path(root,inpts.file))) {
          print("copy GEOtop inpts file to sim folder")
          file.copy(from = file.path(eroot_loc1, inpts.file), to = sim_path , overwrite=TRUE)
        } else {
          print("GEOtop inpts file does not exist")
        }
        
        if (exists("meteo.folder")){
          print("copy meteo input data to sim folder")
          file.copy(file.path(eroot_loc1, meteo.folder), sim_path , recursive=TRUE, overwrite=TRUE)
        }
        
        if (exists("soil.folder")){
          print("copy soil input files to sim folder")
          file.copy(file.path(eroot_loc1, soil.folder), sim_path , recursive=TRUE, overwrite=TRUE)
        }
        
        if (exists("horizon.folder")){
          print("copy horizon files data to sim folder")
          file.copy(file.path(eroot_loc1, horizon.folder), sim_path , recursive=TRUE, overwrite=TRUE)
        }
        
        # sim point horizon files
        print("copy horizon files to sim folder")
        file.copy(file.path(esPath, "hor"), sim_path , recursive=TRUE, overwrite=TRUE)
        
        #create output directory
        dir.create(paste(sim_path, "/out", sep=""))
        
        #create recovery directory
        dir.create(paste(sim_path, "/rec", sep=""))
        
        # write listpoints
        write.table(lsp_split[[x]],paste(sim_path, '/listpoints.txt' ,sep=''), sep=',', row.names=FALSE, quote=FALSE)
        
        if(!VSCjobs) 
        {
          #run geotop in sim path
          if (run_hidden) {
            system(paste("nohup", file.path(gtexPath, gtex), sim_path,"&", sep=' '))
          } else {
            system(paste(file.path(gtexPath, gtex), sim_path, sep=' '))
          }
        }
        
        # get geotop system 
        x <- paste(file.path(gtexPath, gtex), sim_path, ">", file.path(sim_path,"toposub.log"),"&", sep=' ')
        return(x)
      })
      
      if (VSCjobs) {
        # create job files for VSC dependent on number of cores
        fromr2system <- unlist(fromr2system)
        simPERcore   <- length(fromr2system)/Ncores
        
        for (i in 1:Ncores)
        {
         sysT <- gsub(pattern = " ", replacement = "_", Sys.time()) 
          
         N <- paste("#$ -N ", "_TopoSUB_", i, sep="")
         V <- "#$ -V"
         pe <- "#$ -pe mpich 16"
         l <-  paste("#$ -l h_rt=", VSCtime, sep="")
         M <-  paste("#$ -M ", VSCmail, sep="")    
         m <-  "#$ -m beas"
         
         sims <- fromr2system[(1+(i-1)*simPERcore):(i*simPERcore)]
         wait <- "wait" 
         
         infile <- c(N,V,pe,l,M,m,sims,wait)
          
         # write in job files
         write.table(x = infile, file = file.path(parallelPath,paste("job",i,".sh",sep="")), 
                                                  quote = FALSE, row.names = F, col.names = FALSE)
        }
        
        # run jobs on VSC
        if(run_jobs) {
          #system(command = paste("cd", parallelPath, sep=" "))
          for (i in 1:Ncores) {
            system(command = paste("chmod +x",  file.path(parallelPath, paste("job", i, ".sh", sep=""))))
            system(command = paste("qsub",  file.path(parallelPath, paste("job", i, ".sh", sep=""))))
          } 
        }
        
      }

    } else {
      #run geotop in sim path
      if (run_hidden) {
        system(paste("nohup", file.path(gtexPath, gtex), esPath, "&", sep=' '))
      } else {
        system(paste(file.path(gtexPath, gtex), esPath, sep=' '))
      }
    }
   
      setwd(eroot_loc1)
  }

}
