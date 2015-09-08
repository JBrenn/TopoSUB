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
#     --> 1: locations (character), 2: setup parameter (numeric), 3: switches (logical T/F or numeric 1/0)
#   
#==============================================================================
# TopoSUB preprocessor
#==============================================================================
TopoSUB_preprocessor <- function(location.file="locations.txt", setup.file="setup.txt", 
                                  PredNamesList=list(topo=c("dem"),
                                                      clas=c("landuse", "soil")),
                                  mode_ls="ols",
                                  run_lsm=FALSE, copy_master=FALSE)
{
  #load libs
  require(raster)
  
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
  
  # set working dir 1 | root dir
  setwd(root)
  # include source code(s)
  sapply(dir(src, full.names = T), source)
  
  eroot_loc1 <- file.path( root, sim, paste("1d/1d_", exper1, sep="")) # 1d path
  dir <- formatC( as.numeric(es), width=6, flag="0" ) # format experiment dir name
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
  dir.create(esPath)
  dir.create(paste(esPath, '/fuzRst', sep=''))
  dir.create(paste(esPath, '/rec', sep=''))
  mst <- paste(eroot_loc1,'/_master',sep='')
  dir.create(mst, recursive=T)
  
  # copy geotop.inpts to sim dir
  print(paste("copy files in folder ", mst, sep=""))

  # copy location and setup file
  file.copy(file.path(root,location.file), esPath , overwrite=T)
  file.copy(file.path(root,setup.file), esPath , overwrite=T)

  # copy master folder (nescessary for first run of new simulation 1d_00x)
  if (copy_master) 
  {
    if (file.exists(file.path(root,inpts.file))) {
      print("copy GEOtop inpts file")
      file.copy(file.path(eroot_loc1,inpts.file), esPath , overwrite=T)
    } else {
      print("GEOtop inpts file does not exist")
    }
    
    # copy predictor maps to sim dir
    if (exists("pred.folder")) {
      print("copy predictor maps")
      dir.create( file.path(eroot_loc1, pred.folder) )
      
      src.files.pred <- dir(file.path(root,pred.folder),full.names = T)
      dest.files.pred <- file.path(eroot_loc1,pred.folder,dir(file.path(root,pred.folder)))
      file.copy(src.files.pred, dest.files.pred, overwrite=T )         
    } else {
      print("predictor maps do not exist")
    }
    
    # copy meteo files to sim dir
    if (exists("meteo.folder")) {
      print("copy meteo files")
      dir.create( file.path(esPath, meteo.folder) )
      
      src.files.meteo <- dir(file.path(root,meteo.folder),full.names = T)
      dest.files.meteo <- file.path(eroot_loc1,meteo.folder,dir(file.path(eroot_loc1,meteo.folder)))
      file.copy(src.files.meteo, dest.files.meteo, overwrite=T )         
    } else {
      print("meteo files do not exist")
    }
    
    #copy soil files to sim dir
    if (exists("soil.folder")) {
      print("soil files ...")
      dir.create( file.path(esPath, soil.folder) )
      
      src.files.soil <- dir(file.path(root,soil.folder),full.names = T)
      dest.files.soil <- file.path(eroot_loc1,soil.folder,dir(file.path(eroot_loc1,soil.folder)))
      file.copy(src.files.soil, dest.files.soil, overwrite=T )   
    } else {
      print("soil files do not exist")
    }
    
    #copy horizon files to sim dir
    if (exists("horizon.folder")) {
      print("meteo files ...")
      dir.create( file.path(esPath, horizon.folder) )
      
      src.files.horizon <- dir(file.path(root,horizon.folder),full.names = T)
      dest.files.horizon <- file.path(eroot_loc1,horizon.folder,dir(file.path(eroot_loc1,horizon.folder)))
      file.copy(src.files.horizon, dest.files.horizon, overwrite=T )         
    } else {
      print("horizon files do not exist")
    }
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
  
  if (findn==1) Nclust=findN(scaleDat=scaleDat_samp, nMax=nMax.findn, thresh=thresh.findn)
  
  
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
  clust1=Kmeans(scaleDat=scaleDat_samp,iter.max=iter.max,centers=Nclust, nstart=nstart1)
  
  #scale whole dataset
  scaleDat_all= simpleScale(data=gridmaps@data, pnames=predNames)
  
  #====================pca experiment stuff - all
  if (pca==1){
    pc.dem <- prcomp(x=gridmaps@data, scale=F)
    demdata <- as.data.frame(pc.dem$x)
    demdata[1:npca]->scaleDat_all #select pc's
  }
  #==============================================
  
  #kmeans whole dataset
  clust2=Kmeans(scaleDat=scaleDat_all,iter.max=iter.max,centers=clust1$centers, nstart=nstart2)
  
  #remove small samples, redist to nearestneighbour attribute space
  if(samp_reduce==1){
    clust3=sample_redist(pix= length(clust2$cluster),samples=Nclust,thresh_per=thresh_per, clust_obj=clust2)# be careful, samlple size not updated only clust2$cluster changed
  }else{clust2->clust3}
  
  #make map of clusters
  gridmaps$clust <- clust3$cluster
  gridmaps$landform <- as.factor(clust3$cluster)
  write.asciigrid(gridmaps["clust"], paste(esPath,"/landform_",Nclust,".asc",sep=''),na.value=-9999)
  
    asp=meanAspect(dat=gridmaps@data, agg=gridmaps$landform)
    
    fun=c('mean', 'sd', 'sum', 'median')
    for (FUN in fun){
      # excluded gridmaps$landform from calculations?
      samp=FUN_sampleCentroids(dat=gridmaps@data,predNames=predNames, agg=gridmaps$landform, FUN=FUN)
      assign(paste('samp_', FUN, sep=''), samp)
    }
    
    #write to disk for cmeans(replaced by kmeans 2)
    write.table(samp_mean,paste(esPath, '/samp_mean.txt' ,sep=''), sep=',', row.names=FALSE)
    write.table(samp_sd,paste(esPath, '/samp_sd.txt' ,sep=''), sep=',', row.names=FALSE)
    write.table(samp_mean,paste(esPath, '/samp_sum.txt' ,sep=''), sep=',', row.names=FALSE)
    write.table(samp_sd,paste(esPath, '/samp_median.txt' ,sep=''), sep=',', row.names=FALSE)
    
    #make driving topo data file  
    lsp <- listpointsMake(samp_mean=samp_mean, samp_sum=samp_sum, asp=asp)
    
    #   narow <- which(lsp$ele == -1)
    #   
    #   if ("landcover" %in% names(lsp)) 
    #   {
    #     lsp$landcover <- round(lsp$landcover)
    #     lsp$landcover[which(lsp$landcover == -1)] <- 1
    #   }
    #   if ("soil" %in% names(lsp)) 
    #   {
    #     lsp$soil <- round(lsp$soil)
    #     lsp$soil[which(lsp$soil == -1)] <- 1
    #   }
    #   
    #   lsp$ele[which(lsp$ele == -1)] <- 0
    #   lsp$slp[which(lsp$slp == -1)] <- 0
    #   lsp$svf[which(lsp$svf == -1)] <- 0
    
    #lsp <- lsp[lsp$ele != -1,]
    
    write.table(lsp,paste(esPath, '/listpoints.txt' ,sep=''), sep=',', row.names=FALSE)
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
    
      setwd(gtexPath)
      system(paste(gtex,esPath, sep=' '))
      setwd(eroot_loc1)
    
  }

 
 
}
