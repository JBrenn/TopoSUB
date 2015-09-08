#toposub 

# cut vegetation period (april - september)
vegetationperiodcut <- function(esPath, col, sim_dat)
{
  date <- as.Date(sim_dat$Date12.DDMMYYYYhhmm.,format="%d/%m/%Y")
  month <- months(date, abbreviate=T)
  
  #veg_per <- unique(month)[4:9]
  
  out <- sim_dat[(month == "Apr" | month == "May" | month == "Jun" | month == "Jul" | month == "Aug" | month == "Sep"),]
  return(out)
}

# argument FUN for timeseries()-function
#   mean of yearly sums for swe and eta

FUN_crispSpatial <- function(col, Nclust, esPath, landform, ascii=TRUE, 
                             coorsys="+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
  
  dir.create(paste(esPath,'/crispRst/',sep=''))
  
  meanX <- read.table(paste(esPath, '/meanX_', col,'.txt', sep=''), sep=',', header=T)
  
  as.vector(meanX$x)->meanX
  length(meanX)->l
  seq(1,l,1)->seq
  as.vector(seq)->seq
  
  data.frame(seq,meanX) -> meanXdf

  subs(landform, meanXdf, by=1, which=2) -> rst
  #rstVal <- getValues(rst)
  #set coordinate system
  crs(rst) <- coorsys
  
  if (!is.na(rst@data@values[1]))
    rst@data@values <- ifelse(rst@data@values==rst@data@values[1], NA, rst@data@values)
  
#  writeRaster(x=rst, filename=paste(esPath, '/crispRst/',col,'_',l,'.asc', sep=''), 
#              overwrite=T, format="ascii", NAflag=-9999)

  if (ascii==TRUE) {
    rstGrid <- as(rst, 'SpatialGridDataFrame')
    write.asciigrid(x=rstGrid, fname=paste(esPath, '/crispRst/',col,'_',l,'.asc', sep=''), na.value=-9999)
    print(paste(esPath, '/crispRst/',col,'_',l,'.asc', " written", sep=''))
  } else {
    writeRaster(x=rst, filename=paste(esPath, '/crispRst/',col,'_',l,'.tif', sep=''), format="GTiff", overwrite=T, 
                datatype="FLT4S", NAflag=-9999)
    print(paste(esPath, '/crispRst/',col,'_',l,'.tif', " written", sep=''))
  }  
}

#==============================================================================
# time series aggregation 
#==============================================================================

FUN_timeSeries <- function(esPath,col, sim_dat_cut, FUN, narow){
  meanX<-	tapply(sim_dat_cut[,col],sim_dat_cut$IDpoint, FUN)
  
  meanX[,1] <- c(meanX[1:(narow-1),1],-9999,meanX[(narow-1):length(meanX[,1]),1])
  
  write.table(meanX, paste(esPath, '/meanX_', col,'.txt', sep=''), sep=',')
}

SEAS_timeSeries <- function(esPath,col, sim_dat_cut, FUN, seas){
  meanX<-	tapply(sim_dat_cut[,col],sim_dat_cut$IDpoint, FUN)
  
  write.table(meanX, paste(esPath, '/meanX_', col, seas, '.txt', sep=''), sep=',')
}

#==============================================================================
# sample Centroids 
#==============================================================================

FUN_sampleCentroids <- function(dat,predNames, agg, FUN){
  samp <- aggregate(dat[predNames], by=list(agg), FUN=FUN)
  #samp$svf<-round(samp$svf,2) 
  return(samp)
}

#just coefficients
FUNlinMod <- function(param,col, predNames, mod, scaleIn){
  require(MASS)
  require(relaimpo)
  require(nlme)
  #calc annual mean per TV
  meanX<-  tapply(param[,col],param$IDpoint, mean)
  #read in listpoints
  listpoints<-read.table(paste(esPath, '/listpoints.txt',sep=''), sep=',', header=T)
  #loop to create dataframe of TV and predictors
  dat <- data.frame(meanX)
  for (pred in predNames){
    dat <- data.frame(dat, listpoints[pred])
  }
  
  ##################################################
  
  #pc.dem <- prcomp(x=listpoints[predNames])
  #demdata <- as.data.frame(pc.dem$x)
  
  #	fit <- lm(dat$meanX ~ demdata$PC1+ demdata$PC2 + demdata$PC3)
  #	summary(fit) # show results
  #	coefficients(fit)->coef
  #	round(summary(fit)$r.squared,2)->r2
  #	coef[1:length(predNames)+1]->x
  #	coeffs<-c(col,x,r2)
  ############################################################
  
  #scale inputs
  if(scaleIn==TRUE) {dat= simpleScale(dat, pnames=c('meanX', predNames)
  )}
  
  #select ols or gls
  if(mod=='ols'){fit <- lm(dat$meanX ~ dat$ele+ dat$slp+  dat$svf + dat$landcover+ dat$aspC+ dat$aspS)}else{
    fit <- gls(meanX ~  ele  + slp +  svf + landcover + aspC + aspS, dat=dat)
  }
  
  
  # !remember order of coeffs is tied to order of header in cryosub_prog [order of predNames]! ==> potential bug - fix
  summary(fit) # show results
  coefficients(fit)->coef
  round(summary(fit)$r.squared,2)->r2 # not working with 'gls', include pseudo R-squared?
  # former
  # coef[1:length(predNames)+1]->x
  coef[2:(length(predNames)+1)]->x
  coeffs<-c(col,x,r2)
  
  return(coeffs)
  #return(list(coeffs=coeffs,fit=fit, fit_simp=fit_simp))
}

meanCoeffs <- function(weights, nrth){
  
  ele <- mean(abs(weights$ele))
  slp<- mean(abs(weights$slp))
  svf<- mean(abs(weights$svf))
  aspS<- mean(abs(weights$aspS))
  landcover <- mean(abs(weights$landcover))
  if(nrth==TRUE){north<- mean(abs(weights$north))}else{aspC<- mean(abs(weights$aspC))}
  if(nrth==TRUE){x<-c(ele,slp,svf,landcover,aspS, north)}else{x<-c(ele,slp,svf,landcover,aspC,aspS)}
  coeff_vec<- (x/sum(x))
  r2 <- mean(weights$r2)
  x<-c(coeff_vec,r2)
  
  return(x)
}

FUNsampleRandomGrid<-function(gridmaps, nRand, predNames, demName){
  #beg function
  require(raster)
  
  #index of random sample = rand_vec
  ele<-raster(gridmaps[demName])
  sampleRandom(ele, nRand,cells=T, na.rm=T)->x
  x[,1]->rand_vec
  
  #read in rasters/stack
  for (pred in predNames){
    assign(pred, raster(gridmaps[pred]))
    if (pred==predNames[1]){stack(get(pred))->stk}else {stack(stk, get(pred))->stk}
  }
  
  #extract indexed values from all layers -> cbind
  sample_df=c()
  for (i in 1:length(predNames)){
    extract(stk[[i]], rand_vec)-> sample_vec
    sample_df<-cbind(sample_df, sample_vec)
  }
  
  #rectify names and class
  colnames(sample_df)<-predNames
  as.data.frame(sample_df)->sample_df
  #end function
}

