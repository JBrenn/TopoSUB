#v3.11 
#2/3/12

#==============================================================================
# INPUT FUNCTIONS
#==============================================================================

concat <- function(){
#method to concatenate
"&" <- function(...) UseMethod("&") 
"&.default" <- .Primitive("&") 
"&.character" <- function(...) paste(...,sep="") 
}


##makes raster stack of inputs
stack_raster<-function(demLoc,predNames, predRoot, predFormat, demName){
	require(rgdal)
	require(raster)
	
	#make gridmaps file
	gridmaps <- readGDAL(demLoc)
	names(gridmaps)[1] <- demName
	for (pred in predNames){ # repeat readin of dem not a problem
		gridmaps@data[pred]<- readGDAL(paste(predRoot,'/',pred,predFormat, sep=''))$band1
	}
	return(gridmaps)
}

sampleRandomGrid<-function(nRand, predNames, demName){
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

#converts radians to degrees
radtodeg <- function(radRaster){
	degRaster <- radRaster*(180/pi)
	return(degRaster)
}

#converts degrees to radians
degtorad <- function(degRaster){
	radRaster <- degRaster*(pi/180)
	return(radRaster)
}

#convert NA to numeric
natonum <- function(x,predNames,n){
	
	for (pred in predNames){
		x[pred][is.na(x[pred])]<- n
		#x[is.na(x)]<- n
	}
	return(x)
}

#decompose aspect to cos/sine components (accepts in degrees only)
aspect_decomp <- function(asp){
	aspC<-cos((pi/180)*asp)
	aspS<-sin((pi/180)*asp)
	return(list(aspC=aspC,aspS=aspS))
}

#==============================================================================
# KMEANS CLUSTERING functions
#==============================================================================
#scale and make dataframe 

#@celenius center=true just means remove the mean, and 												scale=TRUE stands for divide by SD; in other words, 												with both options active, you're getting standardized 												variables (with mean 0, unit variance, and values 												expressed in SD units)

simpleScale <- function(data, pnames){
	scaleDat <-data.frame(row=dim(data)[1])
	for (pred in pnames){
		b <- as.vector(assign(paste(pred,'W',sep='') , scale(data[pred],center = TRUE, scale = TRUE)))
		scaleDat <- data.frame(scaleDat, b , check.rows=FALSE)
	}
	
	#remove init column
	scaleDat <- scaleDat[,2:(length(pnames)+1)]
#chnge co names
	colnames(scaleDat)<-pnames
	return(scaleDat)
}


findN<-function(scaleDat, nMax, thresh, eroot_loc1, plot=FALSE){
	
	#calc. approx number of cluster - dont need to do for weighted run
#make exp sequence
nseq=c()
for (i in 2:sqrt(nMax)){
i^2->n
nseq=c(nseq,n)
}

	
wss1 <- (nrow(scaleDat)-1)*sum(apply(scaleDat,2,var))
	
if (plot)
  jpeg(paste(eroot_loc1,'/wss.jpg',sep=''),width=600,height=800)

wss<-sapply(nseq, function(x) sum(kmeans(scaleDat, centers=x)$withinss))
#totss: The total sum of squares.
#withinss: Vector of within-cluster sum of squares, one component per cluster.
#betweens: The between-cluster sum of squares, i.e. totss-tot.withinss.
#tot.withinss: Total within-cluster sum of squares, i.e., sum(withinss).
f<-sapply(nseq, function(x) (kmeans(scaleDat, centers=x)$betweenss)/(kmeans(scaleDat, centers=x)$tot.withinss))
if (exists("wss")) f <- wss

	plot(nseq, f, type="b", xlab="Number of Clusters", ylab="Sum of within groups sum of squares")
	dev.off()
	max(f)->a
  Nclust <- round(head(f[f/a < thresh],1))
  print(paste("Number of cluster for threshhold =", thresh, " : ", Nclust, sep=""))
	
	return(Nclust)
}


Kmeans <- function(scaleDat,iter.max,centers, nstart){
#kmeans algorithm (Hartingen and Wong)
	clust <- kmeans(scaleDat, iter.max=iter.max,centers=Nclust, nstart=nstart)
#make map of clusters
	#gridmaps$clust <- clust$cluster
	#gridmaps$landform <- as.factor(clust$cluster)
	return(clust)
}


#eliminate small samples from kmeans object
sample_redist<-function(pix,samples,thresh_per, clust_obj){

dist(clust_obj$centers, method='euclidean')->dist_matrix # calculate dists
as.matrix(dist_matrix)->dm

(pix/samples)*thresh_per->samples_dead #  define threshold

rank(clust_obj$size)->rank_size # index
sort(clust_obj$size)->sort_size #sort by size
data.frame(rank_size,sort_size)->v #
v$rank_size[v$sort_size < samples_dead]->samples_rm_vec # vector of sample index to be removed


for(i in samples_rm_vec){	#loop thru samples_rm
sort(dm[i,], decreasing=T)->sort_dm
sort_dm[1]->a
as.numeric(names(a))->cluster_redist # nearest neighbour cluster to redistribute pixels to 
clust_obj$cluster[clust_obj$cluster==i]<-cluster_redist#remap all pixels to new cluster
}
return(clust_obj)
}



#correct asp
meanAspect <- function(dat,agg){
	class.asp <- aggregate(dat[c("aspC", "aspS")], by=list(agg), FUN="sum")
	aspMean<-atan2(class.asp$aspS, class.asp$aspC)	
	aspMean <- aspMean*(180/pi )
	aspMean<-ifelse(aspMean<0,aspMean+360, aspMean)# correct negative values
	asp <- aspMean
	asp[is.na(asp)]<- -1
	return(asp)
}
#option - use wss to define approx N (SLOW)

#kmClust<-function(gridmaps,Nclust, nMax=170,thresh=0.05, nstart=5, esPath, WSS=0){


#sample cluster centres	
sampleCentroids <- function(dat,predNames, agg, FUN){
	samp <- aggregate(dat[predNames], by=list(agg), FUN=FUN)
	samp$svf<-round(samp$svf,2) 
	return(samp)
}

listpointsMake <- function(samp_mean, samp_sum, asp, predNames){
	
	#make listpoints	

	mem<-samp_sum[2]/samp_mean[2]
	mem[predNames[1]]->members
	colnames(samp_mean)[1] <- "ID"
	listpoints<-data.frame(members,samp_mean,asp)
	return(listpoints)
}



#horizon source here
hor<-function(listPath){
	#reads listpoints at 'listPath' - writes hor dir and files to listPath

		listpoints<-read.table(paste(listPath,'/listpoints.txt', sep=''), header=T, sep=',')
		ID=listpoints$ID
		ID<-formatC(ID,width=4,flag="0")
		
		slp=listpoints$slp
		svf=listpoints$svf
		
		(((acos(sqrt(svf))*180)/pi)*2)-slp ->hor.el
		round(hor.el,2)->>hor.el
		dir.create(paste(listPath,'/hor',sep=''), showWarnings = TRUE, recursive = FALSE)
		n<-1 #initialise ID sequence
		for (hor in hor.el){
			IDn<-ID[n]
			Angle=c(45,135,225,315)
			
			Height=rep(round(hor,2),4)
			
			hor=data.frame(Angle, Height)
			write.table(hor, paste(listPath,'/hor/hor_point',IDn,'.txt',sep=''),sep=',', row.names=FALSE, quote=FALSE)
			
			n<-n+1
		}}





#==============================================================================
# linear model for weights functions
#==============================================================================

#just coefficients
linMod <- function(param,col, predNames, mod, scaleIn){
	require(MASS)
	require(relaimpo)
require(nlme)
#calc annual mean per TV
	meanX<-	tapply(param[,col],param$IDpoint, mean)
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
if(mod=='ols'){fit <- lm(dat$meanX ~ dat$ele+ dat$slp+ dat$svf + dat$aspC+ dat$aspS)}else{
fit <- gls(meanX ~  ele  + slp + svf + aspC + aspS, dat=dat)
}


	# !remember order of coeffs is tied to order of header in cryosub_prog [order of predNames]! ==> potential bug - fix
	summary(fit) # show results
	coefficients(fit)->coef
	round(summary(fit)$r.squared,2)->r2
	coef[1:length(predNames)+1]->x
	coeffs<-c(col,x,r2)
	
	return(coeffs)
#return(list(coeffs=coeffs,fit=fit, fit_simp=fit_simp))
}

meanCoeffs <- function(weights, nrth){
	
	ele <- mean(abs(weights$ele))
	slp<- mean(abs(weights$slp))
	svf<- mean(abs(weights$svf))
	aspS<- mean(abs(weights$aspS))
	if(nrth==TRUE){north<- mean(abs(weights$north))}else{aspC<- mean(abs(weights$aspC))}
	if(nrth==TRUE){x<-c(ele,slp,svf,aspS, north)}else{x<-c(ele,slp,svf,aspC,aspS)}
	coeff_vec<- (x/sum(x))
	r2 <- mean(weights$r2)
	x<-c(coeff_vec,r2)

return(x)
}


#==============================================================================
# weighted KMEANS CLUSTERING - lsm samples
#==============================================================================

informScale <- function(data, pnames,weights){
	scaleDat <-data.frame(row=dim(data)[1])
	for (pred in pnames){
		a <- weights[pred][1,1]
		b <- as.vector(assign(paste(pred,'W',sep='') , data[pred]*a))
		scaleDat <- data.frame(scaleDat, b , check.rows=FALSE)
	}
	
#remove init column
	scaleDat <- scaleDat[,2:(length(pnames)+1)]
	return(scaleDat)
}



extentandcells <- function(rstPath){
#get extent
	raster(rstPath)->x
#get number cells
	ncell(x)->cells
	return(list(cells=cells,x=x))
	
}

#function to get stack rasters
stackRst<-function(path,ngrid, type){
	gridNa<-c()
	for (n in 1:ngrid){
	n=formatC(n,width=2,digits = 0,flag="0", format="f")
		paste(path,n,type,sep="")->gridi
		gridNa<-c(gridNa,gridi)
	}
	gridNa<-as.list(gridNa)
	stk<-stack(gridNa)
	return(stk)
}


fuzzyMember <- function(esPath,ext,cells,data, samp_mean, samp_sd, Nclust){
#create dir for rasters (fuzzy membership)
	paste(esPath, '/raster_tmp', sep='')->rstdir
	dir.create(rstdir)
	
	#calc distances/ write rasters for n clusters
	for(c in (1:Nclust)){
		distmaps <- as.list(seq(1:Nclust))
		tmp <- rep(NA, cells)
		distsum <- data.frame(tmp)
		distmaps[[c]] <- data.frame(ele=tmp,slp=tmp, aspC=tmp,aspS=tmp, svf=tmp)
		
		for(j in predNames){
			distmaps[[c]][j] <- (((gridmaps@data[j]-samp_mean[c,j])/samp_sd[c,j])^2)
			#distmaps[[c]][j] <- (((aspC-class.c[c,j])/class.sd[c,j])^2)	
		}
		sqrt(rowSums(distmaps[[c]], na.rm=T, dims=1))->v
		setValues(ext,v)->n
		
		c=formatC(c,width=2,digits = 0,flag="0", format="f")#bug fix
		
		writeRaster(n, paste(rstdir,"/tmp_", c, sep=""), overwrite=T)
		rm(distmaps)
		rm(distsum)
	}

	rst=stackRst(paste(rstdir,'/tmp_', sep=''),ngrid=Nclust, type='')
	
	for(c in (1:Nclust)){
		tot<- (subset(rst,c)^(-2/(fuzzy.e-1)))
		c=formatC(c,width=2,digits = 0,flag="0", format="f")#bug fix
		writeRaster(tot,paste(rstdir,"/tot_", c, sep=""), overwrite=T)
	}
	
	tot=stackRst(paste(rstdir,'/tot_', sep=''),ngrid=Nclust, type='')
	totsum <- sum(tot)
	
	#calc membership stack
	for(c in (1:Nclust)){
		x <- (subset(rst,c)^(-2/(fuzzy.e-1))/totsum)
		c=formatC(c,width=2,digits = 0,flag="0", format="f")#bug fix
		writeRaster(x,paste(rstdir,"/tmp_", c, sep=""), overwrite=T)
	}
}


#==============================================================================
# time series cut 
#==============================================================================

timeSeriesCut <- function(esPath,col, sim_dat, beg, end){
	
	#Period:
	#beg <- "01/07/2010 00:00:00"
	#end <- "01/07/2011 00:00:00"
	timeRange <-strptime(c(beg, end), format="%d/%m/%Y %H:%M:%OS")
	beg <- timeRange[1]
	end <- timeRange[2]
	
#Read data from file and prepare date and temperature arrays:
	inDat  <- sim_dat
	dates  <- strptime(inDat[,1], "%d/%m/%Y %H:%M")
	dims   <- dim(inDat)
	nDep   <- dims[[2]] - 1
	nLin   <- dims[[1]]
	tmps   <- data.matrix(inDat[,2:(nDep+1)])
	tmps   <- data.matrix(inDat)
#Cut dates and temperature series to time range:
	lines   <- ((dates)>=beg) & ((dates)<=end) #set all true within tr
	cut <- tmps[lines]
	dCut    <- dates[lines]
	tmpsCut <- tmps[lines,]
	nlines  <- sum(lines)
	as.data.frame(tmpsCut)->sim_dat_cut
	return(sim_dat_cut)
}
#==============================================================================
# time series aggregation 
#==============================================================================

timeSeries <- function(esPath,col, sim_dat_cut, FUN){
	meanX<-	tapply(sim_dat_cut[,col],sim_dat_cut$IDpoint, FUN)
	
	write.table(meanX, paste(esPath, '/meanX_', col,'.txt', sep=''), sep=',')
}

#==============================================================================
# SPATIALISE 
#==============================================================================
spatial<-function(col=col, esPath=esPath, format='asc', Nclust=Nclust){
	
	paste(esPath, '/raster_tmp', sep='')->rstdir
	

	meanX <- read.table(paste(esPath, '/meanX_', col,'.txt', sep=''), sep=',', header=T)
	#stack cluster weight rasters
	
	#brick('netCDF.nc')->rst
	rst=stackRst(paste(rstdir,'/tmp_', sep=''),ngrid=Nclust, type='')
	
	for(n in (1:Nclust)){
		x<-subset(rst,n)*meanX[n,]
		n=formatC(n,width=2,digits = 0,flag="0", format="f")#bug fix
		writeRaster(x,paste(rstdir, "/tv_", n, sep=""), overwrite=T)
	}
	
	tvRst=stackRst(paste(rstdir,'/tv_', sep=''),ngrid=Nclust, type='')
	sum(tvRst)->fuzRst

	
	writeRaster(fuzRst, paste(esPath, '/fuzRst/fuzRst_',col, '.', format, sep=''), overwrite=T)
	
#endScript
}



crispSpatial_noinform<-function(col,Nclust, esPath, landform){

	dir.create(paste(esPath,'/crispRst_noinform/',sep=''))
		
		
		#raster(paste(esPath,"/landform_",es,"Weights.asc",sep=''))->land
	landform->land
	
	meanX <- read.table(paste(esPath, '/meanX_', col,'.txt', sep=''), sep=',', header=T)
		
	as.vector(meanX$x)->meanX
		length(meanX)->l
		seq(1,l,1)->seq
		as.vector(seq)->seq
		
		data.frame(seq,meanX)->meanXdf
		
		
		subs(land, meanXdf,by=1, which=2)->rst
		
		writeRaster(rst, paste(esPath, '/crispRst_noinform/',col,'_',l,'.asc', sep=''),overwrite=T)
	
}

crispSpatial<-function(col,Nclust, esPath, landform){

	dir.create(paste(esPath,'/crispRst/',sep=''))
		
		
		#raster(paste(esPath,"/landform_",es,"Weights.asc",sep=''))->land
	landform->land
	
	meanX <- read.table(paste(esPath, '/meanX_', col,'.txt', sep=''), sep=',', header=T)
		
	as.vector(meanX$x)->meanX
		length(meanX)->l
		seq(1,l,1)->seq
		as.vector(seq)->seq
		
		data.frame(seq,meanX)->meanXdf
		
		
		subs(land, meanXdf,by=1, which=2)->rst
		
		writeRaster(rst, paste(esPath, '/crispRst/',col,'_',l,'.asc', sep=''),overwrite=T)
	
}

plotRst <- function(esPath,rootRst, name, formt='.asc',Nclust){
	
	if (name=='gst')(name <- "X100.000000"  )
	if (name=='swe')(name <- "snow_water_equivalent.mm."  )
	if (name=='swin')(name <-  "SWin.W.m2."      )
	if (name=='tair')(name <-  "Tair.C."       )
	
	if (rootRst=='crisp'){rootRst <- paste(esPath,'/crispRst', sep='')}
	if (rootRst=='fuzzy'){rootRst <- paste(esPath,'/fuzRst',sep='')}
rst<-raster(paste(rootRst,'/',name,'_',Nclust,formt,sep=''))
plot(rst)
}



copyFile <- function(expRoot, masterRoot,filename, minExp, maxExp){
seq(minExp,maxExp,1)->expSeq
expSeq<-formatC(expSeq,width=6,digits = 0,flag="0", format="f")
for(i in expSeq){
	
	file.copy(paste(masterRoot,'/',filename, sep=''), paste(expRoot, i,sep=''), overwrite=TRUE)
	
}
}

	
