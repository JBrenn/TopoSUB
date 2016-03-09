\name{TopoSUB_preprocessor}
\alias{TopoSUB_preprocessor}
\title{
TopoSUB preprocessor: landscape clustering and GEOtop
}
\description{
The TopoSUB preprocessor is clustering landscape features as topography, climate, land use and soil types in order to prvide input simulation points for the LSM GEOtop. Each cluster centroid is simulated in the model's one dimensional mode.
}
\usage{
TopoSUB_preprocessor(location.file="locations.txt", setup.file="setup.txt", 
                      PredNamesList=list(topo=c("dem"), clas=c("landuse", "soil")), mode_ls="ols", 
                      run_lsm=FALSE, copy_master=FALSE,
                      uniform_class = c(landcover=NULL, soil=NULL))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{location.file}{
  name of file file defining data locations, e.g. input maps
}
  \item{setup.file}{
  name of file defining TopoSUB model parameter
}
 \item{PredNamesList}{
  list defining continious (e.g. topography, climate) and discrete features (e.g. land use, soil types)
}
 \item{mode_ls}{
  mode of least square esimation, default "ols": ordinary least square
}
 \item{run_lsm}{
  boolean, default FALSE: execute function without running land surface model (GEOtop), TRUE running LSM
}
 \item{copy_master}{
  boolean, default FALSE: execute function without copying _master folder, TRUE copy _master folder
}
  \item{uniform_class}{
  define uniform value for classes, e.g. one landcover/soil type class for the whole simulation domain, default c(landcover=NULL, soil=NULL) no uniform classes set.
  }
} 
\details{
PredNamesList is defined by two named character vectors: list( topo=c(), clas=c() )
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Fiddes, J., & Gruber, S. (2012). TopoSUB: a tool for efficient large area numerical modelling in complex topography at sub-grid scales. Geoscientific Model Development Discussions, 5(5), 1245–1257. doi: 10.5194/gmd-5-1245-2012
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
