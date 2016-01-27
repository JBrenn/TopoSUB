\name{TopoSUB_remap}
\alias{TopoSUB_remap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mapping functionaltities for TopoSUB simulation.
}
\description{
A post-processing tool for TopoSUB-GEOtop simulations. Re-mapping of cluster centroids for climate periods (30years) on yearly, seasonal and monthly basis. Calculation of differences between periods (e.g. "near future" - "baseline") and post-processing functions for specific variables (e.g. snow cover days from snow water equivalent, drought indicies from soil water content). Output is stored in TopoSUB simulation path: OUTcsv - averages year and month, OUTperiod: raster maps for climate perids and differences, OUTsequence: ...
}
\usage{
TopoSUB_remap(data, variable, wpath, 
              location.file = "locations.txt", setup.file = "setup.txt",
              periods=list(baseline = c(1980,2010), per1 = c(2020,2050), 
                            per2 = c(2045,2075), per3 = c(2070,2100)),
              periods_aggr = list(aggr = c("season", "veg_period"), fun = "mean", 
                                  diff = c("absolute","percentage")),
              sequence = list(period = c(1980,2010),
                              aggr = c("year","season","month")),
              postprocess = NULL, coords = "+proj=utm +zone=32 ellps=WGS84")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data.table as retrieved from function "TopoSUB_read" 
}
  \item{variable}{name of variable to re-map, corresponds to names(data) 
}
 \item{wpath}{path into topoSUB simulation 
}
 \item{periods}{list containing period ranges
}
 \item{periods_aggr}{list containing aggregation features (aggr), function for aggregation (fun) and features for calculating diffenences between periods (diff)
}
 \item{sequence}{list containing sequence range (period) and features for aggregation (aggr)
}
 \item{postprocess}{postprocess function, if NULL no postprocessing
}
 \item{coords}{specification of coordinate system
}

}


}
\details{
%%  ~~ If necessary, more details than the description above ~~ 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
