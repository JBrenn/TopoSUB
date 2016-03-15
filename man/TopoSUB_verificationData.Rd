\name{TopoSUB_verificationData}
\alias{TopoSUB_verificationData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrival of sim/obs data
}
\description{
Retrieve zoo object of sim/obs data set for modek verification

}
\usage{
TopoSUB_verificationData(wpath, station, variable, meta_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wpath}{
GEOtop-TopoSUB simulation folder
}
  \item{station}{
station name refering to file names in simulation folder "obs"  
}
  \item{variable}{
variable name refering to GEOtop-TopoSUB variable output names, except for soil water content for which names are "SWC_z[depth in cm]_[SensorName]"
}
  \item{meta_data}{
data frame of meta data containing representative custer cetroid for each observation station, from function \emph{TopoSUB_getCluster4verification}
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
Johannes Brenner, \email{johannesbrenner@gmail.com}
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
