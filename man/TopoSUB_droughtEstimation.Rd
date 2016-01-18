\name{TopoSUB_droughtEstimation}
\alias{TopoSUB_droughtEstimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sequential dry days of a SMC series modeled with TopoSUB/GEOtop
}
\description{
Calculates number of sequential dry days ("drought" duration D), intensity (I), and severity (S) of a soil moisture time series. Climatic periods of 30 years are conducted, baseline is covering the first 30 years of the time series. Method adapted from Sheffield & Wood (2008). 
}
\usage{
TopoSUB_droughtEstimation(data_)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_}{
data table, soil moisture output TopoSUB/GEOtop simulation, expected columns: Date, IDpoint, SMC
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
list containing output from function \emph{TopoSUB_droughtEstim} for each climate period, starting with the baseline period and proceeding with an one year moving window towards the last 30 years of the time series.

}
\references{
Sheffield, J., & Wood, E. F. (2008). Projected changes in drought occurrence under future global warming from multi-model, multi-scenario, IPCC AR4 simulations. Climate Dynamics, 31(1), 79–105. http://doi.org/10.1007/s00382-007-0340-z
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{

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
