\name{TopoSUB_droughtEstim}
\alias{TopoSUB_droughtEstim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sequential dry days of a SMC series
}
\description{
Calculates number of sequential dry days ("drought" duration D), intensity (I), and severity (S) of a soil moisture time series (used within function \emph{TopoSUB_droughtEstimation}). Method adapted from Shefield & Wood (2008).
}
\usage{
TopoSUB_droughtEstim(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
data.table, columns: Date, IDpoint, Variable, Month, Year, YearMon, base_Q10, VAR_QU10 (grouped by IDpoint, Month)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data frame, containing start and end day of "drought", D, I, and S.
}
\references{
Sheffield, J., & Wood, E. F. (2008). Projected changes in drought occurrence under future global warming from multi-model, multi-scenario, IPCC AR4 simulations. Climate Dynamics, 31(1), 79–105. http://doi.org/10.1007/s00382-007-0340-z
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
