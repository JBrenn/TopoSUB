\name{TopoSUB_EfluxOcanopy}
\alias{TopoSUB_EfluxOcanopy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of latent and sensible heat fluxes over canopy
}
\description{
Calculation of latent and sensible heat fluxes over canopy from GEOtop point output file}
\usage{
TopoSUB_EfluxOcanopy(data, canopy_fraction=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{list, containing zoo objects of latent and sensible heat fluxes (ground veg - g_veg, ground bare - g_unveg, canopy - veg, canopy fraction - cf); list names: "LE"  and "H", zoo header refers to GEOtop point file
}
  \item{canopy_fraction}{numeric, default = NULL, canopy fraction for simulation point, set if not available from simulation point output}
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
Johannes Brenner, \email{johannesbrenner@gmail.com
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
