\name{TopoSUB_read}
\alias{TopoSUB_read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read topoSUB output
}
\description{
Read topoSUB output and merge data to a data.table object
}
\usage{
TopoSUB_read(wpath, keys, doLEHcalc=TRUE, SnowCoverThres=5, select)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wpath}{
path into topoSUB simulation
}
\item{keys}{
keywords, for further information refer to \pkg{geotopbricks}
}
\item{doLEHcalc}{
boolean, if TRUE (default) latent and sensible heat fluxes over canopy are calculated (for details see \emph{AnalyseGeotop::GEOtop_EfluxOcanopy})
}
\item{SnowCoverThres} Threshold (in mm H20) above which snow cover is given  
}
\item{select}{
variables to select, see select argument in data.table::fread 
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
