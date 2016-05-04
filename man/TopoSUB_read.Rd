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
TopoSUB_read(wpath, keys, doLEHcalc=TRUE, SnowCoverThres=5, select, setup.file="setup.txt", add_listfile=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wpath}{
path into topoSUB simulation
}
\item{keys}{
vector of keywords, e.g. c("PointOutputFileWriteEnd","SoilLiqContentProfileFileWriteEnd", "SoilIceContentProfileFileWriteEnd"), for further information refer to \pkg{geotopbricks}
}
\item{select}{
named list of variables to select, see select argument in data.table::fread, list names refer to argument keys, names of variable to select refer to coloumn names of TopoSUB-GEOtop output files
}
\item{doLEHcalc}{
boolean, if TRUE (default) latent and sensible heat fluxes over canopy are calculated (for details see \emph{AnalyseGeotop::GEOtop_EfluxOcanopy})
}
\item{SnowCoverThres}{
Threshold (in mm H20) above which a snow cover day is defined
}
\item{setup.file}{
name of TopoSUB setup configuration file
}
\item{add_listpoints}{
boolean, if TRUE, attributes of listpoint file are join to output data, default=FALSE
}
\item{write_sqlite}{
boolean, if TRUE, sqlite database is written from function output, default=FALSE
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
\keyword{TopoSUB}
\keyword{read output}% __ONLY ONE__ keyword per line
