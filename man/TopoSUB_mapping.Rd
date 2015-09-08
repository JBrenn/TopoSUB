\name{TopoSUB_mapping}
\alias{TopoSUB_mapping}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mapping of TopoSub output}
\description{
mapping of TopoSub output
}
\usage{
TopoSUB_mapping(wpath, key, vars, date, fun, Nclust, devision)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wpath}{
GEOtop simuation path
}
  \item{keys}{
vector of keywords, for further information refer to \pkg{geotopbricks}
}
  \item{vars}{ 
list of variables, list entries are conected to the vector of keywords
}
  \item{date}{
dates processing, date-object vector; if single date maps of this day will be visualized; if sequence of days an aggregated map will be produced, according to the function provided (see below); if NULL all dates in the output will be processed and visualized (e.g. for further processing with ImageMagick - animation)
}
  \item{fun}{ 
function for aggregation, if date is a sequence of dates
}
  \item{Nclust}{ 
number of cluster centroids simulated
}
 \item{devision}{ 
e.g. number of years for climatic yearly mean, if function sum is used
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
%%  ~~who you are~~
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
