\name{TopoSUB_basinClimMean}
\alias{TopoSUB_basinClimMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate time evolution of climatic mean (30 years) 
}
\description{
Aggregate, group TopoSUB output data.table and calculate time evolution of climatic mean (30 years). Data input from \emph{TopoSUB_read()} }
\usage{
TopoSUB_basinClimMean(data, var, summfun, climK=30, doClimMean=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
tbl object, from \emph{TopoSUBread()}
}

  \item{var}{
character, choose variable from coloum names of data
}

  \item{summfun}{
summary function to create yearly aggregates from daily data
}

  \item{climK}{
integer, k value for moving window, default = 30
}

  \item{doClimMean}{
boolean, if TRUE (default) time evolution of climatic mean (30 years) is calculated
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
