\name{TopoSUB_POSTcritSWC_fc_wp}
\alias{TopoSUB_POSTcritSWC_fc_wp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Critical soil moisture levels
}
\description{
Calculation of critical soil moisture levels dependent on soil parameterization, in particular on field capacity and wilting point (for details see Allen et al. 2010, Jasper et al. 2007).
}
\usage{
TopoSUB_POSTcritSWC_fc_wp(data, dry_thres = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
zoo object of soil oisture data for X cluster centroids, coloumns correspond to the IDs in the listpoints file.
}  \item{dry_thres}{
threshold defining severity of dryness, default = 30: 30 percent (severe dryness) of field capacity; 50: 50 percent (moderate dryness) of field capacity. 
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
Allen, C. D., Macalady, A. K., Chenchouni, H., Bachelet, D., Mcdowell, N., Vennetier, M., … Cobb, N. (2010). A global overview of drought and heat-induced tree mortality reveals emerging climate change risks for forests. Forest Ecology and Management, 259(4), 660–684. doi: 10.1016/j.foreco.2009.09.001

Jasper, K., Calanca, P., & Fuhrer, J. (2006). Changes in summertime soil water patterns in complex terrain due to climatic change. Journal of Hydrology, 327(3-4), 550–563. doi: 10.1016/j.jhydrol.2005.11.061
}
\author{
Johannes Brenner, johannesbrenner@gmail.com
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
