\name{nclusters}
\alias{nclusters}
\title{nclusters}
\description{Returns the number of micro-clusters from the DSC object.}

\usage{
	nclusters(x, type=c("auto", "micro", "macro"), ...)
}

\arguments{
    \item{x}{A DSC object.}
    \item{type}{Return the number of  micro- or macro-clusters in DSC.
    Auto used the class of dsc to decide.}
    \item{...}{Additional arguments are passed on.}
}

%\details{
%}

\value{
An integer; the number of micro- or macro-clusters in the clustering.
}

\seealso{
	\code{\link{DSC}}
}

\author{
    Michael Hahsler
}


\examples{
# setting up the objects
stream <- DSD_Gaussians(k=3)
dstream <- DSC_DStream(gridsize=0.1)
update(dstream, stream, 500)

# retrieving the results
get_centers(dstream)
nclusters(dstream)
}
