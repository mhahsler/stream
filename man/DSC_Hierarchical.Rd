\name{DSC_Hierarchical}
\alias{DSC_Hierarchical}
\title{Hierarchical Micro-Cluster Reclusterer}
\description{
    Implementation of hierarchical clustering to recluster a set of micro-clusters.
    }

\usage{
	DSC_Hierarchical(k=NULL, h=NULL, method = "complete", min_weight=NULL, 
  description=NULL)
}

\arguments{
	\item{k}{The number of desired clusters.}
  \item{h}{Height where to cut the dendrogram.}
	\item{method}{the agglomeration method to be used. This should be (an unambiguous
    abbreviation of) one of "ward", "single", "complete", "average", "mcquitty", "median"
    or "centroid".}
  \item{min_weight}{micro-clusters with a weight less than this will be 
    ignored for reclustering.}
  \item{description}{optional character string to describe the clustering method.}

}

\details{
Please refer to \code{hclust} for more details on the behavior of the algorithm.

Note that this clustering cannot be updated iteratively and every time
it is used for (re)clustering, the old clustering is deleted.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, \code{DSC_Macro}, and \code{DSC_Hierarchical}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{RObj}{The underlying R object.}
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}}
}

\author{
    Michael Hahsler
}


\examples{
# Cassini dataset
stream <- DSD_mlbenchGenerator("cassini")

# Use hierarchical clustering to recluster micro-clusters
dbstream <- DSC_DBSTREAM(r=.05)
update(dbstream, stream, 500)

# reclustering using single-link and specifying k
hc <- DSC_Hierarchical(k=3, method="single")
recluster(hc, dbstream)
hc
plot(hc, stream, type="both")

# reclustering by specifying height
hc <- DSC_Hierarchical(h=.2, method="single")
recluster(hc, dbstream)
hc
plot(hc, stream, type="both")

# For comparison we use hierarchical clustering directly on the data 
# Note: hierarchical clustering is not a data stream clustering algorithm!
hc <- DSC_Hierarchical(k=3, method="single")
update(hc, stream, 500)
plot(hc, stream)
}
