\name{DSC_Kmeans}
\alias{DSC_Kmeans}
\title{Kmeans Macro-clusterer}
\description{
    Class implements the k-means algorithm for reclustering a set of micro-clusters.
    }

\usage{
    DSC_Kmeans(k, weighted = TRUE, iter.max = 10, nstart = 1,
	    algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
      min_weight = NULL, description=NULL)
}

\arguments{
	\item{k}{either the number of clusters, say k, or a set of initial
	(distinct) cluster centers. If a number, a random set of (distinct)
	rows in x is chosen as the initial centers.}
	\item{weighted}{use a weighted k-means (algorithm is ignored).}
  \item{iter.max}{the maximum number of iterations allowed.}
	\item{nstart}{if centers is a number, how many random sets should be
	chosen?}
	\item{algorithm}{character: may be abbreviated.}
  \item{min_weight}{micro-clusters with a weight less than this will be 
    ignored for reclustering.}
  \item{description}{optional character string to describe the clustering method.}
}

\details{
Please refer to function \code{kmeans} in \pkg{stats} for more details
on the algorithm.

Note that this clustering cannot be updated iteratively and every time
it is used for (re)clustering, the old clustering is deleted.
}

\value{
    An object of class \code{DSC_Kmeans} (subclass of \code{DSC}, 
    \code{DSC_R}, \code{DSC_Macro})
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}}
}

\author{
    Michael Hahsler
}

\examples{
stream <- DSD_Gaussians(k=3, noise=0)

# create micro-clusters via sampling
sample <- DSC_Sample(k=20)
update(sample, stream, 500)
sample
  
# recluster micro-clusters
kmeans <- DSC_Kmeans(k=3)
recluster(kmeans, sample)
plot(kmeans, stream, type="both")

# For comparison we use k-means directly to cluster data
# Note: k-means is not a data stream clustering algorithm
kmeans <- DSC_Kmeans(k=3)
update(kmeans, stream, 500)
plot(kmeans, stream)
}
