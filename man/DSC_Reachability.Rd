\name{DSC_Reachability}
\alias{DSC_Reachability}
\title{Reachability Micro-Cluster Reclusterer}
\description{
    Implementation of reachability clustering (based on DBSCAN's concept of 
    reachability) to recluster a set of micro-clusters.
    Two micro-clusters are directly reachable 
    if they are within each other's epsilon-neighborhood (i.e., the distance between
    the centers is less then epsilon). Two micro-clusters are reachable if 
    they are connected by a chain of pairwise directly reachable micro-clusters.
    All mutually reachable micro-clusters are put in the same cluster.
}

\usage{
	DSC_Reachability(epsilon, min_weight=NULL, description=NULL)
}

\arguments{
	\item{epsilon}{radius of the epsilon-neighborhood.}
  \item{min_weight}{micro-clusters with a weight less than this will be 
    ignored for reclustering.}
  \item{description}{optional character string to describe the clustering method.}

}

\details{
Reachability uses internally \code{DSC_Hierarchical} with single link.

Note that this clustering cannot be updated iteratively and every time
it is used for (re)clustering, the old clustering is deleted.
}

\value{
An object of class \code{DSC_Reachability}. The object
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{RObj}{The underlying R object.}
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}}
}

\references{
Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu (1996). A density-based algorithm for discovering clusters in large spatial databases with noise. In Evangelos Simoudis, Jiawei Han, Usama M. Fayyad. Proceedings of the Second International Conference on Knowledge Discovery and Data Mining (KDD-96). AAAI Press. pp. 226-231.
}

\author{
    Michael Hahsler
}

\examples{
stream <- DSD_mlbenchGenerator("cassini")

# Recluster micro-clusters from DSC_Sample with reachability
sample <- DSC_Sample(k = 200)
update(sample, stream, 1000)

reach <- DSC_Reachability(epsilon=0.3)
recluster(reach, sample)
  
plot(reach, stream, type="both")  

# For comparison we using reachability clustering directly on data points
# Note: reachability is not a data stream clustering algorithm taking O(n^2) 
# time and space.
reach <- DSC_Reachability(epsilon=0.2)
update(reach, stream, 500)
reach
plot(reach, stream)
}
