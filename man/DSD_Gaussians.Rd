\name{DSD_Gaussians}
\alias{DSD_Gaussians}
\title{Mixture of Gaussians Data Stream Generator}
\description{
A data stream generator that produces a data stream with a mixture of static 
Gaussians.
}

\usage{
DSD_Gaussians(k=2, d=2, mu, sigma, p, separation=0.2, noise=0, noise_range)
}

\arguments{
  \item{k}{Determines the number of clusters.}
  \item{d}{Determines the number of dimensions.}
  \item{mu}{A matrix of means for each dimension of each cluster.}
  \item{sigma}{A list of length \code{k} of covariance matrices.}
  \item{p}{A vector of probabilities that determines the likelihood of
		   generated a data point from a particular cluster.}
  \item{separation}{Minimum distance between cluster centers to reduce overlap 
    between clusters (0-.8).}
  \item{noise}{Noise probability between 0 and 1.
  Noise is uniformly distributed within noise range (see below).
  } 
  \item{noise_range}{A matrix with d rows and 2 columns. The first column
    contains the minimum values and the second column contains the maximum
    values for noise.}
}

\details{
\code{DSD_Gaussians} creates a mixture of \code{k} \code{d}-dimensional
static Gaussians in approximately unit space.
The centers \code{mu} and the covariance matrices \code{sigma}
can be supplied or will be randomly generates. The probability vector \code{p}
defines for each cluster the probability that the next data point will
be chosen from it (defaults to equal probability).

The generation method is similar to the one suggested by Jain 
and Dubes (1988).
}

\value{
Returns a \code{DSD_Gaussians} 
object (subclass of 
\code{DSD_R}, \code{DSD}) which is a list of the defined
params. The params are either passed in from the function or
created internally. They include: 

	\item{description}{A brief description of the DSD object.}
	\item{k}{The number of clusters.}
	\item{d}{The number of dimensions.}
	\item{mu}{The matrix of means of the dimensions in each cluster.}
	\item{sigma}{The covariance matrix.}
	\item{p}{The probability vector for the clusters.}
	\item{noise}{A flag that determines if or if not noise is generated.}
}
	   
\seealso{
	\code{\link{DSD}}
}

\references{
Jain and Dubes(1988) Algorithms for clustering data, 
Prentice-Hall, Inc., Upper Saddle River, NJ, USA.
}

\author{
    Michael Hahsler
}


\examples{
# create data stream with three clusters in 3-dimensional data space
stream1 <- DSD_Gaussians(k=3, d=3)

plot(stream1)


# create data stream with specified clusterpositions, 
# 20% noise in a given bounding box and
# with different densities (1 to 9 between the two clusters) 
stream2 <- DSD_Gaussians(k=2, d=2, 
    mu=rbind(c(-.5,-.5), c(.5,.5)), 
    noise=0.2, noise_range=rbind(c(-1,1),c(-1,1)),
    p=c(.1,.9))
plot(stream2)
}
