\name{DSO_Sample}
\alias{DSO_Sample}
\title{Sampling from a Data Stream (Data Stream Operator)}
\description{
    Extracts a sample form a data stream using Reservoir Sampling.
}

\usage{
	DSO_Sample(k = 100, biased = FALSE)
}

\arguments{
	\item{k}{the number of points to be sampled from the stream.}
	\item{biased}{if \code{FALSE} then a regular (unbiased)
  reservoir sampling is used. If true then
  the sample is biased towards keeping more recent data points 
    (see Details section).}
}

\details{
    If \code{biased=FALSE} then the reservoir sampling algorithm by McLeod 
    and Bellhouse (1983) is used. This sampling makes sure that each 
    data point has the same chance to be sampled. All sampled points
    will have a weight of 1. Note that this might not be ideal for an evolving
    stream since very old data points have the same chance to be in the sample
    as newer points.
    
    If \code{bias=TRUE} then sampling prefers newer points using the modified
    reservoir sampling algorithm 2.1 by Aggarwal (2006). New points are always 
    added. They replace a random point in thre reservoir with a probability
    of reservoir size over \code{k}. This an exponential bias function of
    \eqn{2^{-lambda}} with \eqn{lambda=1/k}.
}

\value{
An object of class \code{DSO_Sample} (subclass of
\code{DSO}).
}

\seealso{
	\code{\link{DSO}}
}

\references{
Vitter, J. S. (1985): Random sampling with a reservoir. ACM Transactions
on Mathematical Software, 11(1), 37-57.

McLeod, A.I., Bellhouse, D.R. (1983): A Convenient Algorithm for Drawing a Simple Random Sample. Applied Statistics, 32(2), 182-184.
  
Aggarwal C. (2006) On Biased Reservoir Sampling in the Presence of Stream Evolution. International Conference on Very Large Databases (VLDB'06). 607-618.
}

\author{
    Michael Hahsler
}

\examples{
stream <- DSD_Gaussians(k=3, noise=0.05)

sample <- DSO_Sample(k=20)

update(sample, stream, 500)
sample

# plot points in sample
plot(get_points(sample))
}
