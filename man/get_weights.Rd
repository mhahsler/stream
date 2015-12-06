\name{get_weights}
\alias{get_weights}
\title{Get Cluster Weights}
\description{Get the weights of the clusters in the DSC}

\usage{
	get_weights(x, type=c("auto", "micro", "macro"), scale=NULL, ...)
}

\arguments{
	\item{x}{The DSC object the weights are being requested from.}
	\item{type}{Return wrights of micro- or macro-clusters in x.
	Auto uses the class of x to decide.}
	\item{scale}{a range (from, to) to scale the weights. 
	Returns by default the raw weights.}
	\item{...}{Additional arguments are passed on.}
}

\details{
The cluster weights are typically a function of how many points were
assigned to each cluster.
}

\value{
A vector containing the weight of each micro-cluster or macro-cluster.
Internally the call is delegated to the appropriate
\code{get_microweights()} or \code{get_macroweights()} method.
}

\seealso{
	\code{\link{DSC}}
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Gaussians(k=3, d=2)
dstream <- DSC_DStream(gridsize=.1)
update(dstream, stream, 500)
dstream
  
# getting the micro-cluster weights
get_weights(dstream)

# D-Stream also has macro-clusters
get_weights(dstream, type="macro")
}
