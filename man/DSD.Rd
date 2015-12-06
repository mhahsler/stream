\name{DSD}
\alias{DSD}
\alias{DSD_R}
\alias{description}
\alias{description}
\alias{DSD_R}
\alias{DSD_R}
\title{Data Stream Data Generator Base Classes}
\description{Abstract base classes for DSD (Data Stream Data
Generator).}

\details{
The \code{DSD} class cannot be instantiated, but it serves
as a abstract base class from which all DSD objects inherit. 

\code{DSD_R} inherits form \code{DSD} and is the abstract 
parent class for DSD implemented in R.
To create a new R-based implementation there are
only two function that needs to be implemented
for a new \code{DSD} subclass:
A creator function (the name should start with \code{DSD_}) and a 
method \code{get_points()} for that class. 

\code{DSD} provides common functionality like
\code{print()}, \code{plot()}, etc.

Note that calling \code{DSD()} or \code{DSD_R()} results in an error since
both are abstract classes.
}

\seealso{
	\code{\link{animate_data}},
	\code{\link{animate_cluster}},
  \code{\link{cluster}},
	\code{\link{evaluate}},
  \code{\link{get_points}},
	\code{\link[stream]{plot}},
	\code{\link{recluster}},
  \code{\link{write_stream}}
}

\author{
    Michael Hahsler
}

\examples{
# create data stream with three clusters in 3-dimensional space 
stream <- DSD_Gaussians(k=3, d=3)

# get points from stream
get_points(stream, n=5)  

# get points with true cluster assignment
p <- get_points(stream, n=5, cluster=TRUE)
attr(p, "cluster")
  
# plotting the data (scatter plot matrix, first and third dimension, and first
#  two principal components)
plot(stream)
plot(stream, dim=c(1,3))
plot(stream, method="pc")
}
