\name{get_assignment}
\alias{get_assignment}
\title{Assignment Data Points to Clusters}
\description{Get the assignment of data points to clusters in a \code{DSC}
(nearest-neighbor).}

\usage{
get_assignment(dsc, points, type=c("auto", "micro", "macro"), 
  method="auto", ...)
}

\arguments{
	\item{dsc}{The DSC object with the clusters for assignment.}
	\item{points}{The points to be assigned as a data.frame.}
	\item{type}{Use micro- or macro-clusters in DSC for assignment. Auto
	used the class of dsc to decide.}
	\item{method}{ assignment method \code{"model"} uses the assignment model
  of the underlying algorithm (unassigned points return \code{NA}). 
  \code{"nn"} performs nearest neighbor assignment using Euclidean distance.
	\code{"auto"} uses model if it is avaialble and defaults to nn otherwise. } 
	\item{...}{Additional arguments are passed on.}
}

\details{
Each data point is assigned either using the original model's
assignment rule or Euclidean nearest neighbor assignment.
}


\value{
A vector containing the assignment of each point. \code{NA} means that a
  data point was not assigned to a cluster.
}

\seealso{
	\code{\link{DSC}}
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Gaussians(k=3, d=2, noise=.05)

dbstream <- DSC_DBSTREAM(r=.1)
update(dbstream, stream, n=100)

# find the assignment for the next 100 points to 
# micro-clusters in dsc. This uses the model's assignemnt function
points <- get_points(stream, n=100)
a <- get_assignment(dbstream, points)
a

# show the MC assignment areas. Assigned points as blue circles and 
# the unassigned points as red dots
plot(dbstream, stream, assignment= TRUE, type="none")
points(points[!is.na(a),], col="blue")
points(points[is.na(a),], col="red", pch=20)

# use nearest neighbor assignment instead 
get_assignment(dbstream, points, method="nn")
}
