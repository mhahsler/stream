\name{get_points}
\alias{get_points}
\title{Get Points from a Data Stream Generator}

\description{
Gets points from a DSD object.
}

\usage{
	get_points(x, n=1, outofpoints=c("stop", "warn", "ignore"), ...)
}

\arguments{
	\item{x}{The DSD object.}
	\item{n}{Request up to \eqn{n} points from the stream.}
	\item{outofpoints}{Action taken if less than \eqn{n} data points
	    are available. The default is to stop with an error. 
	    For warn and ignore all avaialable (possibly zero) points 
	    are returned. }
	\item{...}{Additional parameters to pass to \code{get_points()}
	implementations. }
}

\details{
Each DSD object has a unique way for returning data points, but they all are
called through the generic function, \code{get_points()}. This is done by
using the S3 class system. See the man page for the specific DSD class on
the semantics for each implementation of \code{get_points()}.
}

\value{
Returns a matrix of \code{x$d} columns and \code{n} rows.
}

\seealso{
	\code{\link{DSD}}
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Gaussians()
get_points(stream, 100)
}
