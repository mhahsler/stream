\name{DSD_mlbenchData}
\alias{DSD_mlbenchData}
\title{Stream Interface for Data Sets From mlbench}
\description{
Provides a convenient stream interface for data sets from the mlbench package.
}

\usage{DSD_mlbenchData(data=NULL, loop = FALSE, random = FALSE, scale = FALSE)}

\arguments{
	\item{data}{The name of the dataset from mlbench. If missing then
	    a list of all available data sets is shown and returned.}
	\item{loop}{A flag that tells the stream to loop or 
				not to loop over the data frame.}
	\item{random}{A flag that determines if the data should be in a random order.}
	\item{scale}{A flag that determines if the data should be scaled.}
}

\details{
The \code{DSD_mlbenchData} class is designed to be a wrapper class for data
that is held in memory in either a data frame or matrix form. It is a subclass 
of \code{DSD_Memory}.

Call \code{DSD_mlbenchData} with a missing value for data to get a list of all
available data sets.
}

\value{
Returns a \code{DSD_mlbenchData} object which 
is also of class \code{DSD_Memory}.
}

\seealso{
	\code{\link{DSD}},
	\code{\link{DSD_Memory}},
	\code{\link{reset_stream}}
}

\author{
    Michael Hahsler and Matthew Bolanos
}


\examples{
stream <- DSD_mlbenchData("Shuttle")
stream

plot(stream, n=100)
}
