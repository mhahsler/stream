\name{DSD_mlbenchGenerator}
\alias{DSD_mlbenchGenerator}
\title{mlbench Data Stream Generator}
\description{
A data stream generator class that interfaces data generators found in mlbench. 
}

\usage{DSD_mlbenchGenerator(method, ...)}

\arguments{
	\item{method}{The name of the mlbench data generator.}
	\item{...}{Parameters for the mlbench data generator.}
}

\details{
The \code{DSD_mlbenchGenerator} class is designed to be a wrapper class for data created
by data generators in the mlbench library.

Call \code{DSD_mlbenchGenerator} with missing method to get a list of available 
methods.
}

\value{
Returns a \code{DSD_mlbenchGenerator} object (subclass of \code{DSD_R}, \code{DSD})
 which is a list of the defined parameters. The parameters are either passed
in from the function or created internally. They include: 

	\item{description}{The name of the class of the DSD object.}
	\item{method}{The name of the mlbench data generator.}
	\item{variables}{The variables for the mlbench data generator.}
}
\seealso{
        \code{\link{DSD}}
}

\author{
    John Forrest
}


\examples{
stream <- DSD_mlbenchGenerator(method="cassini")

plot(stream, n=500)
}
