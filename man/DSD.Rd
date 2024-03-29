% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DSD.R
\name{DSD}
\alias{DSD}
\alias{DSD_R}
\title{Data Stream Data Generator Base Classes}
\usage{
DSD(...)

DSD_R(...)
}
\arguments{
\item{...}{further arguments.}
}
\description{
Abstract base classes for DSD (Data Stream Data Generator).
}
\details{
The \code{DSD} class cannot be instantiated, but it serves as a abstract
base class from which all DSD objects inherit. Implementations can be found in the
See Also section below.

\code{DSD} provides common functionality like:
\itemize{
\item \code{\link[=get_points]{get_points()}}
\item \code{print()}
\item \code{\link[=plot]{plot()}}
\item \code{\link[=reset_stream]{reset_stream()}} (if available)
\item \code{\link[=close_stream]{close_stream()}} (if needed)
}

\code{DSD_R} inherits form \code{DSD} and is the abstract parent class for
DSD implemented in R. To create a new R-based implementation there are only
two function that needs to be implemented for a new \code{DSD} subclass
called \code{Foo} would be:
\enumerate{
\item A creator function \code{DSD_Foo(...)} and
\item a method \code{get_points.DSD_Foo(x, n = 1L)} for that class.
}

For details see \code{vignette()}
}
\examples{
DSD()

# create data stream with three clusters in 3-dimensional space
stream <- DSD_Gaussians(k = 3, d = 3)

# get points from stream
get_points(stream, n = 5)

# plotting the data (scatter plot matrix, first and third dimension, and first
#  two principal components)
plot(stream)
plot(stream, dim = c(1, 3))
plot(stream, method = "pca")
}
\seealso{
Other DSD: 
\code{\link{DSD_BarsAndGaussians}()},
\code{\link{DSD_Benchmark}()},
\code{\link{DSD_Cubes}()},
\code{\link{DSD_Gaussians}()},
\code{\link{DSD_MG}()},
\code{\link{DSD_Memory}()},
\code{\link{DSD_Mixture}()},
\code{\link{DSD_NULL}()},
\code{\link{DSD_ReadDB}()},
\code{\link{DSD_ReadStream}()},
\code{\link{DSD_Target}()},
\code{\link{DSD_UniformNoise}()},
\code{\link{DSD_mlbenchData}()},
\code{\link{DSD_mlbenchGenerator}()},
\code{\link{DSF}()},
\code{\link{animate_data}()},
\code{\link{close_stream}()},
\code{\link{get_points}()},
\code{\link{plot.DSD}()},
\code{\link{reset_stream}()}
}
\author{
Michael Hahsler
}
\concept{DSD}
