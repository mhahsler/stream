\name{DSD_BarsAndGaussians}
\alias{DSD_BarsAndGaussians}
\title{Data Stream Generator for Bars and Gaussians}
\description{
A data stream generator which creates the shape of two bars and two Gaussians clusters with different density.}

\usage{
DSD_BarsAndGaussians(angle= NULL, noise = 0)
}

\arguments{
  \item{angle}{rotation in degrees. \code{NULL} will produce a random rotation.}
  \item{noise}{The amount of noise that should be added to the output.}
}

%\details{
%}

\value{
Returns a \code{DSD_BarsAndGaussians} object.
}
	   
\seealso{
	\code{\link{DSD}}
}

\author{
    Michael Hahsler
}


\examples{
# create data stream with three clusters in 2D
stream <- DSD_BarsAndGaussians(noise=0.1)

# plotting the data
plot(stream)
}
