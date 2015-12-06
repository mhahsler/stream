\name{DSD_UniformNoise}
\alias{DSD_UniformNoise}
\title{Uniform Noise Data Stream Generator}
\description{
This generator produces uniform noise in a d-dimensional unit (hyper) cube.
}

\usage{
DSD_UniformNoise(d=2, range=NULL)
}

\arguments{
  \item{d}{Determines the number of dimensions.}
  \item{range}{A matrix with two columns and \code{d} rows giving
  the minimum and maximum for each dimension. Defaults to the range of \eqn{[0,1]}.}
}

%\details{
%}

\value{
Returns a \code{DSD_UniformNoise} object.(subclass of \code{DSD_R}, 
\code{DSD}).
}
	   
\seealso{
	\code{\link{DSD}}
}

\author{
    Michael Hahsler
}


\examples{
# create data stream with three clusters in 2D
stream <- DSD_UniformNoise(d=2)
plot(stream, n=100)

# specify a different range for each dimension 
stream <- DSD_UniformNoise(d=3, range=rbind(c(0,1), c(0,10), c(0,5)))
plot(stream, n=100)
}
