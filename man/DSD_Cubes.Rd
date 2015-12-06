\name{DSD_Cubes}
\alias{DSD_Cubes}
\title{Static Cubes Data Stream Generator}
\description{
A data stream generator that produces a data stream with static (hyper) cubes 
filled uniformly with data points.
}

\usage{
DSD_Cubes(k=2, d=2, center, size, p, noise = 0, noise_range)
}

\arguments{
  \item{k}{Determines the number of clusters.}
  \item{d}{Determines the number of dimensions.}
  \item{center}{A matrix of means for each dimension of each cluster.}
  \item{size}{A \code{k} times \code{d} matrix with the cube dimensions.}
  \item{p}{A vector of probabilities that determines the likelihood of
		   generated a data point from a particular cluster.}
  \item{noise}{Noise probability between 0 and 1.
  Noise is uniformly distributed within noise range (see below).
  } 
  \item{noise_range}{A matrix with d rows and 2 columns. The first column
    contains the minimum values and the second column contains the maximum
    values for noise.}
}

%\details{
%}

\value{
Returns a \code{DSD_Cubes} 
object (subclass of 
\code{DSD_R}, \code{DSD}).
}
	   
\seealso{
	\code{\link{DSD}}
}

%\references{
%}
\author{
    Michael Hahsler
}


\examples{
# create data stream with three clusters in 3D
stream <- DSD_Cubes(k=3, d=3)

# plotting the data
plot(stream)
}
