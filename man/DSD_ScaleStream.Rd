\name{DSD_ScaleStream}
\alias{DSD_ScaleStream}
\title{Scale a Stream from a DSD}

\description{
  Make an unscaled data stream into a scaled data stream.
}

\usage{
  DSD_ScaleStream(dsd, center=TRUE, scale=TRUE, n=1000, reset=FALSE)
}

\arguments{
  \item{dsd}{A object of class \code{DSD} that will be scaled.}
  \item{center, scale}{logical or a numeric vector of length equal to
    the number of columns used for centering/scaling 
    (see function \code{scale}).}
  \item{n}{The number of points used to creating the centering/scaling }
  \item{reset}{Try to reset the stream to its beginning after taking \code{n} 
    points for scaling.}
}

\details{
  \code{scale_stream()} estimates the values for centering and scaling 
  (see \code{scale} in \pkg{base}) using \code{n} points from the 
  stream.
}

\value{
  An object of class \code{DSD_ScaleStream} (subclass of \code{DSD_R},
    \code{DSD}).
}

\seealso{
  \code{\link{DSD}},
  \code{\link{reset_stream}},
  \code{\link{scale}} in \pkg{base},
  
}

\author{
    Michael Hahsler
}

\examples{
stream <- DSD_Gaussians(k=3, d=3)
plot(stream)

# scale stream using 100 points
stream_scaled <- DSD_ScaleStream(stream, n=100)
plot(stream_scaled)
}
