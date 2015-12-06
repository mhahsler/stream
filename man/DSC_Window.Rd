\name{DSC_Window}
\alias{DSC_Window}
\title{A sliding window from a Data Stream}
\description{
    Implements a sliding window which keeps a fixed amount (window length) of the 
  most recent data points of the stream.
}

\usage{
	DSC_Window(horizon = 100, lambda=0)
}

\arguments{
	\item{horizon}{the window length.}
	\item{lambda}{decay factor damped window model. \code{lambda=0} means no
	dampening.}
}

\details{
  If \code{lambda} is greater than 0 then the weight 
  uses a damped window model (Zhu and Shasha, 2002).
  The weight for points in 
  the window follows \eqn{2^{-lambda*t}} where \eqn{t} is the age of the point.
}

\value{
An object of class \code{DSC_Window} (subclass of
\code{DSC}, \code{DSC_R}, \code{DSC_Micro}).
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}}
}

\references{
Zhu, Y. and Shasha, D. (2002). StatStream: Statistical Monitoring of Thousands 
of Data Streams in Real Time, Intl. Conference of Very Large Data Bases (VLDB'02).
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Gaussians(k=3, d=2, noise=0.05)

window <- DSC_Window(horizon=100)
window

update(window, stream, 200)
window

# plot micro-clusters
plot(window, stream)

# animation for a window using a damped window model. The weight decays 
# with a half-life of 25
\dontrun{
window <- DSC_Window(horizon=25, lambda=1/25)
animate_cluster(window, stream, horizon=1, n=100, xlim=c(0,1), ylim=c(0,1))
}
}
