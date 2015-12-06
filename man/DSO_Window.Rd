\name{DSO_Window}
\alias{DSO_Window}
\title{Sliding Window (Data Stream Operator)}
\description{
    Implements a sliding window 
    data stream operator
    which keeps a fixed amount (window length) of the 
  most recent data points of the stream.
}

\usage{
	DSO_Window(horizon = 100, lambda=0)
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
An object of class \code{DSO_Window} (subclass of
\code{DSO}.
}

\seealso{
	\code{\link{DSO}}
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

window <- DSO_Window(horizon=100)
window

update(window, stream, 200)
window

# plot points in window
plot(get_points(window))
}
