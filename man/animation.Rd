\name{animation}
\alias{animate}
\alias{animation}
\alias{animate_data}
\alias{animate_cluster}
\title{Animates the plotting of a DSD and the clustering process}

\description{
Generates an animation of a data stream or a data steam clustering.
}

\usage{
animate_data(dsd, horizon = 100, n = 1000, wait = .1, plot.args = NULL, ...)
animate_cluster(dsc, dsd, measure, horizon = 100, n = 1000, 
  type=c("auto", "micro", "macro"), assign="micro", 
  assignmentMethod=c("auto","model", "nn"),
  noise = c("class", "ignor"),
  wait=.1, plot.args = NULL, ...)
}

\arguments{
  \item{dsd}{a DSD object}
  \item{dsc}{a DSC object}
  \item{horizon}{the number of points displayed at once/used for evaluation. }
  \item{n}{the number of points to be plotted}
  \item{measure}{the evaluation measure that should be graphed below the animation}
  \item{type}{evaluate \code{"micro"} or \code{"macro"}-clusters? \code{"auto"}
      chooses micro if \code{dsc} is of class \code{DSC_micro} and no \code{macro}
      is given. Otherwise macro is used.}
  \item{assign}{assign new points to the closest \code{"micro"} or 
    \code{"macro"}-cluster to calculate the evaluation measure.}
  \item{assignmentMethod}{how to assign data points to micro-clusters. Options are
  \code{"model"} and \code{"nn"} (nearest neighbor). \code{"auto"} uses model if 
    available and nn otherwise.}
	\item{noise}{how to handle noise (as a separate class or ignor). }
  \item{wait}{the time interval between each frame}
  \item{plot.args}{ a list with plotting parameters for the clusters. }
  \item{...}{extra arguments are added to \code{plot.args}.}
}

\details{
Animations are recorded using the library animation and can be replayed
  (which gives a smoother experience since the is no more computation done) and
  saved in various formats (see Examples section below).
}

\seealso{
  \code{\link{evaluate_cluster}} for stream evaluation without animation.
  See \code{\link[animation]{ani.replay}} for replaying and saving animations.
}

\author{
    Michael Hahsler
}

\examples{
\dontrun{
stream <- DSD_Benchmark(1)
animate_data(stream, horizon=100, n=5000, xlim=c(0,1), ylim=c(0,1))

### animations can be replayed with the animation package
library(animation)
animation::ani.options(interval=.1) ## change speed
ani.replay()
  
### animations can also be saved as HTML, animated gifs, etc.
saveHTML(ani.replay())  
  
### animate the clustering process with evaluation
### Note: we choose to ignor noise points even if the algorithm would assign 
###       them to a cluster
reset_stream(stream)
dbstream <- DSC_DBSTREAM(r=.04, lambda=.1, gaptime=100, Cm=3, 
  shared_density=TRUE, alpha=.2)  

animate_cluster(dbstream, stream, horizon=100, n=5000,  
  measure="crand", type="macro", assign="micro", noise = "ignor",
  plot.args = list(xlim=c(0,1), ylim=c(0,1), shared = TRUE))
}
}
