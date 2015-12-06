\name{update}
\alias{update}
\alias{update.DSC_R}
\alias{update.DSC_TwoStage}
\alias{update.DSO_Sample}
\alias{update.DSO_Window}
\title{Update a Data Stream Clustering Model}

\description{
Update a clustering model by clustering a number of input points from a data stream into a clustering object.
}

\usage{
\method{update}{DSC_R}(object, dsd, n = 1, verbose = FALSE, block=10000L, ...)
\method{update}{DSC_TwoStage}(object, dsd, n = 1, verbose = FALSE, 
  block=10000L, ...)
\method{update}{DSO_Sample}(object, dsd, n = 1, verbose = FALSE, ...)
\method{update}{DSO_Window}(object, dsd, n = 1, verbose = FALSE, ...)
}

\arguments{
  \item{object}{an object of a subclass of DST (data stream mining task).}
  \item{dsd}{a DSD object (data stream).}
  \item{n}{number of points to cluster.}
  \item{verbose}{report progress.}
  \item{block}{maximal number of data points passed on at once to the algorithm. 
  This only is used since R loops are very slow.}
  \item{...}{extra arguments for clusterer.}
}

\details{
\code{update} takes n times a single 
data points out of the DSD updates the model in \code{object}. 
Note that update directly 
modifies the object (which is a reference class) and 
thus the result does not need to be reassigned to the object name.

%\code{cluster} is the low level implementation of updating a
%data stream clustering model and is called by \code{update}.
}

\value{
The updated model is returned invisibly for reassignment
(however, this is not necessary).  

To obtain the updated model for a \code{DSC} (data stream clustering
model), call \code{get_centers()} on the DSC object.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSD}}, and
	\code{\link{animation}} for producing an animation of the clustering process.
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Gaussians(k=3)
dstream <- DSC_DStream(gridsize=.05)  

update(dstream, stream, 500)
plot(dstream, stream)
}
