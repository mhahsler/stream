\name{get_centers}
\alias{get_centers}
\title{Get Cluster Centers from a DSC}

\description{
Gets the cluster centers (micro- or macro-clusters) from a DSC object.
}

\usage{
	get_centers(x, type=c("auto", "micro", "macro"), ...)
}

\arguments{
	\item{x}{The DSC object the centers are being requested from.}
	\item{type}{get centers for micro- or macro-clusters. Auto
	        used the class of dsc to decide.}
	\item{...}{Additional parameters.}
}

%\details{
%}

\value{
A data.frame with the micro- or macro-cluster centers as rows.
}

\seealso{
	\code{\link{DSC}}
}

\author{
    Michael Hahsler
}


\examples{
stream <- DSD_Gaussians(k=3, d=2)
dstream <- DSC_DStream(gridsize=.1)
update(dstream, stream, 500)
dstream
  
# getting the micro-cluster centers
get_centers(dstream)

# D-Stream also has macro-clusters
get_centers(dstream, type="macro")
}
