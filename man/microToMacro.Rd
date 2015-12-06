\name{microToMacro}
\alias{microToMacro}
\title{
Translate Micro-cluster IDs to Macro-cluster IDs
}
\description{
Translates micro-cluster ids into macro-cluster ids for 
a \code{DSC_Macro} object.
}
\usage{
microToMacro(x, micro=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{DSC_Macro} object that also contains information about 
  micro-clusters.}
  \item{micro}{A vector with micro-cluster ids. If \code{NULL} then
  the assignments for all micro-clusters in x are returned. }
}
%\details{
%}
\value{
A vector of the same length as \code{micro} with the macro-cluster ids.
}
\seealso{
\code{\link{DSC_Macro}}
}

\author{
    Michael Hahsler
}

\examples{
stream <- DSD_Gaussians(k=3, d=2, noise=0.05, p=c(.2,.4,.6))

# recluster a micro-clusters
micro <- DSC_DStream(gridsize=0.05)
update(micro, stream, 500)

macro <- DSC_Kmeans(k=3)
recluster(macro, micro)

# translate all micro-cluster ids
microToMacro(macro)

# plot some data points in gray
plot(stream, col="gray", cex=.5, xlim=c(0,1), ylim=c(0,1))
# add micro-clusters and use the macro-cluster ids as color and weights as size
points(get_centers(macro, type="micro"), 
  col=microToMacro(macro), 
  cex=get_weights(macro, type="micro", scale=c(.5,3)))
# add macro-cluster centers (size is weight)
points(get_centers(macro, type="macro"), 
  cex = get_weights(macro, type="macro", scale=c(2,5)),
  pch=3,lwd=3, col=1:3)
}

