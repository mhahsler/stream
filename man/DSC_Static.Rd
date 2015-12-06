\name{DSC_Static}
\alias{DSC_Static}
\title{Create as Static Copy of a Clustering}
\description{
    This representation cannot perform clustering anymore, but it
    also does not need the supporting data structures. It only stores
    the cluster centers and weights.
    }

\usage{
	DSC_Static(x, type=c("auto", "micro", "macro"), k_largest=NULL, min_weight=NULL)
}

\arguments{
	\item{x}{The clustering (a DSD object) to copy.}
	\item{type}{which clustering to copy.}
	\item{k_largest}{only copy the k largest (highest weight) clusters.}
	\item{min_weight}{only copy clusters with a weight larger or equal to \code{min_weight}.}
}

%\details{
%}

\value{
An object of class \code{DSC_Static} (sub class of  \code{DSC}, \code{DSC_R}).
The list also contains either
 \code{DSC_Micro} or \code{DSC_Macro} depending on what type 
 of clustering was copied.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}},
	\code{\link{DSC_Macro}}
}

\author{
    Michael Hahsler
}

\examples{
stream <- DSD_Gaussians(k=3, noise=0.05)

dstream <- DSC_DStream(gridsize=0.05)
update(dstream, stream, 500)
dstream
plot(dstream, stream)
  
# create a static copy of the clustering
static <- DSC_Static(dstream)
static
plot(static, stream)
  
# copy only the 5 largest clusters
static2 <- DSC_Static(dstream, k_largest=5)
static2
plot(static2, stream)

# copy all clusters with a weight of at least .3
static3 <- DSC_Static(dstream, min_weight=.3)
static3
plot(static3, stream)
}
