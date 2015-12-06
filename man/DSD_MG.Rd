\name{DSD_MG}
\title{DSD Moving Generator}
\alias{DSD_MG}
\alias{DSD_MovingGenerator}
\alias{MovingGenerator}
\alias{add_cluster}
\alias{get_clusters}
\alias{remove_cluster}

\description{
  Creates an evolving DSD that consists of several \code{MGC}s.
}

\usage{
DSD_MG(dimension = 2, ..., labels=NULL, description=NULL)

add_cluster(x, c, label=NULL)
get_clusters(x) 
remove_cluster(x, i)
}

\arguments{
  \item{dimension}{the dimension of the DSD object}
  \item{...}{initial set of \code{MGC}s}
  \item{x}{A \code{DSD_MG} object.}
  \item{c}{The cluster that should be added to the \code{DSD_MG} object.}
  \item{i}{The index of the cluster that should be removed from the \code{DSD_MG} object.}
  \item{label, labels}{integer representing the cluster label. \code{NA} represents noise.
    If labels are not specified, then each new cluster gets a new label.}
  \item{description}{ An optional string used by \code{print} to describe the 
    data generator. }
}

\details{
  This DSD is able to generate complex datasets that are able to evolve over a period of time.
  Its behavior is determined by the \code{MGC}s it is composed of. 
}

\seealso{
  \code{\link{MGC_Function}}, \code{\link{MGC_Linear}}, \code{\link{MGC_Noise}}, \code{\link{MGC_Random}} for details on the different \code{MGC} objects.
}

\author{
    Matthew Bolanos
}


\examples{
### create an empty DSD_MG
stream <- DSD_MG(dim = 2)
stream  
  
### add two clusters
c1 <- MGC_Random(density=50, center=c(50,50), parameter=1, randomness = )
add_cluster(stream, c1)
stream
  
c2 <- MGC_Noise(density=1, range=rbind(c(-20,120), c(-20,120)))
add_cluster(stream, c2)
stream
  
get_clusters(stream)
plot(stream, xlim=c(-20,120), ylim=c(-20,120))
    
\dontrun{
animate_data(stream, n=5000, xlim=c(-20,120), ylim=c(-20,120))
}
    
### remove cluster 1
remove_cluster(stream,1)
    
get_clusters(stream)
plot(stream, xlim=c(-20,120), ylim=c(-20,120))

### create a more complicated cluster structure (using 2 clusters with the same
### label to form an L shape)
stream <- DSD_MG(dim=2,
  MGC_Static(density=10, center=c(.5,.2), par=c(.4,.2), shape=MGC_Shape_Block),
  MGC_Static(density=10, center=c(.6,.5), par=c(.2,.4), shape=MGC_Shape_Block),
  MGC_Static(density=5, center=c(.39,.53), par=c(.16,.35), shape=MGC_Shape_Block),
  MGC_Noise(density=1, range=rbind(c(0,1), c(0,1))),
  labels= c(1, 1, 2, NA)
  )
   
plot(stream, xlim=c(0,1), ylim=c(0,1))
  
  
### simulate the clustering of a splitting cluster  
c1 <- MGC_Linear(dim = 2, keyframelist = list(
  keyframe(time = 1,  dens = 20, center = c(0,0),   param = 10),
  keyframe(time = 50, dens = 10, center = c(50,50), param = 10),
  keyframe(time = 100,dens = 10, center = c(50,100),param = 10)
))

### Note: Second cluster appearch at time=50
c2 <- MGC_Linear(dim = 2, keyframelist = list(
  keyframe(time = 50, dens = 10, center = c(50,50), param = 10),
  keyframe(time = 100,dens = 10, center = c(100,50),param = 10)
))

stream <- DSD_MG(dim = 2, c1, c2)
stream
  
dbstream <- DSC_DBSTREAM(r=10, lambda=0.1)
  
\dontrun{
purity <- animate_cluster(dbstream, stream, n=2500, type="both", 
  xlim=c(-10,120), ylim=c(-10,120), evaluationMeasure="purity", horizon=100)
}
}
