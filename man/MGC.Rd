\name{MGC}
\alias{MGC}
\alias{MGC_Static}
\alias{MGC_Random}
\alias{MGC_Function}
\alias{MGC_Noise}
\alias{MGC_Linear}
\alias{add_keyframe}
\alias{keyframe}
\alias{get_keyframes}
\alias{remove_keyframe}
\alias{MGC_Shape_Gaussian}
\alias{MGC_Shape_Block}


\title{Moving Generator Cluster}
\description{
Creates an evolving cluster for a \code{DSD_MG}. 
}
\usage{
MGC_Static(density, center, parameter, shape = NULL)
MGC_Function(density, center, parameter, shape = NULL)
MGC_Random(density, center, parameter, randomness = 1, shape = NULL) 
MGC_Noise(density, range)

MGC_Linear(dimension = 2, keyframelist = NULL, shape = NULL) 
keyframe(time, density, center, parameter, reset = FALSE)
add_keyframe(x, time, density, center, parameter, reset = FALSE)
get_keyframes(x) 
remove_keyframe(x, time)
}

\arguments{
  \item{center}{A list that defines the center of the cluster. The list should have a length equal to the dimensionality. For \code{MGC_Function}, this list consists of functions that define the movement of the cluster. For \code{MGC_Random}, this attribute defines the beginning location for the \code{MGC} before it begins moving.}
  \item{density}{The density of the cluster. For \code{MGC_Function}, this attribute is a function and defines the density of a cluster at a given timestamp.}
  \item{dimension}{Dimensionality of the data stream.}
  \item{keyframelist}{a list of keyframes to initialize the \code{MGC_Linear} object with.}
  \item{parameter}{Parameters for the shape. For the default shape \code{MGC_Shape_Gaussian} the parameter is the standard deviation, one per dimension. If a single value is specified then it is recycled for all dimensions.}
  \item{randomness}{The maximum amount the cluster will move during one time step.}
  \item{range}{The area in which the noise should appear.}
  \item{reset}{Should the cluster reset to the first keyframe (time 0) after this keyframe is finished?}
  \item{shape}{A function creating the shape of the cluster. It gets passed on the 
parameters argument from above. Available functions are \code{MGC_Shape_Gaussian} (the parameters
are a vector containing standard deviations) and \code{MGC_Shape_Block} (parameters are the dimensions of the uniform block).}
  \item{time}{The time stamp the keyframe should be located or which keyframe should be removed.}
  \item{x}{An object of class \code{MGC_Linear}.}
}

\details{
An \code{MGC} describes a single cluster for use within an \code{DSD_MG}. There are currently four different \code{MGC}s that allow a user to express many different behaviors within a single data stream.

An \code{MGC_Linear} creates an evolving Gaussian cluster for a \code{DSD_MG} who's behavior is determined by several keyframes. Keyframes can be added and removed.

An \code{MGC_Function} allows for a creation of a \code{DSD_MG} that is defined by functions of time.

An \code{MGC_Random} allows for a creation of a \code{DSD_MG} that moves randomly.

An \code{MGC_Noise} allows for a creation of noise within a \code{DSD_MG}.

}


\seealso{
\code{\link{DSD_MG}} for details on how to use an \code{MGC} within a \code{DSD}
}

\author{
    Matthew Bolanos
}


\examples{
### Two static clusters
stream <- DSD_MG(dim=2,
  MGC_Static(den = 1, center=c(1, 0), par=.1),
  MGC_Static(den = 1, center=c(2, 0), par=.4, shape=MGC_Shape_Block)
)

plot(stream)

### Example of several MGC_Randoms
stream <- DSD_MG(dimension=2,
  MGC_Random(den = 100, center=c(1, 0), par=.1, rand=.1),
  MGC_Random(den = 100, center=c(2, 0), par=.4, shape=MGC_Shape_Block, rand=.1)
)
  
\dontrun{
  animate_data(stream, 2500, xlim=c(0,3), ylim=c(-2,2), horizon=100)
}
  
  
### Example of several MGC_Functions
stream <- DSD_MG(dim = 2)

### block-shaped cluster moving from bottom-left to top-right increasing size
c1 <- MGC_Function(
  density = function(t){100},
  parameter = function(t){1*t},
  center = function(t) c(t,t),
  shape = MGC_Shape_Block
  )
add_cluster(stream,c1)

### cluster moving in a circle (default shape is Gaussian)
c2 <- MGC_Function(
  density = function(t){25},
  parameter = function(t){5},
  center= function(t) c(sin(t/10)*50+50, cos(t/10)*50+50)
)
add_cluster(stream,c2)

\dontrun{
animate_data(stream,10000,xlim=c(-20,120),ylim=c(-20,120), horizon=100)
}
  
### Example of several MGC_Linears: A single cluster splits at time 50 into two.
### Note that c2 starts at time=50!
stream <- DSD_MG(dim = 2)
c1 <- MGC_Linear(dim = 2)
add_keyframe(c1, time=1,  dens=50, par=5, center=c(0,0))
add_keyframe(c1, time=50, dens=50, par=5, center=c(50,50))
add_keyframe(c1, time=100,dens=50, par=5, center=c(50,100))
add_cluster(stream,c1)

c2 <- MGC_Linear(dim = 2, shape=MGC_Shape_Block)
add_keyframe(c2, time=50, dens=25, par=c(10,10),  center=c(50,50))
add_keyframe(c2, time=100,dens=25, par=c(30,30), center=c(100,50))
add_cluster(stream,c2)

\dontrun{
animate_data(stream,5000,xlim=c(0,100),ylim=c(0,100), horiz=100)
}
  
### two fixed and a moving cluster  
stream <- DSD_MG(dim = 2,
  MGC_Static(dens=1, par=.1, center=c(0,0)),
  MGC_Static(dens=1, par=.1, center=c(1,1)),
  MGC_Linear(dim=2,list(
    keyframe(time = 0, dens=1, par=.1, center=c(0,0)),
    keyframe(time = 1000, dens=1, par=.1, center=c(1,1)),
    keyframe(time = 2000, dens=1, par=.1, center=c(0,0), reset=TRUE)
  )))

noise <- MGC_Noise(dens=.1, range=rbind(c(-.2,1.2),c(-.2,1.2)))
add_cluster(stream, noise)
  
\dontrun{
animate_data(stream, n=2000*3.1, xlim=c(-.2,1.2), ylim=c(-.2,1.2), horiz=200)
}
}

