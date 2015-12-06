\name{DSC_DStream}
\alias{DSC_DStream}
\alias{dstream}
\alias{d-stream}
\alias{D-Stream}
\alias{get_attraction}
\title{D-Stream Data Stream Clustering Algorithm}
\description{
    Implements the D-Stream data stream clustering algorithm.
}
\usage{
DSC_DStream(gridsize, lambda = 1e-3, gaptime=1000L, 
  Cm=3, Cl=.8, attraction=FALSE, epsilon=.3, Cm2=Cm, k=NULL, N = 0)
get_attraction(x, relative=FALSE, grid_type = "dense", dist=FALSE)
}

\arguments{
	\item{gridsize}{Size of grid cells.}
	\item{lambda}{ Fading constant used function to calculate the 
    decay factor \eqn{2^-lambda}.    
    (Note: in the paper the authors use lamba to denote the decay 
    factor and not the fading constant!)}
	\item{gaptime}{sporadic grids are removed every
    gaptime number of points.}
	\item{Cm}{density threshold used to detect dense 
      grids as a proportion of the 
	    average expected density (Cm > 1). The average density is given
      by the total weight of the clustering over \eqn{N}, the number of 
      grid cells.}
	\item{Cl}{density threshold to detect sporadic grids (0 > Cl > Cm).  
      Transitional grids have a density between Cl and Cm. }
  \item{attraction}{compute and store information about the 
    attraction between
    adjacent grids. If \code{TRUE} then attraction is used to 
    create macro-clusters, otherwise macro-clusters are 
    created by merging adjacent dense grids.}
  \item{epsilon}{overlap parameter for attraction as a proportion of 
    \code{gridsize}.}
  \item{Cm2}{threshold on attraction to join two dense grid cells 
    (as a proportion on the average expected attraction). 
    In the original algorithm \code{Cm2} is equal to \code{Cm}.}
  \item{k}{alternative to Cm2 (not in the original algorithm). 
    Create k clusters based on attraction. In case 
    of more than k unconnected components, closer groups of MCs are joined.}
  \item{N}{ Fix the number of grid cells used for the calculation 
    of the density thresholds with Cl and Cm. If \code{N} is not 
    given (0) then the algorithm tries to determine N from the data. Note   
    that this means that N potentially increases over time and outliers
    might produce an extremely large value which will lead to a sudden
    creation of too many dense micro-clusters. The original paper
    assumed that N is known a priori.} 
  \item{x}{DSC_DStream object to get attraction values from.}
  \item{relative}{calculates relative attraction (normalized by the cluster 
    weight).}
  \item{grid_type}{the attraction between what grid types should be returned?}
  \item{dist}{make attraction symmetric and transform into a distance.}
}

\details{
D-Stream creates an equally spaced grid and estimates the density in each 
  grid cell using the count of points falling in the cells. Grid cells are 
  classified based on density into dense, transitional and sporadic cells.
  The density is faded after every new point by a factor of \eqn{2^{-lambda}}.
  Every gaptime number of points sporadic grid cells are removed.
  
  For reclustering D-Stream (2007 version) merges adjacent dense grids to
  form macro-clusters and then assigns adjacent transitional grids to 
  macro-clusters. This behavior is implemented as \code{attraction=FALSE}. 
  
  The 2009 version of the algorithm adds the concept of attraction between 
  grids cells. If \code{attraction=TRUE} is used then the algorithm
  produces macro-clusters based on attraction between dense adjacent grids 
  (uses \code{Cm2} which in the original algorithm is equal to \code{Cm}).

  For many functions (e.g., \code{get_centers()}, \code{plot()}), 
  D-Stream adds a parameter \code{grid_type} with possible 
  values of \code{"dense"}, \code{"transitional"}, \code{"sparse"}, 
  \code{"all"} and \code{"used"}. This only returns the selected type
  of grid cells. \code{"used"} includes dense and adjacent transitional cells 
  which are used in D-Stream for reclustering.

  For plot D-Stream also provides extra parameters \code{"grid"} and 
  \code{"grid_type"} to show micro-clusters as grid cells
  (density represented by gray values). 

  Note that \code{DSC_DStream} can at this point not be saved to disk using
  save() or saveRDS(). This functionality will be added later!
}

\value{
An object of class \code{DSC_DStream} (subclass of  \code{DSC}, \code{DSC_R}, \code{DSC_Micro}). 
}

\references{ 
Yixin Chen and Li Tu. 2007. Density-based clustering for real-time stream data. In Proceedings of the 13th ACM SIGKDD international conference on Knowledge discovery and data mining (KDD '07). ACM, New York, NY, USA, 133-142.

Li Tu and Yixin Chen. 2009. Stream data clustering based on grid density and attraction. ACM Trans. Knowl. Discov. Data 3, 3, Article 12 (July 2009), 27 pages.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}}
}

\author{
    Michael Hahsler
}

\examples{
stream <- DSD_BarsAndGaussians(noise=.05)
plot(stream)

# we set Cm=.8 to pick up the lower density clusters
dstream1 <- DSC_DStream(gridsize=1, Cm=1.5)
update(dstream1, stream, 1000)
dstream1

# micro-clusters (these are "used" grid cells)
nclusters(dstream1)
head(get_centers(dstream1))

# plot (DStream provides additional grid visualization)
plot(dstream1, stream)
plot(dstream1, stream, grid=TRUE)

# look only at dense grids
nclusters(dstream1, grid_type="dense")
plot(dstream1, stream, grid=TRUE, grid_type="dense")

# look at transitional and sparse cells
plot(dstream1, stream, grid=TRUE, grid_type="transitional")
plot(dstream1, stream, grid=TRUE, grid_type="sparse")

### Macro-clusters
# standard D-Stream uses reachability
nclusters(dstream1, type="macro")
get_centers(dstream1, type="macro")
plot(dstream1, stream, type="both", grid=TRUE)
evaluate(dstream1, stream, measure="crand", type="macro")

# use attraction for reclustering
dstream2 <- DSC_DStream(gridsize=1, attraction=TRUE, Cm=1.5)
update(dstream2, stream, 1000)
dstream2

plot(dstream2, stream, type="both", grid=TRUE)
evaluate(dstream2, stream, measure="crand", type="macro")
}

