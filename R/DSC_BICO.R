#' BICO - Fast computation of k-means coresets in a data stream
#'
#' BICO maintains a tree which is inspired by the clustering tree of BIRCH,
#' a SIGMOD Test of Time award-winning clustering algorithm.
#' Each node in the tree represents a subset of these points. Instead of
#' storing all points as individual objects, only the number of points,
#' the sum and the squared sum of the subset's points are stored as key features
#' of each subset. Points are inserted into exactly one node.
#'
#' In this implementation, the nearest neighbour search on the first level
#' of the tree ist sped up by projecting all points to random 1-d subspaces.
#' The first estimation of the optimal clustering cost is computed in a
#' buffer phase at the beginning of the algorithm.
#'
#' This implementation interfaces the original C++ implementation available here: \url{http://ls2-www.cs.tu-dortmund.de/grav/de/bico}.
#' For micro-clustering, the algorithm computes the coreset of the stream. Reclustering is performed by using the \code{kmeans++} algorithm on the coreset.
#'
#' @param k number of centres
#' @param space coreset size
#' @param p number of random projections used for nearest neighbour search in first level
#' @param iterations number of repetitions for the kmeans++ procedure in the offline component
#'
#' @references Hendrik Fichtenberger, Marc Gille, Melanie Schmidt, Chris Schwiegelshohn, Christian Sohler: BICO: BIRCH Meets Coresets for k-Means Clustering. ESA 2013: 481-492
#'
#' @author
#' R-Interface:
#' Matthias Carnein (\email{Matthias.Carnein@@uni-muenster.de}),
#' Dennis Assenmacher,
#' C-Implementation:
#' Hendrik Fichtenberger,
#' Marc Gille,
#' Melanie Schmidt,
#' Chris Schwiegelshohn,
#' Christian Sohler
#'
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' BICO <- DSC_BICO(k = 3, p = 10, space = 100, iterations = 10)
#' update(BICO, stream, n = 500)
#' plot(BICO,stream, type = "both")
#'
#' @export
DSC_BICO <- function(k=5, space=10, p=10, iterations=10) {

  BICO <- BICO_R$new(k, space, p, iterations)

  structure(
    list(
      description = "BICO",
      RObj = BICO
    ), class = c("DSC_BICO", "DSC_Micro", "DSC_R", "DSC")
  )
}


BICO_R <- setRefClass("BICO_R", fields = list(
  C ="ANY"
))


BICO_R$methods(
  initialize = function(k, space, p, iterations) {
    C <<- new(BICO, k, space, p, iterations) ## Exposed C class
    .self
  }
)


BICO_R$methods(
  cluster = function(newdata){
    .self$C$cluster(as.matrix(newdata))
  }
)


BICO_R$methods(
  get_microclusters = function() {
    .self$C$get_microclusters()
  }
)

BICO_R$methods(
  get_microweights = function() {
    .self$C$get_microweights()
  }
)

BICO_R$methods(
  get_macroclusters = function() {
    .self$C$get_macroclusters()
  }
)

BICO_R$methods(
  get_macroweights = function() {
    .self$C$get_macroweights()
  }
)

BICO_R$methods(
  microToMacro = function(micro=NULL, ...){
    assignment = .self$C$microToMacro()+1 ## from C to R indexing
    if(is.null(micro)) micro <- 1:length(assignment)
    structure(assignment[micro], names=micro)
  }
)

