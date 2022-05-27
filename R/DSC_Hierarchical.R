#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#' Hierarchical Micro-Cluster Reclusterer
#'
#' Macro Clusterer.
#' Implementation of hierarchical clustering to recluster a set of
#' micro-clusters.
#'
#' Please refer to \code{hclust} for more details on the behavior of the
#' algorithm.
#'
#' Note that this clustering cannot be updated iteratively and every time it is
#' used for (re)clustering, the old clustering is deleted.
#'
#' @family DSC_Macro
#'
#' @param k The number of desired clusters.
#' @param h Height where to cut the dendrogram.
#' @param method the agglomeration method to be used. This should be (an
#' unambiguous abbreviation of) one of "ward", "single", "complete", "average",
#' "mcquitty", "median" or "centroid".
#' @param min_weight micro-clusters with a weight less than this will be
#' ignored for reclustering.
#' @param description optional character string to describe the clustering
#' method.
#' @return A list of class \code{DSC}, \code{DSC_R}, \code{DSC_Macro}, and
#' \code{DSC_Hierarchical}. The list contains the following items:
#'
#' \item{description}{The name of the algorithm in the DSC object.}
#' \item{RObj}{The underlying R object.}
#' @author Michael Hahsler
#' @seealso \code{\link{DSC}}, \code{\link{DSC_Macro}}
#' @examples
#'
#' # Cassini dataset
#' stream <- DSD_mlbenchGenerator("cassini")
#'
#' # Use hierarchical clustering to recluster micro-clusters
#' dbstream <- DSC_DBSTREAM(r = .05)
#' update(dbstream, stream, 500)
#'
#' # reclustering using single-link and specifying k
#' hc <- DSC_Hierarchical(k = 3, method = "single")
#' recluster(hc, dbstream)
#' hc
#' plot(hc, stream, type = "both")
#'
#' # reclustering by specifying height
#' hc <- DSC_Hierarchical(h = .2, method = "single")
#' recluster(hc, dbstream)
#' hc
#' plot(hc, stream, type = "both")
#'
#' # For comparison we use hierarchical clustering directly on the data
#' # Note: hierarchical clustering is not a data stream clustering algorithm!
#' hc <- DSC_Hierarchical(k = 3, method = "single")
#' update(hc, stream, 500)
#' plot(hc, stream)
#'
#' @export
DSC_Hierarchical <- function(k=NULL, h=NULL, method = "complete",
  min_weight=NULL, description=NULL) {

  hierarchical <- hierarchical$new(
    k=k, h=h, method=method, min_weight=min_weight)

  if(is.null(description)) description <- paste("Hierarchical (", method, ")",
    sep='')

  l <- list(description = description, RObj = hierarchical)

  class(l) <- c("DSC_Hierarchical","DSC_Macro","DSC_R","DSC")
  l
}


### calculate centroids
.centroids <- function(centers, weights, assignment){

  macroID <- unique(assignment)
  macroID <- macroID[!is.na(macroID)]
  assignment[is.na(assignment)] <- -1 ### prevent NAs in matching

  cs <- t(sapply(macroID, FUN=
      function(i) {
        take <- assignment==i
        colSums(centers[take, ,drop=FALSE] *
            matrix(weights[take], nrow=sum(take), ncol=ncol(centers))) /
          sum(weights[take])
      }))

  ### handle 1-d case
  if(ncol(centers) == 1) cs <- t(cs)
  rownames(cs) <- NULL
  colnames(cs) <- colnames(centers)

  cs <- data.frame(cs)

  ws <- sapply(macroID, FUN =
      function(i) sum(weights[assignment==i], na.rm=TRUE))

  list(centers=cs, weights=ws)
}


hierarchical <- setRefClass("hierarchical",
  fields = list(
    data	= "data.frame",
    dataWeights = "numeric",
    d	= "matrix",
    method  = "character",
    k	= "ANY",
    h = "ANY",
    assignment = "numeric",
    details = "ANY",
    centers	= "data.frame",
    weights = "numeric",
    min_weight = "numeric"
  ),

  methods = list(
    initialize = function(
      k=NULL,
      h=NULL,
      method	= "complete",
      min_weight = NULL
    ) {

      if(is.null(k) && is.null(h)) stop("Either h or k needs to be specified.")
      if(!is.null(k) && !is.null(h)) stop("Only h or k  can be specified.")

      if(is.null(min_weight)) min_weight <<- 0
      else min_weight <<- as.numeric(min_weight)

      data	<<- data.frame()
      dataWeights	<<- numeric()
      weights	<<- numeric()
      centers	<<- data.frame()
      method	<<- method
      k	<<- k
      h <<- h

      .self
    }

  ),
)

hierarchical$methods(
  cluster = function(x,  weight = rep(1,nrow(x)), ...) {
    #if(nrow(x)==1)
    #  warning("DSC_Hierarchical does not support iterative updating! Old data is overwritten.")


    ### filter weak clusters
    if(min_weight>0) {
      x <- x[weight>min_weight,]
      weight <- weight[weight>min_weight]
    }

    data <<- x
    dataWeights <<- weight

    if((!is.null(k) && nrow(data) <=k) || nrow(data)<2) {
      centers <<- x
      weights <<- weight
    }else{
      hierarchical <- hclust(d=dist(x), method = method)
      details <<- hierarchical

      if(is.null(k) || k < length(unlist(hierarchical['height'])))
        assignment <<- cutree(hierarchical, k = k, h = h)
      else
        assignment <<- 1

      ### find centroids
      centroids <- .centroids(x, weight, assignment)
      centers <<- centroids$centers
      weights <<- centroids$weights
    }
  },

  get_microclusters = function(...) { .nodots(...); data },
  get_microweights = function(...) { .nodots(...); dataWeights },

  get_macroclusters = function(...) { .nodots(...); centers },
  get_macroweights = function(...) { .nodots(...); weights },

  microToMacro = function(micro=NULL, ...){
    .nodots(...);
    if(is.null(micro)) micro <- 1:nrow(data)
    structure(assignment[micro], names=micro)
  }
)

