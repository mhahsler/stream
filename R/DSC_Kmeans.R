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


#' Kmeans Macro-clusterer
#'
#' Macro Clusterer.
#' Class implements the k-means algorithm for reclustering a set of
#' micro-clusters.
#'
#' [update()] and [`recluster()`] invisibly return the assignment of the data points
#' to clusters.
#'
#' Please refer to function [stats::kmeans()] for more details on
#' the algorithm.
#'
#' **Note** that this clustering cannot be updated iteratively and every time it is
#' used for (re)clustering, the old clustering is deleted.
#'
#' @family DSC_Macro
#'
#' @param formula `NULL` to use all features in the stream or a model [formula] of the form `~ X1 + X2`
#'   to specify the features used for clustering. Only `.`, `+` and `-` are currently
#'   supported in the formula.
#' @param k either the number of clusters, say k, or a set of initial
#' (distinct) cluster centers. If a number, a random set of (distinct) rows in
#' x is chosen as the initial centers.
#' @param weighted use a weighted k-means (algorithm is ignored).
#' @param iter.max the maximum number of iterations allowed.
#' @param nstart if centers is a number, how many random sets should be chosen?
#' @param algorithm character: may be abbreviated.
#' @param min_weight micro-clusters with a weight less than this will be
#' ignored for reclustering.
#' @param description optional character string to describe the clustering
#' method.
#' @return An object of class `DSC_Kmeans` (subclass of [DSC],
#' [DSC_R], [DSC_Macro])
#' @author Michael Hahsler
#' @examples
#' set.seed(1000)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0)
#'
#' # create micro-clusters via sampling
#' sample <- DSC_Sample(k = 20)
#' update(sample, stream, 500)
#' sample
#'
#' # recluster micro-clusters
#' kmeans <- DSC_Kmeans(k=3)
#' recluster(kmeans, sample)
#' plot(kmeans, stream, type = "both")
#'
#'
#' # For comparison we use k-means directly to cluster data
#' # Note: k-means is not a data stream clustering algorithm
#' kmeans <- DSC_Kmeans(k = 3)
#' update(kmeans, stream, 500)
#' plot(kmeans, stream, type = "macro")
#' @export
DSC_Kmeans <-
  function(formula = NULL,
    k,
    weighted = TRUE,
    iter.max = 10,
    nstart = 10,
    algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
      "MacQueen"),
    min_weight = NULL,
    description = NULL) {
    algorithm <- match.arg(algorithm)
    if (!is.null(description))
      desc <- description
    else if (weighted)
      desc <- "k-Means (weighted)"
    else
      desc <- "k-Means"

    structure(
      list(
        description = desc,
        formula = formula,
        RObj = kmeans_refClass$new(
          k = k,
          weighted = weighted,
          iter.max = iter.max,
          nstart = nstart,
          algorithm = algorithm,
          min_weight = min_weight
        )
      ),
      class = c("DSC_Kmeans", "DSC_Macro", "DSC_R", "DSC")
    )
  }


kmeans_refClass <- setRefClass(
  "kmeans",
  fields = list(
    k	    = "numeric",
    weighted = "logical",
    iter.max    = "numeric",
    nstart	    = "numeric",
    algorithm   = "character",
    assignment  = "numeric",
    data      = "data.frame",
    weights	    = "numeric",
    clusterCenters = "data.frame",
    clusterWeights = "numeric",
    details      = "ANY",
    min_weight   = "numeric",
    colnames = "ANY"
  ),

  methods = list(
    initialize = function(k      = 3,
      weighted = TRUE,
      iter.max    = 10,
      nstart	    = 1,
      algorithm   = c("Hartigan-Wong", "Lloyd",
        "Forgy", "MacQueen"),
      min_weight = NULL) {
      k  	<<- k
      weighted <<- weighted
      iter.max	<<- iter.max
      nstart	<<- nstart
      algorithm   <<- match.arg(algorithm)
      assignment	<<- numeric()
      weights	<<- numeric()
      clusterWeights <<- numeric()
      clusterCenters <<- data.frame()
      data	<<- data.frame()

      if (is.null(min_weight))
        min_weight <<- 0
      else
        min_weight <<- min_weight

      colnames <<- NULL

      .self
    }

  ),
)

kmeans_refClass$methods(
  cluster = function(x, weight = rep(1, nrow(x)), ...) {
    #  if(nrow(x)==1)
    #    warning("DSC_Kmeans does not support iterative updating! Old data is overwritten.")

    ### filter weak clusters
    if (min_weight > 0) {
      x <- x[weight > min_weight, ]
      weight <- weight[weight > min_weight]
    }


    weights <<- weight
    data <<- x

    if (nrow(data) > k) {
      if (weighted)
        km <- kmeansW(
          x = data,
          weight = weights,
          centers = k,
          iter.max = iter.max,
          nstart = nstart
        )
      else
        km <- kmeans(
          x = data,
          centers = k,
          iter.max = iter.max,
          nstart = nstart,
          algorithm = algorithm
        )

      assignment <<- km$cluster
      clusterCenters <<- data.frame(km$centers)
      details <<- km
    } else {
      assignment <<- 1:nrow(data)
      clusterCenters <<- x
      details <<- NULL
    }

    clusterWeights <<- sapply(
      1:k,
      FUN =
        function(i)
          sum(weights[assignment == i])
    )

    invisible(data.frame(.class = assignment))
  },

  get_macroclusters = function(...) {
    clusterCenters
  },
  get_macroweights = function(...) {
    clusterWeights
  },

  get_microclusters = function(...) {
    data
  },
  get_microweights = function(x) {
    weights
  },

  microToMacro = function(micro = NULL, ...) {
    if (is.null(micro))
      micro <- 1:nrow(data)
    structure(assignment[micro], names = micro)
  }
)
