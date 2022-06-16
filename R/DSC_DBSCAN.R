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

#' DBSCAN Macro-clusterer
#'
#' Macro Clusterer.
#' Implements the DBSCAN algorithm for reclustering micro-clusterings.
#'
#' DBSCAN is a weighted extended version of the implementation in \pkg{fpc}
#' where each micro-cluster center considered a pseudo point. For weighting we
#' use in the MinPts comparison the sum of weights of the micro-cluster instead
#' of the number.
#'
#' DBSCAN first finds core points based on the number of other points in its
#' eps-neighborhood. Then core points are joined into clusters using
#' reachability (overlapping eps-neighborhoods).
#'
#' [update()] and [recluster()] invisibly return the assignment of the data points to clusters.
#'
#' **Note** that this clustering cannot be updated iteratively and every time it is
#' used for (re)clustering, the old clustering is deleted.
#'
#' @aliases DBSCAN dbscan
#' @family DSC_Macro
#'
#' @param formula `NULL` to use all features in the stream or a model [formula] of the form `~ X1 + X2`
#'   to specify the features used for clustering. Only `.`, `+` and `-` are currently
#'   supported in the formula.
#' @param eps radius of the eps-neighborhood.
#' @param MinPts minimum number of points required in the eps-neighborhood.
#' @param weighted logical indicating if a weighted version of DBSCAN should be
#' used.
#' @param description optional character string to describe the clustering
#' method.
#' @return An object of class `DSC_DBSCAN` (a subclass of [DSC],
#' [DSC_R], [DSC_Macro]).
#' @author Michael Hahsler
#' @references
#' Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu
#' (1996). A density-based algorithm for discovering clusters in large spatial
#' databases with noise. In Evangelos Simoudis, Jiawei Han, Usama M. Fayyad.
#' _Proceedings of the Second International Conference on Knowledge
#' Discovery and Data Mining (KDD-96)._ AAAI Press. pp. 226-231.
#' @examples
#' # 3 clusters with 5% noise
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#'
#' # Use a moving window for "micro-clusters and recluster with DBSCAN (macro-clusters)
#' cl <- DSC_TwoStage(
#'   micro = DSC_Window(horizon = 100),
#'   macro = DSC_DBSCAN(eps = .05)
#' )
#'
#' update(cl, stream, 500)
#' cl
#'
#' plot(cl, stream)
#' @export
DSC_DBSCAN <-
  function(formula = NULL,
    eps,
    MinPts = 5,
    weighted = TRUE,
    description = NULL) {
    DBSCAN <- DBSCAN$new(eps = eps,
      MinPts = MinPts,
      weighted = weighted)
    if (!is.null(description))
      desc <- description
    else if (weighted)
      desc <- "DBSCAN (weighted)"
    else
      desc <- "DBSCAN"

    l <- list(description = desc, formula = formula, RObj = DBSCAN)
    class(l) <- c("DSC_DBSCAN", "DSC_Macro", "DSC_R", "DSC")
    l
  }


DBSCAN <- setRefClass(
  "DBSCAN",
  fields = list(
    eps	        = "numeric",
    MinPts	    = "numeric",
    weighted    = "logical",
    assignment  = "numeric",
    details	    = "ANY",
    data        = "data.frame",
    weights	    = "numeric",
    clusterCenters = "data.frame",
    clusterWeights = "numeric",
    colnames = "ANY"
  ),

  methods = list(
    initialize = function(eps = .1,
      MinPts	= 5,
      weighted = TRUE) {
      eps     <<- eps
      MinPts  <<- MinPts
      weighted <<- weighted

      data    <<- data.frame()
      weights <<- numeric()
      clusterWeights <<- numeric()
      clusterCenters <<- data.frame()

      colnames <<- NULL

      .self
    }

  ),
)

DBSCAN$methods(
  cluster = function(x, weight = NULL, ...) {
    #if(nrow(x)==1)
    #  warning("DSC_DBSCAN does not support iterative updating! Old data is overwritten.")

    if (is.null(weight))
      weights <<- rep(1, nrow(x))
    else {
      if (length(weight) != nrow(x))
        stop("number of weights does not match number of points")
      weights <<- weight
    }

    data <<- x

    if (!weighted)
      weight <- NULL

    DBSCAN <-
      dbscan::dbscan(data,
        eps = eps,
        minPts = MinPts,
        weights = weight)

    assignment <<- DBSCAN$cluster

    ### FIXME: we currently remove unassigned data!
    row_sub <- unlist(lapply(assignment, function(x)
      all(x != 0)))
    data <<- data[row_sub, , drop = FALSE]
    assignment <<- assignment[row_sub]
    details <<- DBSCAN


    if (length(assignment > 0)) {
      k <- max(assignment)
      clusterCenters <<- as.data.frame(t(sapply(
        1:k,
        FUN =
          function(i)
            colMeans(data[assignment == i, , drop = FALSE])
      )))

      clusterWeights <<- sapply(
        1:k,
        FUN =
          function(i)
            sum(weights[assignment == i], na.rm = TRUE)
      )
    } else{
      ### no clusters found
      k <- 0
      clusterCenters <<- data.frame()
      clusterWeights <<- numeric(0)
    }

    asgn <- DBSCAN$cluster
    asgn[asgn == 0] <- NA
    invisible(data.frame(.class = asgn))
  },

  get_microclusters = function(...) {
    data
  },
  get_microweights = function(...) {
    weights
  },

  get_macroclusters = function(...) {
    clusterCenters
  },
  get_macroweights = function(...) {
    clusterWeights
  },

  microToMacro = function(micro = NULL, ...) {
    if (is.null(micro))
      micro <- 1:nrow(data)
    structure(assignment[micro], names = micro)
  }
)
