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

# Reachability and single-link hierarchical clustering are equivalent


#' Reachability Micro-Cluster Reclusterer
#'
#' Macro Clusterer.
#' Implementation of reachability clustering (based on DBSCAN's concept of
#' reachability) to recluster a set of micro-clusters.
#'
#'
#' Two micro-clusters are directly reachable if they are within each other's
#' epsilon-neighborhood (i.e., the distance between the centers is less then
#' epsilon). Two micro-clusters are reachable if they are connected by a chain
#' of pairwise directly reachable micro-clusters.  All mutually reachable
#' micro-clusters are put in the same cluster.
#'
#' Reachability uses internally [DSC_Hierarchical] with single link.
#'
#' [update()] and [`recluster()`] invisibly return the assignment of the data points
#' to clusters.
#'
#' **Note** that this clustering cannot be updated iteratively and every time it is
#' used for (re)clustering, the old clustering is deleted.
#'
#' @family DSC_Macro
#'
#' @param formula `NULL` to use all features in the stream or a model [formula] of the form `~ X1 + X2`
#'   to specify the features used for clustering. Only `.`, `+` and `-` are currently
#'   supported in the formula.
#' @param epsilon radius of the epsilon-neighborhood.
#' @param min_weight micro-clusters with a weight less than this will be
#' ignored for reclustering.
#' @param description optional character string to describe the clustering
#' method.
#' @return An object of class `DSC_Reachability`. The object contains the
#' following items:
#'
#' \item{description}{The name of the algorithm in the DSC object.}
#' \item{RObj}{The underlying R object.}
#' @author Michael Hahsler
#' @references
#' Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu
#' (1996). A density-based algorithm for discovering clusters in large spatial
#' databases with noise. In Evangelos Simoudis, Jiawei Han, Usama M. Fayyad.
#' _Proceedings of the Second International Conference on Knowledge
#' Discovery and Data Mining (KDD-96)._ AAAI Press. pp. 226-231.
#' @examples
#' #' # 3 clusters with 5% noise
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#'
#' # Use a moving window for "micro-clusters and recluster with DBSCAN (macro-clusters)
#' cl <- DSC_TwoStage(
#'   micro = DSC_Window(horizon = 100),
#'   macro = DSC_Reachability(eps = .05)
#' )
#'
#' update(cl, stream, 500)
#' cl
#'
#' plot(cl, stream)
#' @export
DSC_Reachability <-
  function(formula = NULL,
    epsilon,
    min_weight = NULL,
    description = NULL) {
    hierarchical <- hierarchical$new(h = epsilon,
      method = "single",
      min_weight = min_weight)

    if (is.null(description))
      description <- "Reachability"

    l <-
      list(description = description,
        formula = formula,
        RObj = hierarchical)

    class(l) <-
      c("DSC_Reachability",
        "DSC_Hierarchical",
        "DSC_Macro",
        "DSC_R",
        "DSC")
    l
  }
