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

#' Re-clustering micro-clusters
#'
#' Use an ***offline** macro clustering algorithm to recluster micro-clusters into a final
#' clusters.
#'
#' Takes centers and weights of the micro-clusters and applies the macro
#' clustering algorithm.
#'
#' See [DSC_TwoStage] for a convenient combination of micro and macro clustering.
#'
#' @family DSC
#'
#' @param macro an empty [DSC_Macro].
#' @param micro an updated [DSC_Micro] with micro-clusters.
#' @param type controls which clustering is used from `micro`. Typically
#' `auto`.
#' @param ... additional arguments passed on.
#' @return The object `macro` is altered in place and contains the clustering.
#' @author Michael Hahsler
#' @examples
#' set.seed(0)
#' ### create a data stream and a micro-clustering
#' stream <- DSD_Gaussians(k = 3, d = 3)
#'
#' ### sample can be seen as a simple online clusterer where the sample points
#' ### are the micro clusters.
#' sample <- DSC_Sample(k = 50)
#' update(sample, stream, 500)
#' sample
#'
#' ### recluster using k-means
#' kmeans <- DSC_Kmeans(k = 3)
#' recluster(kmeans, sample)
#'
#' ### plot clustering
#' plot(kmeans, stream, type = "both", main = "Macro-clusters (Sampling + k-means)")
#' @export
recluster <-
  function(macro, micro, type = "auto", ...)
    UseMethod("recluster")

#' @export
recluster.DSC <- function(macro, micro, type = "auto", ...) {
  stop(gettextf(
    "recluster not implemented for class '%s'.",
    toString(class(macro))
  ))
}

### reclustering is done with a DSC_Macro object!

#' @rdname recluster
#' @export
recluster.DSC_Macro <- function(macro, micro, type = "auto", ...) {
  cen <- get_centers(micro, type = type)
  dsd <- DSD_Memory(cen)
  weight <- get_weights(micro, scale = NULL, type = type)
  update(macro, dsd, n = nrow(cen), weight = weight, ...)
}
