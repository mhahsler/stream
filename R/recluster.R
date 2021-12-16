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


## wrapper for recluster functions



#' Re-clustering micro-clusters
#' 
#' Use a macro clustering algorithm to recluster micro-clusters into a final
#' clustering.
#' 
#' Takes centers and weights of the micro-clusters and applies the macro
#' clustering algorithm.
#' 
#' @param macro a macro clustering algorithm (class "DSC_Macro")
#' @param micro a DSC object containing micro-clusters.
#' @param type controls which clustering is used from \code{dsc} (typically
#' micro-clusters).
#' @param ... additional arguments passed on.
#' @return The object macro is altered and contains the clustering.
#' @author Michael Hahsler
#' @examples
#' 
#' set.seed(0)
#' ### create a data stream and a micro-clustering
#' stream <- DSD_Gaussians(k=3, d=3)
#' 
#' sample <- DSC_Sample(k=50)
#' update(sample, stream, 500)
#' sample
#'   
#' ### recluster using k-means
#' kmeans <- DSC_Kmeans(k=3)
#' recluster(kmeans, sample)
#' 
#' ### plot clustering
#' plot(kmeans, stream, main="Macro-clusters (Sampling + k-means)")
#' 
#' @export recluster
recluster <- function(macro, micro, type="auto", ...) UseMethod("recluster")

recluster.DSC <- function(macro, micro, type="auto", ...) {
    stop(gettextf("recluster not implemented for class '%s'.", 
		    paste(class(macro), collapse=", ")))
}

### reclustering is done with a DSC_Macro object!
recluster.DSC_Macro <- function(macro, micro, type="auto", ...) {
    cen <- get_centers(micro, type=type)
    dsd <- DSD_Memory(cen)
    weight <- get_weights(micro, scale=NULL, type=type)
    update(macro, dsd, n=nrow(cen), weight=weight, ...)
}

