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


### Subclasses need to implement methods for:
# Construction (DSC_...)
# get_macroclusters(x)
# get_macroweights(x)
# microToMacro(x, micro=NULL)
# a cluster method in the RObj
#######

#' Abstract Class for Macro Clusterers
#'
#' Abstract class for all DSC Macro Clusterers. `DSC_Macro` cannot be instantiated.
#'
#' `DSC_Macro` provide [microToMacro] that returns the assignment of Micro-cluster IDs to Macro-cluster IDs.
#'
#' @param ... further arguments.
#' @aliases DSC_Macro
#' @author Michael Hahsler
#' @seealso [DSC]
DSC_Macro <- abstract_class_generator("DSC")

#' Translate Micro-cluster IDs to Macro-cluster IDs
#'
#' Returns the assignment of micro-cluster ids to macro-cluster ids for a `DSC_Macro`
#' object.
#'
#' @name microToMacro
#' @param x a \code{DSC_Macro} object that also contains information about
#' micro-clusters.
#' @param micro A vector with micro-cluster ids. If `NULL` then the
#' assignments for all micro-clusters in `x` are returned.
#' @return A vector of the same length as `micro` with the macro-cluster
#' ids.
#' @author Michael Hahsler
#' @seealso \code{\link{DSC_Macro}}
#' @examples
#'
#' stream <- DSD_Gaussians(k=3, d=2, noise=0.05, p=c(.2,.4,.6))
#'
#' # recluster a micro-clusters
#' micro <- DSC_DStream(gridsize=0.05)
#' update(micro, stream, 500)
#'
#' macro <- DSC_Kmeans(k=3)
#' recluster(macro, micro)
#'
#' # translate all micro-cluster ids
#' microToMacro(macro)
#'
#' # plot some data points in gray
#' plot(stream, col="gray", cex=.5, xlim=c(0,1), ylim=c(0,1))
#' # add micro-clusters and use the macro-cluster ids as color and weights as size
#' points(get_centers(macro, type="micro"),
#'   col=microToMacro(macro),
#'   cex=get_weights(macro, type="micro", scale=c(.5,3)))
#' # add macro-cluster centers (size is weight)
#' points(get_centers(macro, type="macro"),
#'   cex = get_weights(macro, type="macro", scale=c(2,5)),
#'   pch=3,lwd=3, col=1:3)
#'
#' @export microToMacro
microToMacro <- function(x, micro=NULL) UseMethod("microToMacro")
microToMacro.default <- function(x, micro=NULL) {
        stop(gettextf("microToMacro not implemented for class '%s'.",
		                          paste(class(x), collapse=", ")))
}


get_centers.DSC_Macro <- function(x, type=c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "macro"

    if(type=="macro") return(get_macroclusters(x, ...))
    else return(get_microclusters(x, ...))
}


get_weights.DSC_Macro <- function(x, type=c("auto", "micro", "macro"),
	scale=NULL, ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "macro"

    if(type=="macro") w <- get_macroweights(x, ...)
    else w <- get_microweights(x, ...)

    if(!is.null(scale)) {
	if(length(unique(w)) ==1) w <- rep(mean(scale), length(w))
	else w <- map(w, range=scale, from.range=c(0,
			    max(w, na.rm=TRUE)))
    }


    w
}
