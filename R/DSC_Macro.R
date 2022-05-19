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
#' @family DSC
#'
#' @param ... further arguments.
#' @author Michael Hahsler
#' @seealso [DSC]
#' @export
DSC_Macro <- abstract_class_generator("DSC")


#' @export
get_centers.DSC_Macro <- function(x, type=c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "macro"

    if(type=="macro") return(get_macroclusters(x, ...))
    else return(get_microclusters(x, ...))
}

#' @export
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
