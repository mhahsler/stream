#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Sioftware Foundation; either version 2 of the License, or
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


#' Data Stream Clusterer Base Classes
#'
#' Abstract base classes for all DSC (Data Stream Clusterer) and DSC_R classes.
#' Concrete implementations are functions starting with `DSC_` (R Studio use auto-completion with Tab to select one).
#'
#' The `DSC` and `DSC_R` classes cannot be instantiated (calling
#' `DSC()` or `DSC_R()` produces only a message listing the available implementations),
#' but they serve as a base
#' class from which other DSC classes inherit.
#'
#' Class `DSC` provides several generic functions that can operate on all
#' DSC subclasses. See Functions section below.
#' Additional, separately documented functions are:
#'
#' * [update] Add new data points from a stream to a clustering.
#' * [plot] function is also provides for `DSC`.
#' * [get_assignment] Find out what cluster new data points would be assigned to.
#'
#' `get_centers` and `get_weights` are typically overwritten by
#' subclasses of `DSC`. [DSC_R] provides these functions for R-based
#' DSC implementations.
#'
#' Since `DSC` objects often contain external pointers, regular saving and
#' reading operations will fail. Use [saveDSC] and [readDSC]
#' which will serialize the objects first appropriately.
#'
#' @family DSC
#'
#' @param x,object a DSC object.
#' @param dsd a data stream object.
#' @param n number of data points taken from the stream.
#' @param type Return weights of micro- or macro-clusters in x.  Auto uses the
#' class of x to decide.
#' @param scale a range (from, to) to scale the weights.  Returns by default
#' the raw weights.
#' @param ... further parameter
#' @author Michael Hahsler
#' @export DSC
#' @examples
#'
#' DSC()
#'
#' stream <- DSD_Gaussians(k=3, d=2)
#' dstream <- DSC_DStream(gridsize=.1)
#' update(dstream, stream, 500)
#' dstream
#'
#' # get micro-cluster centers
#' get_centers(dstream)
#'
#' # get the number of clusters
#' nclusters(dstream)
#'
#' # get the micro-cluster weights
#' get_weights(dstream)
#'
#' # D-Stream also has macro-clusters
#' get_weights(dstream, type="macro")
#'
#' @export
DSC <- abstract_class_generator("DSC")

### all DSC classes have these interface methods

#' @rdname DSC
#' @export
update.DSC <- function(object, dsd, n = 1, ...) {
  stop("No implementaiton for update found!")
}


#' @describeIn DSC Gets the cluster centers (micro- or macro-clusters) from a DSC object.
#' @export get_centers
get_centers <- function(x, type = c("auto", "micro", "macro"), ...)
  UseMethod("get_centers")

get_centers.default <- function(x, type = c("auto", "micro", "macro"), ...) {
  stop(gettextf("get_centers not implemented for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get the weights of the clusters in the DSC (returns 1s if not implemented by the clusterer)
#' @export get_weights
get_weights <- function(x, type=c("auto", "micro", "macro"), scale=NULL, ...)
  UseMethod("get_weights")

get_weights.default <- function(x, type=c("auto", "micro", "macro"),
  scale=NULL, ...) {
  .nodots(...)
  m <- rep(1, nclusters(x, type=type))
  if(!is.null(scale)) {
    if(length(unique(m)) ==1)  w <- rep(mean(scale), length(w))
    else m <- map(m, range=scale, from.range=c(0,
      max(m, na.rm=TRUE)))
  }
  m
}

### End of interface
#####################################################################3

### make a deep copy of the


#' @describeIn DSC Create a Deep Copy of a DSC Object that contain reference classes (e.g., Java data structures for MOA).
#' @export
get_copy <- function(x) UseMethod("get_copy")

get_copy.default <- function(x, ...) {
  stop(gettextf("get_copy not implemented for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get micro-clusters if the object is a `DSC_Micro`.
#' @export
get_microclusters <- function(x, ...) UseMethod("get_microclusters")

#' @export
get_microclusters.DSC <- function(x, ...) {
  stop(gettextf("No micro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get micro-clusters if the the object is a `DSC_Macro`.
#' @export
get_macroclusters <- function(x, ...) UseMethod("get_macroclusters")

#' @export
get_macroclusters.DSC <- function(x, ...) {
  stop(gettextf("No macro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get micro-cluster weights if the object is a `DSC_Micro`.
#' @export
get_microweights <- function(x, ...) UseMethod("get_microweights")

#' @export
get_microweights.DSC <- function(x, ...) {
  stop(gettextf("No weights for micro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get macro-cluster weights if the object is a `DSC_Macro`.
#' @export
get_macroweights <- function(x, ...) UseMethod("get_macroweights")

#' @export
get_macroweights.DSC <- function(x, ...) {
  stop(gettextf("No weights for macro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}


#' @describeIn DSC Returns the number of micro-clusters from the DSC object.
#' @export
nclusters <- function(x, type=c("auto", "micro", "macro"), ...)
  UseMethod("nclusters")

#' @export
nclusters.DSC <- function(x, type=c("auto", "micro", "macro"), ...) {
  nrow(get_centers(x, type=type, ...))
}

#' @export
print.DSC <- function(x, ...) {
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse=", "), "\n")
  if(!is(nc <- try(nclusters(x, type="micro"), silent=TRUE), "try-error"))
    cat(paste('Number of micro-clusters:', nc, '\n'))
  if(!is(nc <- try(nclusters(x, type="macro"), silent=TRUE), "try-error"))
    cat(paste('Number of macro-clusters:', nc, '\n'))
}

#' @export
summary.DSC <- function(object, ...) print(object)

