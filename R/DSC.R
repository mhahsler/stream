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


#' Data Stream Clustering Base Class
#'
#' Abstract base classes for Data Stream Clustering (DSC).
#' Concrete implementations are functions starting with `DSC_` (RStudio use auto-completion with Tab to select one).
#'
#' The `DSC` class cannot be instantiated (calling
#' `DSC()` produces only a message listing the available implementations),
#' but they serve as a base
#' class from which other DSC classes inherit.
#'
#' Data stream clustering has typically an
#'
#'   - **online clustering component** (see [DSC_Micro]), and an
#'   - **offline reclustering component** (see [DSC_Macro]).
#'
#' Class `DSC` provides several generic functions that can operate on all
#' DSC subclasses. See Usage and Functions sections for methods.
#' Additional, separately documented methods are:
#'
#' * [update()] adds new data points from a stream to a clustering.
#' * [predict()] predicts the cluster assignment for new data points.
#' * `plot()` plots cluster centers (see [plot.DSC()]).
#'
#' `get_centers()` and `get_weights()` are typically overwritten by
#' subclasses of `DSC`.
#'
#' Since `DSC` objects often contain external pointers, regular saving and
#' reading operations will fail. Use [saveDSC()] and [readDSC()]
#' which will serialize the objects first appropriately.
#'
#' @family DST
#' @family DSC
#'
#' @param x a DSC object.
#' @param type Return weights of micro- or macro-clusters in x.  Auto uses the
#' class of x to decide.
#' @param scale a range (from, to) to scale the weights.  Returns by default
#' the raw weights.
#' @param ... further parameter
#' @author Michael Hahsler
#' @export DSC
#' @examples
#' DSC()
#'
#' set.seed(1000)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#' dstream <- DSC_DStream(gridsize = .1, gaptime = 100)
#' update(dstream, stream, 500)
#' dstream
#'
#' # get micro-cluster centers
#' get_centers(dstream)
#'
#' # get the micro-cluster weights
#' get_weights(dstream)
#'
#' # get the number of clusters
#' nclusters(dstream)
#'
#' # get the whole model as a data.frame
#' get_model(dstream)
#'
#' # D-Stream also has macro-clusters
#' get_weights(dstream, type = "macro")
#' get_centers(dstream, type = "macro")
#'
#' # plot the clustering result
#' plot(dstream, stream)
#' plot(dstream, stream, type = "both")
#'
#' # predict macro clusters for new points (see predict())
#' points <- get_points(stream, n = 5)
#' points
#'
#' predict(dstream, points, type = "macro")
#' @export
DSC <- abstract_class_generator("DSC")

### all DSC classes have these interface methods

#' @describeIn DSC Gets the cluster centers (micro- or macro-clusters) from a DSC object.
#' @export
get_centers <- function(x, type = c("auto", "micro", "macro"), ...)
  UseMethod("get_centers")

#' @describeIn DSC Get the weights of the clusters in the DSC (returns 1s if not implemented by the clusterer)
#' @export
get_weights <-
  function(x,
    type = c("auto", "micro", "macro"),
    scale = NULL,
    ...)
    UseMethod("get_weights")

#' @export
get_weights.DSC <- function(x,
  type = c("auto", "micro", "macro"),
  scale = NULL,
  ...) {
  .nodots(...)
  m <- rep(1, nclusters(x, type = type))
  if (!is.null(scale)) {
    if (length(unique(m)) == 1L)
      w <- rep(mean(scale), length(w))
    else
      m <- map(m, range = scale, from.range = c(0,
        max(m, na.rm = TRUE)))
  }
  m
}

### make a deep copy of the
#' @describeIn DSC Create a Deep Copy of a DSC Object that contain reference classes (e.g., Java data structures for MOA).
#' @export
get_copy <- function(x)
  UseMethod("get_copy")

#' @describeIn DSC Returns the number of micro-clusters from the DSC object.
#' @export
nclusters <- function(x, type = c("auto", "micro", "macro"), ...)
  UseMethod("nclusters")

#' @export
nclusters.DSC <-
  function(x, type = c("auto", "micro", "macro"), ...) {
    nrow(get_centers(x, type = type, ...))
  }

#' @export
print.DSC <- function(x, ...) {
  cat(.line_break(paste(x$description)), "\n")
  cat("Class:", toString(class(x)), "\n")
  if (!is(nc <-
      try(nclusters(x, type = "micro"), silent = TRUE)
    , "try-error"))
    cat(paste('Number of micro-clusters:', nc, '\n'))
  if (!is(nc <-
      try(nclusters(x, type = "macro"), silent = TRUE)
    , "try-error"))
    cat(paste('Number of macro-clusters:', nc, '\n'))
}

#' @export
summary.DSC <- function(object, ...)
  print(object)

#' @export
get_model.DSC <- function(x, ...)
  cbind(weight = get_weights(x),  get_centers(x))

### End of interface
#####################################################################3

#' @describeIn DSC Used as internal interface.
#' @export
get_microclusters <- function(x, ...)
  UseMethod("get_microclusters")

#' @describeIn DSC Used as internal interface.
#' @export
get_microweights <- function(x, ...)
  UseMethod("get_microweights")

#' @describeIn DSC Used as internal interface.
#' @export
get_macroclusters <- function(x, ...)
  UseMethod("get_macroclusters")

#' @describeIn DSC Used as internal interface.
#' @export
get_macroweights <- function(x, ...)
  UseMethod("get_macroweights")
