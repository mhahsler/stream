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


#' Create as Static Copy of a Clustering
#'
#' This representation cannot perform clustering anymore, but it also does not
#' need the supporting data structures. It only stores the cluster centers and
#' weights.
#'
#' @family DSC
#'
#' @param x The clustering (a DSD object) to copy or a list with components `centers` (a data frame or matrix) and
#'   `weights` (a vector with cluster weights).
#' @param type which clustering to copy.
#' @param k_largest only copy the k largest (highest weight) clusters.
#' @param min_weight only copy clusters with a weight larger or equal to
#'   `min_weight`.
#' @return An object of class `DSC_Static` (sub class of [DSC],
#'   [DSC_R]). The list also contains either [DSC_Micro] or
#'   [DSC_Macro] depending on what type of clustering was copied.
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k = 3, noise = 0.05)
#'
#' dstream <- DSC_DStream(gridsize = 0.05)
#' update(dstream, stream, 500)
#' dstream
#' plot(dstream, stream)
#'
#' # create a static copy of the clustering
#' static <- DSC_Static(dstream)
#' static
#' plot(static, stream)
#'
#' # copy only the 5 largest clusters
#' static2 <- DSC_Static(dstream, k_largest = 5)
#' static2
#' plot(static2, stream)
#'
#' # copy all clusters with a weight of at least .3
#' static3 <- DSC_Static(dstream, min_weight = .3)
#' static3
#' plot(static3, stream)
#'
#' # create a manual clustering
#' static4 <- DSC_Static(list(
#'              centers = data.frame(X1 = c(1, 2), X2 = c(1, 2)),
#'              weights = c(1, 2)),
#'              type = "macro")
#' static4
#' plot(static4)
#' @export
DSC_Static <- function(x,
  type = c("auto", "micro", "macro"),
  k_largest = NULL,
  min_weight = NULL) {
  ### just a list
  if (!inherits(x, "DSC")) {
    if (!is.list(x))
      stop("Manually created clusters need a list with components 'centers' and 'weights'!")

    type <- match.arg(type)
    if (type == "auto")
      stop("Manually created clusters need type 'micro' or 'macro'!")
    macro <- type == "macro"

    centers <- as.data.frame(x$c)
    if (is.null(centers))
      stop("No centers specified!")
    centers <- as.data.frame(centers)

    weights <- x$w
    if (is.null(weights))
      weights <- rep(1, nrow(centers))
    if (length(weights) != nrow(centers))
      stop("Number of rows of centers and number of weights do not agree!")

  } else {
    ### figure out type
    type <- get_type(x, type)
    macro <- type == "macro"

    ### make sure it is a data.frame
    centers <- as.data.frame(get_centers(x, type))
    weights <- get_weights(x, type)

    if (!is.null(k_largest)) {
      if (k_largest > nclusters(x)) {
        warning("Less clusters than k. Using all clusters.")
      } else{
        o <- head(order(weights, decreasing = TRUE), n = k_largest)
        centers <- centers[o,]
        weights <- weights[o]
      }
    }

    if (!is.null(min_weight)) {
      take <- weights >= min_weight
      centers <- centers[take,]
      weights <- weights[take]
    }
  }

  static <- Static$new(centers, weights, macro = macro)

  l <- list(description = "Static clustering", RObj = static)

  if (macro)
    micromacro <- "DSC_Macro"
  else
    micromacro <- "DSC_Micro"

  class(l) <- c("DSC_Static", micromacro, "DSC_R", "DSC")

  l
}

### NOTE: uptodate is used to signal if a macro-clustering should be updated.

Static <- setRefClass(
  "Static",
  fields = list(
    centers		 = "data.frame",
    weights		 = "numeric",
    macro		   = "logical",
    microToMacro = "integer"
  ),

  methods = list(
    initialize  = function(centers	= data.frame(),
      weights	  = numeric(),
      macro	    = FALSE) {
      centers	  <<- centers
      weights	  <<- weights
      macro	    <<- macro
      microToMacro <<- integer()
      .self
    }

  ),
)

Static$methods(
  cluster = function(newdata, ...) {
    stop("DSC_Static: Static clusterings cannot be updated!")
  },

  get_macroweights = function(...) {
    if (!macro)
      stop("This is a micro-clustering!")
    weights
  },

  get_macroclusters = function(...) {
    if (!macro)
      stop("This is a micro-clustering!")
    centers
  },

  get_microweights = function(...) {
    if (macro)
      stop("This is a macro-clustering!")
    weights
  },

  get_microclusters = function(...) {
    if (macro)
      stop("This is a macro-clustering!")
    centers
  }
)

