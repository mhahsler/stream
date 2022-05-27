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
# Constructor
# method cluster for RObj
# get_microclusters(x)
# get_microweights(x)
########

#' Abstract Class for Micro Clusterers (Online Component)
#'
#' Abstract class for all clustering methods that can operate **online** and result in a set of
#' micro-clusters.
#'
#' Micro-clustering algorithms are data stream mining tasks [DST]
#' which implement the **online component of data stream clustering.**
#' The clustering is performed sequentially by using [update()]
#' to add new points from a data stream to the clustering. The result is
#' a set of micro-clusters that can be retrieved using [get_clusters()].
#'
#' Available clustering methods can be found in the See Also section below.
#'
#' Many data stream clustering algorithms define both, the online and an offline
#' component to recluster micro-clusters into larger clusters called macro-clusters.
#' This is implemented here as class [DSC_TwoStage].
#'
#' `DSC_Micro` cannot be instantiated.
#'
#' @family DSC_Micro
#' @family DSC
#'
#' @param ... further arguments.
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_BarsAndGaussians(noise = .05)
#'
#' # Use a DStream to create micro-clusters
#' dstream <- DSC_DStream(gridsize = 1, Cm = 1.5)
#' update(dstream, stream, 1000)
#' dstream
#' nclusters(dstream)
#' plot(dstream, stream, main = "micro-clusters")
#' @export
DSC_Micro <- abstract_class_generator("DSC")

#' @export
get_centers.DSC_Micro <-
  function(x, type = c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if (type == "auto")
      type <- "micro"

    if (type == "macro")
      return(get_macroclusters(x, ...))
    else
      return(get_microclusters(x, ...))
  }

#' @export
get_weights.DSC_Micro <-
  function(x,
    type = c("auto", "micro", "macro"),
    scale = NULL,
    ...) {
    type <- match.arg(type)
    if (type == "auto")
      type <- "micro"

    if (type == "macro")
      w <- get_macroweights(x, ...)
    else
      w <- get_microweights(x, ...)

    if (!is.null(scale)) {
      if (length(unique(w)) == 1)
        w <- rep(mean(scale), length(w))
      else
        w <- map(w, range = scale, from.range = c(0,
          max(w, na.rm = TRUE)))
    }

    w
  }
