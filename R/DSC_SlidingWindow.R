#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2022 Michael Hahsler
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


#' DSC_SlidingWindow -- Data Stream Clusterer Using a Sliding Window
#'
#' The clusterer keeps a sliding window for the stream and rebuilds a DSC clustering model at regular
#' intervals. By default is uses [DSC_Kmeans]. Other [DSC_Macro] clusterer can be used.
#'
#' This constructor creates a clusterer based on [`DST_SlidingWindow`]. The clusterer has
#' a `update()` and `predict()` method.
#'
#' The difference to setting up a [DSC_TwoStage] is that `DSC_SlidingWindow` rebuilds
#' the model in regular intervals, while `DSC_TwoStage` rebuilds the model on demand.
#'
#' @family DSC
#' @family DSC_Macro
#'
#' @param formula a formula for the classification problem.
#' @param model regression model (that has a formula interface).
#' @param window size of the sliding window.
#' @param rebuild interval (number of points) for rebuilding the regression. Set rebuild to
#'   `Inf` to prevent automatic rebuilding. Rebuilding can be initiated manually when
#'   calling `update()`.
#' @param ... additional parameters are passed on to the clusterer (default is [DSC_Kmeans]).
#'
#' @return An object of class `DST_SlidingWindow`.
#' @author Michael Hahsler
#' @examples
#' library(stream)
#'
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#'
#' # define the stream clusterer.
#' cl <- DSC_SlidingWindow(
#'   formula = ~ . - `.class`,
#'   k = 3,
#'   window = 50,
#'   rebuild = 10
#'   )
#' cl
#'
#' # update the clusterer with 100 points from the stream
#' update(cl, stream, 100)
#'
#' # get the cluster model
#' cl$model$result
#'
#' plot(cl$model$result)
#' @export
DSC_SlidingWindow <- function(formula = NULL,
  model = DSC_Kmeans,
  window,
  rebuild,
  ...) {

  dst <- DST_SlidingWindow(
    f = function(data, model, ...) {
        dsc <- model(...)
        dsd <- DSD_Memory(data)
        update(dsc, dsd, n = -1)
        dsc
      },
    window = window,
    rebuild = rebuild,
    model = model,
    formula = formula,
    ...
  )

  dst$description <- paste0("Data Stream Clusterer on a Sliding Window",
    "\nFunction: ", deparse1(substitute(model)))
  class(dst) <- c("DSC_SlidingWindow", class(dst))
  dst

  }
