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


#' DSRegressor_SlidingWindow -- Data Stream Regressor Using a Sliding Window
#'
#' The Regressor keeps a sliding window for the stream and rebuilds a regression model at regular
#' intervals. By default is builds a decision tree using [lm()].
#'
#' This constructor creates a regressor based on [`DST_SlidingWindow`]. The regressor has
#' a `update()` and `predict()` method.
#'
#' @family DSRegressor
#'
#' @param formula a formula for the classification problem.
#' @param model regression model (that has a formula interface).
#' @param window size of the sliding window.
#' @param rebuild interval (number of points) for rebuilding the regression. Set rebuild to
#'   `Inf` to prevent automatic rebuilding. Rebuilding can be initiated manually when
#'   calling `update()`.
#' @param ... additional parameters are passed on to the regressor (default is `lm()`).
#'
#' @return An object of class `DST_SlidingWindow`.
#' @author Michael Hahsler
#' @examples
#' library(stream)
#'
#' # create a data stream for the iris dataset
#' data <- iris[sample(nrow(iris)), ]
#' stream <- DSD_Memory(data)
#'
#' # define the stream Regressor.
#' cl <- DSRegressor_SlidingWindow(
#'   Sepal.Length ~ Petal.Length + Petal.Length,
#'   window = 50,
#'   rebuild = 10
#'   )
#' cl
#'
#' # update the regressor with 100 points from the stream
#' update(cl, stream, 100)
#'
#' # predict the class for the next 50 points
#' newdata <- get_points(stream, n = 50)
#' pr <- predict(cl, newdata)
#' pr
#'
#' plot(pr, newdata$Sepal.Length)
#' abline(0, 1, col = "red")
#'
#' # get the tree model
#' cl$model$result
#' @export
DSRegressor_SlidingWindow <- function(formula,
  model = stats::lm,
  window,
  rebuild,
  ...) {
  dst <- DST_SlidingWindow(
    f = model,
    formula = formula,
    window = window,
    rebuild = rebuild,
    ...
  )

  dst$description <- paste0("Data Stream Regressor on a Sliding Window",
    "\nFunction: ", deparse1(substitute(model)))
  class(dst) <- c("DSRegressor_SlidingWindow", class(dst))
  dst

  }
