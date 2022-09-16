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


#' DSClassifier_SlidingWindow -- Data Stream Classifier Using a Sliding Window
#'
#' The classifier keeps a sliding window for the stream and rebuilds a classification model at regular
#' intervals. By default is builds a decision tree using [rpart()].
#'
#' This constructor creates classifier based on [`DST_SlidingWindow`]. The classifier has
#' a `update()` and `predict()` method.
#'
#' @family DSClassifier
#'
#' @param formula a formula for the classification problem.
#' @param model classifier model (that has a formula interface).
#' @param window size of the sliding window.
#' @param rebuild interval (number of points) for rebuilding the classifier. Set rebuild to
#'   `Inf` to prevent automatic rebuilding. Rebuilding can be initiated manually when
#'   calling `update()`.
#' @param ... additional parameters are passed on to the classifier (default is `rpart()`).
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
#' # define the stream classifier.
#' cl <- DSClassifier_SlidingWindow(
#'   Species ~ Sepal.Length + Sepal.Width + Petal.Length,
#'   window = 50,
#'   rebuild = 10
#'   )
#' cl
#'
#' # update the classifier with 100 points from the stream
#' update(cl, stream, 100)
#'
#' # predict the class for the next 50 points
#' newdata <- get_points(stream, n = 50)
#' pr <- predict(cl, newdata, type = "class")
#' pr
#'
#' table(pr, newdata$Species)
#'
#' # get the tree model
#' cl$model$result
#' @importFrom rpart rpart
#' @export
DSClassifier_SlidingWindow <- function(formula,
  model = rpart::rpart,
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

  dst$description <- paste0("Data Stream Classifier on a Sliding Window",
    "\nFunction: ", deparse1(substitute(model)))
  class(dst) <- c("DSClassifier_SlidingWindow", class(dst))
  dst

  }
