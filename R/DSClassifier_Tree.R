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


#' DSClassifier_Tree -- Tree-based Classifier Using a Sliding Window
#'
#' The classifier keeps a sliding window for the stream and rebuilds rpart models at regular
#' intervals.
#'
#' @family DSClassifier
#'
#' @param formula a formula for the classification problem.
#' @param window size of the sliding window.
#' @param rebuild interval (number of points) for rebuilding the classifier. Set rebuild to
#'   `Inf` to prevent automatic rebuilding. Rebuilding can be initiated manually when
#'   calling `update()`.
#' @param ... additional parameters are passed on to `rpart()`.
#'
#' @return An object of class `DSClassifier_Tree`
#' @author Michael Hahsler
#' @examples
#' library(stream)
#'
#' # create a data stream for the iris dataset
#' data <- iris[sample(nrow(iris)), ]
#' stream <- DSD_Memory(data)
#' stream
#'
#' # define the stream classifier.
#' cl <- DSClassifier_Tree(
#'   Species ~ Sepal.Length + Sepal.Width + Petal.Length,
#'   window = 50,
#'   rebuild = 10
#'   )
#'
#' cl
#'
#' # update the classifier with 100 points from the stream
#' update(cl, stream, 100)
#'
#' # predict the class for the next 50 points
#' newdata <- get_points(stream, n = 50)
#' pr <- predict(cl, newdata)
#' pr
#'
#' table(pr, newdata$Species)
#'
#' # get the tree model
#' cl$model$tree
#' @export
DSClassifier_Tree <- function(formula,
  window, rebuild, ...)
  structure(
    list(
      description = paste0("Tree Classifier (rpart)",
        "\nFormula: ",
        deparse(formula)),
      formula = formula,
      window = DSAggregate_Window(horizon = window),
      rebuild = rebuild,
      model = list2env(list(tree = NULL, count = 0)),
      rpart.args = list(...)
    ),
    class = c("DSClassifier_Tree", "DSClassifier", "DST")
  )

#' @rdname DSClassifier_Tree
#' @param dsd A [DSD] object with the data stream.
#' @param n number of points from `dsd` to use for the update.
#' @param rebuild logical; perform a rebuild after the update.
#' @param ... further arguments are currently unused.
#' @export
update.DSClassifier_Tree <- function(object,
  dsd,
  n = 1L,
  rebuild = FALSE,
  ...) {
  update(object$window, dsd, n = n)

  object$model$count <- object$model$count + n
  if (rebuild || object$model$count > object$rebuild ||
      is.null(object$model$tree) &&
      object$model$count >= object$window$horizon) {
    object$model$tree = do.call(rpart::rpart, args = c(
      list(
        formula = object$formula,
        data = get_points(object$window)
      ),
      object$rpart.args
    ))
    object$model$count <- 0
  }
}

#' @rdname DSClassifier_Tree
#' @param object the updated `DSClassifier` object.
#' @param newdata dataframe with the new data.
#' @param type prediction type (see `predict.rpart()`).
#' @export
predict.DSClassifier_Tree <-
  function(object,
    newdata, type = "vector", ...) {
    if (is.null(object$model$tree))
      stop("Classifier has not been trained! Not enought points!")

    predict(object$model$tree,
      newdata = newdata,
      type = type)
  }
