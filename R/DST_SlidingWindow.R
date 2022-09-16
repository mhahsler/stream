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


#' DST_SlidingWindow -- Call R Functions on a Sliding Window
#'
#' Keeps a sliding window of the data stream an calls an R function at regular
#' intervals with the contents of the window.
#'
#' The function needs to have the from
#'
#' `f <- function(data, ...) {...}`
#'
#' where `data` is the data.frame
#' with the points in the sliding window (See `get_points()` in [DSAggregate_Window]). The
#' function will be executed at regular intervals and the result can be retrieved.
#'
#' Many modelling functions provide a formula interface which lets them be directly used
#' inside a `DST_SlidingWindow` (see Examples section).
#'
#' If the function returns a model that supports `predict()`, then predict can directly be
#' called on the `DST_SlidingWindow` object.
#'
#' @family DST
#'
#' @param f the function to be called.
#' @param window size of the sliding window.
#' @param rebuild interval (number of points) for calling the R function. Set rebuild to
#'   `Inf` to prevent automatic calling. Calling the function can be initiated manually when
#'   calling `update()`.
#' @param ... further arguments to `DST_SlidingWindow` will be passed on when calling `f()`.
#'
#' @return An object of class `DST_SlidingWindow`.
#' @author Michael Hahsler
#' @examples
#' library(stream)
#'
#' # create a data stream for the iris dataset
#' data <- iris[sample(nrow(iris)), ]
#' stream <- DSD_Memory(data)
#' stream
#'
#' ## Example 1: Use a function on the sliding window
#' summarizer <- function(data) summary(data)
#'
#' s <- DST_SlidingWindow(summarizer,
#'   window = 100, rebuild = 50)
#' s
#'
#' # update window with 49 points. The function is not yet called
#' update(s, stream, 49)
#' s$model$result

#' # updating with the 50th point will trigger a function call (see rebuild parameter)
#' # note that the window is only 1/2 full and we have 50 NAs
#' update(s, stream, 1)
#' s$model$result
#'
#' # 50 more points and the function will be recomputed
#' update(s, stream, 50)
#' s$model$result
#'
#'
#' ## Example 2: Use classifier on the sliding window
#' reset_stream(stream)
#'
#' # rpart, like most models in R, already have a formula interface that uses a
#' # data parameter. We can use these types of models directly
#' library(rpart)
#' cl <- DST_SlidingWindow(
#'   rpart, formula = Species ~ Petal.Length + Petal.Width,
#'   window = 100, rebuild = 50)
#' cl
#'
#' # update window with 50 points so the model is built
#' update(cl, stream, 50)
#' cl$model$result
#'
#' # 50 more points and the function will be recomputed
#' update(cl, stream, 50)
#' cl$model$result
#'
#' # rpart supports predict, so we can use it directly with the DST_SlidingWindow
#' new_points <- get_points(stream, n = 5)
#' predict(cl, new_points, type = "class")
#'
#' ## Example 3: Regression using a sliding window
#' reset_stream(stream)
#'
#' ## lm can be directly used
#' reg <- DST_SlidingWindow(
#'   lm, formula = Sepal.Length ~ Petal.Width + Petal.Length,
#'   window = 100, rebuild = 50)
#' reg
#'
#' update(reg, stream, 100)
#' reg$model$result
#'
#' # lm supports predict, so we can use it directly with the DST_SlidingWindow
#' new_points <- get_points(stream, n = 5)
#' predict(reg, new_points)
#' @export
DST_SlidingWindow <- function(f,
  window, rebuild, ...)
  structure(
    list(
      description = paste0("Data Stream Task: Call function on a Sliding Window",
        "\nFunction: ", deparse1(substitute(f))),
      f = f,
      window = DSAggregate_Window(horizon = window),
      rebuild = rebuild,
      model = list2env(list(result = NULL, count = 0)),
      f.args = list(...)
    ),
    class = c("DST_SlidingWindow", "DST")
  )

#' @rdname DST_SlidingWindow
#' @param object A [DST_SlidingWindow] object with the data stream.
#' @param dsd A [DSD] object with the data stream.
#' @param n number of points from `dsd` to use for the update.
#' @param rebuild logical; perform a rebuild after the update.
#' @export
update.DST_SlidingWindow <- function(object,
  dsd,
  n = 1L,
  rebuild = FALSE,
  ...) {
  update(object$window, dsd, n = n)

  object$model$count <- object$model$count + n
  if (rebuild || object$model$count >= object$rebuild ||
      is.null(object$model$result) &&
      object$model$count >= object$window$RObj$horizon) {
    object$model$result = do.call(object$f,
      c(list(data = get_points(object$window)), object$f.args))

    object$model$count <- 0
  }
}

#' @rdname DST_SlidingWindow
#' @param object the updated `DST_SlidingWindow` object.
#' @param newdata dataframe with the new data.
#' @param ... additional parameters passed on to the `predict()` function of the underlying model.
#' @export
predict.DST_SlidingWindow <-
  function(object,
    newdata, ...) {
    if (is.null(object$model$result))
      stop("Model has not been trained! Update with more points!")

    predict(object$model$result,
      newdata = newdata, ...)
  }
