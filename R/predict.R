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

#' Make a Prediction for a Data Stream Mining Task
#'
#' `predict()` for data stream mining tasks [DST].
#'
#' @name predict
#'
#' @family DST
#' @family DSC
#'
#' @param object The [DST] object.
#' @param newdata The points to make predictions for as a data.frame.
#' @param ... Additional arguments are passed on.
#' @return A data.frame with columns containing the predictions. The columns depend on the type of the
#'   data stream mining task.
#' @author Michael Hahsler
#' @examples
#' set.seed(1500)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = .1)
#'
#' dbstream <- DSC_DBSTREAM(r = .1)
#' update(dbstream, stream, n = 100)
#' plot(dbstream, stream, type = "both")
#'
#' # find the assignment for the next 100 points to
#' # micro-clusters in dsc. This uses the model's assignment function
#' points <- get_points(stream, n = 10)
#' points
#'
#' pr <- predict(dbstream, points, type = "macro")
#' pr
#'
#' # Note that the clusters are labeled in arbitrary order. Check the
#' # agreement.
#' agreement(pr[,".class"], points[,".class"])
#' @export
predict.DST <- function(object, newdata, ...)
  stop("predict() not implemented for ", paste0(class(object), collapse = ", "))

#' @rdname predict
#' @param type Use micro- or macro-clusters in [DSC] for assignment.
#' @param method assignment method
#'   * `"model"` uses the assignment method of the underlying algorithm
#'     (unassigned points return `NA`). Not all algorithms implement this option.
#'   * `"nn"` performs nearest neighbor assignment using Euclidean distance.
#'   * `"auto"` uses the model assignment method. If this method is not
#'     implemented/available then method `"nn"` is used instead.
#' @export
predict.DSC <-
  function(object,
    newdata,
    type = c("auto", "micro", "macro"),
    method = "auto",
    ...) {
    cl <-
      get_assignment(object, newdata, type = type, method = method, ...)

    if (!is.data.frame(cl))
      cl <- data.frame(.class = cl)

    cl
  }
