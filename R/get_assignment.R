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

#' Assignment Data Points to Clusters [deprecated]
#'
#' **Deprecation Notice:** use [stream::predict()] for a more general interface to apply
#' a data stream model to new data. `get_assignment()` is deprecated.
#'
#' Get the assignment of data points to clusters in a \code{DSC} using the
#' model's assignment rules or nearest neighbor assignment. The clustering is
#' not modified.
#'
#' Each data point is assigned either using the original model's assignment
#' rule or Euclidean nearest neighbor assignment. If the user specifies the
#' model's assignment strategy, but is not available, then nearest neighbor
#' assignment is used and a warning is produced.
#'
#' @family DSC
#'
#' @param dsc The [DSC] object with the clusters for assignment.
#' @param points The points to be assigned as a data.frame.
#' @param type Use micro- or macro-clusters in [DSC] for assignment.
#' @param method assignment method
#'   * `"model"` uses the assignment method of the underlying algorithm
#'     (unassigned points return `NA`). Not all algorithms implement this option.
#'   * `"nn"` performs nearest neighbor assignment using Euclidean distance.
#'   * `"auto"` uses the model assignment method. If this method is not
#'     implemented/available then method `"nn"` is used instead.
#' @param ... Additional arguments are passed on.
#' @return A vector containing the assignment of each point. `NA` means
#' that a data point was not assigned to a cluster.
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = .05)
#'
#' dbstream <- DSC_DBSTREAM(r = .1)
#' update(dbstream, stream, n = 100)
#'
#' # find the assignment for the next 100 points to
#' # micro-clusters in dsc. This uses the model's assignment function
#' points <- get_points(stream, n = 100)
#' a <- predict(dbstream, points)
#' head(a)
#'
#' # show the MC assignment areas. Assigned points as blue circles and
#' # the unassigned points as red dots
#' plot(dbstream, stream, assignment = TRUE, type = "none")
#' points(points[!is.na(a[, ".class"]),], col = "blue")
#' points(points[is.na(a[, ".class"]),], col = "red", pch = 20)
#'
#' # use nearest neighbor assignment instead
#' a <- predict(dbstream, points, method = "nn")
#' head(a)
#' @export
get_assignment <-
  function(dsc,
    points,
    type = c("auto", "micro", "macro"),
    method = "auto",
    ...)
    UseMethod("get_assignment")

### default method is Euclidean nearest neighbor "nn"

#' @rdname get_assignment
#' @export
get_assignment.DSC <-
  function(dsc,
    points,
    type = c("auto", "micro", "macro"),
    method = c("auto", "nn", "model"),
    ...) {
    method <- match.arg(method)
    if (method == "auto")
      method <- "nn"

    if (method == "model") {
      warning("method model not implemented! using Euclidean nearest neighbor instead!")
      method <- "nn"
    }

    points <- remove_info(points)
    c <- get_centers(dsc, type = type, ...)

    if (nrow(c) > 0L) {
      dist <- dist(points, c, method = "Euclidean")
      # Find the minimum distance and save the class
      predict <- apply(dist, 1L, which.min)

    } else {
      #warning("There are no clusters!")
      predict <- rep(NA_integer_, nrow(points))
    }

    attr(predict, "method") <- method

    predict
  }
