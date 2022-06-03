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

### FIXME: Should go to get_assignments
# for the \code{points} data frame. Points are assigned to an outlier only if
# they are inside \code{outlier_threshold} distance from its position. This
# implementation is heavily dependent on the Euclidean distance measure and
# should be overwritten by concrete outlier detection clusterer
# implementations. } }


#' Abstract Class for Outlier Detection Clusterers
#'
#' The abstract class for all outlier detection clusterers. Cannot be
#' instantiated. An implementation is available in package
#' \pkg{streamMOA}.
#'
#' @param x The DSC object.
#' @param outlier_correlated_id ids of outliers.
#' @param ... further arguments.
#' @docType class
#' @examples
#' DSOutlier()
#' @author Dalibor Krleža
#' @export
DSOutlier <- abstract_class_generator("DSOutlier")

#' @describeIn DSOutlier Clean Outliers from the Outlier Detecting Clusterer.
#' @export
clean_outliers <- function(x, ...)
  UseMethod("clean_outliers")

clean_outliers.default <- function(x, ...) {
  stop(gettextf(
    "clean_outlier not implemented for class '%s'.",
    paste(class(x), collapse = ", ")
  ))
}

#' @rdname DSOutlier
#' @export
clean_outliers.DSOutlier <- clean_outliers.default

#' @describeIn DSOutlier Re-checks the outlier having `outlier_correlated_id`.
#' If this object is still an outlier, the method
#' returns TRUE.
#' @export
recheck_outlier <- function(x, outlier_correlated_id, ...)
  UseMethod("recheck_outlier")

recheck_outlier.default <- function(x, outlier_correlated_id, ...) {
  stop(gettextf(
    "recheck_outlier not implemented for class '%s'.",
    paste(class(x), collapse = ", ")
  ))
}

#' @rdname DSOutlier
#' @export
recheck_outlier.DSOutlier <- recheck_outlier.default

#' @describeIn DSOutlier Returns spatial positions of all current outliers.
#' @export
get_outlier_positions <- function(x, ...)
  UseMethod("get_outlier_positions")

get_outlier_positions.default <- function(x, ...) {
  stop(gettextf(
    "check_outlier not implemented for class '%s'.",
    paste(class(x), collapse = ", ")
  ))
}

#' @rdname DSOutlier
#' @export
get_outlier_positions.DSOutlier <- get_outlier_positions.default

#' @export
get_outlier_positions.DSC_TwoStage <-
  function(x, ...)
    get_outlier_positions(x$micro_dsc, ...)

#' @describeIn DSOutlier Returns the current number
#' of outliers.
#' @export
noutliers <- function(x, ...)
  UseMethod("noutliers")

noutliers.default <- function(x, ...) {
  stop(gettextf(
    "noutliers not implemented for class '%s'.",
    paste(class(x), collapse = ", ")
  ))
}

#' @rdname DSOutlier
#' @export
noutliers.DSOutlier <- function(x, ...) {
  nrow(get_outlier_positions(x))
}

#' @export
print.DSOutlier <- function(x, ...) {
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse = ", "), "\n")
}

#' @export
plot.DSOutlier <- function(x,
  dsd = NULL,
  n = 500,
  col_points = NULL,
  col_clusters = c("red", "blue", "green"),
  weights = TRUE,
  scale = c(1, 5),
  cex = 1,
  pch = NULL,
  method = c("pairs", "scatter", "pca"),
  dim = NULL,
  type = c("auto", "micro", "macro", "both"),
  # we keep 'both' for compatibility reasons
  assignment = FALSE,
  outliers = TRUE,
  ...) {
  NextMethod()

  if (outliers)
    points(get_outlier_positions(x), pch = .outlier_pch, col = .outlier_col)
}
