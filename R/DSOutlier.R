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

DSOutlier <-
  function(...)
    stop("DSOutlier is an abstract class and cannot be instantiated!")

check_outliers <- function(x, points, ...)
  UseMethod("check_outliers")
check_outliers.default <- function(x, points, ...) {
  stop(gettextf(
    "check_outlier not implemented for class '%s'.",
    paste(class(x), collapse = ", ")
  ))
}
check_outliers.DSOutlier <- check_outliers.default


recheck_outlier <- function(x, outlier_correlated_id, ...)
  UseMethod("recheck_outlier")
recheck_outlier.default <- function(x, outlier_correlated_id, ...) {
  stop(gettextf(
    "recheck_outlier not implemented for class '%s'.",
    paste(class(x), collapse = ", ")
  ))
}
recheck_outlier.DSOutlier <- recheck_outlier.default

get_outlier_positions <- function(x, ...)
  UseMethod("get_outlier_positions")
get_outlier_positions.default <- function(x, ...) {
  stop(gettextf(
    "check_outlier not implemented for class '%s'.",
    paste(class(x), collapse = ", ")
  ))
}
get_outlier_positions.DSOutlier <- get_outlier_positions.default

noutliers <- function(x, ...)
  UseMethod("noutliers")
noutliers.default <- function(x, ...) {
  stop(gettextf(
    "noutliers not implemented for class '%s'.",
    paste(class(x), collapse = ", ")
  ))
}
noutliers.DSOutlier <- function(x, ...) {
  nrow(get_outlier_positions(x))
}

print.DSOutlier <- function(x, ...) {
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse = ", "), "\n")
}
