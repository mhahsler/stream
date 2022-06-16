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


#' Data Stream Data Generator Base Classes
#'
#' Abstract base classes for DSD (Data Stream Data Generator).
#'
#' The `DSD` class cannot be instantiated, but it serves as a abstract
#' base class from which all DSD objects inherit. Implementations can be found in the
#' See Also section below.
#'
#' `DSD` provides common functionality like:
#'
#' * [get_points()]
#' * `print()`
#' * [plot()]
#' * [reset_stream()] (if available)
#'
#' `DSD_R` inherits form `DSD` and is the abstract parent class for
#' DSD implemented in R. To create a new R-based implementation there are only
#' two function that needs to be implemented for a new `DSD` subclass
#' called `Foo` would be:
#'
#' 1. A creator function `DSD_Foo(...)` and
#' 2. a method `get_points.DSD_Foo(x, n = 1L)` for that class.
#'
#' For details see `vignette()`
#'
#' @family DSD
#'
#' @param ... further arguments.
#' @author Michael Hahsler
#' @examples
#' DSD()
#'
#' # create data stream with three clusters in 3-dimensional space
#' stream <- DSD_Gaussians(k = 3, d = 3)
#'
#' # get points from stream
#' get_points(stream, n = 5)
#'
#' # plotting the data (scatter plot matrix, first and third dimension, and first
#' #  two principal components)
#' plot(stream)
#' plot(stream, dim = c(1, 3))
#' plot(stream, method = "pca")
#' @export DSD
DSD <- abstract_class_generator("DSD")

#' @rdname DSD
#' @export
DSD_R <- abstract_class_generator("DSD")

#' Get Points from a Data Stream Generator
#'
#' Gets points from a [DSD] object.
#'
#' Each DSD object has a unique way for creating/returning data points, but they all are
#' called through the generic function, `get_points()`. This is done by
#' using the S3 class system. See the man page for the specific [DSD] class on
#' the semantics for each implementation of `get_points()`.
#'
#' **Additional Point Information**
#'
#' Additional point information (e.g., known cluster/class assignment, noise status) can be requested
#' with `info = TRUE`. This information is returned as additional columns. The column names start with
#' `.` and are ignored by [DST] implementations. `remove_info()` is a convenience function to remove the
#' information columns.
#' Examples are
#' * `.id` for point IDs
#' * `.class` for known cluster/class labels used for plotting and evaluation
#' * `.time` a time stamp for the point (can be in seconds or an index for ordering)
#'
#' **Resetting a Stream##
#'
#' Many streams can be reset using [reset_stream()].
#'
#' @family DSD
#'
#' @param x A [DSD] object.
#' @param n integer; request up to $n$ points from the stream.
#' @param outofpoints Action taken if less than `n` data points are
#'   available. The default is to stop with an error.  Other actions that
#'   might be supported by different `DSD` are:
#'    - `warn`: return the available points (maybe an empty data.frame) with a warning.
#'    - `ignore`: silently return the available points.
#'    - `block`: wait till enough data points are available.
#' @param info return additional columns with information about the data point (e.g., a known cluster assignment).
#' @param ... Additional parameters to pass to the `get_points()` implementations.
#' @return Returns a [data.frame] with (up to) `n` rows and as many columns as `x` produces.
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians()
#' points <- get_points(stream, n = 5)
#' points
#'
#' remove_info(points)
#' @export
get_points <-
  function(x, ...)
    UseMethod("get_points")

#' @rdname get_points
#' @method get_points DSD
#' @export
get_points.DSD <-
  function(x,
    n = 1L,
    outofpoints = c("stop", "warn", "ignore"),
    info = TRUE,
    ...)
    UseMethod("get_points")

#' @rdname get_points
#' @param points a data.frame with points.
#' @export
remove_info <- function(points) {
  info_cols <- grep('^\\.', colnames(points))
  if (length(info_cols) > 0L)
    points <- points[, -info_cols, drop = FALSE]

  points
}

split_info <- function(points) {
  info_cols <- grep('^\\.', colnames(points))
  list(points = points[, -info_cols, drop = FALSE],
    info = points[, info_cols, drop = FALSE])
}


#' Reset a Data Stream to its Beginning
#'
#' Resets the position in a [DSD] object to the beginning or, if available, any other position in
#' the stream.
#'
#' Resets the counter of the stream object. For example, for [DSD_Memory],
#' the counter stored in the environment variable is moved back to 1. For
#' [DSD_ReadCSV] objects, this is done by calling [seek()] on the
#' underlying connection.
#'
#' `reset_stream()` is implemented for:
#'
#' `r func <- 'reset_stream'; classes <- gsub('.*\\.', '', as.character(methods(func))); paste(paste0('* [', classes, ']'), collapse = "\n")`
#'
#' @family DSD
#'
#' @param dsd An object of class a subclass of [DSD] which implements a
#' reset function.
#' @param pos Position in the stream (the beginning of the stream is position
#' 1).
#' @author Michael Hahsler
#' @examples
#' # initializing the objects
#' stream <- DSD_Gaussians()
#' replayer <- DSD_Memory(stream, 100)
#' replayer
#'
#' p <- get_points(replayer, 50)
#' replayer
#'
#' # reset replayer to the beginning of the stream
#' reset_stream(replayer)
#' replayer
#'
#' # set replayer to position 21
#' reset_stream(replayer, pos = 21)
#' replayer
#' @export
reset_stream <- function(dsd, pos = 1)
  UseMethod("reset_stream")

#' @export
reset_stream.DSD <- function(dsd, pos = 1) {
  stop(gettextf(
    "reset_stream not implemented for class '%s'.",
    paste(class(dsd), collapse = ", ")
  ))
}


### end of interface
#############################################################
### helper

#' @export
print.DSD <- function(x, ...) {
  .nodots(...)

  k <- x[["k"]]
  if (is.null(k))
    k <- NA
  d <- x[["d"]]
  if (is.null(d))
    d <- NA

  cat(.line_break(x$description))
  cat("Class:", paste(class(x), collapse = ", "), "\n")
}

#' @export
summary.DSD <- function(object, ...)
  print(object)
