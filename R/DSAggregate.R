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


#' Data Stream Aggregator Base Classes
#'
#' Abstract base classes for all DSAggregate (Data Stream Aggregator) classes to aggregate streams.
#'
#' The `DSAggreagate` class cannot be instantiated, but it serve as a base
#' class from which other DSAggregate subclasses inherit.
#'
#' Data stream operators use `update.DSAggregate()` to process new data from the
#' [DSD] stream. The result of the operator can be obtained via [get_points()]
#' and [get_weights()] (if available).
#'
#' @family DST
#' @family DSAggregate
#'
#' @param x,object a concrete implementation of `DSAggregate`.
#' @param dsd a data stream object.
#' @param n the number of data points used for the update.
#' @param ... Further arguments.
#' @author Michael Hahsler
#' @examples
#' DSAggregate()
#' @export
DSAggregate <- abstract_class_generator("DSAggregate")

#' @rdname DSAggregate
#' @export
update.DSAggregate <- function(object, dsd, n = 1, ...) {
  stop("No implementation for update found!")
}

#' @rdname DSAggregate
#' @export
get_points.DSAggregate <- function(x, ...) {
  stop("Implementation is missing!")
}

#' @rdname DSAggregate
#' @export
get_weights.DSAggregate <- function(x, ...) {
  stop("Implementation is missing!")
}

#' @export
print.DSAggregate <- function(x, ...) {
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse=", "), "\n")
}
