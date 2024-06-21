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

#' Conceptual Base Class for All Data Stream Mining Tasks
#'
#' Conceptual base class for all data stream mining tasks.
#'
#' Base class for data stream mining tasks. Types of `DST` are
#'   - [DSAggregate] to aggregate data streams (e.g., with a sliding window).
#'   - [DSC] for data stream clustering.
#'   - [DSClassifier] classification for data streams.
#'   - [DSRegressor] regression for data streams.
#'   - [DSOutlier] outlier detection for data streams.
#'   - [DSFP] frequent pattern mining for data streams.
#'
#' The common interface for all [DST] classes consists of
#'
#'   - [update()] update the DST with data points.
#'   - description() a string describing the DST.
#'   - get_model() returns the DST's current model (often as a data.frame or a R model object).
#'   - [predict()] use the learned DST model to make predictions.
#'
#' and the methods in the Methods Section below.
#'
#' @family DST
#'
#' @param x an object of a concrete implementation of a DST.
#' @param ... Further arguments.
#' @author Michael Hahsler
#' @examples
#' DST()
#' @export DST
DST <- function(...) {
  message(
    "DST is an abstract class and cannot be instantiated!\n",
    "Available subclasses are:\n\t",
    paste(setdiff(
      grep("^DS[^_]*$", ls("package:stream"), value = TRUE), "DST"
    ),
      collapse = ",\n\t")
  )
  invisible(NULL)
}

#' @export
print.DST <- function(x, ...) {
  cat(.line_break(paste(x$description)), "\n")
  cat("Class:", toString(class(x)), "\n")
}

#' @rdname DST
#' @export
description <- function(x, ...)
  UseMethod("description")

.desc <- function(x) {
  if (is.list(x) &&
      !is.null(x$description) && is.character(x$description))
    x$description
  else
    stop("This object does not have a description field!")
}

#' @export
description.DST <- function(x, ...)
  .desc(x)

#' @export
description.DSC <- function(x, ...)
  .desc(x)

#' @export
description.DSD <- function(x, ...)
  .desc(x)

#' @rdname DST
#' @export
get_model <- function(x, ...)
  UseMethod("get_model")

#' @export
get_model.DST <- function(x, ...)
  stop("get_model() not implemented for class ", toString(class(x)))

