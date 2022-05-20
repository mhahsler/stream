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
#' Conceptual base class for all data stream mining tasks. Current tasks are data
#' stream clustering [DSC], outlier detection [DSOutlier], classification on data streams
#' [DSClassify] and frequent pattern mining on data streams [DSFP].
#'
#' The basic interface for all `DST` classes can be found in the Functions Section.
#'
#'
#' @aliases update
#'
#' @param x,object an object of a concrete implementation of a DST.
#' @param dsd a data stream.
#' @param n the number of data points used for the update.
#' @param ... Further arguments.
#' @author Michael Hahsler
#' @seealso [DSC], [DSOutlier], [DSClassify], [DSFP], [DSO]
#' @examples
#' DST()
#' @export DST
DST <- function(...) {
  message("DST is an abstract class and cannot be instantiated!\n",
    "Available subclasses are:\n\t",
    paste(setdiff(grep("^DS[^_]*$", ls("package:stream"), value = TRUE), "DST"),
      collapse=",\n\t"))
  invisible(NULL)
}

#' @describeIn DST update the data stream task with new data points coming from a data stream.
#' @export
update.DST <- function(object, dsd, n = 1, ...) {
  stop("No implementaiton for update found!")
  }

#' @rdname DST
#' @export
description <- function(x, ...)
  UseMethod("description")

description.default <- function(x, ...)
  stop("description() not implemented for this class!")


.desc <- function(x) {
  if (is.list(x) &&
      !is.null(x$description) && is.character(x$description))
    x$description
  else
    stop("This object does not have a description field!")
}

#' @describeIn DST Get a description of the task as a character string.
#' @export
description.DST <- description.default

#' @export
description.DSC <- function(x, ...)
  .desc(x)

#' @export
description.DSD <- function(x, ...)
  .desc(x)

#' @export
description.DSO <- function(x, ...)
  .desc(x)
