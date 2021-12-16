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
#' @param ... Further arguments.
#' @author Michael Hahsler
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
