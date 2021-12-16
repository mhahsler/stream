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





#' Data Stream Operator Base Classes
#'
#' Abstract base classes for all DSO (Data Stream Operator) classes.
#'
#' The `DSO` class cannot be instantiated, but it serve as a base
#' class from which other DSO classes inherit.
#'
#' Data stream operators use [update] to process new data from the
#' [DSD] stream. The result of the operator can be obtained via [get_points]
#' and [get_weights] (if available).
#'
#' @param ... Further arguments.
#' @author Michael Hahsler
#' @seealso \code{\link{DSO_Window}},
#' \code{\link{DSO_Sample}}
#' @examples
#' DSO()
#' @export DSO
DSO <- abstract_class_generator("DSO")

print.DSO <- function(x, ...) {
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse=", "), "\n")
  #if(!is(nc <- try(nclusters(x, type="micro"), silent=TRUE), "try-error"))
  #  cat(paste('Number of micro-clusters:', nc, '\n'))
  #if(!is(nc <- try(nclusters(x, type="macro"), silent=TRUE), "try-error"))
  #  cat(paste('Number of macro-clusters:', nc, '\n'))
}
