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


#' Placeholder for a DSD Stream
#'
#' Placeholder for a [DSD]. `DSD_NULL` does not produce points and creates an error
#' for [get_points()].
#'
#' @family DSD
#'
#' @return Returns a `DSD_NULL` object (subclass of [DSD]).
#' @author Michael Hahsler
#' @examples
#' nullstream <- DSD_NULL()
#' nullstream
#'
#' ## This will produce an error
#' \dontrun{
#' get_points(nullstream)}
#' @export
DSD_NULL <- function()
structure(list(description = "NULL stream") , class = c("DSD_NULL", "DSD"))

#' @export
get_points.DSD_NULL <- function(x,
  n = 1,
  outofpoints = "stop",
  info = TRUE,
  ...)
  stop("DSD_NULL does not produce points.")
