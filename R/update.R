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

#' Update a Data Stream Mining Task Model with Points from a Stream
#'
#' `update()` for data stream mining tasks [DST].
#'
#' @name update
#'
#' @family DST
#'
#' @param object The [DST] object.
#' @param dsd A [DSD] object with the data stream.
#' @param n number of points to use for the update.
#' @param ... Additional arguments are passed on.
#' @return `NULL` or a data.frame `n` rows containing update information for each data point.
#' @author Michael Hahsler
#' @examples
#' set.seed(1500)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = .1)
#'
#' dbstream <- DSC_DBSTREAM(r = .1)
#' info <- update(dbstream, stream, n = 100)
#' plot(dbstream, stream, type = "both")
#'
#' # DBSTREAM returns cluster assignments (see DSC_DBSTREAM).
#' head(info)
#' @export
update.DST <- function(object, dsd, n = 1, ...) {
  stop("No implementation for update found for class", , paste0(class(object), collapse = ", "))
}
