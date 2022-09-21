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

#' Apply Multiple Task to the Same Data Stream
#'
#' Apply multiple task ([DST]) to the same data stream. The tasks can be accessed
#' as a list as `$dsts`.
#'
#' @param dsts a list of [DST] objects.
#' @author Michael Hahsler
#' @examples
#' set.seed(1500)
#'
#' stream <- DSD_Gaussians(k = 3, d = 2)
#'
#' ## define multiple tasks as a list
#' tasks <- DST_Multi(list(
#'    DSAggregate_Window(horizon = 10),
#'    DSC_DStream(gridsize = 0.1)
#' ))
#' tasks
#'
#' ## update both tasks with the same stream
#' update(tasks, stream, n = 1000)
#'
#' ## inspect the results of the tasks
#' tasks$dsts[[1]]
#' get_model(tasks$dsts[[1]])
#'
#' tasks$dsts[[2]]
#' plot(tasks$dsts[[2]])
#' @export
DST_Multi <- function(dsts) {
  # creating the DSD object
  structure(list(
    description = paste0("  - ", sapply(dsts, description), collapse = "\n"),
    dsts = dsts
  ),
    class = c("DST_Multi", "DST"))
}

#' @export
update.DST_Multi <- function(object, dsd, n = 1L, ...) {
  ps <- DSD_Memory(get_points(dsd, n = n, info = TRUE, ...))

  for (dst in object$dsts) {
    reset_stream(ps)
    update(dst, ps, n = n)
  }
}
