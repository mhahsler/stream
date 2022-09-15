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

#' Task to Write a Stream to a File or a Connection
#'
#' Writes points from a data stream DSD object to a file or a connection.
#'
#' @family DST
#'
#' @param file A file name or a R connection to be written to.
#' @param append Append the data to an existing file.
#' @param ... further arguments are passed on to [write_stream()]. Note that `close` is
#'   always `FALSE` and cannot be specified.
#' @author Michael Hahsler
#' @examples
#' set.seed(1500)
#'
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' writer <- DST_WriteStream(file = "data.txt", info = TRUE, header = TRUE)
#'
#' update(writer, stream, n = 2)
#' readLines("data.txt")

#' update(writer, stream, n = 3)
#' readLines("data.txt")
#'
#' # clean up
#' file.remove("data.txt")
#' @export
DST_WriteStream <- function(file, append = TRUE, ...) {
  write_stream_params <- list(...)
  structure(
    list(
      description = "Write stream to File/Connection",
      file = file,
      append = append,
      write_stream_params = write_stream_params
    ),
    class = c("DST_WriteStream", "DST")
  )
}

#' @export
update.DST_WriteStream <- function(object, dsd, n = 1L, ...) {
  do.call(write_stream , c(
    list(
      dsd = dsd,
      file = object$file,
      n = n,
      append = object$append,
      close = FALSE
    ),
    object$write_stream_params
  ))
}
