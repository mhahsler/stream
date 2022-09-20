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
#' **Note:** `header = TRUE` is not supported for files. The header would be
#'  added for every call for update.
#'
#' @family DST
#'
#' @param file A file name or a R connection to be written to.
#' @param append Append the data to an existing file.
#' @param dsd a `DSD_WriteStream` object with an open connection.
#' @param ... further arguments are passed on to [write_stream()]. Note that `close` is
#'   always `FALSE` and cannot be specified.
#' @author Michael Hahsler
#' @examples
#' set.seed(1500)
#'
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' writer <- DST_WriteStream(file = "data.txt", info = TRUE)
#'
#' update(writer, stream, n = 2)
#' readLines("data.txt")

#' update(writer, stream, n = 3)
#' readLines("data.txt")
#'
#' # clean up
#' close_stream(writer)
#'
#' file.remove("data.txt")
#' @export
DST_WriteStream <- function(file, append = FALSE, ...) {
  if (is(file, "character")) {
    if (file.exists(file)) {
      if (!append)
        stop("file exists already. Please remove the file first.")
      file <- file(file, open = "a")
    } else {
      file <- file(file, open = "w")
    }
  }

  structure(
    list(
      description = paste(
        "Write stream to File/Connection\n",
        "Connection: ",
        summary(file)$description
      ),
      file = file,
      write_stream_params = list(...)
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
      append = TRUE,
      close = FALSE
    ),
    object$write_stream_params
  ))
}

#' @rdname DST_WriteStream
#' @export
close_stream.DST_WriteStream <- function(dsd, ...)
  close(dsd$file, ...)
