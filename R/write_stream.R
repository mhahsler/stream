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

#' Write a Data Stream to a File
#'
#' Writes points from a data stream DSD object to a file or a connection.
#'
#' @param dsd The DSD object that will generate the data points for output.
#' @param file A file name or a R connection to be written to.
#' @param n The number of data points to be written. For finite streams, `n = -1` writes all available data points.
#' @param block Write stream in blocks to improve file I/O speed.
#' @param info Save the class/cluster labels and other information columns with the data.
#' @param sep The character that will separate attributes in a data point.
#' @param append Append the data to an existing file. If `FALSE`, then the file will be overwritten.
#' @param header A flag that determines if column names will be output
#' (equivalent to `col.names` in [write.table()]).
#' @param row.names A flag that determines if row names will be output.
#' @param close close stream after writing.
#' @param ... Additional parameters that are passed to [write.table()].
#' @return There is no value returned from this operation.
#' @author Michael Hahsler
#' @seealso [write.table]
#' @examples
#' # create data and write 10 points to disk
#' stream <- DSD_Gaussians(k = 3, d = 5)
#' stream
#'
#' write_stream(stream, file="data.txt", n = 10, header = TRUE, info = TRUE)
#'
#' readLines("data.txt")
#'
#' # clean up
#' file.remove("data.txt")
#'
#' # create a finite stream and write all data to disk using n = -1
#' stream2 <- DSD_Memory(stream, n = 5)
#' stream2
#'
#' write_stream(stream2, file="data.txt", n = -1, header = TRUE, info = TRUE)
#'
#' readLines("data.txt")
#' @export
write_stream <- function(dsd,
  file,
  n,
  block = 100000L,
  info = FALSE,
  append = FALSE,
  sep = ",",
  header = FALSE,
  row.names = FALSE,
  close = TRUE,
  ...)
  UseMethod("write_stream")

#' @export
write_stream.DSD <- function(dsd,
  file,
  n = 100,
  block = 100000L,
  info = FALSE,
  append = FALSE,
  sep = ",",
  header = FALSE,
  row.names = FALSE,
  close = TRUE,
  flush = TRUE,
  ...) {
  # make sure files are not overwritten, and no header after first write
  if (is(file, "character")) {
    if (file.exists(file)) {
      if (!append)
        stop("file exists already. Please remove the file first.")
      file <- file(file, open = "a")
      header <- FALSE
    } else {
      file <- file(file, open = "w")
    }
  }

  if (!is(file, "connection"))
    stop("Please pass a valid connection!")

  # needs opening
  if (!isOpen(file))
    open(file)

  # all following calls have to have col.names=FALSE regardless
  for (bl in .make_block(n, block)) {
    p <- get_points(dsd, bl, info = info)

    ## suppress warning for append and col.names
    suppressWarnings(
      write.table(
        p,
        file,
        sep = sep,
        append = TRUE,
        col.names = header,
        row.names = row.names,
        ...
      )
    )
  }

  if (flush)
    flush(file)

  if (close)
    close(file)
}
