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
#' @family DSD
#'
#' @param dsd The DSD object that will generate the data points for output.
#' @param file A file name or a R connection to be written to.
#' @param n The number of data points to be written.
#' @param block Write stream in blocks to improve file I/O speed.
#' @param info Save the class/cluster labels and other information columns with the data.
#' @param sep The character that will separate attributes in a data point.
#' @param append Append the data to an existing file.
#' @param header A flag that determines if column names will be output
#' (equivalent to `col.names` in [write.table()]).
#' @param row.names A flag that determines if row names will be output.
#' @param close close stream after writing.
#' @param ... Additional parameters that are passed to [write.table()].
#' @return There is no value returned from this operation.
#' @author Michael Hahsler
#' @seealso [write.table]
#' @examples
#' # creating data and writing it to disk
#' stream <- DSD_Gaussians(k = 3, d = 5)
#' write_stream(stream, file="data.txt", n = 10, header = TRUE, info = TRUE)
#'
#' readLines("data.txt")
#'
#' # clean up
#' file.remove("data.txt")
#' @export
write_stream <- function(dsd,
  file,
  n = 100,
  block = 100000L,
  info = FALSE,
  append = FALSE,
  sep = ",",
  header = FALSE,
  row.names = FALSE,
  close = TRUE,
  ...)
  UseMethod("write_stream")

write_stream.default <- function(dsd,
  file,
  n = 100,
  block = 100000L,
  info = FALSE,
  append = FALSE,
  sep = ",",
  header = FALSE,
  row.names = FALSE,
  close = TRUE,
  ...) {
  stop(gettextf("write_stream not implemented for class '%s'.", class(dsd)))
}

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
  ...) {
  # make sure files are not overwritten
  if (is(file, "character") && file.exists(file) && !append)
    stop("file exists already. Please remove the file first.")

  # string w/ file name (clears the file)
  if (is(file, "character")) {
    if (append)
      file <- file(file, open = "a")
    else
      file <- file(file, open = "w")
  }
  # error
  else if (!is(file, "connection"))
    stop("Please pass a valid connection!")

  # needs opening
  else if (!isOpen(file))
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

  if (close)
    close(file)
}
