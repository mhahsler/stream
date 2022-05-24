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
#' @param class Save the class/cluster labels of the points as the last column.
#' @param sep The character that will separate attributes in a data point.
#' @param append Append the data to an existing file.
#' @param header A flag that determines if column names will be output
#' (equivalent to \code{col.names} in \code{write.table()}).
#' @param row.names A flag that determines if row names will be output.
#' @param write_outliers A flag that determines if outliers will be output.
#' @param close close stream after writing.
#' @param ... Additional parameters that are passed to \code{write.table()}.
#' @return There is no value returned from this operation.
#' @author Michael Hahsler, Dalibor Krleža
#' @seealso \code{\link{write.table}}
#' @examples
#'
#' # creating data and writing it to disk
#' stream <- DSD_Gaussians(k=3, d=5, outliers=1,
#'   outlier_options=list(outlier_horizon=10))
#' write_stream(stream, file="data.txt", n=10, class=TRUE, write_outliers=TRUE)
#'
#' #file.show("data.txt")
#'
#' # clean up
#' file.remove("data.txt")
#'
#' @export
write_stream <- function(dsd,
  file,
  n = 100,
  block = 100000L,
  class = FALSE,
  append = FALSE,
  sep = ",",
  header = FALSE,
  row.names = FALSE,
  write_outliers = FALSE,
  ...)
  UseMethod("write_stream")

write_stream.default <- function(dsd,
  file,
  n = 100,
  block = 100000L,
  class = FALSE,
  append = FALSE,
  sep = ",",
  header = FALSE,
  row.names = FALSE,
  write_outliers = FALSE,
  ...) {
  stop(gettextf("write_stream not implemented for class '%s'.", class(dsd)))
}

#' @rdname write_stream
#' @export
write_stream.DSD <- function(dsd,
  file,
  n = 100,
  block = 100000L,
  class = FALSE,
  append = FALSE,
  sep = ",",
  header = FALSE,
  row.names = FALSE,
  write_outliers = FALSE,
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
    if (write_outliers) {
      p <- get_points(dsd, bl, class = class, outlier = TRUE)
      p <- cbind(p, outlier = attr(p, "outlier"))
    } else
      p <- get_points(dsd, bl, class = class)

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
