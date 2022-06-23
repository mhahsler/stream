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


#' Read a Data Stream from a File or a Connection
#'
#' A DSD class that reads a data stream (text format) from a file or any R connection.
#'
#' `DSD_ReadStream` uses [readLines()] and [read.table()] to read data from an R
#' connection line-by-line and convert it into a data.frame.
#' The connection is responsible for maintaining where the stream
#' is currently being read from. In general, the connections will consist of
#' files stored on disk but have many other possibilities (see
#' [connection]).
#'
#' The implementation tries to gracefully deal with slightly corrupted data by
#' dropping points with inconsistent reading and producing a warning. However,
#' this might not always be possible resulting in an error instead.
#'
#' **Column names**
#'
#' If the file has column headers in the first line, then they can be used by setting `header = TRUE`.
#' Alternatively, column names can be set using `col.names` or a named vector for `take`. If no column
#' names are specified then default names will be created.
#'
#' Columns with names that start with `.` are considered information columns and are ignored by `DST`s.
#' See [get_points()] for details.
#'
#' Other information columns are are used by various functions.
#'
#' **Reading the whole stream**
#' By using `n = -1` in `get_points()`, the whole stream is returned.
#'
#' **Resetting and closing a stream**
#'
#' The position in the file can be reset to the beginning or another position using
#' [reset_stream()]. This fails of the underlying connection is not seekable (see [connection]).
#'
#' `DSD_ReadStream` maintains an open connection to the stream and needs to be closed
#' using [close_stream()].
#'
#' `DSD_ReadCSV` reads a stream from a comma-separated values file.
#' @family DSD
#'
#' @param file A file/URL or an open connection.
#' @param k Number of true clusters, if known.
#' @param take indices of columns to extract from the file.
#' @param sep The character string that separates dimensions in data points in
#' the stream.
#' @param header Does the first line contain variable names?
#' @param skip the number of lines of the data file to skip before beginning to
#' read data.
#' @param col.names A vector of optional names for the variables. The default is to use `"V"` followed by the
#'   column number. Additional information (e.g., class labels) need to have names starting with `.`.
#' @param colClasses A vector of classes to be assumed for the columns passed
#' on to [read.table()].
#' @param ... Further arguments are passed on to [read.table()].  This can
#' for example be used for encoding, quotes, etc.
#' @param dsd A object of class `DSD_ReadCSV`.
#' @return An object of class `DSD_ReadCSV` (subclass of [DSD_R], [DSD]).
#' @author Michael Hahsler
#' @seealso [readLines()], [read.table()].
#' @examples
#' # Example 1: creating data and writing it to disk
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' write_stream(stream, "data.txt", n = 100, info = TRUE, header = TRUE)
#' readLines("data.txt", n = 5)
#'
#' # reading the same data back
#' stream2 <- DSD_ReadStream("data.txt", header = TRUE)
#' stream2
#'
#' # get points
#' get_points(stream2, n = 5)
#' plot(stream2, n = 20)
#'
#' # clean up
#' close_stream(stream2)
#' file.remove("data.txt")
#'
#' # Example 2:  Read part of the kddcup1999 data (take only cont. variables)
#' # col 42 is the class variable
#' file <- system.file("examples", "kddcup10000.data.gz", package = "stream")
#' stream <- DSD_ReadCSV(gzfile(file),
#'         take = c(1, 5, 6, 8:11, 13:20, 23:41, .class = 42), k = 7)
#' stream
#'
#' get_points(stream, 5)
#'
#' # plot 100 points (projected on the first two principal components)
#' plot(stream, n = 100, method = "pca")
#'
#' close_stream(stream)
#' @export
DSD_ReadStream <- function(file,
  k = NA,
  take = NULL,
  sep = ",",
  header = FALSE,
  skip = 0,
  col.names = NULL,
  colClasses = NA,
  ...) {
  header <- as.logical(header)
  skip <- as.integer(skip)

  # error if no string or connection is passed
  if (is(file, "character"))
    file <- file(file)
  if (!is(file, "connection"))
    stop("Please pass a valid connection!")

  # open the connection if its closed. We will keep it open
  # (it holds the position in the stream) till close_stream() is called.
  if (!isOpen(file))
    open(file, "r")

  # read first point to figure out structure!
  if (skip > 0L)
    readLines(file, n = skip)
  point <- read.table(
    text = readLines(con = file, n = 1L + header),
    sep = sep,
    header = header,
    colClasses = colClasses,
    ...
  )

  # reset stream if possible (otherwise first point is lost)
  if (isSeekable(file)) {
    seek(file, where = 0L, rw = "r")
    if (skip + header > 0L)
      readLines(file, n = skip + header)
  } else
    warning("Stream is not seekable. First data point is lost.")

  # select columns to take
  if (!is.null(take)) {
    if (is.character(take))
      take <- pmatch(take, colnames(point))
    if (any(is.na(take)))
      stop("Invalid column name specified in take!")
    point <- point[, take]
  }

  # deal with header and update colnames in point
  has_name <- names(take) != ""
  if (length(has_name) > 0L)
    colnames(point)[has_name] <- names(take)[has_name]

  if (!is.null(col.names))
    colnames(point) <- col.names

  # dimensions
  d <- ncol(remove_info(point))

  # fix data types for reading: integer -> numeric, factor -> character
  colClasses <- sapply(point[1L,], class)
  colClasses[colClasses == "integer"] <- "numeric"
  colClasses[colClasses == "factor"] <- "character"

  l <- list(
    description = paste0(
      'File Data Stream: ',
      basename(summary(file)$description),
      ' (d = ',
      d,
      ', k = ',
      k,
      ')'
    ),
    d = d,
    k = k,
    file = file,
    sep = sep,
    take = take,
    header = header,
    colClasses = colClasses,
    col.names = colnames(point),
    read.table.args = list(...),
    skip = skip
  )
  class(l) <- c("DSD_ReadStream", "DSD_R", "DSD")

  l
}

#' @export
get_points.DSD_ReadStream <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  info = TRUE,
  ...) {
  .nodots(...)

  .DEBUG <- TRUE
  #.DEBUG <- FALSE

  outofpoints <- match.arg(outofpoints)

  # get all points
  if(is.infinite(n) || n < 1) {
    outofpoints <- "ignore"
    n <- -1L
  }

  n <- as.integer(n)

  lines <- character(0)
  try(lines <- readLines(con = x$file, n = n), silent = !.DEBUG)

  if (length(lines) < n) {
    if (outofpoints == "stop")
        stop(
          "Not enough data points left in the stream! Connection not seekable, ",
          length(lines),
          " data points are lost."
        )

    if (outofpoints == "warn")
      warning(
        "Not enough data points left in stream, returning the remaining ",
        length(lines),
        " points!"
      )
  }

  d <- NULL
  suppressWarnings(try(d <- do.call(read.table,
    c(
      list(
        text = lines,
        sep = x$sep,
        nrows = n,
        colClasses = x$colClasses
      ),
      x$read.table.args
    )),
    silent = !.DEBUG)
  )

  if (is.null(d)) {
    ## no data: create conforming data.frame with 0 rows
    d <- data.frame()
    for (i in seq_along(x$colClasses))
      d[[i]] <- do.call(x$colClasses[i], list(0))
  } else {
    ## take columns
    if (!is.null(x$take))
      d <- d[, x$take, drop = FALSE]
  }

  ## remove additional columns from a bad line
  if (ncol(d) > length(x$col.names))
    d <- d[, seq_along(x$col.names), drop = FALSE]

  colnames(d) <- x$col.names

  if (!info)
    d <- remove_info(d)

  d
}

#' @export
reset_stream.DSD_ReadStream <- function(dsd, pos = 1) {
  pos <- as.integer(pos)

  if (!isSeekable(dsd$file))
    stop("Underlying conneciton does not support seek!")

  # go to the the first line of the data
  seek(dsd$file, where = 0L, rw = "r")
  if (dsd$skip + dsd$header > 0L)
    readLines(dsd$file, n = dsd$skip + dsd$header)

  # skip to the pos
  if (pos > 1L)
    readLines(dsd$file, n = pos - 1L)

  invisible(NULL)
}

#' @rdname DSD_ReadStream
#' @export
DSD_ReadCSV <- DSD_ReadStream

#' @rdname DSD_ReadStream
#' @export
close_stream.DSD_ReadStream <- function(dsd)
  close(dsd$file)

