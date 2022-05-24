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


#' Read a Data Stream from File
#'
#' A DSD class that reads a data stream from a file or any R connection.
#'
#' \code{DSD_ReadCSV} uses [read.table()] to read in data from an R
#' connection. The connection is responsible for maintaining where the stream
#' is currently being read from. In general, the connections will consist of
#' files stored on disk but have many other possibilities (see
#' [connection]).
#'
#' The implementation tries to gracefully deal with slightly corrupted data by
#' dropping points with inconsistent reading and producing a warning. However,
#' this might not always be possible resulting in an error instead.
#'
#' The position in the file can be reset to the beginning using
#' [reset_stream()]. The connection can be closed using
#' `close_stream()`.
#'
#' @family DSD
#'
#' @param file A file/URL or an open connection.
#' @param k Number of true clusters, if known.
#' @param o Number of outliers, if known.
#' @param take indices of columns to extract from the file.
#' @param class column index for the class attribute/cluster label.  If
#' \code{take} is specified then it needs to also include the class/label
#' column.
#' @param outlier column index for the outlier mark.  If \code{take} is
#' specified then it needs to also include the outlier column.
#' @param loop If enabled, the object will loop through the stream when the end
#' has been reached. If disabled, the object will warn the user upon reaching
#' the end.
#' @param sep The character string that separates dimensions in data points in
#' the stream.
#' @param header Does the first line contain variable names?
#' @param skip the number of lines of the data file to skip before beginning to
#' read data.
#' @param col.names A vector of optional names for the variables. The default is to use `"V"` followed by the
#'   column number.
#' @param colClasses A vector of classes to be assumed for the columns passed
#' on to \code{read.table}.
#' @param ... Further arguments are passed on to [read.table].  This can
#' for example be used for encoding, quotes, etc.
#' @param dsd A object of class \code{DSD_ReadCSV}.
#' @return An object of class \code{DSD_ReadCSV} (subclass of [DSD_R],
#' \code{DSD}).
#' @author Michael Hahsler, Dalibor Krleža
#' @seealso [read.table()].
#' @examples
#' # creating data and writing it to disk
#' stream <- DSD_Gaussians(k = 3, d = 5, outliers = 1, space_limit = c(0,2),
#'   outlier_options = list(outlier_horizon = 10))
#' write_stream(stream, "data.txt", n = 10, header = TRUE, sep = ",")
#'
#' # reading the same data back (as a loop)
#' stream2 <- DSD_ReadCSV("data.txt", sep = ",", header = TRUE, loop = TRUE)
#' stream2
#'
#' # get points (fist a single point and then 20 using loop)
#' get_points(stream2)
#' p <- get_points(stream2, n = 20)
#' plot(p)
#'
#' # clean up
#' close_stream(stream2)
#' file.remove("data.txt")
#'
#' # example with a part of the kddcup1999 data (take only cont. variables)
#' file <- system.file("examples", "kddcup10000.data.gz", package="stream")
#' stream <- DSD_ReadCSV(gzfile(file),
#'         take=c(1, 5, 6, 8:11, 13:20, 23:42), class = 42, k = 7)
#' stream
#'
#' get_points(stream, 5, class = TRUE)
#'
#' # plot 100 points (projected on the first two principal components)
#' plot(stream, n=100, method = "pca")
#'
#' close_stream(stream)
#' @export
DSD_ReadCSV <- function(file,
  k = NA,
  o = NA,
  take = NULL,
  class = NULL,
  outlier = NULL,
  loop = FALSE,
  sep = ",",
  header = FALSE,
  skip = 0,
  col.names = NULL,
  colClasses = NA,
  ...) {
  env <- environment()
  if (is.na(o) && !is.null(outlier))
    stop("The outlier column is defined, but the number of outliers is not supplied")
  if (!is.na(o) && is.null(outlier))
    stop("The number of outliers is supplied, but the outlier column was not supplied")
  # if the user passes a string, create a new connection and open it
  if (is(file, "character"))
    file <- file(file)

  # error if no string or connection is passed
  if (!is(file, "connection"))
    stop("Please pass a valid connection!")

  # open the connection if its closed
  if (!isOpen(file))
    open(file)

  # filename
  filename <- basename(summary(file)$description)

  # seekable?
  if (loop &&
      !isSeekable(file))
    stop("Loop only allowed for seekable connections!")

  # read first point to figure out structure!
  if (skip > 0)
    readLines(file, n = skip)
  point <- read.table(
    text = readLines(con = file, n = 1 + header),
    sep = sep,
    header = header,
    colClasses = colClasses,
    ...
  )

  # reset stream if possible (otherwise first point is lost)
  if (isSeekable(file)) {
    seek(file, where = 0)
    if (skip > 0)
      readLines(file, n = skip)
    if (header)
      readLines(file, n = 1)
  }

  # select columns take
  if (!is.null(take)) {
    if (is.character(take))
      take <- pmatch(take, colnames(point))
    if (any(is.na(take)))
      stop("Invalid column name specified in take!")
    point <- point[, take]
  }

  # dimensions
  d <- ncol(point) -!is.null(class)

  # header?
  if (header)
    header <- colnames(point)
  else
    header <- NULL

  if (!is.null(col.names)) {
    if (length(col.names) != d)
      stop("length of col.names does not match the number of columns in the stream!")
    header <- col.names
  }

  # data types?
  colClasses <- sapply(point[1, ], class)
  ### integer -> numeric, factor -> character
  colClasses[colClasses == "integer"] <- "numeric"
  colClasses[colClasses == "factor"] <- "character"

  # class?
  if (is.character(class)) {
    if (is.null(header))
      stop("Only numeric column index allowed if no headers are available!")
    class <- pmatch(class, header)
    if (is.na(class))
      stop("No matching column name for class!")
  } else if (!is.null(class)) {
    if (!is.null(take))
      class <- match(class, take)
    if (is.na(class))
      stop("Invalid class column index!")
  }
  # outlier?
  if (is.character(outlier)) {
    if (is.null(header))
      stop("Only numeric column index allowed if no headers are available!")
    outlier <- pmatch(outlier, header)
    if (is.na(outlier))
      stop("No matching column name for outlier indicators!")
  } else if (!is.null(outlier)) {
    if (!is.null(take))
      outlier <- match(outlier, take)
    if (is.na(outlier))
      stop("Invalid outlier column index!")
  }
  if (!is.null(outlier) && !is.logical(point[, outlier]))
    stop("Outlier column must have logical values!")

  # creating the DSD object
  l <- list(
    description = paste('File Data Stream (', filename, ')', sep = ''),
    d = d,
    k = k,
    o = o,
    file = file,
    sep = sep,
    take = take,
    header = header,
    colClasses = colClasses,
    read.table.args = list(...),
    class = class,
    outlier = outlier,
    loop = loop,
    skip = skip,
    env = env
  )
  class(l) <- c("DSD_ReadCSV", "DSD_R", "DSD_data.frame", "DSD")

  l
}

## it is important that the connection is OPEN

#' @export
get_points.DSD_ReadCSV <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  cluster = FALSE,
  class = FALSE,
  outlier = FALSE,
  ...) {
  .nodots(...)

  .DEBUG <- TRUE
  #.DEBUG <- FALSE

  outofpoints <- match.arg(outofpoints)
  noop <- function(...) {
  }
  msg <- switch(
    outofpoints,
    "stop" = stop,
    "warn" = warning,
    "ignore" = noop
  )

  n <- as.integer(n)

  ## remember position
  if (!isSeekable(x$file))
    pos <- NA
  else
    pos <- seek(x$file)

  d <- NULL
  eof <- FALSE

  ## only text connections can do read.table without readLine (would be faster)
  #if(summary(x$file)$text == "text"){
  #  suppressWarnings(
  #    try(d <- do.call(read.table, c(list(file=x$file, sep=x$sep, nrows=n,
  #      colClasses=x$colClasses), x$read.table.args)),
  #      silent = TRUE))
  #}

  try(lines <- readLines(con = x$file, n = n), silent = !.DEBUG)

  ## EOF?
  if (length(lines) < 1)
    eof <- TRUE
  else {
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
  }

  if (eof)
    msg("The stream is at its end (EOF)!")
  ## loop?
  if (is.null(d) || nrow(d) < n || eof) {
    if (!x$loop) {
      ## try to undo read in case of stop
      if (outofpoints == "stop" && !is.na(pos))
        seek(x$file, pos)
      if (!eof)
        msg("Not enough points in the stream!")
    } else {
      ## looping
      while (nrow(d) < n) {
        reset_stream(x)
        try(lines <-
            readLines(con = x$file, n = n - nrow(d)),
          silent = !.DEBUG)

        ## EOF?
        if (length(lines) == 0)
          eof <- TRUE
        else {
          d2 <- NULL
          suppressWarnings(try(d2 <- do.call(read.table,
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
          if (!is.null(d2) && nrow(d2 > 0))
            d <- rbind(d, d2)
          else
            msg("Read failed (use smaller n for unreliable sources)!")
        }

      }
    }
  }

  ## no data!
  if (is.null(d)) {
    if (!eof)
      msg("Read failed (use smaller n for unreliable sources)!")

    ## create conforming data.frame with 0 rows
    d <- data.frame()
    for (i in 1:length(x$colClasses))
      d[[i]] <- do.call(x$colClasses[i], list(0))
  } else {
    ## take columns
    if (!is.null(x$take))
      d <- d[, x$take, drop = FALSE]
  }

  ## remove additional columns from a bad line
  if (ncol(d) > x$d+!is.null(class))
    d <- d[, 1:(x$d+!is.null(class)), drop = FALSE]

  if (nrow(d) > 0) {
    if (!is.null(x$header))
      colnames(d) <- x$header

    removals <- c()
    ### handle missing cluster/class info
    if (!is.null(x$class)) {
      cl <- d[, x$class]
      removals <- c(x$class)
    } else{
      if (cluster || class) {
        cl <- rep(NA_integer_, nrow(d))
      }
    }
    ### handle outlier info
    if (!is.null(x$outlier)) {
      out <- d[, x$outlier]
      removals <- c(removals, x$outlier)
    } else{
      out <- rep(FALSE, nrow(d))
    }
    if (length(removals) > 0)
      d <- d[, -removals, drop = FALSE]

    if (class)
      d <- cbind(d, class = cl)
    if (cluster)
      attr(d, "cluster") <- cl
    if (outlier)
      attr(d, "outlier") <- out
  }

  d
}

#' @export
reset_stream.DSD_ReadCSV <- function(dsd, pos = 1) {
  pos <- as.integer(pos)

  if (!isSeekable(dsd$file))
    stop("Underlying conneciton does not support seek!")
  seek(dsd$file, where = 0)

  if (dsd$skip > 0)
    readLines(dsd$file, n = dsd$skip)
  if (!is.null(dsd$header))
    readLines(dsd$file, n = 1)
  if (pos > 1)
    readLines(dsd$file, n = pos - 1L)
  invisible(NULL)
}

#' @rdname DSD_ReadCSV
#' @export
close_stream <- function(dsd) {
  if (!is(dsd, "DSD_ReadCSV"))
    stop("'dsd' is not of class 'DSD_ReadCSV'")
  close(dsd$file)
}
