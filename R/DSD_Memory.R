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

#' A Data Stream Interface for Data Stored in Memory
#'
#' This class provides a data stream interface for data stored in memory as
#' matrix-like objects (including data frames). All or a portion of the stored
#' data can be replayed several times.
#'
#' In addition to regular data.frames other matrix-like objects that provide
#' subsetting with the bracket operator can be used. This includes `ffdf`
#' (large data.frames stored on disk) from package \pkg{ff} and
#' \code{big.matrix} from \pkg{bigmemory}.
#'
#' **Reading the whole stream**
#' By using `n = -1` in `get_points()`, the whole stream is returned.
#'
#' @family DSD
#'
#' @param x A matrix-like object containing the data.  If `x` is a DSD
#' object then a data frame for `n` data points from this DSD is created.
#' @param n Number of points used if `x` is a DSD object. If `x` is a
#' matrix-like object then `n` is ignored.
#' @param k Optional: The known number of clusters in the data
#' @param loop Should the stream start over when it reaches the end?
#' @param description character string with a description.
#' @return Returns a `DSD_Memory` object (subclass of [DSD_R], [DSD]).
#' @author Michael Hahsler
#' @examples
#' # Example 1: store 1000 points from a stream
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' replayer <- DSD_Memory(stream, k = 3, n = 1000)
#' replayer
#' plot(replayer)
#'
#' # creating 2 clusterers of different algorithms
#' dsc1 <- DSC_DBSTREAM(r = 0.1)
#' dsc2 <- DSC_DStream(gridsize = 0.1, Cm = 1.5)
#'
#' # clustering the same data in 2 DSC objects
#' reset_stream(replayer) # resetting the replayer to the first position
#' update(dsc1, replayer, 500)
#' reset_stream(replayer)
#' update(dsc2, replayer, 500)
#'
#' # plot the resulting clusterings
#' reset_stream(replayer)
#' plot(dsc1, replayer, main = "DBSTREAM")
#' reset_stream(replayer)
#' plot(dsc2, replayer, main = "D-Stream")
#'
#'
#' # Example 2: use a data.frame to create a stream (3rd col. contains the assignment)
#' df <- data.frame(x = runif(100), y = runif(100),
#'   .class = sample(1:3, 100, replace = TRUE))
#'
#' # add some outliers
#' out <- runif(100) > .95
#' df[['.outlier']] <- out
#' df[['.class']] <- NA
#' head(df)
#'
#' stream <- DSD_Memory(df)
#' stream
#'
#' reset_stream(stream)
#' get_points(stream, n = 5)
#'
#' # get the remaining points
#' rest <- get_points(stream, n = -1)
#' nrow(rest)
#'
#' # plot all available points with n = -1
#' reset_stream(stream)
#' plot(stream, n = -1)
#' @export
DSD_Memory <- function(x,
  n,
  k = NA,
  loop = FALSE,
  description = NULL) {
  stream_desc <- NULL
  if (is(x, "DSD")) {
    if (is.na(k) && !is.null(x$k))
      k <- x$k

    stream_desc <- x$description

    x <- get_points(x, n, info = TRUE)
  }

  d <- ncol(x)

  state <- new.env()
  assign("counter", 1L, envir = state)

  if (is.null(description)) {
    description <- "Memorized Stream"

    if (!is.null(stream_desc))
      description <- paste0(description, " for ", stream_desc)
  }

  # creating the DSD object
  structure(
    list(
      description = description,
      strm = x,
      state = state,
      d = d,
      k = k,
      loop = loop
    ),
    class = c("DSD_Memory", "DSD_R", "DSD")
  )
}

#' @export
get_points.DSD_Memory <- function(x,
  n = 1L,
  outofpoints = c("stop", "warn", "ignore"),
  info = TRUE,
  ...) {
  .nodots(...)

  outofpoints <- match.arg(outofpoints)

  # get all points
  if (is.infinite(n) || n < 1) {
    if (x$loop) {
      warning("Cannot return all points with loop on. Disabling loop for this call.")
    }

    if (x$state$counter > nrow(x$strm))
      return(x$strm[0, , drop = FALSE])

    pos <- x$state$counter
    x$state$counter <- 1L
    return(x$strm[seq(pos, nrow(x$str)), , drop = FALSE])
  }

  n <- as.integer(n)

  if (x$state$counter > nrow(x$strm)) {
    if (x$loop)
      x$state$counter <- 1L
    else {
      if (outofpoints == "stop")
        stop("The stream is at its end!")
      if (outofpoints == "warn")
        warning("The stream is at its end! No more points available!")
      return(x$strm[0, , drop = FALSE])
    }
  }

  n_left <- nrow(x$strm) - x$state$counter + 1L

  if (n_left < n && !x$loop) {
    if (outofpoints == "stop")
      stop("Not enough data points left in stream! Only ",
        n_left,
        " are available.")
    if (outofpoints == "warn")
      warning("Not enough data points left in stream, returning the remaining ", n_left, " points!")
    n <- n_left
  }

  o <- NULL
  if (n_left >= n) {
    ### regular case
    d <-
      x$strm[x$state$counter:(x$state$counter + n - 1L), , drop = FALSE]
    x$state$counter <- x$state$counter + n
  } else{
    ### we need to loop!
    # take what is left and reset counter
    d <- x$strm[x$state$counter:nrow(x$strm), , drop = FALSE]

    togo <- n - n_left
    x$state$counter <- 1L

    while (togo > 0L) {
      n_left <- nrow(x$strm) - x$state$counter + 1L

      if (n_left < togo) {
        # take the whole stream
        d <- rbind(d, x$strm)

        togo <- togo - n_left
      } else{
        # take the rest
        d <-
          rbind(d, x$strm[1:(x$state$counter + togo - 1), , drop = FALSE])

        x$state$counter <- x$state$counter + togo
        togo <- 0L
      }
    }
  }

  d <- data.frame(d)

  if (!info)
    d <- remove_info(d)

  d
}

#' @export
print.DSD_Memory <- function(x, ...) {
  NextMethod() # calling the super classes print()
  pos <- x$state$counter
  if (pos > nrow(x$strm))
    if (!x$loop)
      pos <- "'end'"
  else
    pos <- 1
  cat(
    paste(
      'Contains',
      nrow(x$strm),
      'data points - currently at position',
      pos,
      '- loop is',
      x$loop,
      '\n'
    )
  )
}

#' @export
reset_stream.DSD_Memory <- function(dsd, pos = 1) {
  dsd$state$counter <- pos
}
