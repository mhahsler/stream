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
#' subsetting with the bracket operator can be used. This includes \code{ffdf}
#' (large data.frames stored on disk) from package \pkg{ff} and
#' \code{big.matrix} from \pkg{bigmemory}.
#'
#' @family DSD
#'
#' @param x A matrix-like object containing the data.  If \code{x} is a DSD
#' object then a data frame for \code{n} data points from this DSD is created.
#' @param n Number of points used if \code{x} is a DSD object. If \code{x} is a
#' matrix-like object then \code{n} is ignored.
#' @param k Optional: The known number of clusters in the data
#' @param loop Should the stream start over when it reaches the end?
#' @param class Vector with the class/cluster label (only used if \code{x} is
#' not a DSD object).
#' @param outlier A logical vector with outlier marks (only used if \code{x} is
#' not a DSD object). FALSE = the correspnding data instance in the \code{x}
#' data frame is not an outlier, TRUE = the corresponding data instance in the
#' \code{x} data frame is an outlier.
#' @param description character string with a description.
#' @return Returns a \code{DSD_Memory} object (subclass of \code{DSD_R},
#' \code{DSD}).
#' @author Michael Hahsler, Dalibor Krleža
#' @examples
#'
#' # store 1000 points from a stream
#' stream <- DSD_Gaussians(k=3, d=2)
#' replayer <- DSD_Memory(stream, k=3, n=1000)
#' replayer
#' plot(replayer)
#'
#' # creating 2 clusterers of different algorithms
#' dsc1 <- DSC_DBSTREAM(r=0.1)
#' dsc2 <- DSC_DStream(gridsize=0.1, Cm=1.5)
#'
#' # clustering the same data in 2 DSC objects
#' reset_stream(replayer) # resetting the replayer to the first position
#' update(dsc1, replayer, 500)
#' reset_stream(replayer)
#' update(dsc2, replayer, 500)
#'
#' # plot the resulting clusterings
#' reset_stream(replayer)
#' plot(dsc1, replayer, main="DBSTREAM")
#' reset_stream(replayer)
#' plot(dsc2, replayer, main="D-Stream")
#'
#' ### use a data.frame to create a stream (3rd col. contains the assignment)
#' df <- data.frame(x=runif(100), y=runif(100),
#'   class=sample(1:3, 100, replace=TRUE))
#' head(df)
#' ### add some outliers
#' out <- runif(100) >.95
#' ### re-assign classes for outliers
#' df[which(out),"class"]<-sample(4:(4+sum(out)-1),sum(out),replace=FALSE)
#'
#' stream <- DSD_Memory(df[,c("x", "y")], class=df[,"class"], outlier=out)
#' stream
#' reset_stream(stream)
#' plot(stream, n=100)
#'
#' @export
DSD_Memory <- function(x,
  n,
  k = NA,
  loop = FALSE,
  class = NULL,
  outlier = NULL,
  description = NULL) {
  if (is(x, "DSD")) {
    if (is.na(k) && !is.null(x$k))
      k <- x$k

    x <- get_points(x, n, cluster = TRUE, outlier = TRUE)
    class <- attr(x, "cluster")
    outlier <- attr(x, "outlier")
  } else{
    ### x is a matrix-like object
    if (!is.null(class) && length(class) != nrow(x))
      stop("Length of class and rows of x do not agree!")
    if (!is.null(outlier) && length(outlier) != nrow(x))
      stop("Length of outlier and rows of x do not agree!")
  }

  d <- ncol(x)

  state <- new.env()
  assign("counter", 1L, envir = state)

  if (is.null(description))
    description <- "Memory Stream Interface"

  # creating the DSD object
  structure(
    list(
      description = description,
      strm = x,
      state = state,
      d = d,
      k = k,
      o = sum(outlier),
      loop = loop,
      class = class,
      outlier = outlier
    ),
    class = c("DSD_Memory", "DSD_R", "DSD_data.frame", "DSD")
  )
}

#' @export
get_points.DSD_Memory <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  cluster = FALSE,
  class = FALSE,
  outlier = FALSE,
  ...) {
  .nodots(...)

  n <- as.integer(n)
  outofpoints <- match.arg(outofpoints)

  if (x$state$counter > nrow(x$strm)) {
    if (x$loop)
      x$state$counter <- 1L
    else {
      if (outofpoints == "stop")
        stop("The stream is at its end!")
      if (outofpoints == "warn")
        warning("The stream is at its end! No more points available!")
      return(x$strm[0, ])
    }
  }

  n_left <- nrow(x$strm) - x$state$counter + 1L

  if (n_left < n && !x$loop) {
    if (outofpoints == "stop")
      stop("Not enough data points left in stream! Only ",
        n_left,
        " are available.")
    if (outofpoints == "warn")
      warning("Not enough data points left in stream! Remaining points returned.")
    n <- n_left
  }

  o <- NULL
  if (n_left >= n) {
    ### regular case
    d <-
      x$strm[x$state$counter:(x$state$counter + n - 1L), , drop = FALSE]
    a <- x$class[x$state$counter:(x$state$counter + n - 1L)]
    if (!is.null(x$outlier))
      o <- x$outlier[x$state$counter:(x$state$counter + n - 1L)]
    x$state$counter <- x$state$counter + n
  } else{
    ### we need to loop!


    # take what is left and reset counter
    d <- x$strm[x$state$counter:nrow(x$strm), , drop = FALSE]
    a <- x$class[x$state$counter:nrow(x$strm)]
    if (!is.null(x$outlier))
      o <- x$outlier[x$state$counter:nrow(x$strm)]

    togo <- n - n_left
    x$state$counter <- 1L

    while (togo > 0L) {
      n_left <- nrow(x$strm) - x$state$counter + 1L

      if (n_left < togo) {
        # take the whole stream
        d <- rbind(d, x$strm)
        a <- append(a, x$class)
        if (!is.null(x$outlier))
          o <- append(o, x$outlier)

        togo <- togo - n_left
      } else{
        # take the rest
        d <- rbind(d, x$strm[1:(x$state$counter + togo - 1), ,drop = FALSE])
        a <- append(a, x$class[1:(x$state$counter + togo - 1)])
        if (!is.null(x$outlier))
          o <- append(o, x$outlier[1:(x$state$counter + togo - 1)])

        x$state$counter <- x$state$counter + togo
        togo <- 0L
      }
    }
  }

  d <- data.frame(d)

  ### handle missing cluster/class info
  if ((cluster ||
      class) && is.null(a))
    a <- rep(1L, nrow(d))
  if (cluster)
    attr(d, "cluster") <- a
  if (outlier)
    attr(d, "outlier") <- o
  if (class)
    d <- cbind(d, class = a)

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
