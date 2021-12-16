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


## DSD - Data Stream Data interface
## all DSD classes have these functions
## and an additional function to create the DSD
##
## A new DSD class (e.g., myDSD) needs the following:
## 1. a constructor function. myDSD <- function(PARAMETERS) which
## returns an object with the class  c("DSD_myDSD","DSD_R","DSD")
## 2. get_points.myDSD <- function(x, n=1, ...)
##
## See DSD_Gaussian_Static.R for an example



#' Data Stream Data Generator Base Classes
#'
#' Abstract base classes for DSD (Data Stream Data Generator).
#'
#' The `DSD` class cannot be instantiated, but it serves as a abstract
#' base class from which all DSD objects inherit.
#'
#' `DSD` provides common functionality like:
#'
#' * [get_points]
#' * `print`
#' * [plot]
#' * [reset_stream] (if available)
#'
#' `DSD_R` inherits form `DSD` and is the abstract parent class for
#' DSD implemented in R. To create a new R-based implementation there are only
#' two function that needs to be implemented for a new `DSD` subclass
#' called `Foo` would be:
#'
#' 1. A creator function `DSD_Foo()` and
#' 2. a method `get_points.DSD_Foo()` for that class.
#'
#'
#' @param ... further arguments.
#' @aliases DSD DSD_R description description DSD_R DSD_R
#' @author Michael Hahsler
#' @seealso [animate],
#' [update], [evaluate], [get_points],
#' [plot]
#' [write_stream.DSD], [reset_stream]
#' @examples
#'
#' DSD()
#'
#' # create data stream with three clusters in 3-dimensional space
#' stream <- DSD_Gaussians(k=3, d=3)
#'
#' # get points from stream
#' get_points(stream, n=5)
#'
#' # get points with true cluster assignment
#' p <- get_points(stream, n=5, cluster=TRUE)
#' attr(p, "cluster")
#'
#' # plotting the data (scatter plot matrix, first and third dimension, and first
#' #  two principal components)
#' plot(stream)
#' plot(stream, dim=c(1,3))
#' plot(stream, method="pc")
#'
#' @export DSD
DSD <- abstract_class_generator("DSD")
DSD_R <- abstract_class_generator("DSD")

#' Get Points from a Data Stream Generator
#'
#' Gets points from a DSD object.
#'
#' Each DSD object has a unique way for returning data points, but they all are
#' called through the generic function, `get_points`. This is done by
#' using the S3 class system. See the man page for the specific DSD class on
#' the semantics for each implementation of `get_points`.
#'
#' @param x The DSD object.
#' @param n Request up to $n$ points from the stream.
#' @param outofpoints Action taken if less than $n$ data points are
#' available. The default is to stop with an error.  For warn and ignore all
#' available (possibly zero) points are returned.
#' @param ... Additional parameters to pass to `get_points`
#' implementations.
#' @return Returns a matrix of `x$d` columns and `n` rows.
#' @author Michael Hahsler
#' @seealso \code{\link{DSD}}
#' @examples
#'
#' stream <- DSD_Gaussians()
#' get_points(stream, 100)
#'
#' @export get_points
get_points <- function(x, n=1, outofpoints=c("stop", "warn", "ignore"), ...)
  UseMethod("get_points")
get_points.default <- function(x, n=1,
                               outofpoints=c("stop", "warn", "ignore"), ...) {
  stop(gettextf("get_points not implemented for class '%s'.",
                paste(class(x), collapse=", ")))
}

### in case the stream can be reset (e.g., a stream from a file)


#' Reset a Data Stream to its Beginning
#'
#' Resets the counter in a DSD object to the beginning or any other position in
#' the stream.
#'
#' Resets the counter of the stream object. For example, for [DSD_Memory],
#' the counter stored in the environment variable is moved back to 1. For
#' [DSD_ReadCSV] objects, this is done by calling `seek()` on the
#' underlying connection.
#'
#' @param dsd An object of class a subclass of [DSD] which implements a
#' reset function.
#' @param pos Position in the stream (the beginning of the stream is position
#' 1).
#' @author Michael Hahsler
#' @seealso [DSD_ReadCSV], [DSD_MG],
#' [DSD_ScaleStream], [DSD_Memory]
#' @examples
#'
#' # initializing the objects
#' stream <- DSD_Gaussians(k=3, d=2)
#' replayer <- DSD_Memory(stream, 100)
#' replayer
#'
#' p <- get_points(replayer, 50)
#' replayer
#'
#' # reset replayer to the begining of the stream
#' reset_stream(replayer)
#' replayer
#'
#' # set replayer to position 21
#' reset_stream(replayer, pos=21)
#' replayer
#'
#' @export reset_stream
reset_stream <- function(dsd, pos=1) UseMethod("reset_stream")
reset_stream.DSD <- function(dsd, pos=1) {
  stop(gettextf("reset_stream not implemented for class '%s'.",
                paste(class(dsd), collapse=", ")))
}


### end of interface
#############################################################
### helper
print.DSD <- function(x, ...) {
  .nodots(...)

  k <- x$k
  if(is.null(k)) k <- NA
  d <- x$d
  if(is.null(d)) d <- NA
  o <- x$o
  if(is.null(o)) o <- NA

  cat(.line_break(x$description))
  cat("Class:", paste(class(x), collapse=", "), "\n")
  cat(paste('With', k, 'clusters', 'and', o, 'outliers', 'in', d, 'dimensions', '\n'))
}

summary.DSD <- function(object, ...) print(object)

#' @rdname plot.DSC
plot.DSD <- function(x, n = 500, col= NULL, pch= NULL,
                     ..., method="pairs", dim=NULL, alpha=.6) {
  ## method can be pairs, plot or pc (projection with PCA)

  d <- get_points(x, n, cluster = TRUE, outlier = TRUE)
  assignment <- attr(d, "cluster")

  ### stream has no assignments!
  if(length(assignment)==0) assignment <- rep(1L, nrow(d))

  noise <- is.na(assignment)

  ### assignment is not numeric
  if(!is.numeric(assignment)) assignment <- as.integer(as.factor(assignment))

  ### add alpha shading to color
  if(is.null(col)) {
    col <- rgb(cbind(t(col2rgb(assignment)/255)), alpha=alpha)
  }else{
    if(length(col)==1L) col <- rep(col, length(assignment))
  }

  col[noise] <-  .noise_col

  if(is.null(pch)) {
    #pch <- rep(1, n)
    pch <- as.integer(assignment)
    pch <- pch %% 25
    pch[noise] <- .noise_pch
  }

  if(!is.null(dim)) d <- d[,dim, drop = FALSE]

  if(ncol(d)>2 && method=="pairs") {
    pairs(d, col=col, pch=pch, ...)
  } else if(ncol(d)>2 && method=="pc") {
    ## we assume Euclidean here
    p <- prcomp(d)

    plot(p$x, col=col, pch=pch, ...)
    title(sub = paste("Explains ",
                      round(sum(p$sdev[1:2]) / sum(p$sdev)* 100, 2),
                      "% of the point variability", sep=""))
  } else if(ncol(d) == 1) {
    plot(d[[1]], rep(0, length(d[[1]])), col = assignment, pch= pch,
         ylab = "", xlab = colnames(d)[1], ...)
  } else {
    if(ncol(d)>2) d <- d[,1:2, drop = FALSE]
    plot(d, col=col, pch=pch, ...)
  }
}
