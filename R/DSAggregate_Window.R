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


#' Sliding Window (Data Stream Operator)
#'
#' Implements a sliding window data stream operator which keeps a fixed amount
#' (window length) of the most recent data points of the stream.
#'
#' If `lambda` is greater than 0 then the weight uses a damped window
#' model (Zhu and Shasha, 2002).  The weight for points in the window follows
#' \eqn{2^(-lambda*t)} where \eqn{t} is the age of the point.
#'
#' @family DSAggregate
#'
#' @param horizon the window length.
#' @param lambda decay factor damped window model. `lambda = 0` means no
#' dampening.
#' @return An object of class `DSAggregate_Window` (subclass of [DSAggregate]).
#' @author Michael Hahsler
#' @references Zhu, Y. and Shasha, D. (2002). StatStream: Statistical
#' Monitoring of Thousands of Data Streams in Real Time, Intl. Conference of
#' Very Large Data Bases (VLDB'02).
#' @examples
#' set.seed(1500)
#'
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
#'
#' window <- DSAggregate_Window(horizon = 10)
#' window
#'
#' # update with only two points
#' update(window, stream, 2)
#' get_points(window)
#'
#' # update window
#' update(window, stream, 100)
#' get_points(window)
#' @export
DSAggregate_Window <- function(horizon = 100, lambda = 0)
  structure(
    list(
      description =
        if (lambda > 0)
          "Damped sliding window"
      else
        "Sliding window",
      RObj = WindowDSAggregate$new(horizon = as.integer(horizon), lambda = lambda)
    ),
    class = c("DSAggregate_Window", "DSAggregate", "DST")
  )

#' @export
update.DSAggregate_Window <-
  function(object,
    dsd,
    n = 1,
    verbose = FALSE,
    ...) {
    ### TODO: we do not need to get all points if n is very large!
    object$RObj$update(get_points(dsd, n = n), verbose = verbose, ...)
  }

#' @export
get_points.DSAggregate_Window <- function(x, ...)
  x$RObj$get_points(...)

#' @export
get_weights.DSAggregate_Window <- function(x, ...)
  x$RObj$get_weights(...)

# implements a ring-buffer. pos is the current insert position
WindowDSAggregate <- setRefClass(
  "WindowDSAggregate",
  fields = list(
    horizon	= "integer",
    pos	= "integer",
    lambda = "numeric",
    data	= "ANY"   ### data.frame or NULL
  ),

  methods = list(
    initialize = function(horizon	= 100L, lambda = 0) {
      horizon	<<- horizon
      data <<- NULL ### don't know yet!
      pos	<<- 1L
      lambda <<- lambda

      .self
    },

    update = function(x, ...) {
      ### fist time we get data
      if (is.null(data))
        data <<-
          data.frame(matrix(
            NA,
            nrow = horizon,
            ncol = ncol(x),
            dimnames = list(NULL, colnames(x))
          ))

      if (ncol(x) != ncol(data))
        stop("Dimensionality mismatch between window and data!")

      n <- nrow(x)

      i <- 0L
      while (i < n) {
        ## process the next m points: all or to fill the current horizon
        m <- min(horizon - pos + 1L, n - i)
        data[pos:(pos + m - 1L),] <<-
          x[(i + 1L):(i + m), , drop = FALSE]

        i <- i + m
        pos <<- pos + m
        if (pos > horizon)
          pos <<- 1L
      }

      # fix row names for data_frame
      rownames(data) <<- NULL
    },

    reset = function(...) {
      pos <<- 1L
      data <<- NULL
    },

    get_points = function(...) {
      if (is.null(data))
        return(data.frame())  ### gives 0 nrows and 0 length (we do not know if it is supposed to be a data.frame or a list

      if (pos == 1L)
        return(data)
      else
        return(data[c(pos:(horizon), 1L:(pos - 1L)), , drop = FALSE])
    },

    get_weights = function(...) {
      if (lambda <= 0)
        rep(1, horizon)
      else
        2 ^ (-lambda * (seq((horizon - 1L), 0)))
    }
  )
)

### DSC interface to WindowDSAggregate
WindowDSC <- setRefClass(
  "WindowDSC",
  contains = "WindowDSAggregate",

  methods = list(
    cluster = function(x, ...)
      update(x, ...),
    get_microclusters = function(...)
      get_points(...),
    get_microweights = function(...)
      get_weights(...)
  )
)
