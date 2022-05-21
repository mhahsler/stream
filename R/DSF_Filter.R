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

#' Apply a Filter to a Data Stream
#'
#' Applies a filter (i.e., a convolution with a filter kernel) to a data stream.
#'
#' A filter kernel is a vector with kernel weights. A few filter are provided.
#'
#' @family DSF
#'
#' @param dsd A object of class [DSD].
#' @param dim columns to which the filter should be applied. Default is all.
#' @param kernel filter kernel as a vector of weights.
#' @param na.rm logical; should NAs be ignored?
#' @return An object of class `DSF_Filter` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @seealso [stats::filter]
#' @examples
#' data(presidents)
#'
#' ## create a data stream with two copies of president approval ratings.
#' ## we will filter the second one and leave the first unfiltered.
#' stream <- data.frame(approval_orig = presidents, approval_MA = presidents) %>% DSD_Memory()
#' plot(stream, dim = 1, n = 120, method = "ts")
#'
#' ## apply a moving average filter to dimension 1
#' filteredStream <- stream %>% DSF_Filter(kernel = filter_MA(5), dim = 2)
#' filteredStream
#'
#' ## resetting the filtered stream also resets the original stream
#' reset_stream(filteredStream)
#' get_points(filteredStream, n = 10)
#'
#' reset_stream(filteredStream)
#' plot(filteredStream, dim = 1, n = 120, method = "ts")
#'
#' ## the second dimension is filtered
#' reset_stream(filteredStream)
#' plot(filteredStream, dim = 2, n = 120, method = "ts")
#'
#' ## look at different filters
#' plot(filter_MA(20), type = "l")
#' plot(filter_hamming(20), type = "l")
#' plot(filter_sinc(10, 100, width = 20), type = "l")
#'
#' @export
DSF_Filter <-
  function(dsd,
    dim = NULL,
    kernel = NULL,
    na.rm = TRUE) {
    # creating the DSD object
    l <- list(
      description = paste0(dsd$description, "\n\t + filtered"),
      dsd = dsd,
      dim = dim,
      window = DSO_Window(horizon = length(kernel)),
      kernel = kernel,
      na.rm = na.rm
    )
    class(l) <-
      c("DSF_Filter", "DSF", "DSD_R", "DSD_data.frame", "DSD")

    l
  }

#' @export
get_points.DSF_Filter <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  cluster = FALSE,
  class = FALSE,
  outlier = FALSE,
  ...) {
  .nodots(...)

  #if (any(cluster || class || outlier))
  #  stop("Clusters, class or outliers not supported for DSF_Filter!")

  for (i in seq(n)) {
    update(x$window, x$dsd, n = 1)
    win <- get_points(x$window)

    # preallocate the space
    if (i == 1L) {
      ps <- data.frame(matrix(NA, nrow = 0, ncol = ncol(win), dimnames = list(NULL, colnames(win))))
      ps[n, ] <- NA
    }

    ## only filter variables in dim
    if (is.null(x$dim))
      ps[i,] <-
      sapply(
        win,
        FUN = function(p)
          mean(p * x$kernel, na.rm = x$na.rm) * length(x$kernel)
      )
    else {
      ps[i,] <- win[nrow(win), , drop = FALSE]
      ps[i, x$dim] <-
        sapply(
          win[, x$dim, drop = FALSE],
          FUN = function(p)
            mean(p * x$kernel, na.rm = x$na.rm) * length(x$kernel)
        )
    }
  }

  ps
}

#' @export
reset_stream.DSF_Filter <- function(dsd, pos = 1) {
  ## clean window
  dsd$window$RObj$reset()

  #reset_stream(dsd$dsd, pos = pos)
  NextMethod()
}

#' @rdname DSF_Filter
#' @param width filter width.
#' @export
filter_MA <- function(width) rep.int(1, width)/width

#' @rdname DSF_Filter
#' @export
filter_hamming <- function(width) {
  M <- width - 1
  k <- (0:M)/M
  c <- 0.54 - 0.46 * cos(2 * pi * k)
  c / sum(c)
}

#' @rdname DSF_Filter
#' @param bw transition bandwidth.
#' @param fc cutoff frequency.
#' @param fs sampling frequency.
#' @export
filter_sinc <- function(fc, fs, width = NULL, bw = NULL)  {
  if (!is.null(bw)) {
    M <- 4 / (bw / fs)
    M <- floor(M)
    if (!M %% 2)
      M <- M + 1
  } else
    M <- width

  f <- fc / fs

  i <- 1:M
  k <- sin(2 * pi * f * (i - M / 2)) / (i - M / 2)
  k[i - M / 2 == 0] <- 2 * pi * f
  k <- k * (0.54 - 0.46 * cos(2 * pi * i / M))
  k / sum(k)
}
