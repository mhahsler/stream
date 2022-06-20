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

#' Exponential Moving Average over a Data Stream
#'
#' Applies an exponential moving average to components of a data stream.
#'
#' The exponential moving average is calculated by:
#'
#' \eqn{S_t = \alpha Y_t + (1 - \alpha)\; S_{i-1}}
#'
#' with \eqn{S_0 = Y_0}.
#'
#'
#' @family DSF
#'
#' @param dsd	The input stream as an [DSD] object.
#' @param dim columns to which the filter should be applied. Default is all columns.
#' @param alpha smoothing coefficient in \eqn{[0, 1]}. Larger means discouting older observations faster.
#' @return An object of class `DSF_ExponentialMA` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @examples
#' # Smooth a time series
#' data(presidents)
#'
#' stream <- data.frame(
#'     presidents,
#'     .time = time(presidents)) %>%
#'   DSD_Memory()
#'
#' plot(stream, dim = 1, n = 120, method = "ts", main = "Original")
#'
#' smoothStream <- stream %>% DSF_ExponentialMA(alpha = .7)
#' smoothStream
#'
#' reset_stream(smoothStream)
#' plot(smoothStream, dim = 1, n = 120, method = "ts", main = "With ExponentialMA(.7)")
#' @export
DSF_ExponentialMA <- function(dsd, dim = NULL, alpha = .5) {
  # creating the DSD object

  l <- list(
    dsd = dsd,
    dim = dim,
    alpha = alpha,
    S.env = as.environment(list(S = NULL)),
    description = paste0(dsd$description, "\n  + exponential MA(", alpha, ")")
  )
  class(l) <- c("DSF_ExponentialMA", "DSF", "DSD_R", "DSD")

  l
}

#' @export
get_points.DSF_ExponentialMA <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  info = TRUE,
  ...) {
  .nodots(...)

  d <-
    get_points(x$dsd, n, outofpoints = outofpoints, info = info, ...)

  dims <- x$dim
  if (is.null(dims))
    dims <- seq(ncol(d))

  if (is.null(x$S.env$S))
    x$S.env$S <- d[1, dims, drop = FALSE]

  for (i in seq(nrow(d))) {

    # handle NA
    Y <- d[i, dims]
    if (any(missing <- is.na(Y)))
      Y[missing] <- x$S.env$S[missing]
    if (any(missing <- is.na(x$S.env$S)))
      x$S.env$S[missing] <- Y[missing]

    x$S.env$S <- d[i, dims] <- x$alpha * x$S.env$S + (1 - x$alpha) * Y
  }
  d
}
