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
#' @param alpha smoothing coefficient in \eqn{[0, 1]}. Larger means discounting older observations faster.
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
DSF_ExponentialMA <- function(dsd = NULL,
  dim = NULL,
  alpha = .5) {
  # creating the DSD object

  l <- list(
    dsd = dsd,
    dim = dim,
    alpha = alpha,
    S.env = as.environment(list(S = NULL)),
    description = paste0(
      ifelse(!is.null(dsd), dsd$description, "DSF without a specified DSD")
      ,
      "\n  + exponential MA(",
      alpha,
      ")"
    )
  )
  class(l) <- c("DSF_ExponentialMA", "DSF", "DSD_R", "DSD")

  l
}

#' @export
update.DSF_ExponentialMA <- function(object,
  dsd = NULL,
  n = 1L,
  return = "data",
  info = TRUE,
  ...) {
  .nodots(...)
  return <- match.arg(return)

  if (is.null(dsd))
    dsd <- object$dsd
  if (is.null(dsd))
    stop("No dsd specified in ", deparse(substitute(object)), ". Specify a dsd in update().")

  if (n == 0)
    return(get_points(dsd, n = 0L, info = info))

  d <-
    get_points(dsd, n, info = info, ...)

  dims <- get_dims(object$dim, d)

  if (is.null(object$S.env$S))
    object$S.env$S <- d[1, dims, drop = FALSE]

  for (i in seq_len(nrow(d))) {
    # handle NA
    Y <- d[i, dims, drop = FALSE]
    if (any(missing <- is.na(Y)))
      Y[missing] <- object$S.env$S[missing]
    if (any(missing <- is.na(object$S.env$S)))
      object$S.env$S[missing] <- Y[missing]

    object$S.env$S <-
      d[i, dims] <- object$alpha * object$S.env$S + (1 - object$alpha) * Y
  }
  d
}
