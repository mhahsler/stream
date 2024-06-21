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

#' Scale a Data Stream
#'
#' Make an unscaled data stream into a scaled data stream.
#'
#' If `center` and `scale` are not vectors with scaling factors, then `scale_stream()`
#' estimates the values for centering and scaling (see
#' [scale] in \pkg{base}) using `n` points from the stream and the stream is reset if `reset = TRUE` and the
#' [DSD] object supports resetting.
#'
#' @family DSF
#'
#' @param dsd A object of class [DSD] that will be scaled.
#' @param dim integer vector or names of dimensions that should be scaled? Default is all.
#' @param center,scale logical or a numeric vector of length equal to the
#'   number of columns (selected with dim) used for centering/scaling (see function [scale]).
#' @param n The number of points used by `scale_stream()` to creating the centering/scaling
#' @return An object of class `DSF_Scale` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @seealso [scale] in \pkg{base}
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' get_points(stream, 3)
#'
#' # scale with manually calculated scaling factors
#' points <- get_points(stream, n = 100, info = FALSE)
#' center <- colMeans(points)
#' scale <- apply(points, MARGIN = 2, sd)
#'
#' scaledStream <- stream %>%  DSF_Scale(dim = c(1L, 2L), center = center, scale = scale)
#' colMeans(get_points(scaledStream, n = 100, info = FALSE))
#' apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#'
#' # let DSF_Scale calculate the scaling factors from the first n points of the stream
#' scaledStream <- stream %>% DSF_Scale(dim = c(1L, 2L), n = 100)
#' colMeans(get_points(scaledStream, n = 100, info = FALSE))
#' apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#'
#' ## scale only X2
#' scaledStream <- stream %>% DSF_Scale(dim = "X2", n = 100)
#' colMeans(get_points(scaledStream, n = 100, info = FALSE))
#' apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#' @export
DSF_Scale <-
  function(dsd = NULL,
    dim = NULL,
    center = TRUE,
    scale = TRUE,
    n = 100L) {
    # creating the DSD object
    l <- list(
      description = paste0(
        ifelse(!is.null(dsd), dsd$description, "DSF without a specified DSD"),
        "\n  + scaled"
      ),
      dsd = dsd,
      dim = dim,
      d = dsd$d,
      k = dsd$k,
      center = center,
      scale = scale
    )
    class(l) <-
      c("DSF_Scale", "DSF", "DSD_R", "DSD")

    # estimate factors from stream?
    if (is.logical(center) || is.logical(scale)) {
      if (is.null(dsd))
        stop("Scaling parameters cannot be estimated without a dsd.")

      points <- get_points(dsd, n = n, info = TRUE)

      # translate dim to index and subset
      l$dim <- get_dims(dim, points)

      points <- points[, l$dim, drop = FALSE]
      points <- remove_info(points)

      sc <- scale(points, center = center, scale = scale)

      l$center <- attr(sc, "scaled:center")
      l$scale <- attr(sc, "scaled:scale")
      # fix division by 0 if all values were the same
      l$scale[l$scale == 0] <- 1
    }

    l
  }

### Deprecated
#' @rdname DSF_Scale
#' @name DSF_Scale
#' @section Deprecated:
#' `DSD_ScaleStream` is deprecated. Use `DSF_Scale` instead.
NULL

## it is important that the connection is OPEN

#' @export
update.DSF_Scale <- function(object,
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
    stop("No dsd specified in ", deparse1(substitute(object)), ". Specify a dsd in update().")

  points <-
    get_points(dsd,
      n,
      info = info)

  if (n == 0L)
    return(points)

  points[, object$dim] <-
    scale(points[, object$dim, drop = FALSE], center = object$center, scale = object$scale)

  if (!info)
    points <- remove_info(points)

  points
}

#' Deprecated DSD_ScaleStream
#'
#' Deprecated! Use [DSF_Scale] instead.
#'
#' @param ... arguments are passed on to [DSF_Scale()].
#' @return Produces a warning that `DSD_ScaleStream` is deprecated
#'  and returns an object of class
#' [DSF_Scale] (subclass of [DSF] and [DSD]).
#' @export
DSD_ScaleStream <- function(...)
{
  warning("DSD_ScaleStream is deprecated. Use DSF_Scale instead.")
  DSF_Scale(...)
}
