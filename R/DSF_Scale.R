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
#' @family DST
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
#'
#' # scale with manually calculated scaling factors
#' points <- get_points(stream, n = 100, info = FALSE)
#' center <- colMeans(points)
#' scale <- apply(points, MARGIN = 2, sd)
#'
#' scaledStream <- stream %>%  DSF_Scale(center = center, scale = scale)
#' colMeans(get_points(scaledStream, n = 100, info = FALSE))
#' apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#'
#' # let DSF_Scale calculate the scaling factors
#' scaledStream <- stream %>% DSF_Scale(n = 100)
#' colMeans(get_points(scaledStream, n = 100, info = FALSE))
#' apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#'
#' ## scale only X2
#' scaledStream <- stream %>% DSF_Scale(n = 100, dim = "X2")
#' colMeans(get_points(scaledStream, n = 100, info = FALSE))
#' apply(get_points(scaledStream, n = 100, info = FALSE), MARGIN = 2, sd)
#' @export
DSF_Scale <-
  function(dsd,
    dim = NULL,
    center = TRUE,
    scale = TRUE,
    n = 100) {
    # creating the DSD object
    l <- list(
      description = paste0(dsd$description, "\n  + scaled"),
      dsd = dsd,
      dim = dim,
      d = dsd$d,
      k = dsd$k,
      center = center,
      scale = scale
    )
    class(l) <-
      c("DSF_Scale", "DSF", "DSD_R", "DSD")

    # estimate factors from stream
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

    l
  }

### Deprecated
#' @rdname DSF_Scale
#' @section Deprecated:
#' `DSD_ScaleStream` is deprecated. Use `DSF_Scale` instead.
#' @export
DSD_ScaleStream <- DSF_Scale

## it is important that the connection is OPEN

#' @export
get_points.DSF_Scale <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  info = TRUE,
  ...) {
  .nodots(...)

  points <-
    get_points(x$dsd,
      n,
      info = TRUE)

  points[, x$dim] <- scale(points[, x$dim, drop = FALSE], center = x$center, scale = x$scale)

  if (!info)
    points <- remove_info(points)

  points
}
