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
#' `scale_stream()` estimates the values for centering and scaling (see
#' [scale] in \pkg{base}) using `n` points from the stream.
#'
#' @family DSF
#'
#' @param dsd A object of class [DSD] that will be scaled.
#' @param dim integer vector; what dimensions should be scaled? Default is all.
#' @param center,scale logical or a numeric vector of length equal to the
#' number of columns used for centering/scaling (see function [scale]).
#' @param n The number of points used by `scale_stream()` to creating the centering/scaling
#' @param reset Try to reset the stream to its beginning after taking `n`
#' points for scaling.
#' @return An object of class `DSF_Scale` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @seealso [scale] in \pkg{base}
#' @examples
#' ## create a stream and apply scaling to z-scores using 100 points from the stream
#' stream <- DSD_Gaussians(k = 3, d = 2) %>% DSF_Scale(n = 100)
#' p <- get_points(stream, n = 100)
#' colMeans(p)
#' apply(p, MARGIN = 2, sd)
#' plot(stream)
#'
#' ## scale only dimension 2
#' stream <- DSD_Gaussians(k = 3, d = 2) %>% DSF_Scale(n = 100, dim = 2)
#'
#' plot(p)
#' @export
DSF_Scale <-
  function(dsd,
    dim = NULL,
    center = TRUE,
    scale = TRUE,
    n = 1000,
    reset = FALSE) {
    # creating the DSD object
    l <- list(
      description = paste0(dsd$description, "\n  + scaled"),
      dsd = dsd,
      dim = dim,
      d = dsd$d,
      k = dsd$k,
      o = dsd$o,
      center = FALSE,
      scale = FALSE
    )
    class(l) <-
      c("DSF_Scale", "DSF", "DSD_R", "DSD")

    l <- scale_stream(
      l,
      n = n,
      center = center,
      scale = scale,
      reset = reset
    )

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
  info = FALSE,
  ...) {
  .nodots(...)

  points <-
    get_points(x$dsd,
      n,
      info = info)

  d_cols <- grep('^\\.', colnames(points), invert = TRUE)

  d <- points[, d_cols, drop = FALSE]

  # scale
  if (is.null(x$dim))
    d <- as.data.frame(scale(d, center = x$center, scale = x$scale))
  else
    d[, x$dim] <- as.data.frame(scale(d[, x$dim], center = x$center, scale = x$scale))


  points[, d_cols] <- d

  points
}

# internal
scale_stream <-
  function(dsd,
    n = 1000,
    center = TRUE,
    scale = TRUE,
    reset = FALSE) {

    if (is.null(dsd$dim))
      sc <- scale(get_points(dsd, n = n), center = center, scale = scale)
    else
      sc <- scale(get_points(dsd, n = n)[, dsd$dim, drop = FALSE], center = center, scale = scale)

    dsd$center <- attr(sc, "scaled:center")
    if (is.null(dsd$center))
      dsd$center <- center

    dsd$scale <- attr(sc, "scaled:scale")
    if (is.null(dsd$scale))
      dsd$scale <- scale
    else
      dsd$scale[dsd$scale == 0] <-
      1 # fix division by 0 if all values were the same

    if (reset)
      try(reset_stream(dsd), silent = TRUE)

    dsd
  }
