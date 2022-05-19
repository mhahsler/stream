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


# accepts an open connection


#' Scale a Stream from a DSD
#'
#' Make an unscaled data stream into a scaled data stream.
#'
#' \code{scale_stream()} estimates the values for centering and scaling (see
#' \code{scale} in \pkg{base}) using \code{n} points from the stream.
#'
#' @family DSD
#'
#' @param dsd A object of class \code{DSD} that will be scaled.
#' @param center,scale logical or a numeric vector of length equal to the
#' number of columns used for centering/scaling (see function \code{scale}).
#' @param n The number of points used to creating the centering/scaling
#' @param reset Try to reset the stream to its beginning after taking \code{n}
#' points for scaling.
#' @return An object of class \code{DSD_ScaleStream} (subclass of \code{DSD_R},
#' \code{DSD}).
#' @author Michael Hahsler
#' @seealso \code{\link{scale}}
#' in \pkg{base},
#' @examples
#'
#' stream <- DSD_Gaussians(k=3, d=3)
#' plot(stream)
#'
#' # scale stream using 100 points
#' stream_scaled <- DSD_ScaleStream(stream, n=100)
#' plot(stream_scaled)
#'
#' @export
DSD_ScaleStream <-
  function(dsd,
    center = TRUE,
    scale = TRUE,
    n = 1000,
    reset = FALSE) {
    # creating the DSD object
    l <- list(
      description = paste(dsd$description, "(scaled)"),
      dsd = dsd,
      d = dsd$d,
      k = dsd$k,
      o = dsd$o,
      center = FALSE,
      scale = FALSE
    )
    class(l) <- c("DSD_ScaleStream", "DSD_R", "DSD_data.frame", "DSD")

    l <- scale_stream(
      l,
      n = n,
      center = center,
      scale = scale,
      reset = reset
    )

    l
  }

## it is important that the connection is OPEN

#' @export
get_points.DSD_ScaleStream <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  cluster = FALSE,
  class = FALSE,
  outlier = FALSE,
  ...) {
  .nodots(...)

  d <-
    get_points(x$dsd,
      n,
      cluster = cluster,
      class = class,
      outlier = outlier)

  if (cluster)
    cl <- attr(d, "cluster")
  if (outlier)
    out <- attr(d, "outlier")
  if (class) {
    j <- which("class" == colnames(d))
    if (length(j) == 1L) {
      cl <- d[, j]
      d <- d[,-j]
    } else
      cl <- rep(NA_integer_, nrow(d))
  }

  # scale
  d <- as.data.frame(scale(d, center = x$center, scale = x$scale))

  if (cluster)
    attr(d, "cluster") <- cl
  if (outlier)
    attr(d, "outlier") <- out
  if (class)
    d <- cbind(d, class = cl)

  d
}

#' @export
reset_stream.DSD_ScaleStream <- function(dsd, pos = 1) {
  reset_stream(dsd$dsd, pos = pos)
}


# internal
scale_stream <-
  function(dsd,
    n = 1000,
    center = TRUE,
    scale = TRUE,
    reset = FALSE) {
    sc <- scale(get_points(dsd, n = n), center = center, scale = scale)
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
