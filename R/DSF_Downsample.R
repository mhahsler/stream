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

#' Downsample a Data Stream
#'
#' Creates a new stream that reduces the frequency of a given stream by a given factor.
#'
#' @family DSF
#'
#' @param dsd	The input stream as an [DSD] object.
#' @param factor the downsampling factor.
#' @return An object of class `DSF_Downsample` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @examples
#' # memorize a stream
#' stream <- DSD_Memory(DSD_Gaussians(k = 3, noise = 0.05), n = 10)
#' get_points(stream, 10)
#'
#' reset_stream(stream)
#'
#' # create a filter that downsamples by a factor of 2 (returns points 1, 3, 5, ...)
#' downsampledStream <- DSF_Downsample(stream, factor = 2)
#'
#' get_points(downsampledStream, 5)
#' @export
DSF_Downsample <- function(dsd, factor = 1) {
  # creating the DSD object

  factor <- as.integer(factor)

  l <- list(
    dsd = dsd,
    factor = factor,
    description = paste0(dsd$description, "\n  + downsampled by factor ", factor)
  )
  class(l) <- c("DSF_Downsample", "DSF", "DSD_R", "DSD")

  l
}

#' @export
get_points.DSF_Downsample <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  info = FALSE,
  ...) {
  .nodots(...)

  n_take <- n * x$factor
  take <- seq_len(n) * x$factor - x$factor + 1L

  d <-
    get_points(x$dsd, n = n_take, outofpoints = outofpoints, info = info, ...)
  d <- d[take,]

  if (!is.null(attr(d, "cluster")))
    attr(d, "cluster") <- attr(d, "cluster")[take]
  if (!is.null(attr(d, "outlier")))
    attr(d, "outlier") <- attr(d, "outlier")[take]

  d
}
