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
#' # Simple downsampling example
#' stream <- DSD_Memory(data.frame(rownum = seq(100))) %>% DSF_Downsample(factor = 10)
#' stream
#'
#' get_points(stream, n = 2)
#' get_points(stream, n = 1)
#' get_points(stream, n = 5)
#'
#' # DSD_Memory supports getting the remaining points using n = -1
#' get_points(stream, n = -1)
#'
#' # Downsample a time series
#' data(presidents)
#'
#' stream <- data.frame(
#'     presidents,
#'     .time = time(presidents)) %>%
#'   DSD_Memory()
#'
#' plot(stream, dim = 1, n = 120, method = "ts")
#'
#' # downsample by taking only every 3rd data point (quarters)
#' downsampledStream <- stream %>% DSF_Downsample(factor = 3)
#'
#' reset_stream(downsampledStream)
#' plot(downsampledStream, dim = 1, n = 40, method = "ts")
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
  info = TRUE,
  ...) {
  .nodots(...)

  if (n < 0)
    n_take <- -1L
  else
    n_take <- n * x$factor


  d <-
    get_points(x$dsd,
      n = n_take,
      outofpoints = outofpoints,
      info = info,
      ...)

  take <- seq(1L, nrow(d), by = x$factor)

  d[take, , drop = FALSE]
}
