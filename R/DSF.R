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


#' Data Stream Filter Base Classes
#'
#' Abstract base classes for all data stream filter (DSF) classes.
#' Data stream filters transform a data stream ([DSD]).
#'
#' The `DSF` class cannot be instantiated, but it serve as a base
#' class from which other DSF classes inherit.
#'
#' Data stream filters transform a [DSD] data stream. `DSF` can be used in two ways.
#'
#' 1. **DSD Adapter:**
#'    When a data stream (`dsd`) is specified in the constructor, then the DSF acts as an a
#'    adapter for a connected data stream. The DSF implementations inherit the
#'    interface from [DSD] and provide:
#'
#'    - [get_points()] get the transformed points.
#'    - [reset_stream()] reset the underlying stream
#'    - [close_stream()] close the underlying stream
#'
#' 2. **Stream Transformer:**
#'    When no data stream (`dsd`) is specified in the constructor, then the DSF acts like a
#'    [DST] data stream task and provides:
#'
#'    - [update()] to transform the points from a specified `DSD`.

#' It is convenient to use the pipe ([magrittr::%>%]) to apply one or more filters to data
#' streams (see Examples section).
#'
#' @family DSF
#' @family DSD
#'
#' @param x,object a `DSF` object.
#' @param n number of points to get/use for the update.
#' @param info return additional columns with information about the data point (e.g., a known cluster assignment).
#' @param return a character string indicating what update returns. The only
#' value is currently  `"data"` to return the transformed data.
#' possible values depend on the `DST`.
#' @param ... Further arguments passed on.
#' @author Michael Hahsler
#' @examples
#' DSF()
#'
#' # Example 1: Use as a DSD adapter
#' stream <- DSD_Gaussians(k = 3, d = 2) %>%
#'   DSF_Func(func = function(x) cbind(x, Xsum = x$X1 + x$X2))
#' stream
#'
#' get_points(stream, n = 5)
#'
#' # Example 2: Use as a stream transformer
#' trans <- DSF_Func(func = function(x) cbind(x, Xsum = x$X1 + x$X2))
#' trans
#'
#' update(trans, stream, n = 5)
#'
#' # Example 3: Use as a DST preprocessor
#' clusterer <- DSF_Func(func = function(x) cbind(x, X1_squared = x$X1^2)) %>%
#'                DST_Runner(DSC_Kmeans(k = 3))
#' clusterer
#'
#' update(clusterer, stream, n = 100)
#'
#' # Example 5: Specify a complete pipeline DSD -> DSF -> DST
#' pipeline <- DSD_Gaussians(k = 3, d = 2) %>%
#'                DSF_Func(func = function(x) cbind(x, X1_squared = x$X1^2)) %>%
#'                DST_Runner(DSC_Kmeans(k = 3))
#' pipeline
#'
#' update(pipeline, n = 100)
#' plot(pipeline$dst)
#' @export DSF
DSF <- abstract_class_generator("DSF")

#' @describeIn DSF reset the attached stream if reset is supported.
#' @param dsd a stream object of class [DSD].
#' @param pos position in the stream.
#' @export
reset_stream.DSF <- function(dsd, pos = 1)
  reset_stream(dsd$dsd, pos = pos)


#' @describeIn DSF DSD-like interface to get points if the DSF was created with an attached stream.
#' @export
get_points.DSF <- function(x,
  n = 1L,
  info = TRUE,
  ...)
  update(x,
    dsd = NULL,
    n = n,
    info = info,
    ...)

#' @describeIn DSF updates with data and returns the filtered data.
#' @export
update.DSF <- function(object,
  dsd = NULL,
  n = 1L,
  return = "data",
  ...)
  stop("Not implemented for class", paste(class(object), collapse = ", "))

#' @describeIn DSF close the attached stream if close is supported.
#' @export
close_stream.DSF <- function(dsd, ...)
  close_stream(dsd$dsd, ...)
