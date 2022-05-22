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

#' Apply a Function to Transformation to a Data Stream
#'
#' Applies an R function to transform to a data stream.
#'
#' The function's first argument needs to be a data.frame representing points of the
#' data stream. The function will be called as `ps %>% your_function()`, where `ps` is the
#' data.frame with some points obtained using [get_points()] on the data stream source.
#'
#' @family DSF
#'
#' @param dsd A object of class [DSD].
#' @param func a function that takes a data.frame as the first argument and returns the transformed data.frame.
#' @return An object of class `DSF_Func` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 3)
#'
#' ## Example 1: rename the columns
#' rename <- function(x, names) {
#'   colnames(x) <-  names
#'   x
#' }
#'
#' stream2 <- stream %>% DSF_Func(rename(c("A", "B", "C")))
#' stream2
#'
#' get_points(stream2, n = 5)
#'
#' ## Example 2: add a sum columns
#' sum_column <- function(x) {
#'   x$sum = rowSums(x)
#'  x
#' }
#'
#' stream3 <- stream2 %>% DSF_Func(sum_column())
#' stream3
#' get_points(stream3, n = 5)
#' @export
DSF_Func <-
  function(dsd,
    func = NULL) {
    func <- deparse(substitute(func))

    # creating the DSD object
    l <- list(
      description = paste0(dsd$description, "\n  + function: ", func),
      dsd = dsd,
      func = parse(text = paste('ps <- ps %>%', func))
    )
    class(l) <-
      c("DSF_Func", "DSF", "DSD_R", "DSD_data.frame", "DSD")

    l
  }

#' @export
get_points.DSF_Func <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  cluster = FALSE,
  class = FALSE,
  outlier = FALSE,
  ...) {
  .nodots(...)

  ps <- get_points(x$dsd, n = n, outofpoints = outofpoints, cluster = cluster, class= class, outlier = outlier, ...)
  eval(x$func)
  ps
}
