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

#' Apply a dplyr Transformation to a Data Stream
#'
#' Applies dplyr transformations to a data stream.
#'
#' Since streams are processed on point or block o at a time, only [dplyr] operations that work on individual
#' rows are allowed on streams. Examples are:
#'
#' * [dplyr::select()]
#' * [dplyr::mutate()]
#' * [dplyr::rename()]
#' * [dplyr::transmute()]
#' * [dplyr::filter()]
#'
#' Summary functions can be used, but will only be applied to the requested part of the stream of length `n`.
#'
#' @family DSF
#'
#' @param dsd A object of class [DSD].
#' @param call a dplyr expression.
#' @return An object of class `DSF_dplyr` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @seealso [stats::filter]
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 3)
#'
#' stream2 <- stream %>%
#'   DSF_dplyr(select(X1, X2)) %>%
#'   DSF_dplyr(mutate(Xnew = X1 + X2)) %>%
#'   DSF_dplyr(filter(X1 > .5))
#' stream2
#'
#' get_points(stream2, n = 10)
#' ## Note: you get fewer points because of the filter operation.
#'
#' @export
DSF_dplyr <-
  function(dsd,
    call = NULL) {
    call <- substitute(call)

    # creating the DSD object
    l <- list(
      description = paste0(dsd$description, "\n\t + dplyr transformation: ", deparse(call)),
      dsd = dsd,
      call = call
    )
    class(l) <-
      c("DSF_dplyr", "DSF", "DSD_R", "DSD_data.frame", "DSD")

    l
  }

#' @export
get_points.DSF_dplyr <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  cluster = FALSE,
  class = FALSE,
  outlier = FALSE,
  ...) {
  .nodots(...)

  ps <- get_points(x$dsd, n = n, outofpoints = outofpoints, cluster = cluster, class= class, outlier = outlier, ...)

  eval(parse(text = paste('ps <- ps %>%', deparse(x$call))))
  ps
}
