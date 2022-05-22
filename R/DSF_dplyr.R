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
#' Since streams are processed on point or block o at a time, only [dplyr::dplyr] operations that work on individual
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
#' `DSF_dplyr()` is just an alias for [DSF_Func()].
#'
#' @family DSF
#'
#' @param dsd A object of class [DSD].
#' @param func a dplyr expression.
#' @return An object of class `DSF_dplyr` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @examples
#' if (require(dplyr)) {
#'
#' stream <- DSD_Gaussians(k = 3, d = 3)
#'
#' stream2 <- stream %>%
#'   DSF_dplyr(select(X1, X2)) %>%
#'   DSF_dplyr(filter(X1 > .5)) %>%
#'   DSF_dplyr(mutate(Xsum = X1 + X2))
#' stream2
#'
#' get_points(stream2, n = 10)
#' ## Note: you get fewer points because of the filter operation.
#'
#' }
#' @export
DSF_dplyr <- DSF_Func
