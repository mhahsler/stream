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
#' \pkg{dplyr} needs to be installed and loaded with `library(dplyr)` before `DSF_dplyr` can be used.
#'
#' Since streams are processed one point or block at a time, only [dplyr::dplyr] operations that work on individual
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
#' `DSF_dplyr()` calls the function using `points %>% <func>` and multiple `dplyr` functions can be applied by
#' using `%>%` between them.
#'
#' @family DSF
#'
#' @param dsd A object of class [DSD].
#' @param func a dplyr expression.
#' @param info logical; does the function also receive and modify the info columns?
#' @return An object of class `DSF_dplyr` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @examples
#' if (require(dplyr)) {
#'
#' library(dplyr)
#'
#' stream <- DSD_Gaussians(k = 3, d = 3)
#' plot(stream, xlim = c(0, 1), ylim = c(0, 1))
#'
#' # 1. Select only columns X1 and X2
#' # 2. filter points by X1 > .5 (Note that the info columns also need to be filtered!)
#' # 3. Add a sum columns
#'
#' stream2 <- stream %>%
#'   DSF_dplyr(select(X1, X2)) %>%
#'   DSF_dplyr(filter(X1 > .5), info = TRUE) %>%
#'   DSF_dplyr(mutate(Xsum = X1 + X2))
#' stream2
#'
#' # Note: you get fewer points because of the filter operation.
#' get_points(stream2, n = 10)
#' plot(stream2, xlim = c(0, 1), ylim = c(0, 1))
#'
#' }
#' @export
DSF_dplyr <-
  function(dsd,
    func = NULL,
    info = FALSE) {
    func <- deparse(substitute(func))

    # creating the DSD object
    l <- list(
      description = paste0(dsd$description, "\n  + function: ", func),
      dsd = dsd,
      func = parse(text = paste('ps <- ps %>%', paste0(func, collapse = ' '))),
      info = info
    )
    class(l) <-
      c("DSF_dplyr", "DSF", "DSD_R", "DSD")

    l
  }

#' @export
get_points.DSF_dplyr <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  info = TRUE,
  ...) {
  .nodots(...)

  points <-
    get_points(x$dsd,
      n = n,
      outofpoints = outofpoints,
      info = TRUE,
      ...)

  if (x$info) {
    ps <- points
    eval(x$func)
    return(ps)
  } else {
    points <- split_info(points)
    ps <- points$points
    eval(x$func)
    ps <- cbind(ps, points$info)
    return(ps)
  }
}
