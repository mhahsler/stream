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

#' Target Data Stream Generator
#'
#' A data stream generator that generates a data stream in the shape of a
#' target. It has a single Gaussian cluster in the center and a ring that
#' surrounds it.
#'
#' This DSD will produce a singular Gaussian cluster in the center with a ring around
#' it.
#'
#' @family DSD
#'
#' @param center_sd standard deviation of center
#' @param center_weight proportion of points in center
#' @param ring_r average ring radius
#' @param ring_sd standard deviation of ring radius
#' @param noise proportion of noise
#' @return Returns a `DSD_Target` object.
#' @author Michael Hahsler
#' @examples
#' # create data stream with three clusters in 2D
#' stream <- DSD_Target()
#'
#' get_points(stream, n = 5, info = TRUE)
#'
#' plot(stream)
#' @export
DSD_Target <- function(center_sd = .05,
  center_weight = .5,
  ring_r = .2,
  ring_sd = 0.02,
  noise = 0) {
  # creating the DSD object
  l <- list(
    description = "Target (ball in circle)",
    d = 2,
    k = 2,
    center_sd = center_sd,
    center_weight = center_weight,
    ring_r = ring_r,
    ring_sd = ring_sd,
    noise = noise
  )
  class(l) <- c("DSD_Target", "DSD_R", "DSD")
  l
}


#' @export
get_points.DSD_Target <- function(x,
  n = 1,
  outofpoints = "stop",
  info = FALSE,
  ...) {
  .nodots(...)


  ### choose point type
  type <- sample(
    c(NA, 1:2),
    n,
    replace = TRUE,
    prob = c(
      x$noise,
      (1 - x$noise) * x$center_weight,
      (1 - x$noise) * (1 - x$center_weight)
    )
  )

  p <- sapply(
    type,
    FUN = function(tp) {
      if (is.na(tp)) {
        ### noise
        p <- runif(2,-x$ring_r - 5 * x$ring_sd, x$ring_r + 5 * x$ring_sd)
      } else if (tp == 1) {
        ### ball
        p <- rnorm(2, sd = x$center_sd)
      } else if (tp == 2) {
        ### circle
        r <- x$ring_r + rnorm(1, sd = x$ring_sd)
        angle <- runif(1, 0, 2 * pi)
        p <- c(cos(angle) * r, sin(angle) * r)
      }
      p
    }
  )


  p <- as.data.frame(t(p))
  colnames(p) <- c("x", "y")

  if (info) 
    p[['.class']] <- type

  p
}
