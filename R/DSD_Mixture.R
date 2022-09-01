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


#' Mixes Data Points from Several Streams into a Single Stream
#'
#' This generator mixes multiple streams given specified probabilities.
#' The streams have to contain the same number of dimensions.
#'
#' @family DSD
#'
#' @param ... [DSD] objects.
#' @param prob a numeric vector with the probability for each stream that the next point will
#'   be drawn from that stream.
#' @return Returns a `DSD_Mixture` object.(subclass of [DSD_R], [DSD]).
#' @author Michael Hahsler
#' @examples
#' # create data stream with three clusters in 2D
#' stream1 <- DSD_Gaussians(d = 2, k = 3)
#' stream2 <- DSD_UniformNoise(d = 2,  range = rbind(c(-.5, 1.5), c(-.5, 1.5)))
#'
#' combinedStream <- DSD_Mixture(stream1, stream2, prob = c(.9, .1))
#' combinedStream
#'
#' get_points(combinedStream, n = 20)
#' plot(combinedStream, n = 200)
#' @export
DSD_Mixture <- function(..., prob = NULL) {
  streams <- list(...)

  if (is.null(prob))
    prob <- rep(1, length(streams))
  prob <- prob / sum(prob)

  structure(
    list(
      description = paste0(
        "Stream Mixture (d = ",
        streams[[1]]$d,
        ")\n",
        paste(paste("+", sapply(streams, "[[", "description")), collapse = "\n")
      ),
      d = streams[[1]]$d,
      streams = streams,
      prob = prob
    ),
    class = c("DSD_Mixture", "DSD_R", "DSD")
  )
}

#' @export
get_points.DSD_Mixture <- function(x,
  n = 1,
  info = TRUE,
  ...) {
  .nodots(...)


  stream_id <-
    sample(
      seq_along(x$streams),
      size = n,
      replace = TRUE,
      prob = x$prob
    )

  points <- NULL

  tbl <- table(stream_id)

  for (str_id in names(tbl)) {
    pts <-
      get_points(x$streams[[as.integer(str_id)]], n = tbl[str_id], info = info)
    pts[[".stream"]] <- as.integer(str_id)

    if (is.null(points)) {
      points <- pts[0,]
      points[n, 1] <- NA
    }
    points[stream_id == as.integer(str_id),] <- pts
  }

  if (!info)
    points <- remove_info(points)

  points
}
