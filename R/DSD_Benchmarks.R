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

#' Data Stream Generator for Dynamic Data Stream Benchmarks
#'
#' A data stream generator that generates several dynamic streams indented to
#' be benchmarks to compare data stream clustering algorithms.  The benchmarks
#' can be used to test if a clustering algorithm can follow moving clusters, and merging and
#' separating clusters.
#'
#' Currently available benchmarks are:
#'   - `1`: two tight clusters moving across the data space with noise and intersect in the middle.
#'   - `2`: two clusters are located in two corners of the data space. A third cluster moves between the two clusters
#'     forth and back.
#'
#' The benchmarks are created using [DSD_MG].
#'
#' @family DSD
#'
#' @param i integer; the number of the benchmark.
#' @return Returns a [DSD] object.
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Benchmark(i = 1)
#' get_points(stream, n = 5)
#'
#' \dontrun{
#' stream <- DSD_Benchmark(i = 1)
#' animate_data(stream, n = 10000, horizon = 100, xlim = c(0, 1), ylim = c(0, 1))
#'
#' stream <- DSD_Benchmark(i = 2)
#' animate_data(stream, n = 10000, horizon = 100, xlim = c(0, 1), ylim = c(0, 1))
#' }
#' @export
DSD_Benchmark <- function(i = 1) {
  if (i == 1) {
    return(
      DSD_MG(
        dimension = 2,
        MGC_Linear(dimension = 2, list(
          keyframe(
            time = 0,
            density = 10,
            parameter = .01,
            center = c(.1, .9)
          ),
          keyframe(
            time = 250,
            density = 10,
            parameter = .01,
            center = c(.9, .1)
          ),
          keyframe(
            time = 500,
            density = 10,
            parameter = .01,
            center = c(.1, .9),
            reset = TRUE
          )
        )),
        MGC_Linear(dimension = 2, list(
          keyframe(
            time = 0,
            density = 10,
            parameter = .01,
            center = c(.1, .1)
          ),
          keyframe(
            time = 250,
            density = 10,
            parameter = .01,
            center = c(.9, .9)
          ),
          keyframe(
            time = 500,
            density = 10,
            parameter = .01,
            center = c(.1, .1),
            reset = TRUE
          )
        )),
        MGC_Noise(density = 2, range = rbind(c(0, 1), c(0, 1))),
        labels = c(1, 2, NA),
        description = "Benchmark 1: Two clusters moving diagonally from left to right, meeting in the center (d = 2, k = 2, 5% noise)."
      )
    )
  }


  if (i == 2) {
    return(
      DSD_MG(
        dimension = 2,
        MGC_Static(
          density = 1,
          parameter = .1,
          center = c(0, 0)
        ),
        MGC_Static(
          density = 1,
          parameter = .1,
          center = c(1, 1)
        ),
        MGC_Linear(dimension = 2, list(
          keyframe(
            time = 0,
            density = 1,
            parameter = .1,
            center = c(0, 0)
          ),
          keyframe(
            time = 500,
            density = 1,
            parameter = .1,
            center = c(1, 1)
          ),
          keyframe(
            time = 1000,
            density = 1,
            parameter = .1,
            center = c(0, 0),
            reset = TRUE
          )
        )),
        description = "Benchmark 2: Two fixes clusters. A third cluster moves between them (d = 2, k = 3, no noise)."
      )
    )
  }

  stop("Unknown benchmark!")
}
