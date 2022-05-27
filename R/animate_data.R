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


#' Animates the Plotting of a Data Streams
#'
#' Generates an animation of a data stream.
#'
#' Animations are recorded using the library animation and can be replayed
#' (which gives a smoother experience since the is no more computation done)
#' and saved in various formats (see Examples section below).
#'
#' **Note:** You need to install package \pkg{animation} and
#' its system requirements.
#'
#' @aliases animate animation
#' @family DSD
#' @family plot
#'
#' @param dsd a DSD object
#' @param horizon the number of points displayed at once/used for evaluation.
#' @param n the number of points to be plotted
#' @param wait the time interval between each frame
#' @param plot.args a list with plotting parameters for the clusters.
#' @param ... extra arguments are added to `plot.args`.
#' @author Michael Hahsler
#' @seealso [animation::ani.replay()] for replaying and saving animations.
#' @examples
#' if (interactive) {
#'
#' stream <- DSD_Benchmark(1)
#' animate_data(stream, horizon = 100, n = 5000, xlim = c(0,1), ylim = c(0,1))
#'
#' ### animations can be replayed with the animation package
#' library(animation)
#' animation::ani.options(interval = .1) ## change speed
#' ani.replay()
#'
#' ### animations can also be saved as HTML, animated gifs, etc.
#' saveHTML(ani.replay())
#' }
#' @export
animate_data <- function(dsd,
  horizon = 100,
  n = 1000,
  wait = .1,
  plot.args = NULL,
  ...) {
  cluster.ani(NULL,
    dsd,
    NULL,
    horizon,
    n,
    NULL,
    NULL,
    NULL,
    NULL,
    wait,
    plot.args,
    ...)
}
