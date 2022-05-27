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

#' mlbench Data Stream Generator
#'
#' A data stream generator class that interfaces data generators found in
#' package `mlbench`.
#'
#' The `DSD_mlbenchGenerator` class is designed to be a wrapper class for
#' data created by data generators in the `mlbench` library.
#'
#' Call `DSD_mlbenchGenerator` with missing method to get a list of
#' available methods.
#'
#' @family DSD
#'
#' @param method The name of the mlbench data generator. If missing then a list of
#' all available generators is shown and returned.
#' @param ... Parameters for the mlbench data generator.
#' @return Returns a `DSD_mlbenchGenerator` object (subclass of
#' [DSD_R], [DSD])
#' @author John Forrest
#' @examples
#' DSD_mlbenchGenerator()
#'
#' stream <- DSD_mlbenchGenerator(method = "cassini")
#' stream
#'
#' get_points(stream, n = 5, info = TRUE)
#'
#' plot(stream, n = 500)
#' @export
DSD_mlbenchGenerator <- function(method, ...) {
  methods <- c(
    "2dnormals",
    "cassini",
    "circle",
    "cuboids",
    "friedman1",
    "friedman2",
    "friedman3",
    "hypercube",
    "peak",
    "ringnorm",
    "shapes",
    "simplex",
    "smiley",
    "spirals",
    "threenorm",
    "twonorm",
    "waveform",
    "xor"
  )

  ### FIXME: It would be nice if we know k and d

  if (missing(method)) {
    cat("Available generators are:\n")
    print(methods)
    return(invisible(methods))
  }

  #finds index of partial match in array of methods
  m <- pmatch(tolower(method), methods)
  if (is.na(m))
    stop("DSD_mlbenchGenerator: Invalid data generator")

  # creating the DSD object
  l <- list(
    description = paste("mlbench:", method),
    method = methods[m],
    variables = list(...)
  )
  class(l) <-
    c("DSD_mlbenchGenerator", "DSD_R", "DSD")
  l
}

#' @export
get_points.DSD_mlbenchGenerator <- function(x,
  n = 1,
  outofpoints = "stop",
  info = FALSE,
  ...) {
  .nodots(...)

  d <-
    do.call(paste("mlbench.", x$method, sep = ""), c(list(n), x$variables))
  if (is.null(d$classes))
    d$classes <- rep(NA_integer_, times = n)

  ## the data order needs to be scrambled
  if (n > 1) {
    o <- sample(nrow(d$x))
    d$x <- d$x[o, , drop = FALSE]
    d$classes <- d$classes[o]
  }

  dat <- as.data.frame(d$x)
  colnames(dat) <- paste0("V", 1:ncol(dat))

  if (info)
    dat[['.class']] <- as.integer(d$classes)

  dat
}
