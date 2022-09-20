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

#' Static Cubes Data Stream Generator
#'
#' A data stream generator that produces a data stream with static (hyper)
#' cubes filled uniformly with data points.
#'
#' @family DSD
#'
#' @param k Determines the number of clusters.
#' @param d Determines the number of dimensions.
#' @param center A matrix of means for each dimension of each cluster.
#' @param size A `k` times `d` matrix with the cube dimensions.
#' @param p A vector of probabilities that determines the likelihood of
#' generated a data point from a particular cluster.
#' @param noise Noise probability between 0 and 1.  Noise is uniformly
#' distributed within noise range (see below).
#' @param noise_range A matrix with d rows and 2 columns. The first column
#' contains the minimum values and the second column contains the maximum
#' values for noise.
#' @return Returns a `DSD_Cubes` object (subclass of [DSD_R], [DSD]).
#' @author Michael Hahsler
#' @examples
#' # create data stream with three clusters in 3D
#' stream <- DSD_Cubes(k = 3, d = 3, noise = 0.05)
#'
#' get_points(stream, n = 5)
#'
#' plot(stream)
#' @export
DSD_Cubes <-
  function(k = 2,
    d = 2,
    center,
    size,
    p,
    noise = 0,
    noise_range) {
    # if p isn't defined, we give all the clusters equal probability
    if (missing(p))
      p <- rep(1 / k, k)

    # for each d, random value between 0 and 1
    # we create a matrix of d columns and k rows
    if (missing(center)) {
      center <- matrix(runif(d * k, min = 0.2, max = 0.8), ncol = d)
    } else {
      center <- as.matrix(center)
    }

    # size
    if (missing(size)) {
      size <- matrix(runif(k * d, min = 0.01, max = 0.2), ncol = d)
    }

    # noise
    if (noise == 0)
      noise_range <- NA
    else {
      if (missing(noise_range))
        noise_range <- matrix(c(0, 1),
          ncol = 2,
          nrow = d,
          byrow = TRUE)
      else if (ncol(noise_range) != 2 || nrow(noise_range) != d) {
        stop("noise_range is not correctly specified!")
      }
    }

    # error checking
    if (length(p) != k)
      stop("size of probability vector, p, must equal k")

    if (d < 0)
      stop("invalid number of dimensions")

    if (ncol(center) != d || nrow(center) != k)
      stop("invalid size of mu matrix")

    if (ncol(size) != d || nrow(size) != k)
      stop("invalid size of size matrix")

    l <- list(
      description = paste0("Mixture of (Hyper) Cubes (d = ", d, ", k = ", k, ")"),
      k = k,
      d = d,
      center = center,
      size = size,
      min = center - size,
      max = center + size,
      p = p,
      noise = noise,
      noise_range = noise_range
    )
    class(l) <- c("DSD_Cubes", "DSD_R", "DSD")
    l
  }

#' @export
get_points.DSD_Cubes <- function(x,
  n = 1L,
  info = TRUE,
  ...) {
  .nodots(...)

  if (n < 0L)
    stop("n < 0 not allowed for infinite data stream objects.")

  if (n == 0) {
    data <-
      as.data.frame(matrix(
        nrow = 0,
        ncol = x$d,
        dimnames = list(row = NULL, col = paste0("X", 1:x$d))
      ))

    if (info)
      data[[".class"]] <- integer(0)

    return(data)
  }

  clusterOrder <- sample(
    x = seq(x$k),
    size = n,
    replace = TRUE,
    prob = x$p
  )

  data <- t(sapply(
    clusterOrder,
    FUN = function(i)
      runif(x$d, min = x$min[i,] , max = x$max[i,])
  ))

  ## Replace some points by random noise
  ## TODO: [0,1]^d might not be a good choice. Some clusters can have
  ## points outside this range!
  if (x$noise) {
    repl <- runif(n) < x$noise
    if (sum(repl) > 0) {
      data[repl,] <- t(replicate(
        sum(repl),
        runif(
          x$d,
          min = x$noise_range[, 1],
          max = x$noise_range[, 2]
        )
      ))
      clusterOrder[repl] <- NA
    }
  }

  data <- as.data.frame(data)
  colnames(data) <- c("X1", "X2")

  if (info)
    data[['.class']] <- clusterOrder

  data
}
