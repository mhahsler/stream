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
#' 
#' @param k Determines the number of clusters.
#' @param d Determines the number of dimensions.
#' @param center A matrix of means for each dimension of each cluster.
#' @param size A \code{k} times \code{d} matrix with the cube dimensions.
#' @param p A vector of probabilities that determines the likelihood of
#' generated a data point from a particular cluster.
#' @param noise Noise probability between 0 and 1.  Noise is uniformly
#' distributed within noise range (see below).
#' @param noise_range A matrix with d rows and 2 columns. The first column
#' contains the minimum values and the second column contains the maximum
#' values for noise.
#' @return Returns a \code{DSD_Cubes} object (subclass of \code{DSD_R},
#' \code{DSD}).
#' @author Michael Hahsler
#' @seealso \code{\link{DSD}}
#' @examples
#' 
#' # create data stream with three clusters in 3D
#' stream <- DSD_Cubes(k=3, d=3)
#' 
#' # plotting the data
#' plot(stream)
#' 
#' @export DSD_Cubes
DSD_Cubes <- function(k=2, d=2, center, size, p, noise = 0, noise_range) {

  # if p isn't defined, we give all the clusters equal probability
  if (missing(p)) p <- rep(1/k, k)

  # for each d, random value between 0 and 1
  # we create a matrix of d columns and k rows
  if (missing(center)) {
    center <- matrix(runif(d*k, min=0.2, max=0.8), ncol=d)
  } else {
    center <- as.matrix(center)
  }

  # size
  if (missing(size)) {
    size <- matrix(runif(k*d, min=0.01, max=0.2), ncol=d)
  }

  # noise
  if (noise == 0) noise_range <- NA
  else {
    if (missing(noise_range)) noise_range <- matrix(c(0,1),
                                                    ncol=2, nrow=d, byrow=TRUE)
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

  l <- list(description = "Mixture of (Hyper) Cubes",
            k = k,
            d = d,
            center = center,
            size = size,
            min = center-size,
            max = center+size,
            p = p,
            noise = noise,
            noise_range = noise_range)
  class(l) <- c("DSD_Cubes", "DSD_R", "DSD_data.frame", "DSD")
  l
}

get_points.DSD_Cubes <- function(x, n=1,
  outofpoints=c("stop", "warn", "ignore"),
  cluster = FALSE, class = FALSE, outlier = FALSE, ...) {
  .nodots(...)

  clusterOrder <- sample(x=c(1:x$k),
                         size=n,
                         replace=TRUE,
                         prob=x$p)

  data <- t(sapply(clusterOrder, FUN = function(i)
    runif(x$d, min=x$min[i,] ,max=x$max[i,])))

  ## Replace some points by random noise
  ## TODO: [0,1]^d might not be a good choice. Some clusters can have
  ## points outside this range!
  if(x$noise) {
    repl <- runif(n)<x$noise
    if(sum(repl)>0) {
      data[repl,] <- t(replicate(sum(repl),runif(x$d,
                                                 min=x$noise_range[,1],
                                                 max=x$noise_range[,2])))
      clusterOrder[repl] <- NA
    }
  }

  data <- as.data.frame(data)
  if(cluster) attr(data, "cluster") <- clusterOrder
  if(class) data <- cbind(data, class = clusterOrder)

  data
}
