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


# TODO:
# Additional params
#	- rangeVar (for genPositiveDefMat)
#	- min/max on runif
#
DSD_Gaussians <- function(k=2, d=2, mu, sigma, p, separation=0.2, 
  noise = 0, noise_range) { 
  
  # if p isn't defined, we give all the clusters equal probability
  if (missing(p)) {
    p <- rep(1/k, k)
  }
  
  # for each d, random value between 0 and 1
  # we create a matrix of d columns and k rows
  if (missing(mu)) {
    mu <- matrix(runif(d*k, min=0.2, max=0.8), ncol=d)
    
    if(separation>0) {
      i <- 100L
      while(any(dist(mu)<separation)){
        mu <- matrix(runif(d*k, min=0.2, max=0.8), ncol=d)
        i <- i - 1L
        if(i<1L) stop("Unable to find centers with sufficient separation!")
      }
    }
    
  } else {
    mu <- as.matrix(mu)
  }
  
  # covariance matrix
  if (missing(sigma)) {
    sigma <- replicate(k,clusterGeneration::genPositiveDefMat(
      "unifcorrmat", 
      rangeVar=c(0.001,0.01), 
      dim=d)$Sigma,
      simplify=F)
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
  
  if (ncol(mu) != d || nrow(mu) != k)
    stop("invalid size of mu matrix")
  
  ## TODO: error checking on sigma
  # list of length k
  # d x d matrix in the list
  
  l <- list(description = "Mixture of Gaussians",
    k = k,
    d = d,
    mu = mu,
    sigma = sigma,
    p = p,
    noise = noise,
    noise_range = noise_range)
  class(l) <- c("DSD_Gaussians","DSD_R", "DSD_data.frame", "DSD")
  l
}

get_points.DSD_Gaussians <- function(x, n=1, 
    outofpoints=c("stop", "warn", "ignore"), 
    cluster = FALSE, class = FALSE, ...) {
  .nodots(...)

  clusterOrder <- sample(x=c(1:x$k), 
    size=n, 
    replace=TRUE, 
    prob=x$p)
  
  data <- t(sapply(clusterOrder, FUN = function(i)
    MASS::mvrnorm(1, mu=x$mu[i,], Sigma=x$sigma[[i]])))			
  
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
  colnames(data) <- paste0("X", 1:ncol(data))
  
  if(cluster) attr(data, "cluster") <- clusterOrder
  if(class) data <- cbind(data, class = clusterOrder)

  data
}
