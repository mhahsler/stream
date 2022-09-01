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

#' Mixture of Gaussians Data Stream Generator
#'
#' A data stream generator that produces a data stream with a mixture of static
#' Gaussians.
#'
#' `DSD_Gaussians` creates a mixture of `k` static clusters in a `d`-dimensional
#' space. The cluster
#' centers `mu` and the covariance matrices `sigma` can be supplied
#' or will be randomly generated. The probability vector `p` defines for
#' each cluster the probability that the next data point will be chosen from it
#' (defaults to equal probability). Separation between generated clusters (and outliers; see below)
#' can be imposed by using
#' Euclidean or Mahalanobis distance, which is controlled by the
#' `separation_type` parameter. Separation value then is supplied in the
#' `separation` parameter.
#' The generation method is similar to the one
#' suggested by Jain and Dubes (1988).
#'
#' Noise points which are uniformly chosen from `noise_limit` can be added.
#'
#' Outlier points can be added. The outlier spatial positions
#' `predefined_outlier_space_positions` and the outlier stream positions
#' `predefined_outlier_stream_positions` can be supplied or will be
#' randomly generated. Cluster and outlier separation distance is determined by
#'  and `outlier_virtual_variance` parameters. The
#' outlier virtual variance defines an empty space around outliers, which
#' separates them from their surrounding. Unlike noise, outliers are data
#' points of interest for end-users, and the goal of outlier detectors is to
#' find them in data streams. For more details, read the "Introduction to
#' \pkg{stream}" vignette.
#'
#' @family DSD
#'
#' @param k Determines the number of clusters.
#' @param d Determines the number of dimensions.
#' @param mu A matrix of means for each dimension of each cluster.
#' @param sigma A list of length `k` of covariance matrices.
#' @param p A vector of probabilities that determines the likelihood of
#' generated a data point from a particular cluster.
#' @param noise Noise probability between 0 and 1.  Noise is uniformly
#' distributed within noise range (see below).
#' @param noise_limit A matrix with d rows and 2 columns. The first column
#' contains the minimum values and the second column contains the maximum
#' values for noise.
#' @param noise_separation Minimum separation distance between cluster centers and noise
#' points (measured in standard deviations according to `separation_type`). `0` means separation is ignored.
#' @param separation_type The type of the separation distance calculation. It
#' can be either Euclidean distance or Mahalanobis distance.
#' @param separation Minimum separation distance between clusters
#' (measured in standard deviations according to `separation_type`).
#' @param space_limit Defines the space bounds. All constructs are generated
#' inside these bounds. For clusters this means that their centroids must be
#' within these space bounds.
#' @param variance_limit Lower and upper limit for the randomly generated variance when
#' creating cluster covariance matrices.
#' @param verbose Report cluster and outlier generation process.
#'
#' @return Returns a  object of class `DSD_Gaussian` (subclass of [DSD_R], [DSD]).
#'
#' @author Michael Hahsler
#' @references
#' Jain and Dubes (1988) Algorithms for clustering data,
#' Prentice-Hall, Inc., Upper Saddle River, NJ, USA.
#' @examples
#' # Example 1: create data stream with three clusters in 3-dimensional data space
#' #            with 5 times sqrt(variance_limit) separation.
#' set.seed(1)
#' stream1 <- DSD_Gaussians(k = 3, d = 3)
#' stream1
#'
#' get_points(stream1, n = 5)
#' plot(stream1, xlim = c(0, 1), ylim = c(0, 1))
#'
#'
#' # Example 2: create data stream with specified cluster positions,
#' # 5% noise in a given bounding box and
#' # with different densities (1 to 9 between the two clusters)
#' stream2 <- DSD_Gaussians(k = 2, d = 2,
#'     mu = rbind(c(-.5, -.5), c(.5, .5)),
#'     p = c(.1, .9),
#'     variance_limit = c(0.02, 0.04),
#'     noise = 0.05,
#'     noise_limit = rbind(c(-1, 1), c(-1, 1)))
#'
#' get_points(stream2, n = 5)
#' plot(stream2, xlim = c(-1, 1), ylim = c(-1, 1))
#'
#'
#' # Example 3: create 4 clusters and noise separated by a Mahalanobis
#' # distance. Distance to noise is increased to 6 standard deviations to make them
#' # easier detectable outliers.
#' stream3 <- DSD_Gaussians(k = 4, d = 2,
#'   separation_type = "Mahalanobis",
#'   space_limit = c(5, 20),
#'   variance_limit = c(1, 2),
#'   noise = 0.05,
#'   noise_limit = c(0, 25),
#'   noise_separation = 6
#'   )
#' plot(stream3)
#' @export
DSD_Gaussians <-
  function(k = 3,
    d = 2,
    p,
    mu,
    sigma,
    variance_limit = c(.001, .002),
    separation = 6,
    space_limit = c(0, 1),
    noise = 0,
    noise_limit = space_limit,
    noise_separation = 3,
    separation_type = c("Euclidean", "Mahalanobis"),
    verbose = FALSE) {
    separation_type <-
      match.arg(separation_type)


    # if p is not defined, we give all the clusters equal probability
    if (missing(p))
      p <- rep(1 / k, k)

    if (separation_type == "Euclidean") {
      separation = separation * sqrt(mean(variance_limit))
      noise_separation = noise_separation * sqrt(mean(variance_limit))
    }

    # covariance matrix
    if (missing(sigma)) {
      if (separation_type == "Euclidean") {
        sigma <- replicate(
          k,
          clusterGeneration::genPositiveDefMat(
            "unifcorrmat",
            rangeVar = variance_limit,
            dim = d
          )$Sigma,
          simplify = FALSE
        )
      }

      if (separation_type == "Mahalanobis") {
        genRandomSigma <- function(d, vlim) {
          tmpS <- matrix(
            data = rep(0, length = d ^ 2),
            ncol = d,
            nrow = d
          )
          diag(tmpS) <-
            replicate(d, runif(1, min = vlim[1L], max = vlim[2L]))
          for (i in 1:d)
            for (j in i:d)
              if (i != j)
                tmpS[i, j] <-
            tmpS[j, i] <-
            runif(1, min = 0, max = 0.5) * sqrt(tmpS[i, i]) * sqrt(tmpS[j, j])
          tmpS
        }

        sigma <-
          replicate(k, genRandomSigma(d, variance_limit), simplify = FALSE)
      }
    }

    # prepare inverted covariance matrices / only for Mahalanobis
    inv_sigma <- NULL
    if (separation_type == "Mahalanobis") {
      inv_sigma <- list()
      for (i in 1:length(sigma))
        inv_sigma[[i]] <- MASS::ginv(sigma[[i]])
    }

    if (missing(mu)) {
      # create one centroid at a time and add it if it has separation to all
      # existing centroids.
      mu <- matrix(nrow = 0, ncol = d)
      mu_index <- 1L

      while (mu_index <= k) {
        if (verbose)
          cat(paste("Random cluster centers", mu_index))

        # stop after 1000 tries and give up.
        i <- 1L
        repeat {
          centroid <-
            rbind(runif(d, min = space_limit[1L], max = space_limit[2L]))
          if (verbose)
            cat(paste(
              "... try",
              i,
              "cluster centroid [",
              paste(centroid, collapse = ","),
              "]"
            ))

          if (is.separated(centroid, mu, separation, method = separation_type, inv_sigma))
            break

          i <- i + 1L
          if (i > 1000L)
            stop("Unable to find set of clusters with sufficient separation!")
        }


        mu <- rbind(mu, centroid)
        mu_index <- mu_index + 1L
      }
    }

    mu <- as.matrix(mu)

    ## noise
    noise <- max(0, noise)

    if (noise > 0) {
      if (is.vector(noise_limit))
        noise_limit <-
          matrix(rep(noise_limit, times = d),
            nrow = d,
            byrow = TRUE)

      if (ncol(noise_limit) != 2L || nrow(noise_limit) != d)
        stop("noise_limit is not correctly specified!")
    } else
      noise_limit <- NA

    if (length(p) != k)
      stop("size of probability vector, p, must equal k")

    if (d < 0L)
      stop("invalid number of dimensions")

    if (ncol(mu) != d || nrow(mu) != k)
      stop("invalid size of the mu matrix")

    if (length(sigma) != k ||
        any(sapply(
          sigma,
          FUN = function(s)
            dim(s) != c(d, d)
        )))
      stop(
        "Sigma does not have the correct number of covariance matrices or matrices are malformed."
      )

    l <- list(
      description = paste0("Gaussian Mixture (d = ", d, ", k = ", k, ")"),
      k = k,
      d = d,
      p = p,
      mu = mu,
      sigma = sigma,
      inv_sigma = inv_sigma,
      noise = noise,
      noise_separation = noise_separation,
      noise_limit = noise_limit,
      separation_type = separation_type
    )
    class(l) <- c("DSD_Gaussians", "DSD_R", "DSD")
    l
  }


is.separated <-
  function(p,
    mu,
    separation,
    method = c("Euclidean", "Mahalanobis"),
    inv_sigma = NULL) {
    method <- match.arg(method)
    if (separation <= 0 || nrow(mu) == 0L)
      return(TRUE)

    if (method == "Euclidean") {
      if (any(dist(p, mu) <= separation))
        return(FALSE)
      else
        return(TRUE)
    }

    if (method == "Mahalanobis") {
      if (is.null(inv_sigma))
        stop("Inverted covariance missing.")

      d <- sqrt(sapply(
        seq(nrow(mu)),
        FUN = function(i)
          stats::mahalanobis(p, mu[i, , drop = TRUE], inv_sigma[[i]], inverted = TRUE)
      ))

      if (any(d <= separation))
        return(FALSE)
      else
        return(TRUE)
    }

    stop("Unknown separation method!")
  }

#' @export
get_points.DSD_Gaussians <- function(x,
  n = 1L,
  info = TRUE,
  ...) {
  .nodots(...)

  if(n < 1L)
    stop("n needs to be >= 1.")

  noise_pos <- NULL

  cluster_id <-
    sample(
      x = c(1:x$k),
      size = n,
      replace = TRUE,
      prob = x$p
    )

  data <- t(sapply(
    cluster_id,
    FUN = function(i)
      MASS::mvrnorm(1, mu = x$mu[i, ], Sigma = x$sigma[[i]])
  ))

  ## fix for d==1
  if (x$d == 1)
    data <- t(data)

  ## Replace some points by random noise
  ## TODO: [0,1]^d might not be a good choice. Some clusters can have
  ## points outside this range!
  if (x$noise > 0) {
    noise_pos <- runif(n) < x$noise
    n_noise <- sum(noise_pos)

    if (n_noise > 0) {
      nps <- t(replicate(n_noise, {
        i <- 1L
        repeat {
          pnt <- runif(x$d,
            min = x$noise_limit[, 1],
            max = x$noise_limit[, 2])

          if (is.separated(rbind(pnt),
            x$mu,
            x$noise_separation,
            x$separation_type,
            x$inv_sigma))
            break

          i <- i + 1L
          if (i > 1000L)
            stop("Unable to create a noise point with sufficient separation!")

        }
        pnt
      }))

      data[noise_pos, ] <- nps
      cluster_id[noise_pos] <- NA
    }
  }

  data <- as.data.frame(data)
  colnames(data) <- paste0("X", 1:ncol(data))

  if (info)
    data[[".class"]] <- cluster_id

  data
}

