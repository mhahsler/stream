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
#' Noise points which are uniformly chosen from `noise_range` can be added.
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
#' @param noise_range A matrix with d rows and 2 columns. The first column
#' contains the minimum values and the second column contains the maximum
#' values for noise.
#' @param separation_type The type of the separation distance calculation. It
#' can be either Euclidean norm or Mahalanobis distance.
#' @param separation Depends on the `separation_type` parameter. It means
#' minimum separation distance between clusters (and outliers, if any).
#' @param space_limit Defines the space bounds. All constructs are generated
#' inside these bounds. For clusters this means that their centroids must be
#' within these space bounds.
#' @param variance_limit Upper limit for the randomly generated variance when
#' creating cluster covariance matrices.
#' @param outliers Determines the number of outlier data points created.
#' Outliers generated are statistically separated
#' enough from clusters, so that outlier detectors can find them in the overall
#' data stream.
#' @param outlier_options A list with the following options:
#'    - `predefined_outlier_space_positions` A list of predefined outlier spatial positions.
#'    - `predefined_outlier_stream_positions` A list of predefined outlier stream positions.
#'    - `outlier_horizon` The horizon in the generated data stream in which the requested number of
#'      outliers is added.
#'    - `outlier_virtual_variance` The variance used to create the virtual covariance matrices for outliers. Such virtual
#'      statistical distribution helps to define an empty space around outliers that
#'      separates them from other constructs, both clusters and outliers.
#' @param verbose Report cluster and outlier generation process.
#' @return Returns a  object (subclass of `DSD_R`, `DSD`)
#' which is a list of the defined params. The params are either
#' passed in from the function or created internally. They include:
#'
#' \item{description}{A brief description of the DSD object.}
#'
#' \item{k}{The number of clusters.}
#'
#' \item{d}{The number of dimensions.}
#'
#' \item{mu}{The matrix of means of the dimensions in each cluster.}
#'
#' \item{sigma}{The covariance matrix.}
#'
#' \item{p}{The probability vector for the clusters.}
#'
#' \item{noise}{A flag that determines if or if not noise is generated.}
#'
#' \item{outs}{Outlier spatial positions.}
#'
#' \item{outs_pos}{Outlier stream positions.}
#'
#' \item{outs_vv}{Outlier virtual variance.}
#' @author Michael Hahsler, Dalibor Krleža
#' @references
#' Jain and Dubes(1988) Algorithms for clustering data,
#' Prentice-Hall, Inc., Upper Saddle River, NJ, USA.
#' @examples
#' # Example 1: create data stream with three clusters in 3-dimensional data space
#' stream1 <- DSD_Gaussians(k = 3, d = 3)
#' stream1
#'
#' get_points(stream1, n = 5)
#' plot(stream1)
#'
#' # Example 2: create data stream with specified cluster positions,
#' # 20% noise in a given bounding box and
#' # with different densities (1 to 9 between the two clusters)
#' stream2 <- DSD_Gaussians(k = 2, d = 2,
#'     mu = rbind(c(-.5, -.5), c(.5, .5)),
#'     noise = 0.2,
#'     noise_range = rbind(c(-1, 1),c(-1, 1)),
#'     p = c(.1, .9))
#' get_points(stream2, n = 5)
#' plot(stream2)
#'
#' # Example 3: create 2 clusters and 2 outliers. Clusters and outliers
#' # are separated by Euclidean distance of 0.5 or more.
#' stream3 <- DSD_Gaussians(k = 2, d = 2,
#'     separation_type = "Euclidean",
#'     separation = 0.5,
#'     space_limit = c(0, 1),
#'     outliers = 2)
#' plot(stream3)
#'
#' # Example 4: create 2 clusters and 2 outliers separated by a Mahalanobis
#' # distance of 6 or more.
#' stream4 <- DSD_Gaussians(k = 2, d = 2,
#'   separation_type = "Mahalanobis",
#'   separation = 6,
#'   space_limit = c(0, 25),
#'   variance_limit = 2,
#'   outliers = 2)
#' plot(stream4)
#'
#' # Example 5: spread outliers over 20000 data instances
#' stream5 <- DSD_Gaussians(k = 2, d = 2,
#'   separation_type = "Mahalanobis",
#'   separation = 6,
#'   space_limit = c(0, 45),
#'   variance_limit = 2,
#'   outliers = 20,
#'   outlier_options = list(
#'     outlier_horizon = 20000,
#'     outlier_virtual_variance = 0.3)
#'    )
#' plot(stream5, n = 20000)
#' @export
DSD_Gaussians <-
  function(k = 3,
    d = 2,
    mu,
    sigma,
    p,
    noise = 0,
    noise_range,
    separation_type = c("auto", "Euclidean", "Mahalanobis"),
    separation = 0.2,
    space_limit = c(0.2, 0.8),
    variance_limit = 0.01,
    outliers = 0,
    outlier_options = NULL,
    verbose = FALSE) {
    separation_type <-
      match.arg(separation_type, c("auto", "Euclidean", "Mahalanobis"))
    if (separation_type == "auto")
      separation_type = "Euclidean"



    # if p isn't defined, we give all the clusters equal probability
    if (missing(p)) {
      p <- rep(1 / k, k)
    }

    # covariance matrix
    if (missing(sigma)) {
      if (separation_type == "Euclidean") {
        sigma <- replicate(
          k,
          clusterGeneration::genPositiveDefMat(
            "unifcorrmat",
            rangeVar = c(0.001, variance_limit),
            dim = d
          )$Sigma,
          simplify = F
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
            replicate(d, runif(1, min = 0.001, max = vlim))
          for (i in 1:d)
            for (j in i:d)
              if (i != j)
                tmpS[i, j] <-
            tmpS[j, i] <-
            runif(1, min = 0, max = 0.5) * sqrt(tmpS[i, i]) * sqrt(tmpS[j, j])
          tmpS
        }
        sigma <-
          replicate(k, genRandomSigma(d, variance_limit), simplify = F)
      }
    }

    # prepare inverted covariance matrices / only for Mahalanobis
    if (separation_type == "Mahalanobis") {
      inv_sigma <- list()
      for (i in 1:length(sigma))
        inv_sigma[[i]] <- MASS::ginv(sigma[[i]])
    }
    # for each d, random value between 0 and 1
    # we create a matrix of d columns and k rows
    if (missing(mu)) {
      mu <- matrix(nrow = 0, ncol = d)
      mu_index <- 1
      while (mu_index <= k) {
        if (verbose)
          message(paste("Estimating cluster centers", mu_index))
        i <- 1
        while (i < 1000) {
          centroid <-
            matrix(runif(d, min = space_limit[1], max = space_limit[2]), ncol = d)
          if (verbose)
            message(paste(
              "... try",
              i,
              "cluster centroid [",
              paste(centroid, collapse = ","),
              "]"
            ))
          if (separation_type == "Euclidean" &&
              separation > 0 &&
              !any(dist(rbind(mu, centroid)) < separation))
            break

          if (separation_type == "Mahalanobis" && separation > 0 &&
              !any(mahaDist(centroid, mu_index, mu, inv_sigma, m_th = separation) <=
                  1))
            break

          i <- i + 1
        }
        if (i >= 1000)
          stop("Unable to find set of clusters with sufficient separation!")
        mu <- rbind(mu, centroid)
        mu_index <- mu_index + 1
      }
    } else {
      mu <- as.matrix(mu)
    }

    ## noise
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

    ## outliers
    outs <- NULL
    out_positions <- NULL

    if (outliers > 0) {
      if ((is.null(outlier_options)))
        outlier_options <-
          list(outlier_horizon = 500,
            outlier_virtual_variance = 1)
      if (is.null(outlier_options$outlier_horizon))
        outlier_options$outlier_horizon <- 500
      if (is.null(outlier_options$outlier_virtual_variance))
        outlier_options$outlier_virtual_variance <- 1

      if (is.null(outlier_options$predefined_outlier_space_positions) ||
          is.null(outlier_options$predefined_outlier_stream_positions)) {
        if (separation_type == "Mahalanobis") {
          inv_sigma <- list()
          for (i in 1:length(sigma))
            inv_sigma[[i]] <- MASS::ginv(sigma[[i]])
          if (outliers > 0)
            inv_out_sigma <-
              MASS::ginv(diag(outlier_options$outlier_virtual_variance, d, d))
        }

        outs <- matrix(nrow = 0, ncol = d)
        outs_index <- 1
        while (outs_index <= outliers) {
          if (verbose)
            message(paste("Estimating outlier", outs_index))
          i <- 1L
          while (i < 1000) {
            out <-
              matrix(runif(d, min = space_limit[1], max = space_limit[2]),
                ncol = d)
            if (verbose)
              message(paste(
                "... try",
                i,
                "outlier [",
                paste(out, collapse = ","),
                "]"
              ))
            if (separation_type == "Euclidean" &&
                separation > 0 &&
                !any(dist(rbind(rbind(outs, out), mu)) < separation))
              break

            if (separation_type == "Mahalanobis" &&
                separation > 0 &&
                !any(mahaDist(
                  out,
                  -1,
                  mu,
                  inv_sigma,
                  outs,
                  inv_out_sigma,
                  separation
                ) <= 1))
              break

            i <- i + 1
          }
          if (i >= 1000)
            stop("Unable to find a set of clusters and outliers with sufficient separation!")
          outs <- rbind(outs, out)
          outs_index <- outs_index + 1
        }
        out_positions <-
          sample(1:outlier_options$outlier_horizon, outliers)

      } else {
        outs <- outlier_options$predefined_outlier_space_positions
        out_positions <-
          outlier_options$predefined_outlier_stream_positions
        if (length(outs) != length(out_positions))
          stop(
            "The number of outlier spatial positions must be the same as the number of outlier stream positions."
          )
      }
    }

    # error checking
    if (length(p) != k)
      stop("size of probability vector, p, must equal k")

    if (d < 0)
      stop("invalid number of dimensions")

    if (ncol(mu) != d || nrow(mu) != k)
      stop("invalid size of the mu matrix")
    if (outliers > 0 && (ncol(outs) != d || nrow(outs) != outliers))
      stop("invalid size of the outlier matrix")

    ## TODO: error checking on sigma
    # list of length k
    # d x d matrix in the list

    e1 <-
      new.env() # we need this to maintain the state of the stream generator
    e1$pos <- 1

    l <- list(
      description = paste0("Gaussian Mixture (d = ", d, ", k = ", k, ")"),
      k = k,
      d = d,
      o = outliers,
      mu = mu,
      sigma = sigma,
      p = p,
      noise = noise,
      noise_range = noise_range,
      outs = outs,
      outs_pos = out_positions,
      outs_vv = outlier_options$outlier_virtual_variance,
      env = e1
    )
    class(l) <- c("DSD_Gaussians", "DSD_R", "DSD")
    l
  }

#' @export
get_points.DSD_Gaussians <- function(x,
  n = 1,
  outofpoints = c("stop", "warn", "ignore"),
  info = TRUE,
  ...) {
  .nodots(...)

  noise_pos <- NULL
  outlier_pos <- NULL

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
      MASS::mvrnorm(1, mu = x$mu[i,], Sigma = x$sigma[[i]])
  ))

  ## fix for d==1
  if (x$d == 1)
    data <- t(data)

  ## Replace some points by random noise
  ## TODO: [0,1]^d might not be a good choice. Some clusters can have
  ## points outside this range!
  if (x$noise) {
    noise_pos <- runif(n) < x$noise
    n_noise <- sum(noise_pos)
    if (n_noise > 0) {
      data[noise_pos,] <-
        t(replicate(
          n_noise,
          runif(
            x$d,
            min = x$noise_range[, 1],
            max = x$noise_range[, 2]
          )
        ))
      cluster_id[noise_pos] <- NA
    }
  }

  ## Replace some points by outliers
  if (x$o > 0) {
    outlier_pos <- rep(NA, n)

    # positions needed to match outliers
    f_pos <- x$env$pos
    e_pos <- x$env$pos + (n - 1)
    # which outliers are in the current stream window
    for (i in x$outs_pos[x$outs_pos %in% f_pos:e_pos]) {
      op <- which(x$outs_pos == i) # calculate the outlier position
      sp <- i - f_pos # calculate the stream position
      data[sp,] <- x$outs[op,]
      #cluster_id[sp] <- (x$k + op)
      cluster_id[sp] <- NA
      outlier_pos[sp] <- TRUE
    }
  }

  data <- as.data.frame(data)
  colnames(data) <- paste0("X", 1:ncol(data))

  x$env$pos <- x$env$pos + n

  if (info) {
    data[[".class"]] <- cluster_id
    data[[".outlier"]] <- outlier_pos
  }

  data
}


## this is used for outliers.
#' @export
reset_stream.DSD_Gaussians <- function(dsd, pos = 1) {
  dsd$env$pos <- pos
}

mahaDist <-
  function(t_mu,
    t_sigma_i,
    mu,
    inv_sigma,
    out_mu = NULL,
    inv_out_sigma = NULL,
    m_th = 4) {
    if (!is.null(out_mu) &&
        is.null(inv_out_sigma))
      stop("Inverted virtual covariance for outliers is missing")
    if (t_sigma_i > 0)
      inv_test_sigma <- inv_sigma[[t_sigma_i]]
    else
      inv_test_sigma <- inv_out_sigma
    v <- nrow(mu)
    if (!is.null(out_mu))
      v <- v + nrow(out_mu)
    mx <- matrix(rep(-1, length(v)), ncol = v, nrow = 1)
    if (is.null(out_mu))
      tmu <- mu
    else
      tmu <- rbind(mu, out_mu)
    if (v > 1)
      for (i in 1:v) {
        if (i <= nrow(mu))
          Si <- inv_sigma[[i]]
        else
          Si <- inv_out_sigma
        md <-
          c(
            stats::mahalanobis(t_mu, tmu[i,], Si, inverted = T),
            stats::mahalanobis(tmu[i,], t_mu, inv_test_sigma, inverted = T)
          )
        p <- rep(m_th, 2) / sqrt(md)
        if (sum(p) == 0)
          mx[1, i] <- 0
        else
          mx[1, i] <- 1 / sum(p)
      }
    mx[mx < 0] <- 10000
    mx
  }
