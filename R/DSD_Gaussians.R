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

DSD_Gaussians <- function(k=2, d=2, mu, sigma, p, noise = 0, noise_range,
                          separation_type=c("auto","Euclidean","Mahalanobis"), separation=0.2,
                          mahalanobis_separation=4, outlier_virtual_variance=1,
                          o = 0, lim = c(0.2,0.8), variance_lim = 0.01, outlier_horizon=500,
                          outs, out_positions, verbose=FALSE) {

  separation_type <- match.arg(separation_type, c("auto","Euclidean","Mahalanobis"))
  if(separation_type=="auto") separation_type="Euclidean"

  # if p isn't defined, we give all the clusters equal probability
  if (missing(p)) {
    p <- rep(1/k, k)
  }

  # covariance matrix
  if (missing(sigma)) {
    if(separation_type=="Euclidean") {
      sigma <- replicate(k,clusterGeneration::genPositiveDefMat(
        "unifcorrmat",
        rangeVar=c(0.001,variance_lim),
        dim=d)$Sigma,
        simplify=F)
    }
    if(separation_type=="Mahalanobis") {
      genRandomSigma <- function(d, vlim) {
        tmpS <- matrix(data = rep(0, length=d^2), ncol=d, nrow=d)
        diag(tmpS) <- replicate(d, runif(1, min=0.001, max=vlim))
        for(i in 1:d)
          for(j in i:d)
            if(i!=j) tmpS[i,j] <- tmpS[j,i] <- runif(1, min=0, max=0.5)*sqrt(tmpS[i,i])*sqrt(tmpS[j,j])
        tmpS
      }
      sigma <- replicate(k, genRandomSigma(d, variance_lim), simplify=F)
    }
  }

  # for each d, random value between 0 and 1
  # we create a matrix of d columns and k rows
  if (missing(mu)) {
    mu <- matrix(nrow=0,ncol=d)
    mu_index <- 1
    while(mu_index<=k) {
      if(verbose) message(paste("Estimating cluster centers",mu_index))
      i <- 1
      while(i<1000){
        centroid <- matrix(runif(d, min=lim[1], max=lim[2]), ncol=d)
        if(verbose) message(paste("... try",i,"cluster centroid [",paste(centroid,collapse=","),"]"))
        mu_tmp <- rbind(mu,centroid)
        if(separation_type=="Euclidean" && separation>0 && !any(dist(mu_tmp)<separation)) break;
        if(separation_type=="Mahalanobis" && mahalanobis_separation>0 &&
           !any(mahaDist(mu_tmp,sigma,m_th=mahalanobis_separation)<=1)) break;
        i <- i + 1
      }
      if(i>=1000) stop("Unable to find set of clusters with sufficient separation!")
      mu <- mu_tmp
      mu_index <- mu_index + 1
    }

    #mu <- matrix(runif(d*k, min=lim[1], max=lim[2]), ncol=d)
    #if(verbose) message("Estimating cluster centers, round=1")

    #if(separation_type=="Euclidean" && separation>0) {
    #  i <- 1L
    #  while(any(dist(mu)<separation)){
    #    mu <- matrix(runif(d*k, min=lim[1], max=lim[2]), ncol=d)
    #    if(verbose) message(paste0("Estimating cluster centers, round="),(i+1))
    #    i <- i + 1L
    #    if(i>9999L) stop("Unable to find centers with sufficient Euclidean separation!")
    #  }
    #} else if(separation_type=="Mahalanobis" && mahalanobis_separation>0) {
    #  i <- 1L
    #  while(any(mahaDist(mu,sigma,m_th=mahalanobis_separation)<=1)){
    #    mu <- matrix(runif(d*k, min=lim[1], max=lim[2]), ncol=d)
    #    if(verbose) message(paste0("Estimating cluster centers, round="),(i+1))
    #    i <- i + 1L
    #    if(i>9999L) stop("Unable to find centers with sufficient Mahalanobis separation!")
    #  }
    #}
  } else {
    mu <- as.matrix(mu)
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

  if(noise>0 && o>0)
    stop("outliers cannot be generated with noise!")
  if(missing(outs) || is.null(outs) || missing(out_positions) || is.null(out_positions)) {
    outs <- NULL
    out_positions <- NULL
    out_virtual_sigma <- diag(outlier_virtual_variance, d, d)
    if(o>0) {
      outs <- matrix(nrow=0,ncol=d)
      outs_index <- 1
      while(outs_index<=o) {
        if(verbose) message(paste("Estimating outlier",outs_index))
        i <- 1L
        while(i<1000){
          out <- matrix(runif(d, min=lim[1], max=lim[2]), ncol=d)
          if(verbose) message(paste("... try",i,"outlier [",paste(out,collapse=","),"]"))
          outs_tmp <- rbind(outs,out)
          if(separation_type=="Euclidean" && separation>0 && !any(dist(rbind(outs_tmp,mu))<separation)) break;
          if(separation_type=="Mahalanobis" && mahalanobis_separation>0 &&
             !any(mahaDist(mu,sigma,outs_tmp,out_virtual_sigma,mahalanobis_separation)<=1)) break;
          i <- i + 1
        }
        if(i>=1000) stop("Unable to find a set of clusters and outliers with sufficient separation!")
        outs <- outs_tmp
        outs_index <- outs_index + 1
      }
      out_positions <- sample(1:outlier_horizon,o)
    }
  } else {
    if(length(outs)!=length(out_positions))
      stop("The number of outlier spatial positions (outs) must be the same as the number of outlier stream positions (out_positions).")
  }


  # error checking
  if (length(p) != k)
    stop("size of probability vector, p, must equal k")

  if (d < 0)
    stop("invalid number of dimensions")

  if (ncol(mu) != d || nrow(mu) != k)
    stop("invalid size of the mu matrix")
  if (o>0 && (ncol(outs) != d || nrow(outs) != o))
    stop("invalid size of the outlier matrix")

  ## TODO: error checking on sigma
  # list of length k
  # d x d matrix in the list

  e1 <- new.env() # we need this to maintain the state of the stream generator
  e1$pos <- 1
  e1$data <- matrix(nrow=0,ncol=d)
  e1$clusterOrder <- c()
  e1$outliers <- c()

  l <- list(description = "Mixture of Gaussians",
            k = k,
            d = d,
            o = o,
            mu = mu,
            sigma = sigma,
            p = p,
            noise = noise,
            noise_range = noise_range,
            outs = outs,
            outs_pos = out_positions,
            outs_vv = outlier_virtual_variance,
            env = e1)
  class(l) <- c("DSD_Gaussians","DSD_R", "DSD_data.frame", "DSD")
  l
}

get_points.DSD_Gaussians <- function(x, n=1,
                                     outofpoints=c("stop", "warn", "ignore"),
                                     cluster = FALSE, class = FALSE, outlier = FALSE, ...) {
  .nodots(...)
  remainder <- nrow(x$env$data)-(x$env$pos-1)
  if(remainder<0) remainder <- 0
  n_inner <- if((n-remainder)>0) n-remainder else 0

  if(n_inner<n) {
    data <- x$env$data[x$env$pos:(x$env$pos+(n-n_inner)-1),]
    clusterOrder <- x$env$clusterOrder[x$env$pos:(x$env$pos+(n-n_inner)-1)]
    outliers <- x$env$outliers[x$env$pos:(x$env$pos+(n-n_inner)-1)]
  } else {
    data <- matrix(nrow=0,ncol=x$d)
    clusterOrder <- c()
    outliers <- c()
  }
  if(n_inner>0) {
    tmp_clusterOrder <- sample(x=c(1:x$k), size=n_inner, replace=TRUE, prob=x$p)
    tmp_outliers <- rep(FALSE, n_inner)

    tmp_data <- t(sapply(tmp_clusterOrder, FUN = function(i)
      MASS::mvrnorm(1, mu=x$mu[i,], Sigma=x$sigma[[i]])))

    ## fix for d==1
    if(x$d == 1) tmp_data <- t(tmp_data)

    ## Replace some points by random noise
    ## TODO: [0,1]^d might not be a good choice. Some clusters can have
    ## points outside this range!
    if(x$noise) {
      repl <- runif(n_inner)<x$noise
      if(sum(repl)>0) {
        tmp_data[repl,] <- t(replicate(sum(repl),runif(x$d, min=x$noise_range[,1], max=x$noise_range[,2])))
        tmp_clusterOrder[repl] <- NA
      }
    }

    ## Replace some points by outliers
    if(x$o>0) {
      # positions needed to match outliers
      f_pos <- x$env$pos
      e_pos <- x$env$pos + (n_inner-1)
      # which outliers are in the current stream window
      opositions <- x$outs_pos[x$outs_pos %in% f_pos:e_pos]
      for(i in opositions) {
        op <- which(x$outs_pos==i) # calculate the outlier position
        sp <- i-f_pos # calculate the stream position
        tmp_data[sp,] <- x$outs[op,]
        tmp_clusterOrder[sp] <- (x$k + op)
        tmp_outliers[sp] <- TRUE
      }
    }
    # increase position to the end of the generated batch
    #x$env$pos <- x$env$pos + n_inner
    tmp_data <- as.data.frame(tmp_data)
    colnames(tmp_data) <- paste0("X", 1:ncol(tmp_data))
    x$env$data <- rbind(x$env$data, tmp_data)
    x$env$clusterOrder <- c(x$env$clusterOrder, tmp_clusterOrder)
    x$env$outliers <- c(x$env$outliers, tmp_outliers)
    data <- rbind(data, tmp_data)
    clusterOrder <- c(clusterOrder, tmp_clusterOrder)
    outliers <- c(outliers, tmp_outliers)
  }
  x$env$pos <- x$env$pos + n

  if(class) data <- cbind(data, class = clusterOrder)
  if(cluster) attr(data, "cluster") <- clusterOrder
  if(outlier) attr(data, "outlier") <- outliers

  data
}

reset_stream.DSD_Gaussians <- function(dsd, pos=1) {
  dsd$env$pos <- pos
}

mahaDist <- function(mu, sigma, out_mu=NULL, out_sigma=NULL, m_th=4) {
  if(!requireNamespace("MASS")) stop("To use mahalanobis separation in DSD_Gaussians, you need MASS package")
  if(!is.null(out_mu) && is.null(out_sigma)) stop("virtual covariance for outliers is missing")
  inv_sigma <- list()
  for(i in 1:length(sigma)) inv_sigma[[i]] <- MASS::ginv(sigma[[i]])
  if(!is.null(out_sigma)) inv_out_sigma <- MASS::ginv(out_sigma)
  v <- nrow(mu)
  if(!is.null(out_mu)) v <- v + nrow(out_mu)
  mx <- matrix(rep(-1,length(v)^2),ncol=v,nrow=v)
  if(is.null(out_mu)) tmu <- mu
  else tmu <- rbind(mu, out_mu)
  if(v>1)
    for(i in 1:(v-1)) {
      for(j in (i+1):v) {
        if(i<=nrow(mu)) Si <- inv_sigma[[i]]
        else Si <- inv_out_sigma
        if(j<=nrow(mu)) Sj <- inv_sigma[[j]]
        else Sj <- inv_out_sigma
        md <- c(sqrt(stats::mahalanobis(tmu[j,],tmu[i,],Si,inverted=T)), sqrt(stats::mahalanobis(tmu[i,],tmu[j,],Sj,inverted=T)))
        p <- rep(m_th,2) / md
        if(sum(p)==0) mx[i,j] <- mx[j,i] <- 0
        else mx[i,j] <- mx[j,i] <- 1/sum(p)
      }
    }
  mx[mx<0] <- 10000
  mx
}
