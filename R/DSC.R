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


### DSC - Data Stream Clusterer interface

DSC <- function(...) stop("DSC is an abstract class and cannot be instantiated!")


### all DSC classes have these interface methods
get_centers <- function(x, type = c("auto", "micro", "macro"), ...)
  UseMethod("get_centers")
get_centers.default <- function(x, type = c("auto", "micro", "macro"), ...) {
  stop(gettextf("get_centers not implemented for class '%s'.",
    paste(class(x), collapse=", ")))
}

### get MC weights. In case it is not implemented it returns 1s
get_weights <- function(x, type=c("auto", "micro", "macro"), scale=NULL, ...)
  UseMethod("get_weights")
get_weights.default <- function(x, type=c("auto", "micro", "macro"),
  scale=NULL, ...) {
  .nodots(...)
  m <- rep(1, nclusters(x, type=type))
  if(!is.null(scale)) {
    if(length(unique(m)) ==1)  w <- rep(mean(scale), length(w))
    else m <- map(m, range=scale, from.range=c(0,
      max(m, na.rm=TRUE)))
  }
  m
}

### End of interface
#####################################################################3

### make a deep copy of the
get_copy <- function(x) UseMethod("get_copy")
get_copy.default <- function(x, ...) {
  stop(gettextf("get_copy not implemented for class '%s'.",
    paste(class(x), collapse=", ")))
}

get_microclusters <- function(x, ...) UseMethod("get_microclusters")
get_microclusters.DSC <- function(x, ...) {
  stop(gettextf("No micro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

get_macroclusters <- function(x, ...) UseMethod("get_macroclusters")
get_macroclusters.DSC <- function(x, ...) {
  stop(gettextf("No macro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

get_microweights <- function(x, ...) UseMethod("get_microweights")
get_microweights.DSC <- function(x, ...) {
  stop(gettextf("No weights for micro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

get_macroweights <- function(x, ...) UseMethod("get_macroweights")
get_macroweights.DSC <- function(x, ...) {
  stop(gettextf("No weights for macro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}


### derived functions, plot and print
nclusters <- function(x, type=c("auto", "micro", "macro"), ...)
  UseMethod("nclusters")

nclusters.DSC <- function(x, type=c("auto", "micro", "macro"), ...) {
  nrow(get_centers(x, type=type, ...))
}


print.DSC <- function(x, ...) {
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse=", "), "\n")
  if(!is(nc <- try(nclusters(x, type="micro"), silent=TRUE), "try-error"))
    cat(paste('Number of micro-clusters:', nc, '\n'))
  if(!is(nc <- try(nclusters(x, type="macro"), silent=TRUE), "try-error"))
    cat(paste('Number of macro-clusters:', nc, '\n'))
}

summary.DSC <- function(object, ...) print(object)

#plot.DSC will call super question.
plot.DSC <- function(x, dsd = NULL, n = 500,
                     col_points=NULL,
                     col_clusters=c("red", "blue", "green"),
                     weights=TRUE,
                     scale=c(1,5),
                     cex=1,
                     pch=NULL,
                     method="pairs", dim=NULL,
                     type=c("auto", "micro", "macro", "both", "all", "outliers"), # we keep 'both' for compatibility reasons
                     assignment = FALSE, ### assignment is not implemented
                     ...) {

  type <- match.arg(type)
  if(type == "outliers" && !is(x, "DSC_Outlier"))
    stop("The clusterer is not an outlier detector, cannot draw outliers")

  if(is.null(col_points)) col_points <- .points_col

  if(type !="both" && type != "all") {
    if(type =="auto") type <- get_type(x)
    ## method can be pairs, scatter or pc (projection with PCA)
    if(type != "outliers") {
      centers <- get_centers(x, type=type)
    } else {
      centers <- get_outlier_positions(x)
    }
    k <- nrow(centers)

    if(k<1) {
      warning("No clusters or outliers to plot!")
      plot(NA, NA, xlim=c(0,0), ylim=c(0,0))
      return()
    }

    if(type != "outliers") {
      if(weights) cex_clusters <- get_weights(x, type=type, scale=scale)
      else cex_clusters <- rep(1, k)
    } else cex_clusters <- rep(2, k)

    if(type=="micro") {
      col <- rep(col_clusters[1], k)
      mpch <- rep(1, k)
      lwd <- rep(1, k)
    } else if(type=="macro") {
      cex_clusters <- cex_clusters*1.5
      col <- rep(col_clusters[2], k)
      mpch <- rep(3, k)
      lwd <- rep(2, k)
    } else {
      col <- rep(col_clusters[3], k)
      mpch <- rep(1, k)
      lwd <- rep(1, k)
    }
  } else { ### both
    centers_mi <- get_centers(x, type="micro")
    centers_ma <- get_centers(x, type="macro")
    centers_out <- data.frame()
    if(type=="all" && is(x, "DSC_Outlier")) centers_out <- get_outlier_positions(x)
    k_mi <- nrow(centers_mi)
    k_ma <- nrow(centers_ma)
    k_out <- nrow(centers_out)

    if((k_mi+k_out)<1) {
      warning("No clusters or outliers to plot!")
      plot(NA, NA, xlim=c(0,0), ylim=c(0,0))
      return()
    }

    ### Fix names if necessary
    colnames(centers_mi) <- colnames(centers_ma)
    if(nrow(centers_out)>0) colnames(centers_out) <- colnames(centers_ma)

    centers <- rbind(centers_mi, centers_ma, centers_out)

    if(weights) cex_clusters <- c(get_weights(x, type="micro", scale=scale),
                                  get_weights(x, type="macro", scale=scale*1.5), rep(2, k_out))
    else cex_clusters <- c(rep(cex, k_mi), rep(cex*2, k_ma), rep(2, k_out))

    col <- c(rep(col_clusters[1], k_mi), rep(col_clusters[2], k_ma), rep(col_clusters[3], k_out))
    mpch <- c(rep(1, k_mi), rep(3, k_ma), rep(1, k_out))
    lwd <- c(rep(1, k_mi), rep(2, k_ma), rep(1, k_out))
  }

  ### prepend data if given
  if(!is.null(dsd)) {
    d <- get_points(dsd, n, cluster = TRUE, outlier=TRUE)
    #	names(d) <- names(centers)
    # fix center names
    colnames(centers) <- colnames(d)
    centers <- rbind(d, centers)

    col <- c(rep(col_points,n)[1:n], col)
    cex_clusters <- c(rep(cex, n), cex_clusters)
    mpch <- c(attr(d, "cluster"), mpch)
    mpch <- mpch %% 25
    lwd <- c(rep(1,n), lwd)

    ### handle noise
    noise <- is.na(mpch)
    mpch[noise] <- .noise_pch
    col[noise] <- .noise_col
    #cex_clusters[noise] <- cex_clusters[noise]*.5

  }

  if(!is.null(pch)) mpch <- pch

  if(!is.null(dim)) centers <- centers[,dim]

  ### plot
  if(ncol(centers)>2 && method=="pairs") {
    pairs(centers, col=col, cex=cex_clusters, pch=mpch, lwd=lwd, ...)
  }
  else if(ncol(centers)>2 && method=="pc") {
    ## we assume Euclidean here
    p <- prcomp(centers)
    plot(p$x, col=col, cex=cex_clusters, pch=mpch, lwd=lwd, ...)
  }else if(ncol(centers) == 1){
    plot(centers[[1]], rep(0, length(centers[[1]])),
         col=col, cex=cex_clusters, pch=mpch, lwd=lwd,
         ylab = "", xlab = colnames(centers)[1], ...)
  }else { ## plot first 2 dimensions
    if(ncol(centers)>2) centers <- centers[,1:2]
    plot(centers, col=col, cex=cex_clusters, pch=mpch, lwd=lwd, ...)
  }

}

DSC_Outlier <- function(...) stop("DSC_Outlier is an abstract class and cannot be instantiated!")
clean_outliers <- function(x, ...)
  UseMethod("clean_outliers")
clean_outliers.default <- function(x, ...) {
  stop(gettextf("clean_outliers not implemented for class '%s'.", paste(class(x), collapse=", ")))
}
clean_outliers.DSC_Outlier <- function(x, ...) {
  stop(gettextf("No clean outliers available for class '%s'.", paste(class(x), collapse=", ")))
}
get_outlier_positions <- function(x, ...)
  UseMethod("get_outlier_positions")
get_outlier_positions.default <- function(x, ...) {
  stop(gettextf("get_outlier_positions not implemented for class '%s'.", paste(class(x), collapse=", ")))
}
get_outlier_positions.DSC_Outlier <- function(x, ...) {
  stop(gettextf("No outlier getter method available for class '%s'.", paste(class(x), collapse=", ")))
}
recheck_outlier <- function(x, outlier_correlated_id, ...)
  UseMethod("recheck_outlier")
recheck_outlier.default <- function(x, outlier_correlated_id, ...) {
  stop(gettextf("recheck_outlier not implemented for class '%s'.", paste(class(x), collapse=", ")))
}
recheck_outlier.DSC_Outlier <- function(x, outlier_correlated_id, ...) {
  stop(gettextf("No outlier rechecking method available for class '%s'.", paste(class(x), collapse=", ")))
}
noutliers <- function(x, ...) UseMethod("noutliers")
noutliers.default <- function(x, ...) {
  stop(gettextf("noutliers not implemented for class '%s'.", paste(class(x), collapse=", ")))
}
noutliers.DSC_Outlier <- function(x, ...) {
  nrow(get_outlier_positions(x))
}
print.DSC_Outlier <- function(x, ...) {
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse=", "), "\n")
  if(!is(nc <- try(nclusters(x, type="micro"), silent=TRUE), "try-error"))
    cat(paste('Number of micro-clusters:', nc, '\n'))
  if(!is(nc <- try(nclusters(x, type="macro"), silent=TRUE), "try-error"))
    cat(paste('Number of macro-clusters:', nc, '\n'))
  if(!is(no <- try(noutliers(x), silent=TRUE), "try-error"))
    cat(paste('Number of outliers:', no, '\n'))
}
get_assignment.DSC_Outlier <- function(x, points, type=c("auto", "micro", "macro"),
                                       method=c("auto", "nn", "model"), outlier_threshold=0.05, ...) {

  method <- match.arg(method)
  if(method=="auto") method <- "nn"

  if(method=="model") {
    warning("method model not implemented! using Euclidean nearest neighbor instead!")
    method <- "nn"
  }

  c1 <- get_centers(x, type=type, ...)
  c2 <- get_outlier_positions(x, ...)
  c <- rbind(c1, c2)

  if(nrow(c)>0L) {
    dist <- dist(points, c, method="Euclidean")
    # Find the minimum distance and save the class
    predict <- apply(dist, 1L, which.min)
    outliers <- predict > nrow(c1)
    outliers[apply(dist, 1L, min) > outlier_threshold] <- FALSE

    to_noise <- predict > nrow(c1)
    to_noise[apply(dist, 1L, min) <= outlier_threshold] <- FALSE
    predict[to_noise] <- NA_integer_
  } else {
    warning("There are no clusters and outliers!")
    predict <- rep(NA_integer_, nrow(points))
    outliers <- rep(FALSE, nrow(points))
  }

  attr(predict, "method") <- method
  attr(predict, "outliers") <- outliers

  predict
}

DSC_SinglePass <- function(...) stop("DSC_SinglePass is an abstract class and cannot be instantiated!")
get_assignment.DSC_SinglePass <- function(x, points, type=c("auto", "micro", "macro"),
                                          method=c("auto", "nn", "model"), ...) {
  stop(gettextf("No assignments and update available for class '%s'.", paste(class(x), collapse=", ")))
}
