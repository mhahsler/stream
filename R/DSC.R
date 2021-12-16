#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Sioftware Foundation; either version 2 of the License, or
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


#' Data Stream Clusterer Base Classes
#'
#' Abstract base classes for all DSC (Data Stream Clusterer) and DSC_R classes.
#' Concrete implementations are functions starting with `DSC_` (R Studio use auto-completion with Tab to select one).
#'
#' The `DSC} and `DSC_R` classes cannot be instantiated (calling
#' `DSC()` or `DSC_R()` produces only a message listing the available implementations),
#' but they serve as a base
#' class from which other DSC classes inherit.
#'
#' Class `DSC` provides several generic functions that can operate on all
#' DSC subclasses. See Functions section below.
#' Additional, separately documented functions are:
#'
#' * [update] Add new data points from a stream to a clustering.
#' * [plot] function is also provides for `DSC`.
#' * [get_assignment] Find out what cluster new data points would be assigned to.
#'
#' `get_centers` and `get_weights` are typically overwritten by
#' subclasses of `DSC`. `DSC_R` provides these functions for R-based
#' DSC implementations.
#'
#' Since `DSC` objects often contain external pointers, regular saving and
#' reading operations will fail. Use [saveDSC] and [readDSC]
#' which will serialize the objects first appropriately.
#'
#' @aliases DSC DSC_R
#' @param x a DSC object.
#' @param type Return weights of micro- or macro-clusters in x.  Auto uses the
#' class of x to decide.
#' @param scale a range (from, to) to scale the weights.  Returns by default
#' the raw weights.
#' @param ... further parameter
#' @author Michael Hahsler
#' @seealso [DSC_Micro], [DSC_Macro], [animate_cluster], [update],
#' [evaluate], [get_assignment],
#' [microToMacro],
#' [plot], [prune_clusters],
#' [recluster], [readDSC], [saveDSC]
#' @export DSC
#' @examples
#'
#' DSC()
#'
#' stream <- DSD_Gaussians(k=3, d=2)
#' dstream <- DSC_DStream(gridsize=.1)
#' update(dstream, stream, 500)
#' dstream
#'
#' # get micro-cluster centers
#' get_centers(dstream)
#'
#' # get the number of clusters
#' nclusters(dstream)
#'
#' # get the micro-cluster weights
#' get_weights(dstream)
#'
#' # D-Stream also has macro-clusters
#' get_weights(dstream, type="macro")
#'
#'
DSC<- abstract_class_generator("DSC")

### all DSC classes have these interface methods


#' @describeIn DSC Gets the cluster centers (micro- or macro-clusters) from a DSC object.
#' @export get_centers
get_centers <- function(x, type = c("auto", "micro", "macro"), ...)
  UseMethod("get_centers")
get_centers.default <- function(x, type = c("auto", "micro", "macro"), ...) {
  stop(gettextf("get_centers not implemented for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get the weights of the clusters in the DSC (returns 1s if not implemented by the clusterer)
#' @export get_weights
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


#' @describeIn DSC Create a Deep Copy of a DSC Object that contain reference classes (e.g., Java data structures for MOA).
#' @export get_copy
get_copy <- function(x) UseMethod("get_copy")
get_copy.default <- function(x, ...) {
  stop(gettextf("get_copy not implemented for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get micro-clusters if the object is a `DSC_Micro`.
get_microclusters <- function(x, ...) UseMethod("get_microclusters")
get_microclusters.DSC <- function(x, ...) {
  stop(gettextf("No micro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get micro-clusters if the the object is a `DSC_Macro`.
get_macroclusters <- function(x, ...) UseMethod("get_macroclusters")
get_macroclusters.DSC <- function(x, ...) {
  stop(gettextf("No macro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get micro-cluster weights if the object is a `DSC_Micro`.
get_microweights <- function(x, ...) UseMethod("get_microweights")
get_microweights.DSC <- function(x, ...) {
  stop(gettextf("No weights for micro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}

#' @describeIn DSC Get macro-cluster weights if the object is a `DSC_Macro`.
get_macroweights <- function(x, ...) UseMethod("get_macroweights")
get_macroweights.DSC <- function(x, ...) {
  stop(gettextf("No weights for macro-clusters available for class '%s'.",
    paste(class(x), collapse=", ")))
}


#' @describeIn DSC Returns the number of micro-clusters from the DSC object.
#' @export nclusters
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

#' Plotting Data Stream Data and Clusterings
#'
#' Methods to plot data stream data and clusterings.
#'
#'
#' @aliases plot plot.DSD plot.DSC
#' @param x the DSD or DSC object to be plotted.
#' @param dsd a DSD object to plot the data in the background.
#' @param n number of plots taken from the dsd to plot.
#' @param col,col_points,col_clusters colors used for plotting.
#' @param weights the size of the symbols for micro- and macro-clusters
#' represents its weight.
#' @param scale range for the symbol sizes used.
#' @param cex size factor for symbols.
#' @param pch symbol type.
#' @param method method used for plotting: "pairs" (pairs plot), "scatter"
#' (scatter plot) or "pc" (plot first 2 principal components).
#' @param dim an integer vector with the dimensions to plot. If \code{NULL}
#' then for methods \code{"pairs"} and \code{"pc"} all dimensions are used and
#' for \code{"scatter"} the first two dimensions are plotted.
#' @param alpha alpha shading used to plot the points.
#' @param type Plot micro clusters (\code{type="micro"}), macro clusters
#' (\code{type="macro"}), both micro and macro clusters (\code{type="both"}),
#' outliers(\code{type="outliers"}), or everything together
#' (\code{type="all"}). \code{type="auto"} leaves to the class of dsc to
#' decide.
#' @param assignment logical; show assignment area of micro-clusters.
#' @param \dots further arguments are passed on to plot or pairs in
#' \pkg{graphics}.
#' @author Michael Hahsler
#' @seealso \code{\link{DSC}}, \code{\link{DSD}}
#' @examples
#'
#' stream <- DSD_Gaussians(k=3, d=3)
#'
#' ## plot data
#' plot(stream, n=500)
#' plot(stream, method="pc", n=500)
#' plot(stream, method="scatter", dim=c(1,3), n=500)
#'
#' ## create and plot micro-clusters
#' dstream <- DSC_DStream(gridsize=0.1)
#' update(dstream, stream, 500)
#' plot(dstream)
#'
#' ## plot with data, projected on the first two principal components
#' ## and dimensions 2 and 3
#' plot(dstream, stream)
#' plot(dstream, stream, method="pc")
#' plot(dstream, stream, dim=c(2,3))
#'
#' ## plot micro and macro-clusters
#' plot(dstream, stream, type="both")
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
  if(type == "outliers" && !is(x, "DSOutlier"))
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
    if(type=="all" && is(x, "DSOutlier")) centers_out <- get_outlier_positions(x)
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

