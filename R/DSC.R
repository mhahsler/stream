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
  col_clusters=c("red", "blue"), 
  weights=TRUE,
  scale=c(1,5),
  cex=1,
  pch=NULL,
  method="pairs", dim=NULL, 
  type=c("auto", "micro", "macro", "both"), 
  assignment = FALSE, ### assignment is not implemented
  ...) {
  
  type <- match.arg(type)
  
  if(is.null(col_points)) col_points <- .points_col
  
  if(type !="both") { 
    if(type =="auto") type <- get_type(x)
    ## method can be pairs, scatter or pc (projection with PCA)
    centers <- get_centers(x, type=type)
    k <- nrow(centers)
    
    if(k<1) {
      warning("No clusters to plot!")
      plot(NA, NA, xlim=c(0,0), ylim=c(0,0))
      return()
    }
    
    if(weights) cex_clusters <- get_weights(x, type=type, scale=scale)
    else cex_clusters <- rep(1, k)
    
    if(type=="micro") { 
      col <- rep(col_clusters[1], k)
      mpch <- rep(1, k)
      lwd <- rep(1, k)
    }else{
      cex_clusters <- cex_clusters*1.5
      col <- rep(col_clusters[2], k)
      mpch <- rep(3, k)
      lwd <- rep(2, k)
    }
    
    }else{ ### both
    centers_mi <- get_centers(x, type="micro")
    centers_ma <- get_centers(x, type="macro")
    k_mi <- nrow(centers_mi)
    k_ma <- nrow(centers_ma)
    
    if(k_mi<1) {
      warning("No clusters to plot!")
      plot(NA, NA, xlim=c(0,0), ylim=c(0,0))
      return()
    }
    
    centers <- rbind(centers_mi, centers_ma)
    
    if(weights) cex_clusters <- c(get_weights(x, type="micro", scale=scale), 
        get_weights(x, type="macro", scale=scale*1.5))
    else cex_clusters <- c(rep(cex, k_mi), rep(cex*2,+k_ma))
      
    col <- c(rep(col_clusters[1], k_mi), rep(col_clusters[2], k_ma))
    mpch <- c(rep(1, k_mi), rep(3, k_ma))
    lwd <- c(rep(1, k_mi), rep(2, k_ma))
  }
  
  ### prepend data if given
  if(!is.null(dsd)) {
    d <- get_points(dsd, n, cluster = TRUE)
    #	names(d) <- names(centers)
    # fix center names
    colnames(centers) <- colnames(d)
    centers <- rbind(d, centers)
    
    col <- c(rep(col_points,n)[1:n], col)
    cex_clusters <- c(rep(cex, n), cex_clusters)
    mpch <- c(attr(d, "cluster"), mpch)
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
  }else { ## plot first 2 dimensions
    if(ncol(centers)>2) centers <- centers[,1:2]
    plot(centers, col=col, cex=cex_clusters, pch=mpch, lwd=lwd, ...)
  }
  
}


