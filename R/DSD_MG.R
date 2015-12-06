#######################################################################
# Moving Generator -  Infrastructure for Moving Streams
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

### creator    
DSD_MG<- function(dimension = 2, ..., labels=NULL, description=NULL) {
  
  if(is.null(description)) description <- "Moving Data Generator"
  
  x <- structure(list(description = description,
    RObj = dsd_MG_refClass$new(d = dimension)),
    class = c("DSD_MG", "DSD_R", "DSD_data.frame", "DSD"))
  
  l <- list(...)
  if(length(l) > 0) {
    for(i in 1:length(l)) {
      add_cluster(x, l[[i]], labels[i])
    }
  } 
  
  x
}

add_cluster <- function(x, c, label=NULL) UseMethod("add_cluster")
get_clusters <- function(x) UseMethod("get_clusters")
remove_cluster <- function(x, i) UseMethod("remove_cluster")

dsd_MG_refClass <- setRefClass("dsd_MG", 
  fields = list(
    t = "numeric",
    dimension = "numeric",
    clusters = "list",
    labels = "integer"
  ),
  methods = list(
    initialize = function(d) {
      t <<- 1
      dimension  <<- d
      clusters <<- list()
      labels <<- integer(0)
      .self
    }
    
  ),
)

dsd_MG_refClass$methods(
  add_cluster = function(c, label=NULL) {
    if(c$RObj$dimension != dimension) stop("Cluster dimensions do not match!")
    clusters <<- append(clusters, list(c))
    
    if(is.null(label)) {
      if(length(labels) == 0) label <- 1L
      else label <- max(labels, na.rm=TRUE) + 1L
      } else label <- as.integer(label)
    labels <<- append(labels, label)
    },
  
  get_points = function(n, cluster = FALSE) {
    if(length(clusters)==0) stop("DSD_MG does not contain any clusters!")

    if(cluster) a <- integer(n)
    data <- matrix(NA_real_, nrow=n, ncol=dimension)
    
    j <- 0L
    while(j < n) {
      
      density <- unlist(sapply(clusters, 
        function(x) x$RObj$get_attributes(t, "density")))
      
      density[is.na(density)] <- 0
      if(all(density==0)) stop("No MGC is producing points for this time point.")

      pointsPerSecond <- sum(density)
      pointsLeftInSecond <- pointsPerSecond - (t - floor(t))*pointsPerSecond
      if((j + pointsLeftInSecond) <= n) k <- pointsLeftInSecond
      else k <- n-j
      
      ### got to next timestep...
      if(pointsLeftInSecond<1) {
        t <<- ceiling(t)
        next
      }
      
      k <- floor(k)
      
      if(k>=1) {
        clusterOrder <- sample(x=1:length(clusters), size=k, replace=TRUE, 
          prob=density/sum(density))
        
        data[(j+1):(j+k),] <- t(sapply(clusterOrder, FUN = function(i) {
          clusters[[i]]$RObj$get_points(t)
        }))
        
        if(cluster) {
          a[(j+1):(j+k)] <- labels[clusterOrder]
        }
      }
      
      t <<- t + k/pointsPerSecond
      j <- j+k
    }
    
    data <- data.frame(data)
    
    if(cluster) attr(data,"cluster") <- a
    
    data
  }
)



get_points.DSD_MG <- function(x, n=1, 
  outofpoints=c("stop", "warn", "ignore"), 
  cluster = FALSE, class = FALSE, ...) {
  .nodots(...)

  d <- x$RObj$get_points(n, cluster=TRUE)

  a <- attr(d, "cluster")
  if(!cluster) attr(d, "cluster") <- NULL
  
  if(class) d <- cbind(d, class = a)
  
  d

}

add_cluster.DSD_MG <- function(x, c, label=NULL) {
  ### name noise NA unless specified otherwise
  if(is.null(label) && is(c, "MGC_Noise")) label <- NA
  x$RObj$add_cluster(c, label)
}

reset_stream.DSD_MG <- function(dsd, pos=1) {
  dsd$RObj$t <- pos
}

print.DSD_MG <- function(x, ...) {
  #NextMethod()
  cat(.line_break(paste(x$description)))
  cat("Class:", paste(class(x), collapse=", "), "\n")
  cat(paste('With', length(na.omit(unique(x$RObj$labels))), 'clusters', 'in', 
    x$RObj$dimension, 'dimensions. Time is', round(x$RObj$t, 3), '\n'))
}

get_clusters.DSD_MG <- function(x) {
  x$RObj$clusters
}

remove_cluster.DSD_MG  <- function(x,i) {
  x$RObj$clusters[[i]] <- NULL
  x$RObj$labels <- x$RObj$labels[-i] 
}
