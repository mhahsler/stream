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

### creator    
DSC_Kmeans <- function(k, weighted = TRUE, iter.max = 10, nstart = 1,
  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
    "MacQueen"), 
  min_weight = NULL, description=NULL) {
  
  algorithm <- match.arg(algorithm)
  if(!is.null(description)) desc <- description
  else if(weighted) desc <- "k-Means (weighted)"
  else desc <-"k-Means"
  
  structure(list(description = desc,
    RObj = kmeans_refClass$new(
      k=k, weighted=weighted, iter.max = iter.max, nstart = nstart,
      algorithm = algorithm, min_weight=min_weight)),
    class = c("DSC_Kmeans","DSC_Macro","DSC_R","DSC"))
}


kmeans_refClass <- setRefClass("kmeans", 
  fields = list(
    k	    = "numeric",
    weighted = "logical",
    iter.max    = "numeric",
    nstart	    = "numeric",
    algorithm   = "character",
    assignment  = "numeric",
    data      = "data.frame",
    weights	    = "numeric",
    clusterCenters = "data.frame",
    clusterWeights = "numeric",
    details      = "ANY",
    min_weight   = "numeric"
  ), 
  
  methods = list(
    initialize = function(
      k      = 3,
      weighted = TRUE,
      iter.max    = 10,
      nstart	    = 1,
      algorithm   = c("Hartigan-Wong", "Lloyd", 
        "Forgy","MacQueen"),
      min_weight = NULL
    ) {
      
      k  	<<- k 
      weighted <<- weighted
      iter.max	<<- iter.max 
      nstart	<<- nstart
      algorithm   <<- match.arg(algorithm)
      assignment	<<- numeric() 
      weights	<<- numeric() 
      clusterWeights <<- numeric() 
      clusterCenters <<- data.frame()
      data	<<- data.frame()
      
      if(is.null(min_weight)) min_weight <<- 0
      else min_weight <<- min_weight
      
      .self
    }
    
  ),
)

kmeans_refClass$methods(
  cluster = function(x, weight = rep(1,nrow(x)), ...) {
    
  #  if(nrow(x)==1) 
  #    warning("DSC_Kmeans does not support iterative updating! Old data is overwritten.")
    
    ### filter weak clusters
    if(min_weight>0) {
      x <- x[weight>min_weight,]
      weight <- weight[weight>min_weight]
    }
    
    
    weights <<- weight
    data <<- x
    
    if(nrow(data)>k) {
      if(weighted) km <- kmeansW(x=data, weight=weights, centers=k, 
        iter.max = iter.max, nstart = nstart)
      else km <- kmeans(x=data, centers=k, 
        iter.max = iter.max, nstart = nstart,
        algorithm = algorithm)
      
      assignment <<- km$cluster
      clusterCenters <<- data.frame(km$centers)
      details <<- km
    } else {
      assignment <<- 1:nrow(data)
      clusterCenters <<- x
      details <<- NULL
    }
    
    clusterWeights <<- sapply(1:k, FUN =
        function(i) sum(weights[assignment==i]))
    
  },
  
  get_macroclusters = function(...) { clusterCenters },
  get_macroweights = function(...) { clusterWeights },
  
  get_microclusters = function(...) { data },
  get_microweights = function(x) { weights },
  
  microToMacro = function(micro=NULL, ...){ 
    if(is.null(micro)) micro <- 1:nrow(data)
    structure(assignment[micro], names=micro)
  }  
)



