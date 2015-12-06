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


### Implement a new clusterer
### Create an S3 class with elements description and RObj
### RObj needs to be a reference class with methods
###  * cluster(newdata, ...)
###  * get_microclusters(...), get_microweights(...)
###  * get_macroclusters(...), get_macroweights(...), microToMacro(micro, ...)

DSC_R <- function(...) stop("DSC_R is an abstract class and cannot be instantiated!")


### cluster worker
### geting a block of data improves performance the R implementation
### needs to make sure that points are processed sequencially
### (make especially BIRCH faster by passing block data points at once)
update.DSC_R <- function(object, dsd, n=1, verbose=FALSE, 
  block=10000L, ...) {
  ### object contains an RObj which is  a reference object with a cluster method
  
  ### for data frame/matrix we do it all at once
  if(is.data.frame(dsd) || is.matrix(dsd)) {
    if(verbose) cat("Clustering all data at once for matrix/data.frame.")
    object$RObj$cluster(dsd, ...)  
    return(invisible(object))
  }
  
  n <- as.integer(n)
  block <- as.integer(block)
  if(n>0) {
    if(!is(dsd, "DSD_data.frame"))
      stop("Cannot cluster stream (need a DSD_data.frame.)")
  
    ### for clusterers which also create macro-clusterings
    if(is.environment(object$macro)) object$macro$newdata <- TRUE
    
    ### TODO: Check data
    if(verbose) total <- 0L
    for(bl in .make_block(n, block)) {
      object$RObj$cluster(get_points(dsd, bl), ...)
      if(verbose) {
        total <- total + bl
        cat("Processed", total, "/", n, "points -",
        nclusters(object), "clusters\n")
      }
    }
  }
  
  # so cl <- cluster(cl, ...) also works
  invisible(object)
}

### accessors
get_microclusters.DSC_R <- function(x, ...) x$RObj$get_microclusters(...)
get_microweights.DSC_R <- function(x, ...) x$RObj$get_microweights(...)
get_macroclusters.DSC_R <- function(x, ...) x$RObj$get_macroclusters(...)
get_macroweights.DSC_R <- function(x, ...) x$RObj$get_macroweights(...)
microToMacro.DSC_R <- function(x, micro=NULL, ...)  x$RObj$microToMacro(micro, ...)


### make a deep copy of the reference class in RObj 
get_copy.DSC_R <- function(x) {
	temp <- x
	
	temp$RObj <- x$RObj$copy(TRUE)
  
	if(is.environment(temp$macro)) 
    temp$macro <- as.environment(as.list(temp$macro, all.names=TRUE))
	
	temp
}

