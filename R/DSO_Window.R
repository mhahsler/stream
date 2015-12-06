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

### FIXME: plot?
### FIXME: replace DSC_Window with call to DSO_Window

DSO_Window <- function(horizon = 100, lambda=0)  
  structure(list(
    description = 
      if(lambda>0) "Damped sliding window" else "Sliding window",
    RObj = WindowDSO$new(horizon = as.integer(horizon), lambda=lambda)),
    class = c("DSO_Window", "DSO")
  )

update.DSO_Window <- function(object, dsd, n=1, verbose=FALSE, ...) {
  
  ### some matrix to be processed in one go
  if(!is(dsd, "DSD")) { 
    n <- nrow(dsd)
    dsd <- DSD_Memory(dsd)
  }
  
  ### FIXME: we do not need to get all points if n is very large!
  object$RObj$update(get_points(dsd, n=n), verbose=verbose, ...)
}

get_points.DSO_Window <- function(x, ...) {
  x$RObj$get_points(...)
}

get_weights.DSO_Window <- function(x, ...) {
  x$RObj$get_weights(...)
}

# implements a ring-buffer. pos is the current insert position
WindowDSO <- setRefClass("WindowDSO", 
  fields = list(
    horizon	= "integer",
    pos	= "integer",
    lambda = "numeric",
    data	= "ANY"   ### data.frame or list
  ), 
  
  methods = list(
    initialize = function(horizon	= 100L, lambda = 0) {
      
      horizon	<<- horizon
      data <<- NULL ### don't know yet!
      pos	<<- 1L 
      lambda <<- lambda
      
      .self
    },
    
    update = function(x, ...) {
      isdf <- is.data.frame(x) 
      
      ### fist time we get data
      if(is.null(data)) {
        data <<- if(isdf) data.frame() else list()
      } else {
        if(isdf && !is.data.frame(data)) 
          stop("Stream and Window data type not compatible!")
        if(!isdf && !is.list(data))
          stop("Stream and Window data type not compatible!")
        if(isdf && ncol(x) != ncol(data))
          stop("Dimensionality mismatch between window and data!")
      }
      
      n <- if(isdf) nrow(x) else length(x) 
      
      i <- 0L
      while(i < n) {
        
        ## process the next m points: all or to fill the current horizon
        m <- min(horizon - pos + 1L, n-i)
        
        ## first points? copy to get dim!
        if(isdf) {
          if(nrow(data)==0L) data <<- x[(i+1L):(i+m), , drop=FALSE]
          else data[pos:(pos+m-1L),] <<- x[(i+1L):(i+m), , drop=FALSE] 
        }else{
          data[pos:(pos+m-1L)] <<- x[(i+1L):(i+m)] 
        }
        
        i <- i+m
        pos <<- pos+m
        if(pos>horizon) pos <<- 1L
      }
      
      # fix row names for data_frame
      if(isdf) rownames(data) <<- NULL
    },
    
    get_points = function(...) {
      if(is.null(data)) return(data.frame())  ### gives 0 nrows and 0 length (we do not know if it is supposed to be a data.frame or a list)
      
      isdf <- is.data.frame(data) 
      n <- if(isdf) nrow(data) else length(data) 
      
      if(pos==1 || n<horizon) return(data)
      if(isdf) data[c(pos:(horizon), 1L:(pos-1L)),]
      else data[c(pos:(horizon), 1L:(pos-1L))]
    },
    
    get_weights = function(...) {
      isdf <- is.data.frame(data) 
      n <- if(isdf) nrow(data) else length(data) 
      
      if(lambda <= 0) rep(1, n)
      else 2^(-lambda*((n-1L):0))
    }   
  )
)

### DSC interface to WindowDSO
WindowDSC <- setRefClass("WindowDSC", 
  contains="WindowDSO",
  
  methods = list(
    cluster = function(x, ...) update(x, ...),
    get_microclusters = function(...) get_points(...), 
    get_microweights = function(...) get_weights(...)
  )
)