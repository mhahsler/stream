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


DSO_Sample <- function(k = 100, biased = FALSE) 
  structure(list(description = 
      if(biased) "Reservoir sampling (biased)" else "Reservoir sampling",
    RObj = SampleDSO$new(k = k, biased = biased)),
    class = c("DSO_Sample","DSO"))

update.DSO_Sample <- function(object, dsd, n=1, verbose=FALSE, ...) {
  
  ### some matrix to be processed in one go
  if(!is(dsd, "DSD")) { 
    n <- nrow(dsd)
    dsd <- DSD_Memory(dsd)
  }
  
  ### FIXME: we do not need to get all points if n is very large!
  object$RObj$update(get_points(dsd, n=n), verbose=verbose, ...)
}

get_points.DSO_Sample <- function(x, ...) {
  x$RObj$get_points(...)
}

get_weights.DSO_Sample <- function(x, ...) {
  x$RObj$get_weights(...)
}


SampleDSO <- setRefClass("SampleDSO", 
  fields = list(
    k	= "integer",
    biased = "logical",
    stream_size	= "integer",
    data	= "ANY"   ### data.frame or list (NULL = unknown)
  ), 
  
  methods = list(
    initialize = function(
      k	= 100L,
      biased = FALSE
    ) {
      
      k	<<- as.integer(k)
      biased	<<- biased
      stream_size	<<- 0L 
      
      data <<- NULL
      
      .self
    },
    
    
    
    ### Reservoir sampling: 
    ### unbiased: all values in the stream have the same prob. to be sampled
    ### biased: more recent values have a higher probability
    update = function(x, ...) {
      if(!is(data, class(x)) && !is.null(data)) 
        stop("Data stream data type not compatible!")    
      
      if(is.data.frame(x)) update_data.frame(x)
      else update_list(x)
    },
    
    update_data.frame = function(x, ...) {
      
      if(!biased) {
        ### fast initialization
        if(is.null(data)) {
          if(nrow(x) <= k) data <<- x
          else data <<- x[sample(1:nrow(x), k),]
          stream_size <<- nrow(x)
        }else{
          
          ### reservoir sampling
          for(i in 1:nrow(x)){
            ### fill with values first
            if(nrow(data) < k) {
              data <<- rbind(data, x[i,])
              
            }else{ ### replace values with decreasing probabilities
              r <- sample.int(stream_size+1L, size=1L)
              if(r <= k) data[r, ] <<- x[i,]
              ### Note: we do not need to replace weight (is already 1)
            }   
            
            stream_size <<- stream_size + 1L
          }
        }
        
      }else{ ### biased
        if(is.null(data)) data <<- data.frame()
        
        for(i in 1:nrow(x)){
          ### add all new points and replace point in reservoir with prob=size/k  
          prob <- nrow(data)/k
          if(sample(c(TRUE, FALSE), 1L, prob=c(prob, 1-prob))) {
            data[sample.int(nrow(data), 1L),] <<- x[i,]
          }else{
            data <<- rbind(data, x[i,])
          }
          
          stream_size <<- stream_size + 1L
        }
      }
    },
    
    update_list = function(x, ...) {
      
      if(!biased) {
        ### fast initialization
        if(is.null(data)) {
          if(length(x) <= k) data <<- x
          else data <<- x[sample(1:nrow(x), k)]
          stream_size <<- length(x)
        }else{
          
          ### reservoir sampling
          for(i in 1:length(x)){
            ### fill with values first
            if(length(data) < k) {
              data <<- append(data, x[i])
              
            }else{ ### replace values with decreasing probabilities
              r <- sample.int(stream_size+1L, size=1L)
              if(r <= k) data[r] <<- x[i]
              ### Note: we do not need to replace weight (is already 1)
            }   
            
            stream_size <<- stream_size + 1L
          }
        }
        
      }else{ ### biased
        if(is.null(data)) data <<- list()
        
        for(i in 1:length(x)){
          ### add all new points and replace point in reservoir with prob=size/k  
          prob <- length(data)/k
          if(sample(c(TRUE, FALSE), 1L, prob=c(prob, 1-prob))) {
            data[sample.int(nrow(data), 1L)] <<- x[i]
          }else{
            data <<- append(data, x[i])
          }
          
          stream_size <<- stream_size + 1L
        }
      }
    },
    
    get_points = function(...) { 
      if(!is.null(data)) data else data.frame() 
      },
    
    get_weights = function(...) { 
      n <- if(is.null(data)) 0L else 
        if(is.data.frame(data)) nrow(data) else length(data)
      rep(1, n) 
    }
  )
)


### DSC interface to SampleDSO
SampleDSC <- setRefClass("SampleDSC", 
  contains="SampleDSO",
  
  methods = list(
    cluster = function(x, ...) update(x, ...),
    get_microclusters = function(...) get_points(...), 
    get_microweights = function(...) get_weights(...)
  )
)

