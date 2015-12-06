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

add_keyframe <- function(x, time, density, center, parameter, reset = FALSE) 
  UseMethod("add_keyframe")
get_keyframes <- function(x) 
  UseMethod("get_keyframes")
remove_keyframe <- function(x, time) 
  UseMethod("remove_keyframe")

keyframe <- function(time, density, center, parameter, reset = FALSE) 
  list(time=time, density=density, parameter=parameter, 
    center=center, reset=reset)


MGC_Linear_refClass <- setRefClass("MGC_Linear", 
  fields = list(
    keyframes = "data.frame",
    dimension = "numeric",
    shape = "function"
  ), 
  
  methods = list(
    initialize = function(dimension, shape) {
      keyframes  <<- data.frame(
        time =  numeric(), 
        parameter = list(), 
        density = numeric(), 
        centers = list(),
        reset = logical()
        )
      dimension <<- dimension
      shape <<- shape
      
      .self
    }
    
  ),
)

MGC_Linear_refClass$methods(
  add_keyframe = function(t, par, d, c, r) {
    if(dimension != length(c)) stop("Dimension in keyframe do not match the MGC dimensions!")
    
    ### check if keyframe exists
    exists <- which(keyframes$time==t)
    if(length(exists)>0) {
      warning("Existing keyframe at time ", t, " replaced!")
      keyframes <<- keyframes[-exists,]
    }
    
    keyframes <<- rbind(keyframes,
      data.frame(
        time=t, 
        parameter=I(list(par)),
        density=d, 
        centers=I(list(c)), 
        reset=r)
      )
    keyframes <<- keyframes[order(keyframes$time), ]
  },
  
  get_points = function(time) {
    attributes <- get_attributes(time, c("centers","parameter"))
    shape(center = attributes[["centers"]], parameter = attributes[["parameter"]])
  },
  
  get_attributes = function(time, attributes=NULL) {
    if(is.null(attributes)) attributes <- colnames(keyframes)
    kfs <- keyframes[, attributes, drop=FALSE]
    
    
    ### no keyframe
    if(nrow(keyframes)<1) return(list(time=time, 
      parameter=NA, density=0, center=NA, reset=FALSE)[attributes])

    ### reset?
    if(any(keyframes$reset)) {
      cycletime <- keyframes$time[min(which(keyframes$reset))]
      t <- time %% cycletime + 1
    } else t <- time
    
    nextkf <- findInterval(t, c(-Inf, keyframes$time))
    currentkf <- nextkf - 1L

    ### before first kf
    if(currentkf==0) return(list(time=time, 
      parameter=NA, density=0, center=NA, reset=FALSE)[attributes])
    
    ### at last kf
    #if(nextkf>nrow(kfs)) return(kfs[nrow(kfs),]) 
    if(nextkf>nrow(kfs)) return(lapply(kfs, "[[", nrow(kfs))) 
    
    ### in between two kf (weighted average)
    w <- (t - keyframes$time[currentkf]) / (keyframes$time[nextkf] - keyframes$time[currentkf])
    l <- lapply(kfs, FUN=function(x) (1-w)*x[[currentkf]]+w*x[[nextkf]])
    
    l
  }
)


### creator    
MGC_Linear<- function(dimension = 2, keyframelist=NULL, shape=NULL) {
  if(is.null(shape)) shape <- MGC_Shape_Gaussian
  
  x <- structure(list(description = "Linear Moving Generator Cluster",
    RObj = MGC_Linear_refClass$new(dimension=dimension, shape=shape)),
    class = c("MGC_Linear","MGC"))  
  
  if(!is.null(keyframelist)) lapply(keyframelist, FUN=function(kf)
    do.call("add_keyframe", c(list(x), kf)))
  
  x
}

add_keyframe.MGC_Linear <- function(x, time, density, center, parameter, 
  reset = FALSE) {
  x$RObj$add_keyframe(t=time, par=parameter, d=density, c=center, r=reset)
}

get_keyframes.MGC_Linear <- function(x) {
  x$RObj$keyframes
}

remove_keyframe.MGC_Linear <- function(x, time) {
  x$RObj$keyframes <- x$RObj$keyframes[which(x$RObj$keyframes$time!=time),]
}

print.MGC_Linear <- function(x, ...) {
  cat(paste(x$description, " (", paste(class(x), collapse=", "), ")", '\n', sep=""))
  temp <- '?'
  if(x$RObj$dimension > 0)
    temp <- x$RObj$dimension
  cat(paste('With', nrow(x$RObj$keyframes), 'keyframes', 'in', temp, 'dimensions', '\n'))
}