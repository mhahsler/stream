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

### DSCs may overwrite get_assignment
get_assignment <- function(dsc, points, type=c("auto", "micro", "macro"), 
  method="auto", ...) 
  UseMethod("get_assignment")

### default method is Euclidean nearest neighbor "nn"
get_assignment.DSC <- function(dsc, points, type=c("auto", "micro", "macro"), 
  method=c("auto", "nn", "model"), ...) {
  
  method <- match.arg(method)
  if(method=="auto") method <- "nn"
  
  if(method=="model") {
    warning("method model not implemented! using Euclidean nearest neighbor instead!")
    method <- "nn"
  }
  
  c <- get_centers(dsc, type=type, ...)
  
  if(nrow(c)>0L) {
    dist <- dist(points, c, method="Euclidean")
    # Find the minimum distance and save the class
    predict <- apply(dist, 1L, which.min)
    
  } else {
    warning("There are no clusters!")
    predict <- rep(NA_integer_, nrow(points))
  }
  
  attr(predict, "method") <- method
  
  predict	
}

