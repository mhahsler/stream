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

MGC_Random_refClass <- setRefClass("MGC_Random", 
  fields = list(
    start = "numeric",
    current = "numeric",
    parameter = "numeric",
    density = "numeric",
    lastUpdate = "numeric",
    randomness = "numeric",
    dimension = "numeric",
    shape = "function"
  ), 
  
  methods = list(
    initialize = function(s,p,d,r,sha) {
      start  <<- s
      current <<- s
      density <<- d
      parameter <<- p
      randomness <<- r
      lastUpdate <<- 1
      shape <<- sha
      dimension <<- length(s)
      .self
    }
    
  ),
)

MGC_Random_refClass$methods(
  get_attributes = function(time, attributes=NULL) {
      att <- list(density = density, parameter=parameter, randomness=randomness)
      if(!is.null(attributes)) att <- att[attributes]
      att
  },
  
  get_points = function(time) {
    if(time == 1) current <<- start
    if(floor(time) > lastUpdate) {
      current <<- current + runif(length(current), -randomness, randomness)
      lastUpdate <<- floor(time)
    }
    
    shape(center=current, parameter=parameter)
  }
)

### creator    
MGC_Random<- function(density, center, parameter, randomness=1, shape=NULL) {
  if(is.null(shape)) shape <- MGC_Shape_Gaussian
  
  structure(list(description = "Random Moving Generator Cluster",
    RObj = MGC_Random_refClass$new(center, parameter, density, randomness, shape)),
    class = c("MGC_Random","MGC"))
}