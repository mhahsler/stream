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

MGC_Function_refClass <- setRefClass("MGC_Function", 
  fields = list(
    dimension = "numeric",
    density = "function",
    center= "function",
    parameter = "function",
    shape = "function"
  ), 
  
  methods = list(
    initialize = function(den, cen, par, sha) {
      dimension <<- length(cen(1))
      density <<- den
      center <<- cen
      parameter <<- par
      shape <<- sha
      .self
    }
    
  ),
)

MGC_Function_refClass$methods(
  get_attributes = function(time, attributes=NULL) {
    att <- list(density = density(time), parameter=parameter(time), 
      center = center(time))
    if(!is.null(attributes)) att <- att[attributes]
    att
  },
  
  
  get_points = function(time) {
    shape(center=center(time), parameter=parameter(time))
  }
)

### creator    
MGC_Function<- function(density, center, parameter, shape = NULL) {
  if(is.null(shape)) shape <- MGC_Shape_Gaussian
  
  structure(list(description = "Functional Moving Generator Cluster",
    RObj = MGC_Function_refClass$new(den = density, cen = center, 
      par = parameter, sha = shape)),
    class = c("MGC_Function","MGC"))
}