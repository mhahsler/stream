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
MGC_Static <- function(density = 1, center, parameter, 
  shape = NULL) {
    if(is.null(shape)) shape <- MGC_Shape_Gaussian
  
    x <- MGC_Function( 
      density = function(t) density,
      parameter = function(t) parameter,
      center= function(t) center,
      shape = shape
    )
    
  x$description <- "Static Cluster"
  
  class(x) <- c("MGC_Static", class(x))
  x
}



