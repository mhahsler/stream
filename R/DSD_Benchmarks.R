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

DSD_Benchmark <- function(i=1) {
  if(i==1) {
    return(DSD_MG(dimension = 2,
      MGC_Linear(dimension = 2, list(
        keyframe(time = 0, density=10, parameter=.01, center=c(.1,.9)),
        keyframe(time = 250, density=10, parameter=.01, center=c(.9,.1)),
        keyframe(time = 500, density=10, parameter=.01, center=c(.1,.9), reset=TRUE)
      )),
      MGC_Linear(dimension = 2, list(
        keyframe(time = 0, density=10, parameter=.01, center=c(.1,.1)),
        keyframe(time = 250, density=10, parameter=.01, center=c(.9,.9)),
        keyframe(time = 500, density=10, parameter=.01, center=c(.1,.1), reset=TRUE)
      )),
      MGC_Noise(density=2, range=rbind(c(0,1),c(0,1))),
      labels = c(1,2,NA),
      description = "Benchmark 1: Two clusters moving diagonally from left to right, meeting in the center (5% noise)."
      ))
  }  
  
  
  if(i==2) {
      return(DSD_MG(dimension = 2,
        MGC_Static(density=1, parameter=.1, center=c(0,0)),
        MGC_Static(density=1, parameter=.1, center=c(1,1)),
        MGC_Linear(dimension = 2, list(
          keyframe(time = 0, density=1, parameter=.1, center=c(0,0)),
          keyframe(time = 500, density=1, parameter=.1, center=c(1,1)),
          keyframe(time = 1000, density=1, parameter=.1, center=c(0,0), reset=TRUE)
        )),
        description = "Benchmark 2: Two fixes clusters. A third cluster moves between them (no noise)."))
    }

    stop("Unknown benchmark!")
}

