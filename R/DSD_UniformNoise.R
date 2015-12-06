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


DSD_UniformNoise <- function(d=2, range=NULL) {
  if(is.null(range)) range <- matrix(c(0,1), ncol=2, nrow=d, byrow=TRUE)
  structure(list(description = "Uniform Noise", d = d, range=range),
	      class=c("DSD_UniformNoise", "DSD_R", "DSD_data.frame", "DSD"))
  }
  
get_points.DSD_UniformNoise <- function(x, n=1, 
    cluster=FALSE, class=FALSE, ...) {
  .nodots(...)

    data <- as.data.frame(t(replicate(n, 
      runif(x$d, min=x$range[,1], max=x$range[,2]))))
    
    if(cluster) attr(data, "cluster") <- rep(NA_integer_, n)
    if(class) data <- cbind(data, class = rep(NA_integer_, n))
    
    data
}
