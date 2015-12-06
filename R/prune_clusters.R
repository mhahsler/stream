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


### get rid of clusters with low weight

prune_clusters <- function(dsc, threshold=.05, weight=TRUE) {

    ### make a static copy first
    dsc <- DSC_Static(dsc)
    
    w <- get_weights(dsc)

    if(weight) {
	o <- order(w)
	o <- o[cumsum(w[o])>sum(w)*threshold]
	#o <- o[w[o] > quantile(w,prob=threshold)]
    } else {
	o <- order(w,decreasing = TRUE)
	o <- head(o,length(o)*(1-threshold))
    }
    
    dsc$RObj$weights <- dsc$RObj$weights[o]
    dsc$RObj$centers <- dsc$RObj$centers[o,]

    dsc
}
