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



#' Prune Clusters from a Clustering
#' 
#' Creates a (static) copy of a clustering where a fraction of the weight or
#' the number of clusters with the lowest weights were pruned.
#' 
#' 
#' @param dsc The DSC object to be pruned.
#' @param threshold The numeric vector of probabilities for the quantile.
#' @param weight should a fraction of the total weight in the clustering be
#' pruned? Otherwise a fraction of clusters is pruned.
#' @return Returns an object of class \code{DSC_Static}.
#' @author Michael Hahsler
#' @seealso \code{\link{DSC_Static}}
#' @examples
#' 
#' # 3 clusters with 10% noise
#' stream <- DSD_Gaussians(k=3, noise=0.1)
#' 
#' dbstream <- DSC_DBSTREAM(r=0.1)
#' update(dbstream, stream, 500)
#' dbstream
#' plot(dbstream, stream)
#' 
#' # prune lightest micro-clusters for 20% of the weight of the clustering
#' static <- prune_clusters(dbstream, threshold=0.2)
#' static
#' plot(static, stream)
#' 
#' @export prune_clusters
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
