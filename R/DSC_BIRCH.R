#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2018 Dennis Assenmacher, Matthias Carnein and Michael Hahsler
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




#' Balanced Iterative Reducing Clustering using Hierarchies
#' 
#' Micro Clusterer.
#' BIRCH builds a balanced tree of Clustering Features (CFs) to summarize the
#' stream.
#' 
#' A CF in the calanced tree is a tuple (n, LS, SS) which represents a cluster
#' by storing the number of elements (n), their linear sum (LS) and their
#' squared sum (SS). Each new observation descends the tree by following its
#' closest CF until a leaf node is reached. It is either merged into its
#' closest leaf-CF or inserted as a new one. All leaf-CFs form the
#' micro-clusters. Rebuilding the tree is realized by inserting all leaf-CF
#' nodes into a new tree structure with an increased threshold.
#' 
#' @aliases DSC_BIRCH BIRCH birch
#' @param threshold threshold used to check whether a new datapoint can be
#' absorbed or not
#' @param branching branching factor (maximum amount of child nodes for a
#' nonleaf node) of the CF-Tree.
#' @param maxLeaf maximum number of entries within a leaf node
#' @param outlierThreshold threshold for identifying outliers when rebuilding
#' the CF-Tree
#' @param maxMem memory limitation for the whole CFTree in bytes. Default is 0,
#' indicating no memory restriction.
#' @author Dennis Assenmacher (\email{Dennis.Assenmacher@@uni-muenster.de}),
#' Matthias Carnein (\email{Matthias.Carnein@@uni-muenster.de})
#' @references Zhang T, Ramakrishnan R and Livny M (1996), "BIRCH: An Efficient
#' Data Clustering Method for Very Large Databases", \emph{In Proceedings of
#' the 1996 ACM SIGMOD International Conference on Management of Data.}
#' Montreal, Quebec, Canada , pp. 103-114. ACM.
#' 
#' Zhang T, Ramakrishnan R and Livny M (1997), "BIRCH: A new data clustering
#' algorithm and its applications", \emph{Data Mining and Knowledge Discovery.}
#' Vol. 1(2), pp. 141-182.
#' @examples
#' 
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' 
#' BIRCH <- DSC_BIRCH(threshold = .1, branching = 8, maxLeaf = 20)
#' update(BIRCH, stream, n = 500)
#' 
#' plot(BIRCH,stream)
#' 
#' 
#' @export DSC_BIRCH
DSC_BIRCH <- function(threshold, branching, maxLeaf, maxMem = 0, outlierThreshold = 0.25) {

  structure(
    list(
      description = "BIRCH - Balanced Iterative Reducing Clustering using Hierarchies",
      RObj = birch$new(threshold = threshold, branching=branching, maxLeaf=maxLeaf, maxMem=maxMem, outlierThreshold=outlierThreshold)
    ), class = c("DSC_BIRCH", "DSC_Micro", "DSC_R", "DSC")
  )
}

birch <- setRefClass("BIRCH", fields = list(
  C ="ANY"
))

#  CF-Tree creation: Initializes an empty CF-Tree.
# param threshold Specifies the threshold used to check whether a new datapoint can be absorbed or not.
# param branching Specifies the branching factor (maximum amount of child nodes for a nonleaf node) of the CF-Tree.
# param maxLeaf Specifies the maximum number of entries within a leaf node.
# param maxMemory Specifies the memory limitation for the whole CFTree in bytes. Default is 0, indicating no memory restriction.
# param outlierThreshold Specifies the threshold for identifying outliers when rebuilding the CF-Tree.
birch$methods(
  initialize = function(threshold,branching,maxLeaf,maxMem,outlierThreshold){
    C <<- new(BIRCH,threshold,branching,maxLeaf,maxMem,outlierThreshold) ## Exposed C class
    .self
  })


birch$methods(
  cache = function(){
    stop("SaveDSC not implemented for DSC_BIRCH!")
  })

# CF-Tree insertion: All data objects of a matrix are rowwise inserted into the CF-Tree
birch$methods(
  cluster = function(newdata) {
    .self$C$udpateTree(data.matrix(newdata))
  })

# Deletes the whole tree structure.
birch$methods(
  deleteTree = function(){
    .self$C$deleteTree()
    .self$tree <- NULL
  })

# This function returns all micro clusters of a given CF-Tree.
birch$methods(
  get_microclusters = function(x,...) {
    .self$C$getCentroids()
  })

# This function returns all micro cluster weights of a given CF-Tree.
birch$methods(
  get_microweights = function(x,...){
    .self$C$getWeights()
  })


