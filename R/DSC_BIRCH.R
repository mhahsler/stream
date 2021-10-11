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


DSC_registry$set_entry(name = "DSC_BIRCH",
  DSC_Micro = TRUE, DSC_Macro = FALSE,  DSC_Outlier = FALSE, DSC_SinglePass = FALSE,
  description = "BIRCH - Balanced Iterative Reducing Clustering using Hierarchies")

