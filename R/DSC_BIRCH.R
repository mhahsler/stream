#' Balanced Iterative Reducing Clustering using Hierarchies
#'
#' BIRCH builds a balanced-tree of Clustering Features (CFs) to summarize the stream.
#' A CF is a tuple (n, LS, SS) which represents a cluster by storing the number of elements (n), their linear sum (LS) and their squared sum (SS).
#' Each new observation descends the tree by following its closest CF until a leaf node is reached.
#' It is either merged into its closest leaf-CF or inserted as a new one.
#' All leaf-CFs form the micro-clusters. Rebuilding the tree is realized by inserting all leaf-CF nodes into a new tree structure with an increased treshold.
#'
#' @param treshold treshold used to check whether a new datapoint can be absorbed or not
#' @param branching branching factor (maximum amount of child nodes for a nonleaf node) of the CF-Tree.
#' @param maxLeaf maximum number of entries within a leaf node
#' @param maxmem memory limitation for the whole CFTree in bytes. Default is 0, indicating no memory restriction.
#' @param outlierThreshold threshold for identifying outliers when rebuilding the CF-Tree
#'
#' @references
#' Zhang T, Ramakrishnan R and Livny M (1996), "BIRCH: An Efficient Data Clustering Method for Very Large Databases", In Proceedings of the 1996 ACM SIGMOD International Conference on Management of Data. Montreal, Quebec, Canada , pp. 103-114. ACM.
#' Zhang T, Ramakrishnan R and Livny M (1997), "BIRCH: A new data clustering algorithm and its applications", Data Mining and Knowledge Discovery. Vol. 1(2), pp. 141-182.
#'
#' @author
#' Dennis Assenmacher (\email{Dennis.Assenmacher@@uni-muenster.de})
#' Matthias Carnein (\email{Matthias.Carnein@@uni-muenster.de})
#'
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' BIRCH <- DSC_BIRCH(treshold = .1, branching = 8, maxLeaf = 20, outlierThreshold = 0.25, maxMem=0)
#' update(BIRCH, stream, n = 500)
#' plot(BIRCH,stream)
#'
#' @export
DSC_BIRCH <- function(treshold, branching,maxLeaf, maxMem=0, outlierThreshold=0.25) {

  structure(
    list(
      description = "BIRCH",
      RObj = birch$new(treshold = treshold, branching=branching, maxLeaf=maxLeaf, maxMem=maxMem, outlierThreshold=outlierThreshold)
    ), class = c("DSC_BIRCH", "DSC_Micro", "DSC_R", "DSC")
  )
}




#' Reference Class RBIRCH
#'
#' @field C Exposed C++ class
#'
#' @export BIRCH
birch <- setRefClass("BIRCH", fields = list(
  C ="ANY"
))


#' @title CF-Tree creation
#' @description Initializes an empty CF-Tree.
#' @param treshold Specifies the treshold used to check whether a new datapoint can be absorbed or not.
#' @param branching Specifies the branching factor (maximum amount of child nodes for a nonleaf node) of the CF-Tree.
#' @param maxLeaf Specifies the maximum number of entries within a leaf node.
#' @param maxMemory Specifies the memory limitation for the whole CFTree in bytes. Default is 0, indicating no memory restriction.
#' @param outlierThreshold Specifies the threshold for identifying outliers when rebuilding the CF-Tree.
#' @name BIRCH-initialize
NULL
birch$methods(
  initialize = function(treshold,branching,maxLeaf,maxMem,outlierThreshold){
    C <<- new(BIRCH,treshold,branching,maxLeaf,maxMem,outlierThreshold) ## Exposed C class
    .self
  })

#' @title CF-Tree insertion
#' @description All data objects of a matrix are rowwise inserted into the CF-Tree
#' @param newdata Matrix of data objects.
#' @name BIRCH-cluster
NULL
birch$methods(
  cluster = function(newdata) {
    .self$C$udpateTree(data.matrix(newdata))
  })

#' @title CF-Tree deletion
#' @description Deletes the whole tree structure.
#' @name BIRCH-deleteTree
NULL
birch$methods(
  deleteTree = function(){
    .self$C$deleteTree()
    .self$tree <- NULL
  })

#' @title Centroids of micro clusters
#' @description This function returns all micro clusters of a given CF-Tree.
#' @name BIRCH-get_microclusters
NULL
birch$methods(
  get_microclusters = function(x,...) {
    .self$C$getCentroids()
  })

#' @title Weights of micro clusters
#' @description This function returns all micro cluster weights of a given CF-Tree.
#' @name BIRCH-get_microweights
NULL
birch$methods(
  get_microweights = function(x,...){
    .self$C$getWeights()
  })

