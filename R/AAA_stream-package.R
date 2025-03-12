#' @keywords internal
#'
#' @section Key Classes:
#' - [DSD] Data stream data and [DSF] data stream filter to transform data streams in flight.
#' - [DST] Data stream mining tasks:
#'    - [DSAggregate] Data stream aggregator
#'    - [DSC] Data stream clustering
#'    - [DSClassifier] Data stream classifiers
#'    - [DSOutlier] Data stream outlier detectors
#'    - [DSRegressor] Data stream regressors
#'
#' @import Rcpp
#' @import proxy
#' @import magrittr
#' @importFrom methods is new
#' @importFrom dbscan dbscan
#' @importFrom utils head read.table write.table
# #' @importFrom rpart rpart
#' @importFrom stats predict runif rnorm prcomp mahalanobis terms
#' @importFrom graphics plot par layout title pairs points lines image
#' @importFrom grDevices gray gray.colors rgb col2rgb
#' @importFrom mlbench mlbench.2dnormals mlbench.cassini mlbench.circle  mlbench.cuboids mlbench.friedman1  mlbench.friedman2  mlbench.friedman3 mlbench.hypercube  mlbench.peak  mlbench.ringnorm  mlbench.shapes mlbench.simplex mlbench.smiley  mlbench.spirals mlbench.threenorm  mlbench.twonorm  mlbench.waveform mlbench.xor
#'
#' @useDynLib stream, .registration=TRUE
"_PACKAGE"
