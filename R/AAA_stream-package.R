#' @title `r packageDescription("stream")$Package`: `r packageDescription("stream")$Title`
#'
#' @description `r packageDescription("stream")$Description`
#'
#' @author Michael Hahsler
#' @docType package
#' @name stream-package
#'
#' @import Rcpp
#' @import proxy
#' @importFrom methods is new
#' @importFrom dbscan dbscan
#' @importFrom utils head read.table write.table
#' @importFrom stats runif complete.cases rnorm prcomp na.omit mahalanobis
#' @importFrom graphics plot par layout title pairs points lines image
#' @importFrom grDevices gray gray.colors rgb col2rgb
#' @importFrom mlbench mlbench.2dnormals mlbench.cassini mlbench.circle  mlbench.cuboids mlbench.friedman1  mlbench.friedman2  mlbench.friedman3 mlbench.hypercube  mlbench.peak  mlbench.ringnorm  mlbench.shapes mlbench.simplex mlbench.smiley  mlbench.spirals mlbench.threenorm  mlbench.twonorm  mlbench.waveform mlbench.xor
#'
#' @useDynLib stream, .registration=TRUE
NULL
