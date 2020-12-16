#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2018 Matthias Carnein
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


#' evoStream - Evolutionary Stream Clustering
#'
#' Stream clustering algorithm based on evolutionary optimization.
#' The online component uses a simplified version of \code{DBSTREAM} to generate micro-clusters.
#' The micro-clusters are then incrementally reclustered using an evloutionary algorithm.
#' Evolutionary algorithms create slight variations by combining and randomly modifying existing solutions.
#' By iteratively selecting better solutions, an evolutionary pressure is created which improves the clustering over time.
#' Since the evolutionary algorithm is incremental, it is possible to apply it between observations, e.g. in the idle time of the stream.
#' Whenever there is idle time, we can call the \code{recluster} function of the reference class to improve the macro-clusters (see example).
#' The evolutionary algorithm can also be applied as a traditional reclustering step, or a combination of both.
#' In addition, this implementation also allows to evaluate a fixed number of generations after each observation.
#'
#' @param r radius threshold for micro-cluster assignment
#' @param lambda decay rate
#' @param tgap time-interval between outlier detection and clean-up
#' @param k number of macro-clusters
#' @param incrementalGenerations number of EA generations performed after each observation
#' @param reclusterGenerations number of EA generations performed during reclustering
#' @param crossoverRate cross-over rate for the evolutionary algorithm
#' @param mutationRate mutation rate for the evolutionary algorithm
#' @param populationSize number of solutions that the evolutionary algorithm maintains
#' @param initializeAfter number of micro-cluster required for the initialization of the evolutionary algorithm.
#'
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#'
#' @references Carnein M. and Trautmann H. (2018), "evoStream - Evolutionary Stream Clustering Utilizing Idle Times", Big Data Research.
#'
#' @examples
#' stream <- DSD_Memory(DSD_Gaussians(k = 3, d = 2), 500)
#'
#' ## init evoStream
#' evoStream <- DSC_evoStream(r=0.05, k=3, incrementalGenerations=1, reclusterGenerations=500)
#'
#' ## insert observations
#' update(evoStream, stream, n = 500)
#'
#' ## micro clusters
#' get_centers(evoStream, type="micro")
#'
#' ## micro weights
#' get_weights(evoStream, type="micro")
#'
#' ## macro clusters
#' get_centers(evoStream, type="macro")
#'
#' ## macro weights
#' get_weights(evoStream, type="macro")
#'
#' ## plot result
#' reset_stream(stream)
#' plot(evoStream, stream, type = "both")
#'
#' ## if we have time, evaluate additional generations.
#' ## This can be called at any time, also between observations.
#' ## by default, 1 generation is evaluated after each observation and
#' ## 1000 generations during reclustering but we set it here to 500
#' evoStream$RObj$recluster(500)
#'
#' ## plot improved result
#' reset_stream(stream)
#' plot(evoStream, stream, type="both")
#'
#' ## get assignment of micro to macro clusters
#' microToMacro(evoStream)
#'
#' @export
DSC_evoStream <- function(r, lambda=0.001, tgap=100, k=2, crossoverRate=.8, mutationRate=.001, populationSize=100, initializeAfter=2*k, incrementalGenerations=1, reclusterGenerations=1000) {

  evoStream <- evoStream_R$new(r, lambda, tgap, k, crossoverRate, mutationRate, populationSize, initializeAfter, incrementalGenerations, reclusterGenerations)

  structure(
    list(
      description = "evoStream - Evolutionary Stream Clustering",
      RObj = evoStream
    ), class = c("DSC_evoStream", "DSC_Micro", "DSC_R", "DSC")
  )
}


evoStream_R <- setRefClass("evoStream_R", fields = list(
  C ="ANY"
))

evoStream_R$methods(
  initialize = function(r, lambda, tgap, k, crossoverRate, mutationRate, populationSize, initializeAfter, incrementalGenerations, reclusterGenerations) {
    C <<- new(EvoStream) ## Exposed C class
    C$setFields(r, lambda, tgap, k, crossoverRate, mutationRate, populationSize, initializeAfter, incrementalGenerations, reclusterGenerations) ## since exposed constructors have limited parameters
    .self
  }
)

evoStream_R$methods(
  cluster = function(newdata){
    .self$C$cluster(as.matrix(newdata))
  }
)


evoStream_R$methods(
  get_microclusters = function() {
    as.data.frame(.self$C$get_microclusters())
  }
)

evoStream_R$methods(
  get_microweights = function() {
    .self$C$get_microweights()
  }
)

evoStream_R$methods(
  get_macroclusters = function() {
    as.data.frame(.self$C$get_macroclusters())
  }
)

evoStream_R$methods(
  get_macroweights = function() {
    .self$C$get_macroweights()
  }
)

evoStream_R$methods(
  microToMacro = function(micro=NULL) {
    clusterAssignment = .self$C$microToMacro()+1
    if(!is.null(micro)){
      return(clusterAssignment[micro])
    } else{
      return(clusterAssignment)
    }
  }
)

evoStream_R$methods(
  recluster = function(generations=1) {
    .self$C$recluster(generations)
  }
)

DSC_registry$set_entry(name = "DSC_evoStream",
  DSC_Micro = TRUE, DSC_Macro = FALSE,
  description = "evoStream - Evolutionary Stream Clustering")

