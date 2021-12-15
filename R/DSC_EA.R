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



#' Evolutionary Algorithm
#'
#' Reclustering using an evolutionary algorithm.
#' This approach was designed for \code{evoStream} but can also be used for other micro-clustering algorithms.
#' The evolutionary algorithm uses existing clustering solutions and creates small variations of them by combining and randomly modfiying them.
#' The modified solutions can yield better partitions and thus can improve the clustering over time.
#' The evolutionary algorithm is incremental, which allows to improve existing macro-clusters instead of recomputing them every time.
#'
#' @param k number of macro-clusters
#' @param generations number of EA generations performed during reclustering
#' @param crossoverRate cross-over rate for the evolutionary algorithm
#' @param mutationRate mutation rate for the evolutionary algorithm
#' @param populationSize number of solutions that the evolutionary algorithm maintains
#'
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#'
#' @references Carnein M. and Trautmann H. (2018), "evoStream - Evolutionary Stream Clustering Utilizing Idle Times", Big Data Research.
#'
#' @examples
#' stream <- DSD_Memory(DSD_Gaussians(k = 3, d = 2), 1000)
#'
#' ## online algorithm
#' dbstream <- DSC_DBSTREAM(r=0.1)
#'
#' ## offline algorithm
#' EA <- DSC_EA(k=3, generations=1000)
#'
#' ## create pipeline and insert observations
#' two <- DSC_TwoStage(dbstream, EA)
#' update(two, stream, n=1000)
#'
#' ## plot resut
#' reset_stream(stream)
#' plot(two, stream, type="both")
#'
#' ## if we have time, evaluate additional generations. This can be
#' ## called at any time, also between observations.
#' two$macro_dsc$RObj$recluster(2000)
#'
#' ## plot improved result
#' reset_stream(stream)
#' plot(two, stream, type="both")
#'
#'
#' ## alternatively: do not create twostage but apply directly
#' reset_stream(stream)
#' update(dbstream, stream, n = 1000)
#' recluster(EA, dbstream)
#' reset_stream(stream)
#' plot(EA, stream)
#'
#'
#' @export
DSC_EA <- function(k, generations=2000, crossoverRate=.8, mutationRate=.001, populationSize=100) {


  EA <- EA_R$new(k, generations, crossoverRate, mutationRate, populationSize)


  structure(
    list(
      description = "EA - Reclustering using an evolutionary algorithm",
      RObj = EA
    ), class = c("DSC_EA", "DSC_Macro", "DSC_R", "DSC")
  )
}


EA_R <- setRefClass("EA",
                    fields = list(
                      crossoverRate	= "numeric",
                      mutationRate	= "numeric",
                      populationSize    = "integer",
                      k           = "integer",
                      data        = "data.frame",
                      weights	  = "numeric",
                      generations = "integer",
                      C = "ANY"
                    ),

                    methods = list(
                      initialize = function(k, generations, crossoverRate, mutationRate, populationSize) {

                        k <<- as.integer(k)
                        generations <<- as.integer(generations)
                        crossoverRate <<- crossoverRate
                        mutationRate <<- mutationRate
                        populationSize <<- as.integer(populationSize)
                      }
                    )
)

EA_R$methods(
  cluster = function(x, weight = rep(1,nrow(x)), ...) {

    data <<- x
    weights <<- weight

    ## initialize C object to access reclustering
    C <<- new(EvoStream)
    .self$C$reclusterInitialize(as.matrix(data), weights, .self$k, .self$crossoverRate, .self$mutationRate, .self$populationSize)

    ## recluster based on number of generations or time
    .self$C$recluster(.self$generations)
  },

  get_microclusters = function(...) { as.data.frame(.self$data) },
  get_microweights = function(...) { .self$weights },

  get_macroclusters = function(...) { as.data.frame(.self$C$get_macroclusters()) },
  get_macroweights = function(...) { .self$C$get_macroweights() },

  microToMacro = function(micro=NULL) {
    clusterAssignment = .self$C$microToMacro()+1
    if(!is.null(micro)){
      return(clusterAssignment[micro])
    } else{
      return(clusterAssignment)
    }
  },

  recluster = function(generations=1){.self$C$recluster(generations)}
)


