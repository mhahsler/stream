
#' Evolutionary Algorithm
#'
#' Reclustering using an evolutionary algorithm.
#' This approach is used by \code{evoStream} but can be used for all micro-clusters.
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
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 2)
#' dbstream <- DSC_DBSTREAM(r=0.1)
#' EA <- DSC_EA(k=3, generations=2000)
#'
#' two <- DSC_TwoStage(dbstream, EA)
#' update(two, stream, n=1200)
#' plot(two, stream, type="both")
#'
#' update(dbstream, stream, n = 1200)
#' recluster(EA, dbstream)
#' plot(EA, stream)
#'
#'
#' @export
DSC_EA <- function(k, generations=2000, crossoverRate=.8, mutationRate=.001, populationSize=100) {


  EA <- EA_R$new(k, generations, crossoverRate, mutationRate, populationSize)


  structure(
    list(
      description = "EA",
      RObj = EA
    ), class = c("DSC_EA", "DSC_Macro", "DSC_R", "DSC")
  )
}



#' Reference Class EA_R
#'
#' Reference class used for Reclustering using an evolutionary algorithm
#'
#' @field crossoverRate cross-over rate for the evolutionary algorithm
#' @field mutationRate mutation rate for the evolutionary algorithm
#' @field populationSize number of solutions that the evolutionary algorithm maintains
#' @field k number of macro-clusters
#' @field generations number of EA generations performed during reclustering
#' @field data micro-clusters to recluster
#' @field weights weights of the micro-clusters
#' @field C exposed C class
#'
#' @author Matthias Carnein \email{matthias.carnein@@uni-muenster.de}
#'
EA_R <- setRefClass("EA",
                    fields = list(
                      crossoverRate	        = "numeric",
                      mutationRate	    = "numeric",
                      populationSize    = "integer",
                      k  = "integer",
                      data        = "data.frame",
                      weights	    = "numeric",
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
  }


)
