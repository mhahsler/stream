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


DSC_BICO <- function(k=5, space=10, p=10, iterations=10) {

  BICO <- BICO_R$new(k, space, p, iterations)

  structure(
    list(
      description = "BICO - Fast computation of k-means coresets",
      RObj = BICO
    ), class = c("DSC_BICO", "DSC_Micro", "DSC_R", "DSC")
  )
}


BICO_R <- setRefClass("BICO_R", fields = list(
  C ="ANY"
))

BICO_R$methods(
  cache = function(){
    stop("SaveDSC not implemented for DSC_BICO!")
  })

BICO_R$methods(
  initialize = function(k, space, p, iterations) {
    C <<- new(BICO, k, space, p, iterations) ## Exposed C class
    .self
  }
)


BICO_R$methods(
  cluster = function(newdata){
    .self$C$cluster(as.matrix(newdata))
  }
)


BICO_R$methods(
  get_microclusters = function() {
    .self$C$get_microclusters()
  }
)

BICO_R$methods(
  get_microweights = function() {
    .self$C$get_microweights()
  }
)

BICO_R$methods(
  get_macroclusters = function() {
    .self$C$get_macroclusters()
  }
)

BICO_R$methods(
  get_macroweights = function() {
    .self$C$get_macroweights()
  }
)

BICO_R$methods(
  microToMacro = function(micro=NULL, ...){
    assignment = .self$C$microToMacro() + 1L ## from C to R indexing
    if(is.null(micro)) micro <- 1:length(assignment)
    structure(assignment[micro], names=micro)
  }
)

DSC_registry$set_entry(name = "DSC_BICO",
  DSC_Micro = TRUE, DSC_Macro = FALSE, DSC_Outlier = FALSE, DSC_SinglePass = FALSE,
  description = "BICO - Fast computation of k-means coresets")

