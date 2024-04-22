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


#' BICO - Fast computation of k-means coresets in a data stream
#'
#' Micro Clusterer.
#' BICO maintains a tree which is inspired by the clustering tree of BIRCH.
#' Each node in the
#' tree represents a subset of these points. Instead of storing all points as
#' individual objects, only the number of points, the sum and the squared sum
#' of the subset's points are stored as key features of each subset. Points are
#' inserted into exactly one node.
#'
#' In this implementation, the nearest neighbor search on the first level of
#' the tree is sped up by projecting all points to random 1-d subspaces. The
#' first estimation of the optimal clustering cost is computed in a buffer
#' phase at the beginning of the algorithm. This implementation interfaces the
#' original C++ implementation available here:
#' \url{http://ls2-www.cs.tu-dortmund.de/grav/de/bico}. For micro-clustering,
#' the algorithm computes the coreset of the stream. Reclustering is performed
#' by using the \code{kmeans++} algorithm on the coreset.
#'
#' @aliases DSC_BICO BICO bico
#' @family DSC_Micro
#'
#' @param formula `NULL` to use all features in the stream or a model [formula] of the form `~ X1 + X2`
#'   to specify the features used for clustering. Only `.`, `+` and `-` are currently
#'   supported in the formula.
#' @param k number of centers.
#' @param space coreset size.
#' @param p number of random projections used for nearest neighbor search in
#' first level.
#' @param iterations number of repetitions for the kmeans++ procedure in the
#' offline component.
#' @author R-Interface: Matthias Carnein
#' (\email{Matthias.Carnein@@uni-muenster.de}), Dennis Assenmacher.
#' C-Implementation: Hendrik Fichtenberger, Marc Gille, Melanie Schmidt, Chris
#' Schwiegelshohn, Christian Sohler.
#' @references Hendrik Fichtenberger, Marc Gille, Melanie Schmidt, Chris
#' Schwiegelshohn, Christian Sohler: BICO: BIRCH Meets Coresets for k-Means
#' Clustering. \emph{ESA 2013:} 481-492.
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 2)
#'
#' BICO <- DSC_BICO(k = 3, p = 10, space = 100, iterations = 10)
#' update(BICO, stream, n = 500)
#'
#' plot(BICO,stream)
#' @export
DSC_BICO <- function(formula = NULL,
  k = 5,
  space = 10,
  p = 10,
  iterations = 10) {

  BICO <- BICO_R$new(k, space, p, iterations)

  structure(
    list(
      description = "BICO - Fast computation of k-means coresets",
      formula = formula,
      RObj = BICO
    ),
    class = c("DSC_BICO", "DSC_Micro", "DSC_R", "DSC")
  )
}


BICO_R <- setRefClass("BICO_R", fields = list(C = "ANY",
  colnames = "ANY"))

BICO_R$methods(
  cache = function() {
    stop("SaveDSC not implemented for DSC_BICO!")
  }
)

BICO_R$methods(
  initialize = function(k, space, p, iterations) {
    if (space < 1)
      stop("space needs to be > 0.")
    if (k < 1)
      stop("k needs to be > 0.")
    if (iterations < 1)
      stop("k needs to be > 0.")
    if (p < 1)
      stop("p needs to be > 0.")

    C <<- new(BICO, k, space, p, iterations) ## Exposed C class
    colnames <<- NULL

    .self
  }
)


BICO_R$methods(
  cluster = function(newdata) {
    .self$C$cluster(as.matrix(newdata))
  }
)


BICO_R$methods(
  get_microclusters = function() {
    centers <- .self$C$get_microclusters()
    colnames(centers) <- .self$colnames
    centers
  }
)

BICO_R$methods(
  get_microweights = function() {
    .self$C$get_microweights()
  }
)

BICO_R$methods(
  get_macroclusters = function() {
    centers <- .self$C$get_macroclusters()
    colnames(centers) <- .self$colnames
    centers
  }
)

BICO_R$methods(
  get_macroweights = function() {
    .self$C$get_macroweights()
  }
)

BICO_R$methods(
  microToMacro = function(micro = NULL, ...) {
    assignment = .self$C$microToMacro() + 1L ## from C to R indexing
    if (is.null(micro))
      micro <- 1:length(assignment)
    structure(assignment[micro], names = micro)
  }
)
