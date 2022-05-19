#######################################################################
# stream -  Infrastructure for Data Stream Mining
# Copyright (C) 2013 Michael Hahsler, Matthew Bolanos, John Forrest
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



#' TwoStage Clustering Process
#'
#' Combines a micro and a macro clustering algorithm into a single process.
#'
#' \code{update()} runs the micro-clustering stage and only when macro cluster
#' centers/weights are requested, then the offline stage reclustering is
#' automatically performed.
#'
#' @family DSC
#'
#' @param micro Clustering algorithm used in the online stage
#' (\code{DSC_micro})
#' @param macro Clustering algorithm used for reclustering in the offline stage
#' (\code{DSC_macro})
#' @return An object of class \code{DSC_TwoStage} (subclass of \code{DSC},
#' \code{DSC_Macro}).
#' @author Michael Hahsler
#' @examples
#'
#' stream <- DSD_Gaussians(k=3)
#'
#' # Create a clustering process that uses a window for the online stage and
#' # k-means for the offline stage (reclustering)
#' win_km <- DSC_TwoStage(
#'   micro=DSC_Window(horizon=100),
#'   macro=DSC_Kmeans(k=3)
#'   )
#' win_km
#'
#' update(win_km, stream, 200)
#' win_km
#' plot(win_km, stream, type="both")
#' evaluate(win_km, stream, assign="macro")
#'
#' @export
DSC_TwoStage <- function(micro, macro) {

  state <- new.env()
  state$newdata <- TRUE
  l <- list(
    description = paste(micro$description, " + ",
                        macro$description, sep=''),
    micro_dsc = micro,
    macro_dsc = macro,
    macro = state
  )
  czs <- c("DSC_Macro", "DSC")
### MFH: this should be its own DSOutlier class
#  if(is(micro,"DSOutlier")) {
#    czs <- c("DSOutlier", czs)
#    l$recheck_outliers <- micro$recheck_outliers
#  }
#  if(is(micro,"DSC_SinglePass")) czs <- c("DSC_SinglePass", czs)
  czs <- c("DSC_TwoStage", czs)

  structure(l,class = czs)
}

### TwoStage has its own interface (does not use DSC_R)
#' @export
update.DSC_TwoStage <- function(object, dsd, n=1, verbose=FALSE,
                                block=10000L, ...) {
  ### dsc contains an RObj which is  a reference object with a cluster method

  ### some matrix to be processed in one go
  if(!is(dsd, "DSD")) {
    n <- nrow(dsd)
    dsd <- DSD_Memory(dsd)
  }

  n <- as.integer(n)
  if(n>0) {
    if(!is(dsd, "DSD_data.frame"))
      stop("Cannot cluster stream (need a DSD_data.frame.)")

    ### for DSC_TwoStage
    if(is.environment(object$macro)) object$macro$newdata <- TRUE

    ### TODO: Check data
    for(bl in .make_block(n, block)) {
      update(object$micro_dsc, dsd, n=bl, ...)
      if(verbose) cat("Processed", bl, "points -",
                      nclusters(object), "clusters\n")
    }
  }

  # so cl <- cluster(cl, ...) also works
  invisible(object)
}

### accessors
#' @export
get_centers.DSC_TwoStage <- function(x, type=c("auto", "micro", "macro"), ...) {
  type <- match.arg(type)
  if(type=="micro") get_centers(x$micro_dsc)
  else {
    if(x$macro$newdata) {
      recluster(x$macro_dsc, x$micro_dsc)
      x$macro$newdata <- FALSE
    }
    get_centers(x$macro_dsc)
  }
}

#' @export
get_weights.DSC_TwoStage <- function(x, type=c("auto", "micro", "macro"), ...) {
  type <- match.arg(type)
  if(type=="micro") get_weights(x$micro_dsc, ...)
  else {
    if(x$macro$newdata) {
      recluster(x$macro_dsc, x$micro_dsc)
      x$macro$newdata <- FALSE
    }
    get_weights(x$macro_dsc, ...)
  }
}

#' @export
microToMacro.DSC_TwoStage <- function(x, micro=NULL, ...) {
  if(x$macro$newdata) {
    recluster(x$macro_dsc, x$micro_dsc)
    x$macro$newdata <- FALSE
  }
  microToMacro(x$macro_dsc, micro, ...)
}

#' @export
get_assignment.DSC_TwoStage <- function(dsc, points, type=c("auto", "micro", "macro"),
                                        method="auto", ...) {
  type <- match.arg(type)
  if(type=="micro") {
    if(is(dsc$micro_dsc, "DSC_SinglePass")) dsc$macro$newdata <- TRUE
    get_assignment(dsc$micro_dsc, points, type, method, ...)
  } else {
    if(dsc$macro$newdata) {
      recluster(dsc$macro_dsc, dsc$micro_dsc)
      dsc$macro$newdata <- FALSE
    }
    get_assignment(dsc$macro_dsc, points, type, method, ...)
  }
}

### make a deep copy
#' @export
get_copy.DSC_TwoStage <- function(x) {
  copy <- DSC_TwoStage(micro=get_copy(x$micro_dsc), macro=get_copy(x$macro_dsc))
  copy$macro$newdata <- x$macro$newdata
  copy
}
