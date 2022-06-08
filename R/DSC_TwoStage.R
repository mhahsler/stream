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
#' Combines an **online clustering component** ([DSC_Micro])
#' and an **offline reclustering component** ([DSC_Macro])
#' into a single process.
#'
#' \code{update()} runs the online micro-clustering stage and only when macro cluster
#' centers/weights are requested using [get_centers()] or [get_weights()], then the offline stage
#' reclustering is automatically performed.
#'
#' Available clustering methods can be found in the See Also section below.
#'
#' @family DSC_TwoStage
#' @family DSC
#'
#' @param micro Clustering algorithm used in the online stage
#' ([DSC_Micro])
#' @param macro Clustering algorithm used for reclustering in the offline stage
#' ([DSC_Macro])
#' @return An object of class `DSC_TwoStage` (subclass of [DSC],
#' [DSC_Macro]) which is a named list with elements:
#'
#'   - `description`: a description of the clustering algorithms.
#'   - `micro`: The [DSD] used for creating micro clusters in the online component.
#'   - `macro`: The [DSD] for offline reclustering.
#'   - `state`: an environment storing state information needed for reclustering.
#'
#'  with the two clusterers. The names are
#' ``
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k = 3)
#'
#' # Create a clustering process that uses a window for the online stage and
#' # k-means for the offline stage (reclustering)
#' win_km <- DSC_TwoStage(
#'   micro = DSC_Window(horizon = 100),
#'   macro = DSC_Kmeans(k = 3)
#'   )
#' win_km
#'
#' update(win_km, stream, 200)
#' win_km
#' win_km$micro
#' win_km$macro
#'
#' plot(win_km, stream, type = "both")
#' evaluate(win_km, stream, assign = "macro")
#' @export
DSC_TwoStage <- function(micro, macro) {
  l <- list(
    description = paste(micro$description, " + ",
      macro$description, sep = ''),
    micro = micro,
    macro = macro,
    state = as.environment(list(newdata = TRUE))
  )

  structure(l, class = c("DSC_TwoStage", "DSC_Macro", "DSC"))
}

### TwoStage has its own interface (does not use DSC_R)
#' @export
update.DSC_TwoStage <- function(object,
  dsd,
  n = 1,
  verbose = FALSE,
  block = 10000L,
  ...) {
  ### dsc contains an RObj which is  a reference object with a cluster method

  ### some matrix to be processed in one go
  if (!is(dsd, "DSD")) {
    n <- nrow(dsd)
    dsd <- DSD_Memory(dsd)
  }

  n <- as.integer(n)
  if (n > 0) {
    ### for DSC_TwoStage
    object$state$newdata <- TRUE

    ### TODO: Check data
    for (bl in .make_block(n, block)) {
      res <- update(object$micro, dsd, n = bl, ...)
      if (verbose)
        cat("Processed",
          bl,
          "points -",
          nclusters(object),
          "clusters\n")
    }
  }

  invisible(res)
}

### accessors
#' @export
get_centers.DSC_TwoStage <-
  function(x, type = c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if (type == "micro")
      get_centers(x$micro)
    else {
      if (x$state$newdata) {
        recluster(x$macro, x$micro)
        x$state$newdata <- FALSE
      }
      get_centers(x$macro)
    }
  }

#' @export
get_weights.DSC_TwoStage <-
  function(x, type = c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if (type == "micro")
      get_weights(x$micro, ...)
    else {
      if (x$state$newdata) {
        recluster(x$macro, x$micro)
        x$state$newdata <- FALSE
      }
      get_weights(x$macro, ...)
    }
  }

#' @export
microToMacro.DSC_TwoStage <- function(x, micro = NULL, ...) {
  if (x$state$newdata) {
    recluster(x$macro, x$micro)
    x$state$newdata <- FALSE
  }
  microToMacro(x$macro, micro, ...)
}

#' @export
get_assignment.DSC_TwoStage <-
  function(dsc,
    points,
    type = c("auto", "micro", "macro"),
    method = "auto",
    ...) {
    points <- remove_info(points)

    type <- match.arg(type)
    if (type == "micro") {
      dsc$state$newdata <- TRUE
      get_assignment(dsc$micro, points, type, method, ...)
    } else {
      if (dsc$state$newdata) {
        recluster(dsc$macro, dsc$micro)
        dsc$state$newdata <- FALSE
      }
      get_assignment(dsc$macro, points, type, method, ...)
    }
  }

### make a deep copy
#' @export
get_copy.DSC_TwoStage <- function(x) {
  copy <-
    DSC_TwoStage(micro = get_copy(x$micro),
      macro = get_copy(x$macro))
  copy$state <- as.environment(as.list(x$state))
  copy
}
