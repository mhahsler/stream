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


#' Abstract Class for Implementing R-based Clusterers
#'
#' Abstract class for implementing R-based clusterers.
#'
#'
#' [DSC_R] cannot be instantiated.
#'
#' **Implementing new Classes**
#'
#' To implement a new clusterer you need to create an S3 class with elements `description` and
#' `RObj`. `RObj` needs to be a reference class with methods:
#'
#' * `cluster(newdata, ...)`
#' * `get_microclusters(...)`
#' * `get_microweights(...)`
#' * `get_macroclusters(...)`
#' * `get_macroweights(...)`
#' * `microToMacro(micro, ...)`
#'
#' See [DSC] for details and parameters.
#'
#' `DSC_R` cannot be instantiated.
#'
#' @family DSC
#'
#' @param object a DSC object.
#' @param dsd a data stream object.
#' @param n number of data points taken from the stream.
#' @param verbose logical; show progress?
#' @param block process blocks of data to improve speed.
#' @param return a character string indicating what update returns. The default is `"nothing"`. Other
#' possible values depend on the `DST`. Examples are `"model"` and `"assignment"`.
#' @param ... further arguments.
#' @author Michael Hahsler
#' @export
DSC_R <- abstract_class_generator("DSC")

### cluster worker
### getting a block of data improves performance the R implementation
### needs to make sure that points are processed sequentially
### (make especially BIRCH faster by passing block data points at once)

#' @rdname DSC_R
#' @export
update.DSC_R <- function(object,
  dsd,
  n = 1L,
  verbose = FALSE,
  block = 10000L,
  return = c("nothing", "assignment", "model"),
  ...) {
  ### object contains an RObj which is a reference object with a cluster method
  return <- match.arg(return)
  n <- as.integer(n)
  block <- as.integer(block)

  if (n == 0L)
    return(switch(
      return,
      nothing = invisible(NULL),
      assignment = data.frame(.class = integer(0)),
      model = get_model(object)
    ))

  ### for data frame/matrix we do it all at once. n is ignored!
  if (is.data.frame(dsd) || is.matrix(dsd)) {
    if(!is.data.frame(dsd))
      dsd <- as.data.frame(dsd)

    if (!(n  == 1L || n == -1L || n == nrow(dsd)))
      warning("n is ignored if dsd is a data.frame. Update uses all points.")

    n <- nrow(dsd)
    block <- n
  }

  ### TODO: Check data
  take <- NULL
  if (verbose)
    total <- 0L
  for (bl in .make_block(n, block)) {
    p <- get_points(dsd, bl, info = FALSE)

    ## apply formula
    if (!is.null(object$formula)) {
      if (is.null(object$RObj$colnames)) {
        trms <- terms(object$formula, data = p)
        if (attr(trms, "response") != 0)
          stop("formula for clustering cannot have a response variable before '~'!")

        object$RObj$colnames <- colnames(attr(trms, "factors"))
      }

      p <- p[, object$RObj$colnames, drop = FALSE]
    } else
      if (is.null(object$RObj$colnames))
        object$RObj$colnames <- colnames(p)


      res <- object$RObj$cluster(p, ...)

      if (verbose) {
        total <- total + nrow(p)
        cat("Processed",
          total,
          "/",
          n,
          "points -",
          nclusters(object),
          "clusters\n")
      }
  }

  return(switch(
    return,
    nothing = invisible(NULL),
    assignment = {
      if (is.null(res))
        res <- predict(object, p)
      res
    },
    model = get_model(object)
  ))
}

### accessors
#' @export
get_microclusters.DSC_R <-
  function(x, ...)
    x$RObj$get_microclusters(...)

#' @export
get_microweights.DSC_R <-
  function(x, ...)
    x$RObj$get_microweights(...)

#' @export
get_macroclusters.DSC_R <-
  function(x, ...)
    x$RObj$get_macroclusters(...)

#' @export
get_macroweights.DSC_R <-
  function(x, ...)
    x$RObj$get_macroweights(...)

#' @export
microToMacro.DSC_R <-
  function(x, micro = NULL, ...)
    x$RObj$microToMacro(micro, ...)


### make a deep copy of the reference class in RObj
#' @export
get_copy.DSC_R <- function(x) {
  temp <- x

  temp$RObj <- x$RObj$copy(TRUE)

  if (is.environment(temp$macro))
    temp$macro <-
    as.environment(as.list(temp$macro, all.names = TRUE))

  temp
}
