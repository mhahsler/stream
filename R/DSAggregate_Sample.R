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

#' Sampling from a Data Stream (Data Stream Operator)
#'
#' Extracts a sample form a data stream using Reservoir Sampling.
#'
#' If `biased = FALSE` then the reservoir sampling algorithm by McLeod and
#' Bellhouse (1983) is used. This sampling makes sure that each data point has
#' the same chance to be sampled. All sampled points will have a weight of 1.
#' Note that this might not be ideal for an evolving stream since very old data
#' points have the same chance to be in the sample as newer points.
#'
#' If `bias = TRUE` then sampling prefers newer points using the modified
#' reservoir sampling algorithm 2.1 by Aggarwal (2006). New points are always
#' added. They replace a random point in thre reservoir with a probability of
#' reservoir size over `k`. This an exponential bias function of
#' \eqn{2^{-lambda}} with \eqn{lambda = 1 / k}.
#'
#' @family DSAggregate
#'
#' @param k the number of points to be sampled from the stream.
#' @param biased if `FALSE` then a regular (unbiased) reservoir sampling
#' is used. If true then the sample is biased towards keeping more recent data
#' points (see Details section).
#' @return An object of class `DSAggregate_Sample` (subclass of `DSAggregate`).
#' @author Michael Hahsler
#' @references Vitter, J. S. (1985): Random sampling with a reservoir. ACM
#' Transactions on Mathematical Software, 11(1), 37-57.
#'
#' McLeod, A.I., Bellhouse, D.R. (1983): A Convenient Algorithm for Drawing a
#' Simple Random Sample. Applied Statistics, 32(2), 182-184.
#'
#' Aggarwal C. (2006) On Biased Reservoir Sampling in the Presence of Stream
#' Evolution. International Conference on Very Large Databases (VLDB'06).
#' 607-618.
#' @examples
#' set.seed(1500)
#'
#' stream <- DSD_Gaussians(k = 3, noise = 0.05)
#'
#' sample <- DSAggregate_Sample(k = 50)
#' update(sample, stream, 500)
#' sample
#'
#' head(get_points(sample))
#'
#' # apply k-means clustering to the sample (data without info columns)
#' km <- kmeans(get_points(sample, info = FALSE), centers = 3)
#' plot(get_points(sample, info = FALSE))
#' points(km$centers, col = "red", pch = 3, cex = 2)
#' @export
DSAggregate_Sample <- function(k = 100, biased = FALSE)
  structure(
    list(
      description =
        if (biased)
          "Reservoir sampling (biased)"
      else
        "Reservoir sampling",
      RObj = SampleDSAggregate$new(k = k, biased = biased)
    ),
    class = c("DSAggregate_Sample", "DSAggregate", "DST")
  )

#' @export
update.DSAggregate_Sample <-
  function(object,
    dsd,
    n = 1,
    return = c("nothing", "model"),
    verbose = FALSE,
    ...) {
    ### some matrix to be processed in one go

    return <- match.arg(return)

    if (!is(dsd, "DSD")) {
      n <- nrow(dsd)
      dsd <- DSD_Memory(dsd)
    }

    ### FIXME: we do not need to get all points if n is very large!
    object$RObj$update(get_points(dsd, n = n, info = TRUE), verbose = verbose, ...)

    return(switch(
      return,
      nothing = invisible(NULL),
      model = get_model(object)
    ))
  }

#' @export
get_points.DSAggregate_Sample <- function(x, info = TRUE, ...)
  x$RObj$get_points(info = info, ...)

#' @export
get_weights.DSAggregate_Sample <- function(x, ...)
  x$RObj$get_weights(...)


SampleDSAggregate <- setRefClass(
  "SampleDSAggregate",
  fields = list(
    k	= "integer",
    biased = "logical",
    stream_size	= "integer",
    data	= "ANY"   ### data.frame or list (NULL = unknown)
  ),

  methods = list(
    initialize = function(k	= 100L,
      biased = FALSE) {
      k	<<- as.integer(k)
      biased	<<- biased
      stream_size	<<- 0L

      data <<- NULL

      .self
    },

    ### Reservoir sampling:
    ### unbiased: all values in the stream have the same prob. to be sampled
    ### biased: more recent values have a higher probability

    update = function(x, ...) {
      if (!is(data, class(x)) && !is.null(data))
        stop("Data stream data type not compatible!")

      if (is.data.frame(x))
        update_data.frame(x)
      else
        update_list(x)

      # sample does not return a cluster assignment
      NULL
    },

    update_data.frame = function(x, ...) {
      if (!biased) {
        ### fast initialization
        if (is.null(data)) {
          if (nrow(x) <= k)
            data <<- x
          else
            data <<- x[sample(1:nrow(x), k), ]
          stream_size <<- nrow(x)
        } else{
          ### reservoir sampling
          for (i in 1:nrow(x)) {
            ### fill with values first
            if (nrow(data) < k) {
              data <<- rbind(data, x[i, ])

            } else{
              ### replace values with decreasing probabilities
              r <- sample.int(stream_size + 1L, size = 1L)
              if (r <= k)
                data[r,] <<- x[i, ]
              ### Note: we do not need to replace weight (is already 1)
            }

            stream_size <<- stream_size + 1L
          }
        }

      } else{
        ### biased
        if (is.null(data))
          data <<- data.frame()

        for (i in 1:nrow(x)) {
          ### add all new points and replace point in reservoir with prob=size/k
          prob <- nrow(data) / k
          if (sample(c(TRUE, FALSE), 1L, prob = c(prob, 1 - prob))) {
            data[sample.int(nrow(data), 1L), ] <<- x[i, ]
          } else{
            data <<- rbind(data, x[i, ])
          }

          stream_size <<- stream_size + 1L
        }
      }
    },

    update_list = function(x, ...) {
      if (!biased) {
        ### fast initialization
        if (is.null(data)) {
          if (length(x) <= k)
            data <<- x
          else
            data <<- x[sample(1:nrow(x), k)]
          stream_size <<- length(x)
        } else{
          ### reservoir sampling
          for (i in 1:length(x)) {
            ### fill with values first
            if (length(data) < k) {
              data <<- append(data, x[i])

            } else{
              ### replace values with decreasing probabilities
              r <- sample.int(stream_size + 1L, size = 1L)
              if (r <= k)
                data[r] <<- x[i]
              ### Note: we do not need to replace weight (is already 1)
            }

            stream_size <<- stream_size + 1L
          }
        }

      } else{
        ### biased
        if (is.null(data))
          data <<- list()

        for (i in 1:length(x)) {
          ### add all new points and replace point in reservoir with prob=size/k
          prob <- length(data) / k
          if (sample(c(TRUE, FALSE), 1L, prob = c(prob, 1 - prob))) {
            data[sample.int(nrow(data), 1L)] <<- x[i]
          } else{
            data <<- append(data, x[i])
          }

          stream_size <<- stream_size + 1L
        }
      }
    },

    get_points = function(info = TRUE, ...) {
      if (is.null(data))
        return(data.frame())

      if (!info)
        return(remove_info(data))

      return(data)
    },

    get_weights = function(...) {
      n <- if (is.null(data))
        0L
      else
        if (is.data.frame(data))
          nrow(data)
      else
        length(data)
      rep(1, n)
    }
  )
)


### DSC interface to SampleDSAggregate
SampleDSC <- setRefClass(
  "SampleDSC",
  fields = list(colnames = "ANY"),
  contains = "SampleDSAggregate",
  methods = list(
    cluster = function(x, ...)
      update(x, ...),
    get_microclusters = function(...)
      get_points(info = FALSE, ...),
    get_microweights = function(...)
      get_weights(...)
  )
)
