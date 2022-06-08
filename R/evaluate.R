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

## evaluate clusterings
## FIXME: calculate dist only once

#' Evaluate a Data Stream Mining Task
#'
#' Calculate evaluation measures for a data stream mining task [DST] using
#' a data stream [DSD] object.
#'
#' We provide two evaluation methods:
#'
#' - `evaluate_static()` evaluates the current [DST] model on new data without updating the model.
#' - `evaluate_stream()` evaluates the [DST] model using
#'      _prequential error estimation_ (see Gama, Sebastiao and Rodrigues; 2013). The data points
#'     in the horizon are first used to calculate the evaluation measure and then
#'     they are used for updating the cluster model. A horizon of ` means that each point is evaluated and
#'     then used to update the model.
#'
#' The evaluation measures depend on the task.
#'
#' @family DST
#' @family evaluation
#'
#' @name evaluate
#'
#' @param object The [DST] object that the evaluation measure is being requested
#'   from.
#' @param dsd The [DSD] object used to create the test data.
#' @param measure Evaluation measure(s) to use. If missing then all available
#'   measures are returned.
#' @param n The number of data points being requested.
#' @param horizon Evaluation is done using horizon many previous points (see
#' detail section).
#' @param verbose Report progress?
#' @param ... Further arguments.
#' @return `evaluate` returns an object of class `stream_eval` which
#' is a numeric vector of the values of the requested measures.
#' @author Michael Hahsler
#' @references
#' Joao Gama, Raquel Sebastiao, Pedro Pereira Rodrigues (2013). On
#' evaluating stream learning algorithms. _Machine Learning,_ March 2013,
#' Volume 90, Issue 3, pp 317-346.
#' @export
evaluate_static <-
  function(object,
    dsd,
    measure,
    n,
    ...)
    UseMethod("evaluate_static")

#' @rdname evaluate
#' @export
#' @export
evaluate_stream <-
  function(object,
    dsd,
    measure,
    n,
    horizon,
    ...,
    verbose = FALSE)
    UseMethod("evaluate_stream")
