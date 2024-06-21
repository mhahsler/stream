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

#' Select Features for a Data Stream
#'
#' Select features from a data stream given a list of features.
#'
#' @family DSF
#'
#' @param dsd A object of class [DSD] that will be scaled.
#' @param features a character vector with feature (column) names or the numeric index of
#'  the selected features. All other features will be removed. Note special info columns
#'  starting with `.` are not features.
#' @return An object of class `DSF_FeatureSelection` (subclass of [DSF] and [DSD]).
#' @author Michael Hahsler
#' @examples
#' stream <- DSD_Gaussians(k = 3, d = 3)
#' get_points(stream, 3)
#'
#' stream_2features <- DSF_FeatureSelection(stream, features = c("X1", "X3"))
#' stream_2features
#'
#' get_points(stream_2features, n = 3)
#' @export
DSF_FeatureSelection <-
  function(dsd = NULL,
    features) {

    # creating the DSD object
    l <- list(
      description = paste0(
        ifelse(!is.null(dsd), dsd$description, "DSF without a specified DSD"),
        "\n  + Feature Selection (", toString(features), ")"
      ),
      dsd = dsd,
      features = features
    )
    class(l) <-
      c("DSF_FeatureSelection", "DSF", "DSD_R", "DSD")

    l
  }

#' @export
update.DSF_FeatureSelection <- function(object,
  dsd = NULL,
  n = 1L,
  return = "data",
  info = TRUE,
  ...) {
  .nodots(...)
  return <- match.arg(return)

  if (is.null(dsd))
    dsd <- object$dsd
  if (is.null(dsd))
    stop("No dsd specified in ", deparse1(substitute(object)), ". Specify a dsd in update().")

  points <-
    get_points(dsd,
      n,
      info = info)

  sel <- object$features

  if (info) {
    info_cols <- info_cols(points)
    if (is.numeric(sel))
      sel <- c(sel, info_cols)
    else
      sel <- c(sel, colnames(points)[info_cols])
  }

  points <- points[, sel, drop = FALSE]

  points
}
