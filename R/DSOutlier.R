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

#' Abstract Class for Data Stream Outlier Detectors
#'
#' The abstract class for all data stream outlier detectors. Cannot be
#' instantiated. Some [DSC] implementations also implement outlier/noise
#' detection.
#'
#' [plot()] has an extra logical argument to specify if outliers should be plotted
#' as red crosses.
#'
#' @family DST
#' @family DSOutlier
#'
#' @param ... further arguments.
#' @docType class
#' @examples
#' DSOutlier()
#'
#' #' @examples
#' set.seed(1000)
#' stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.1, noise_separation = 5)
#'
#' outlier_detector <- DSOutlier_DBSTREAM(r = .05, outlier_multiplier = 2)
#' update(outlier_detector, stream, 500)
#' outlier_detector
#'
#' points <- get_points(stream, 20)
#' points
#'
#' # Outliers are predicted as class NA
#' predict(outlier_detector, points)
#'
#' # Plot new points from the stream. Predicted outliers are marked with a red x.
#' plot(outlier_detector, stream)
#'
#' evaluate_static(outlier_detector, stream, measure =
#'   c("noiseActual", "noisePredicted", "noisePrecision", "outlierJaccard"))
#'
#' # use a different detector
#' outlier_detector2 <- DSOutlier_DStream(gridsize = .05, Cl = 0.5, outlier_multiplier = 2)
#' update(outlier_detector2, stream, 500)
#' plot(outlier_detector2, stream)
#'
#' evaluate_static(outlier_detector2, stream, measure =
#'   c("noiseActual", "noisePredicted", "noisePrecision", "outlierJaccard"))
#' @author Michael Hahsler
#' @export
DSOutlier <- abstract_class_generator("DSOutlier")

#' @export
plot.DSOutlier <- function(x,
  dsd = NULL,
  n = 500,
  col_points = NULL,
  type = "none",
  pch = 4L,
  ...,
  outliers = TRUE) {

  if(outliers) {
    if (!is.null(dsd) && inherits(dsd, "DSD"))
      dsd <- get_points(dsd, n, info = FALSE)
    pr <- predict(x, dsd)
    col_points <-  ifelse(!is.na(pr[[".class"]]), .points_col, .outlier_col)
    pch <-  ifelse(!is.na(pr[[".class"]]), 1L, pch)
  }

  plot.DSC(x, dsd = dsd, col_points = col_points, pch = pch, type = type, ...)
}


