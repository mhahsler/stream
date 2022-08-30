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

#' Create a Data Stream Pipeline
#'
#' Define a complete data stream pipe line
#' consisting of a data stream, filters and a data mining task using `%>%`.
#'
#'
#' A data stream pipe line
#' consisting of a data stream, filters and a data mining task:
#'
#' ```DSD %>% DSF %>% DST```
#'
#' Once the pipeline is defined, it can be run using [update()] where points are
#' taken from the `DSD`, filtered through a sequence of `DSFs` and then used to update
#' the task `DST`.
#'
#' @family DST
#'
#' @param dsd A data stream (subclass of [DSD]) typically provided using a `%>%` (pipe).
#' @param dst A data stream mining task (subclass of [DST]).
#' @author Michael Hahsler
#' @examples
#' set.seed(1500)
#'
#' # Set up a pipeline with a DSD data source, DSF Filters and then a DST task
#' cluster_pipeline <- DSD_Gaussians(k = 3, d = 2) %>%
#'                     DSF_Scale() %>%
#'                     DST_Runner(DSC_DBSTREAM(r = .05))
#'
#' cluster_pipeline
#'
#' # the DSD and DST can be accessed directly
#' cluster_pipeline$dsd
#' cluster_pipeline$dst
#'
#' # update the DST using the pipeline
#' update(cluster_pipeline, n = 1000)
#'
#' cluster_pipeline$dst
#' get_centers(cluster_pipeline$dst)
#' plot(cluster_pipeline$dst)
#' @export
DST_Runner <- function(dsd, dst) {
  structure(
    list(
      description = paste("DST pipline runner",
        "\nDSD:", description(dsd),
        "\nDST:", description(dst)),
      dsd = dsd,
      dst = dst
    ),
    class = c("DST_Runner", "DST")
  )
}

#' @export
update.DST_Runner <- function(object, dsd = NULL, n = 1L, ...) {
  if (!is.null(dsd))
    stop("A dsd cannot be specified for update on DST_Runner!")
  update(object$dst, object$dsd, n = n, ...)
}
