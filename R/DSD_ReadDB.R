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


DSD_ReadDB <- function(result, k=NA, o=NA,
                       class=NULL, outlier=NULL, description=NULL) {

  if(is.na(o) && !is.null(outlier))
    stop("The outlier column is defined, but the number of outliers is not supplied")
  if(!is.na(o) && is.null(outlier))
    stop("The number of outliers is supplied, but the outlier column was not supplied")

  # figure out d
  d <- length(DBI::dbColumnInfo(result))
  if(!is.null(class)) d <- d-1L
  if(!is.null(outlier)) d <- d-1L

  # creating the DSD object
  l <- list(
    description = if(is.null(description)) 'DB Query Stream' else description,
    d = d,
    k = k,
    o = o,
    result = result,
    class = class,
    outlier = outlier
  )
  class(l) <- c("DSD_ReadDB", "DSD_R", "DSD_data.frame", "DSD")

  l
}

get_points.DSD_ReadDB <- function(x, n=1,
                                  outofpoints=c("stop", "warn", "ignore"),
                                  cluster = FALSE, class = FALSE, outlier = FALSE, ...) {
  .nodots(...)

  outofpoints <- match.arg(outofpoints)
  n <- as.integer(n)

  d <- DBI::dbFetch(x$result, n = n)

  if(nrow(d) < n) {
    if(outofpoints == "stop") {
      stop("Not enough points in the stream!")
    }
    if(outofpoints == "warn")
      warning("The stream is at its end returning available points!")
  }

  cl <- NULL
  outs <- rep(FALSE,nrow(d))
  removal <- c()
  if(nrow(d) > 0) {
    if(!is.null(x$class)) {
      cl <- d[,x$class]
      removal <- c(x$class)
    }
    if(!is.null(x$outlier)) {
      outs <- d[,x$class]
      removal <- c(removal, x$outlier)
    }
  }
  d <- d[,-removal,drop=FALSE]

  if(class && !is.null(cl)) d <- cbind(d, class = cl)
  if(cluster) attr(d, "cluster") <- cl
  if(outlier) attr(d, "outlier") <- outs

  d
}
