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


DSD_ReadDB <- function(result, k=NA,
  class=NULL, description=NULL) {
  
  # figure out d
  d <- length(DBI::dbColumnInfo(result))
  if(!is.null(class)) d <- d -1L
  
  # creating the DSD object
  l <- list(
    description = if(is.null(description)) 'DB Query Stream' else description,
    d = d,
    k = k,
    result = result,
    class = class
  )
  
  class(l) <- c("DSD_ReadDB", "DSD_R", "DSD_data.frame", "DSD")
  
  l
}

get_points.DSD_ReadDB <- function(x, n=1, 
  outofpoints=c("stop", "warn", "ignore"), 
  cluster = FALSE, class = FALSE, ...) {
  .nodots(...)

  outofpoints <- match.arg(outofpoints)
  n <- as.integer(n) 
  
  ### FIXME: should be dbFetch!
  d <- DBI::fetch(x$result, n)
  
  if(nrow(d) < n) {
    if(outofpoints == "stop") {
      stop("Not enough points in the stream!")
    }
    if(outofpoints == "warn") 
      warning("The stream is at its end returning available points!")
  }
  
  cl <- NULL
  if(nrow(d) > 0) {
    if(!is.null(x$class)) {
      cl <- d[,x$class[1]] 
      d <- d[, -x$class[1]]
    }
  }

  if(cluster) attr(d, "cluster") <- cl
  if(class && !is.null(cl)) d <- cbind(d, class = cl)
  
  d
}
