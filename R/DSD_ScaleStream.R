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


# accepts an open connection
DSD_ScaleStream <- function(dsd,  
  center=TRUE, scale=TRUE, 
  n=1000, reset=FALSE) {
  
  # creating the DSD object
  l <- list(description = paste(dsd$description, "(scaled)"),
    dsd = dsd,
    d = dsd$d,
    k = dsd$k,
    center = FALSE,
    scale = FALSE)
  class(l) <- c("DSD_ScaleStream", "DSD_R", "DSD_data.frame", "DSD")
  
  l <- scale_stream(l, n=n, center=center, scale=scale, reset=reset)
  
  l
}

## it is important that the connection is OPEN
get_points.DSD_ScaleStream <- function(x, n=1, 
  outofpoints=c("stop", "warn", "ignore"),
  cluster=FALSE, class=FALSE, ...) {
  .nodots(...)

  d <- get_points(x$dsd, n, cluster=cluster, class=class)
  
  if(cluster) cl <- attr(d, "cluster")
  if(class) {
    j <- which("class" == colnames(d))
    if(length(j)==1L) {
      cl <- d[, j]
      d <- d[, -j]
    }else cl <- rep(NA_integer_, nrow(d))
  }
  
  # scale
  d <- as.data.frame(scale(d, center= x$center, scale=x$scale))
  
  if(cluster) attr(d, "cluster") <- cl
  if(class) d <- cbind(d, class=cl)
  
  d
}

reset_stream.DSD_ScaleStream <- function(dsd, pos=1) {
  reset_stream(dsd$dsd, pos=pos)
}

scale_stream <- function(dsd, n=1000, center=TRUE, scale=TRUE, reset=FALSE) {
  
  sc <- scale(get_points(dsd, n=n), center=center, scale=scale)
  dsd$center <- attr(sc, "scaled:center")
  if(is.null(dsd$center)) dsd$center <- center
  dsd$scale <- attr(sc, "scaled:scale")
  
  if(is.null(dsd$scale)) dsd$scale <- scale
  else dsd$scale[dsd$scale==0] <- 1 # fix division by 0 if all values were the same
  
  if(reset) try(reset_stream(dsd), silent=TRUE)
  
  dsd
}
