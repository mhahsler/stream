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



DSD_mlbenchGenerator <- function(method, ...) {
  
  methods <- c("2dnormals","cassini","circle","cuboids","friedman1",
    "friedman2","friedman3","hypercube", "peak","ringnorm",
    "shapes","simplex","smiley","spirals","threenorm",
    "twonorm","waveform","xor")
 
  ### FIXME: It would be nice if we know k and d
   
  if(missing(method)) {
    cat("Available generators are:\n")
    print(methods)
    return()
  }
  
  #finds index of partial match in array of methods
  m <- pmatch(tolower(method), methods) 
  if(is.na(m)) stop("DSD_mlbenchGenerator: Invalid data generator")
  
  # creating the DSD object
  l <- list(description = paste("mlbench:", method),
    method = methods[m],
    variables = list(...)
  )
  class(l) <- c("DSD_mlbenchGenerator", "DSD_R", "DSD_data.frame", "DSD")
  l
}

get_points.DSD_mlbenchGenerator <- function(x, n=1, 
  outofpoints=c("stop", "warn", "ignore"), 
  cluster = FALSE, class = FALSE, ...) {
  .nodots(...)

  d <- do.call(paste("mlbench.", x$method,sep=""), c(list(n), x$variables))
  if(is.null(d$classes)) d$classes <- rep(NA_integer_, times = n)
  
  ## the data order needs to be scrambled...
  if(n > 1) {
    o <- sample(nrow(d$x))
    d$x <- d$x[o, , drop=FALSE]
    d$classes <- d$classes[o]
  }
  
  df <- as.data.frame(d$x)  
  names(df) <- paste("V", 1:ncol(df), sep = "")
  if(cluster) attr(df, "cluster") <- as.integer(d$classes)
  if(class) df <- cbind(df, class = as.integer(d$classes))
  
  df
}

