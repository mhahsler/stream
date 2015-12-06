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


## show data and evaluation measure animation (uses prequential error estimation)
## goes to plot (top plot) for now
animate_cluster <- function(dsc, dsd, measure, horizon=100, n=1000,
  type=c("auto", "micro", "macro"), assign="micro", 
  assignmentMethod=c("auto","model", "nn"),
  noise = c("class", "ignor"),
  wait=.1, plot.args = NULL, ...) { 
  
  assignmentMethod <- match.arg(assignmentMethod)
  noise <- match.arg(noise)
  type <- get_type(dsc, type)  
  
  cluster.ani(dsc, dsd, measure, horizon, n, type, assign, 
    assignmentMethod, noise, wait, plot.args, ...) 
}

animate_data <- function(dsd, horizon=100, n=1000, wait=.1, 
  plot.args = NULL, ...) { 
  cluster.ani(NULL, dsd, NULL, horizon, n, NULL, NULL, NULL, NULL, 
    wait, plot.args, ...)
}


## work horse
cluster.ani <- function(dsc, dsd, measure, horizon, n, type, assign, 
  assignmentMethod, noise, wait, plot.args, ...){
  
  if(is.null(plot.args)) plot.args <- list()
  plot.args <- c(plot.args, list(...))
   
  if(!is.null(measure) && length(measure)!=1) 
    stop("animate_cluster can only use a single measure!")
   
  rounds <- n %/% horizon 
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  animation::ani.record(reset = TRUE)
  
  ## setup layout for dsc + eval measure plotting (animate_cluster)
  if(!is.null(dsc)) {
    layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights=c(3,1.5))
    evaluation <- data.frame(points=seq(from=1, by=horizon, length.out=rounds))
    evaluation[[measure]] <- NA_real_
  }
  
  for(i in 1:rounds) {
    d <- DSD_Memory(dsd, n=horizon, loop=FALSE)
    
    if(!is.null(dsc)) {
      ## for animate_cluster
       
      ## evaluate first
      if(!is.null(measure)) {
        reset_stream(d)
        evaluation[i,2] <- evaluate(dsc, d, measure, horizon, 
          type, assign, assignmentMethod, noise, ...)
      }
        
      ## then cluster 
      reset_stream(d)
      update(dsc, d, horizon)
      
      ## then do plotting 
      if(!is.null(measure)) par(mar=c(4.1,4.1,2.1,2.1))
      reset_stream(d)
      ## no warnings for 0 clusters        
      suppressWarnings(do.call(plot, c(list(dsc, d, n=horizon), plot.args))) 
      
      if(!is.null(measure)){
        par(mar=c(2.1,4.1,1.1,2.1))
        
        if(all(is.na(evaluation[,2]))) 
          plot(evaluation, type="l", col="blue", ylim = c(0,1))
        else { 
          plot(evaluation, type="l", col="blue",
            #ylim=c(0,1), 
            ann=FALSE) 
          title(ylab=measure)        
        }
      }
     
    }else{  ## plot just data for animate_data
      suppressWarnings(do.call(plot, c(list(d, n=horizon), plot.args))) 
    }
    
    animation::ani.record()
    if(wait>0) Sys.sleep(wait)  
    
  }
  
  if(!is.null(measure)) evaluation
  else invisible(NULL)
}
