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


#' A sliding window from a Data Stream
#' 
#' Interface for DSO_Window. Represents the points in the sliding window as
#' micro-clusters.
#' 
#' If \code{lambda} is greater than 0 then the weight uses a damped window
#' model (Zhu and Shasha, 2002).  The weight for points in the window follows
#' \eqn{2^{-lambda*t}} where \eqn{t} is the age of the point.
#' 
#' @param horizon the window length.
#' @param lambda decay factor damped window model. \code{lambda=0} means no
#' dampening.
#' @return An object of class \code{DSC_Window} (subclass of \code{DSC},
#' \code{DSC_R}, \code{DSC_Micro}).
#' @author Michael Hahsler
#' @seealso \code{\link{DSC}}, \code{\link{DSC_Micro}}
#' @references Zhu, Y. and Shasha, D. (2002). StatStream: Statistical
#' Monitoring of Thousands of Data Streams in Real Time, \emph{International
#' Conference of Very Large Data Bases (VLDB'02).}
#' @examples
#' 
#' stream <- DSD_Gaussians(k=3, d=2, noise=0.05)
#' 
#' window <- DSC_Window(horizon=100)
#' window
#' 
#' update(window, stream, 200)
#' window
#' 
#' # plot micro-clusters
#' plot(window, stream)
#' 
#' # animation for a window using a damped window model. The weight decays
#' # with a half-life of 25
#' \dontrun{
#' window <- DSC_Window(horizon=25, lambda=1/25)
#' animate_cluster(window, stream, horizon=1, n=100, xlim=c(0,1), ylim=c(0,1))
#' }
#' 
#' @export DSC_Window
DSC_Window <- function(horizon = 100, lambda=0) 
  structure(list(description = if(lambda>0) "Damped sliding window" else "Sliding window",
    RObj = WindowDSC$new(horizon = as.integer(horizon), lambda=lambda)),
    class = c("DSC_Window","DSC_Micro","DSC_R","DSC"))

                                       
