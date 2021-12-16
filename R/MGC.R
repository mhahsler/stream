#######################################################################
# Moving Generator -  Infrastructure for Moving Streams
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

#' Moving Generator Cluster
#'
#' Creates an evolving cluster for a [DSD_MG].
#'
#' An `MGC` describes a single cluster for use within an [DSD_MG].
#' There are currently four different MGCs that allow a user to express
#' many different behaviors within a single data stream.
#'
#' An `MGC_Linear` creates an evolving Gaussian cluster for a
#' [DSD_MG] who's behavior is determined by several keyframes. Several keyframe
#' functions are provided to create, add and remove keyframes. See Examples section
#' for details.
#'
#' An `MGC_Function` allows for a creation of a [DSD_MG] that is
#' defined by functions of time.
#'
#' An `MGC_Random` allows for a creation of a [DSD_MG] that moves
#' randomly.
#'
#' An `MGC_Noise` allows for a creation of noise within a [DSD_MG].
#'
#' @param center A list that defines the center of the cluster. The list should
#' have a length equal to the dimensionality. For `MGC_Function`, this
#' list consists of functions that define the movement of the cluster. For
#' `MGC_Random`, this attribute defines the beginning location for the
#' `MGC` before it begins moving.
#' @param density The density of the cluster. For `MGC_Function, this
#' attribute is a function and defines the density of a cluster at a given
#' timestamp.
#' @param dimension Dimensionality of the data stream.
#' @param keyframelist a list of keyframes to initialize the `MGC_Linear`
#' object with.
#' @param parameter Parameters for the shape. For the default shape
#' `MGC_Shape_Gaussian` the parameter is the standard deviation, one per
#' dimension. If a single value is specified then it is recycled for all
#' dimensions.
#' @param randomness The maximum amount the cluster will move during one time
#' step.
#' @param range The area in which the noise should appear.
#' @param reset Should the cluster reset to the first keyframe (time 0) after
#' this keyframe is finished?
#' @param shape A function creating the shape of the cluster. It gets passed on
#' the parameters argument from above. Available functions are
#' `MGC_Shape_Gaussian` (the parameters are a vector containing standard
#' deviations) and `MGC_Shape_Block` (parameters are the dimensions of the
#' uniform block).
#' @param time The time stamp the keyframe should be located or which keyframe
#' should be removed.
#' @param x An object of class `MGC_Linear`.
#' @param ... Further arguments.
#' @author Matthew Bolanos
#' @seealso [DSD_MG] for details on how to use an `MGC` within
#' a [DSD].
#' @examples
#'
#' MGC()
#'
#' ### Two static clusters
#' stream <- DSD_MG(dim=2,
#'   MGC_Static(den = 1, center=c(1, 0), par=.1),
#'   MGC_Static(den = 1, center=c(2, 0), par=.4, shape=MGC_Shape_Block)
#' )
#'
#' plot(stream)
#'
#' ### Example of several MGC_Randoms
#' stream <- DSD_MG(dimension=2,
#'   MGC_Random(den = 100, center=c(1, 0), par=.1, rand=.1),
#'   MGC_Random(den = 100, center=c(2, 0), par=.4, shape=MGC_Shape_Block, rand=.1)
#' )
#'
#' \dontrun{
#'   animate_data(stream, 2500, xlim=c(0,3), ylim=c(-2,2), horizon=100)
#' }
#'
#'
#' ### Example of several MGC_Functions
#' stream <- DSD_MG(dim = 2)
#'
#' ### block-shaped cluster moving from bottom-left to top-right increasing size
#' c1 <- MGC_Function(
#'   density = function(t){100},
#'   parameter = function(t){1*t},
#'   center = function(t) c(t,t),
#'   shape = MGC_Shape_Block
#'   )
#' add_cluster(stream,c1)
#'
#' ### cluster moving in a circle (default shape is Gaussian)
#' c2 <- MGC_Function(
#'   density = function(t){25},
#'   parameter = function(t){5},
#'   center= function(t) c(sin(t/10)*50+50, cos(t/10)*50+50)
#' )
#' add_cluster(stream,c2)
#'
#' \dontrun{
#' animate_data(stream,10000,xlim=c(-20,120),ylim=c(-20,120), horizon=100)
#' }
#'
#' ### Example of several MGC_Linears: A single cluster splits at time 50 into two.
#' ### Note that c2 starts at time=50!
#' stream <- DSD_MG(dim = 2)
#' c1 <- MGC_Linear(dim = 2)
#' add_keyframe(c1, time=1,  dens=50, par=5, center=c(0,0))
#' add_keyframe(c1, time=50, dens=50, par=5, center=c(50,50))
#' add_keyframe(c1, time=100,dens=50, par=5, center=c(50,100))
#' add_cluster(stream,c1)
#'
#' c2 <- MGC_Linear(dim = 2, shape=MGC_Shape_Block)
#' add_keyframe(c2, time=50, dens=25, par=c(10,10),  center=c(50,50))
#' add_keyframe(c2, time=100,dens=25, par=c(30,30), center=c(100,50))
#' add_cluster(stream,c2)
#'
#' \dontrun{
#' animate_data(stream,5000,xlim=c(0,100),ylim=c(0,100), horiz=100)
#' }
#'
#' ### two fixed and a moving cluster
#' stream <- DSD_MG(dim = 2,
#'   MGC_Static(dens=1, par=.1, center=c(0,0)),
#'   MGC_Static(dens=1, par=.1, center=c(1,1)),
#'   MGC_Linear(dim=2,list(
#'     keyframe(time = 0, dens=1, par=.1, center=c(0,0)),
#'     keyframe(time = 1000, dens=1, par=.1, center=c(1,1)),
#'     keyframe(time = 2000, dens=1, par=.1, center=c(0,0), reset=TRUE)
#'   )))
#'
#' noise <- MGC_Noise(dens=.1, range=rbind(c(-.2,1.2),c(-.2,1.2)))
#' add_cluster(stream, noise)
#'
#' \dontrun{
#' animate_data(stream, n=2000*3.1, xlim=c(-.2,1.2), ylim=c(-.2,1.2), horiz=200)
#' }
#'
#'#' @export MGC
MGC <- abstract_class_generator("MGC")

print.MGC <- function(x, ...) {
  cat(paste(x$description, " (", paste(class(x), collapse=", "), ")", '\n', sep=""))
  cat(paste('In', x$RObj$dimension, 'dimensions', '\n'))
}
