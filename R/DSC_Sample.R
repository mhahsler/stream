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

#' Extract a Fixed-size Sample from a Data Stream
#' 
#' Micro Clusterer.
#' Extracts a sample form a data stream using Reservoir Sampling (\code{DSO_Sample}).  The sample
#' is stored as a set of micro-clusters to be compatible with other data DSC
#' stream clustering algorithms.
#' 
#' If \code{biased=FALSE} then the reservoir sampling algorithm by McLeod and
#' Bellhouse (1983) is used. This sampling makes sure that each data point has
#' the same chance to be sampled. All sampled points will have a weight of 1.
#' Note that this might not be ideal for an evolving stream since very old data
#' points have the same chance to be in the sample as newer points.
#' 
#' If \code{bias=TRUE} then sampling prefers newer points using the modified
#' reservoir sampling algorithm 2.1 by Aggarwal (2006). New points are always
#' added. They replace a random point in thre reservoir with a probability of
#' reservoir size over \code{k}. This an exponential bias function of
#' \eqn{2^{-lambda}} with \eqn{lambda=1/k}.
#' 
#' @param k the number of points to be sampled from the stream.
#' @param biased if \code{FALSE} then a regular (unbiased) reservoir sampling
#' is used. If true then the sample is biased towards keeping more recent data
#' points (see Details section).
#' @return An object of class \code{DSC_Sample} (subclass of \code{DSC},
#' \code{DSC_R}, \code{DSC_Micro}).
#' @author Michael Hahsler
#' @seealso \code{\link{DSC}}, \code{\link{DSC_Micro}}
#' @references Vitter, J. S. (1985): Random sampling with a reservoir.
#' \emph{ACM Transactions on Mathematical Software,} 11(1), 37-57.
#' 
#' McLeod, A.I., Bellhouse, D.R. (1983): A Convenient Algorithm for Drawing a
#' Simple Random Sample. \emph{Applied Statistics,} 32(2), 182-184.
#' 
#' Aggarwal C. (2006) On Biased Reservoir Sampling in the Presence of Stream
#' Evolution. \emph{International Conference on Very Large Databases
#' (VLDB'06).} 607-618.
#' @examples
#' 
#' stream <- DSD_Gaussians(k=3, noise=0.05)
#' 
#' sample <- DSC_Sample(k=20)
#' update(sample, stream, 500)
#' sample
#' 
#' # plot micro-clusters
#' plot(sample, stream)
#' 
#' # reclustering
#' kmeans <- DSC_Kmeans(3)
#' recluster(kmeans, sample)
#' plot(kmeans, stream, type="both")
#' 
#' # sample from an evolving stream
#' stream <- DSD_Benchmark(1)
#' sample <- DSC_Sample(k=20)
#' update(sample, stream, 1000)
#' plot(sample, stream)
#' # Note: the clusters move from left to right and the sample keeps many
#' # outdated points
#' 
#' # use a biased sample to keep more recent data points
#' stream <- DSD_Benchmark(1)
#' sample <- DSC_Sample(k=20, biased=TRUE)
#' update(sample, stream, 1000)
#' plot(sample, stream)
#' 
#' @export DSC_Sample
DSC_Sample <- function(k = 100, biased = FALSE)
  structure(list(description =
      if(biased) "Reservoir sampling (biased)" else "Reservoir sampling",
    RObj = SampleDSC$new(k = k, biased = biased)),
    class = c("DSC_Sample","DSC_Micro","DSC_R","DSC"))

