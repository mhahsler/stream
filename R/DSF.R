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


#' Data Stream Filter Base Classes
#'
#' Abstract base classes for all data stream filter (DSF) classes. Data stream filters transform a data stream ([DSD]).
#'
#' The `DSF` class cannot be instantiated, but it serve as a base
#' class from which other DSF classes inherit.
#'
#' Data stream filters transform a [DSD] data stream.
#' DSF implementations inherit from [DSD] and have the same basic interface.
#'
#' @family DSF
#'
#' @param ... Further arguments.
#' @author Michael Hahsler
#' @examples
#' DSF()
#' @export DSF
DSF <- abstract_class_generator("DSF")

#' @describeIn DSF reset the attached stream.
#' @param dsd a stream object of class [DSD].
#' @param pos position in the stream.
#' @export
reset_stream.DSF <- function(dsd, pos = 1) {
  reset_stream(dsd$dsd, pos = pos)
}

