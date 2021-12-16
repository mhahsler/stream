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

#' Abstract Class for Frequent Pattern Mining Algorithms for Data Streams
#'
#' Abstract class for frequent pattern mining algorithms for data streams.
#' Currently, \pkg{stream} does not implement frequent pattern mining
#' algorithms.
#'
#' @param ... Further arguments.
#' @author Michael Hahsler
#' @seealso \code{\link{DST}}
#' @examples
#' DSFP()
#' @export DSFP
DSFP <- abstract_class_generator("DSFP")
