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

DSC_SinglePass <- function(...) stop("DSC_SinglePass is an abstract class and cannot be instantiated!")
get_assignment.DSC_SinglePass <- function(dsc, points, type=c("auto", "micro", "macro"),
                                          method=c("auto", "nn", "model"), ...) {
  stop(gettextf("No assignments and update available for class '%s'.", paste(class(dsc), collapse=", ")))
}
