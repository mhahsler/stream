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

### interface for DSO_Window (see DSO_Window.R)
DSC_Window <- function(horizon = 100, lambda=0) 
  structure(list(description = if(lambda>0) "Damped sliding window" else "Sliding window",
    RObj = WindowDSC$new(horizon = as.integer(horizon), lambda=lambda)),
    class = c("DSC_Window","DSC_Micro","DSC_R","DSC"))



